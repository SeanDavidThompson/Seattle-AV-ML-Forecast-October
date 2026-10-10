###############################################################################
# Residential driver audit -- land-delta and improvement-delta boosters
#
# Same three signatures as nonstationarity_audit.R, applied to every NWMLS
# (sea_*) and econ (econ_*) feature of the RESIDENTIAL delta models:
#
#   1. MONOTONE      the yearly series goes one way (a time index by another
#                    name).
#   2. OUT OF RANGE  forecast values fall outside the training range.
#   3. NO SPLITS     every forecast value lies beyond the outermost split, so
#                    the whole horizon lands in one leaf: a constant.
#
# Plus the residential-specific exhibit: a NEAREST-YEAR table.  For each
# forecast year x scenario, the training tax year whose 12-month mean median
# SFH price (sea_pmedesfh_lag12) is closest, that year's mean training target
# (land and improvement delta), and the forecast's mean predicted delta.
# With the price LEVEL in a delta model, this is the year the model is
# effectively treating the forecast year as ("TY2029 looks like TY20xx").
#
# Inputs (read only):
#   model_dir  latest lgb_land_delta_cv_* / lgb_impr_delta_cv_*  (the BARE
#              names are residential; lgb_<subgroup>_* are commercial)
#   cache_dir  model_data_land_delta_model.rds, model_data_impr_delta_model.rds
#              (training frames: training ranges and targets)
#              panel_tbl_2027_2031_inputs_<scenario>_res.rds, else
#              panel_tbl_2006_2031_inputs_<scenario>_res.rds (forecast inputs)
#              panel_tbl_2006_2031_forecasted_<scenario>_res.rds (predicted
#              deltas; optional)
# Writes CSVs to out_dir.  Changes nothing.
#
# Run (repo root as working directory):
#   source("ad_hoc/res_driver_audit.R")            # production model_dir
# or
#   RES_AUDIT_DEFINE_ONLY <- TRUE; source("ad_hoc/res_driver_audit.R")
#   res_driver_audit(model_dir = "./data/model_nwmls_growth",
#                    cache_dir = "./data/cache_nwmls_growth",
#                    tag = "growth")
#
# STYLE: no $ operator - it gets stripped in transit. Use x[["name"]].
###############################################################################

library(data.table)
library(lightgbm)

say <- function(...) cat("\n", paste0(...), "\n", sep = "")

###############################################################################
# Helpers
###############################################################################

# Residential delta boosters only.  "^lgb_land_delta_cv" cannot match
# lgb_apt_land_delta_cv_* etc.
find_res_booster <- function(model_dir, target) {
  pat <- sprintf("^lgb_%s_cv_.*\\.rds$", target)
  f <- list.files(model_dir, pattern = pat, full.names = TRUE)
  if (length(f) == 0) return(NULL)
  f[order(file.mtime(f), decreasing = TRUE)][1]
}

as_booster <- function(obj) {
  if (inherits(obj, "lgb.Booster")) return(obj)
  if (!is.list(obj)) return(NULL)
  for (nm in c("model", "booster"))
    if (inherits(obj[[nm]], "lgb.Booster")) return(obj[[nm]])
  NULL
}

model_mode <- function(obj) {
  m <- attr(obj, "nwmls_features", exact = TRUE)
  if (is.null(m)) "level (unstamped)" else m
}

monotone_share <- function(yearly) {
  v <- yearly[!is.na(yearly)]
  if (length(v) < 4) return(NA_real_)
  d <- diff(v)
  d <- d[d != 0]
  if (!length(d)) return(NA_real_)
  max(mean(d > 0), mean(d < 0))
}

# One value per tax_yr (market features are constant within a year) plus the
# row count, so shares can be row-weighted.
yearly_values <- function(d, feats, years = NULL) {
  if (!is.null(years)) d <- d[tax_yr %in% years]
  feats <- intersect(feats, names(d))
  if (!length(feats)) return(data.table(tax_yr = integer(0), n = integer(0)))
  out <- d[, c(list(n = .N),
               lapply(.SD, function(x) mean(suppressWarnings(as.numeric(x)),
                                            na.rm = TRUE))),
           by = tax_yr, .SDcols = feats]
  for (f in feats) set(out, i = which(is.nan(out[[f]])), j = f, value = NA_real_)
  setorder(out, tax_yr)
  out
}

read_panel_cols <- function(path, keep_pat, extra = character(0)) {
  d <- readRDS(path)
  setDT(d)
  keep <- unique(c("tax_yr", extra, grep(keep_pat, names(d), value = TRUE)))
  keep <- intersect(keep, names(d))
  d <- d[, keep, with = FALSE]
  d[, tax_yr := as.integer(tax_yr)]
  d
}

###############################################################################
# Audit
###############################################################################

res_driver_audit <- function(model_dir = "./data/model",
                             cache_dir = "./data/cache",
                             out_dir   = "./output/exhibits",
                             scenarios = c("baseline", "optimistic", "pessimistic"),
                             fcst_from = 2027L, hist_to = 2026L,
                             fcst_to   = 2031L, tag = NULL) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  sfx <- if (is.null(tag) || !nzchar(tag)) "" else paste0("_", tag)
  drv_pat <- "^(sea_|econ_)"

  # ---- boosters -------------------------------------------------------------
  targets <- c(land = "land_delta", impr = "impr_delta")
  bst <- list()
  for (k in names(targets)) {
    bp <- find_res_booster(model_dir, targets[[k]])
    if (is.null(bp)) stop("no lgb_", targets[[k]], "_cv_* in ", model_dir)
    obj <- readRDS(bp)
    b <- as_booster(obj)
    if (is.null(b) || is.null(obj[["x_cols"]]))
      stop(basename(bp), ": booster or x_cols missing")
    tr <- as.data.table(lgb.model.dt.tree(b))
    thr <- tr[!is.na(split_feature),
              .(n_splits = .N,
                thr_min  = min(threshold, na.rm = TRUE),
                thr_max  = max(threshold, na.rm = TRUE)),
              by = .(feature = split_feature)]
    imp <- as.data.table(lgb.importance(b))
    setnames(imp, c("Feature", "Gain"), c("feature", "gain"), skip_absent = TRUE)
    bst[[k]] <- list(file = basename(bp), mode = model_mode(obj),
                     xcols = obj[["x_cols"]], thr = thr, imp = imp)
    cat(k, " delta: ", basename(bp), " | nwmls_features = ", model_mode(obj),
        " | ", length(obj[["x_cols"]]), " features\n", sep = "")
  }

  # ---- training frames: ranges + targets by year ----------------------------
  tf <- list(
    land = file.path(cache_dir, "model_data_land_delta_model.rds"),
    impr = file.path(cache_dir, "model_data_impr_delta_model.rds"))
  tgt <- c(land = "delta_log_land", impr = "delta_log_impr")
  train_yr <- list()
  for (k in names(tf)) {
    if (!file.exists(tf[[k]])) stop("missing training frame ", tf[[k]])
    d <- read_panel_cols(tf[[k]], drv_pat, extra = tgt[[k]])
    train_yr[[k]] <- yearly_values(d, setdiff(names(d), "tax_yr"))
    rm(d); gc(verbose = FALSE)
  }

  # ---- forecast inputs + predicted deltas, one scenario at a time -----------
  fc_yr <- list(); pred_yr <- list()
  for (sc in scenarios) {
    p_in <- file.path(cache_dir, sprintf("panel_tbl_%d_%d_inputs_%s_res.rds",
                                         fcst_from, fcst_to, sc))
    if (!file.exists(p_in))
      p_in <- file.path(cache_dir, sprintf("panel_tbl_2006_2031_inputs_%s_res.rds", sc))
    if (!file.exists(p_in)) {
      cat("  ", sc, ": no extended inputs panel - skipping\n", sep = "")
      next
    }
    cat("  ", sc, ": reading ", basename(p_in), " ...\n", sep = "")
    d <- read_panel_cols(p_in, drv_pat)
    fc_yr[[sc]] <- yearly_values(d, setdiff(names(d), "tax_yr"),
                                 years = fcst_from:fcst_to)
    rm(d); gc(verbose = FALSE)

    p_fc <- file.path(cache_dir, sprintf("panel_tbl_2006_2031_forecasted_%s_res.rds", sc))
    if (file.exists(p_fc)) {
      cat("  ", sc, ": reading ", basename(p_fc), " ...\n", sep = "")
      d <- read_panel_cols(p_fc, "^$",
                           extra = c("land_method", "impr_method",
                                     "log_land_filled", "log_land_filled_lag1",
                                     "log_impr_filled", "log_impr_filled_lag1"))
      d <- d[tax_yr >= fcst_from & tax_yr <= fcst_to]
      pred_yr[[sc]] <- merge(
        d[land_method == "delta",
          .(pred_land_delta = mean(log_land_filled - log_land_filled_lag1, na.rm = TRUE),
            n_land_delta = .N), by = tax_yr],
        d[impr_method == "delta",
          .(pred_impr_delta = mean(log_impr_filled - log_impr_filled_lag1, na.rm = TRUE),
            n_impr_delta = .N), by = tax_yr],
        by = "tax_yr", all = TRUE)
      rm(d); gc(verbose = FALSE)
    }
  }
  if (!length(fc_yr)) stop("no forecast panels found in ", cache_dir)

  # ---- feature table ----------------------------------------------------------
  rows <- list()
  for (k in names(bst)) {
    b  <- bst[[k]]
    ty <- train_yr[[k]][tax_yr <= hist_to]
    feats <- grep(drv_pat, b[["xcols"]], value = TRUE)
    for (f in feats) {
      hv <- if (f %in% names(ty)) ty[[f]] else NA_real_
      for (sc in names(fc_yr)) {
        fy <- fc_yr[[sc]]
        fv <- if (f %in% names(fy)) fy[[f]] else rep(NA_real_, nrow(fy))
        w  <- fy[["n"]]
        th <- b[["thr"]][feature == f]
        n_spl <- if (nrow(th)) th[["n_splits"]][1] else 0L
        t_min <- if (nrow(th)) th[["thr_min"]][1] else NA_real_
        t_max <- if (nrow(th)) th[["thr_max"]][1] else NA_real_
        ok <- !is.na(fv)
        # LightGBM sends x <= threshold left, so "below every split" is
        # x <= thr_min and "above every split" is x > thr_max.
        sh_lo <- if (n_spl > 0 && any(ok)) sum(w[ok][fv[ok] <= t_min]) / sum(w[ok]) else NA_real_
        sh_hi <- if (n_spl > 0 && any(ok)) sum(w[ok][fv[ok] >  t_max]) / sum(w[ok]) else NA_real_
        g <- b[["imp"]][feature == f, gain]
        rows[[length(rows) + 1L]] <- data.table(
          model = k, booster = b[["file"]], nwmls_features = b[["mode"]],
          feature = f, group = if (grepl("^sea_", f)) "nwmls" else "econ",
          scenario = sc,
          gain_pct = if (length(g)) 100 * g[1] else 0,
          mono = monotone_share(hv),
          train_min = suppressWarnings(min(hv, na.rm = TRUE)),
          train_max = suppressWarnings(max(hv, na.rm = TRUE)),
          fc_min = suppressWarnings(min(fv, na.rm = TRUE)),
          fc_max = suppressWarnings(max(fv, na.rm = TRUE)),
          n_splits = n_spl, thr_min = t_min, thr_max = t_max,
          share_fc_below_all_splits = sh_lo,
          share_fc_above_all_splits = sh_hi,
          fc_all_na = !any(ok))
      }
    }
  }
  ft <- rbindlist(rows, fill = TRUE)
  for (cn in c("train_min", "train_max", "fc_min", "fc_max"))
    set(ft, i = which(!is.finite(ft[[cn]])), j = cn, value = NA_real_)
  ft[, sig_monotone := !is.na(mono) & mono >= 0.85]
  ft[, sig_out_of_range := !is.na(fc_min) & !is.na(train_max) &
         (fc_min > train_max | fc_max < train_min)]
  ft[, sig_no_splits := n_splits > 0 & !is.na(fc_min) &
         (thr_max < fc_min | thr_min > fc_max)]
  ft[, n_sig := as.integer(sig_monotone) + as.integer(sig_out_of_range) +
         as.integer(sig_no_splits)]
  setorder(ft, model, scenario, -gain_pct)

  # ---- nearest-year table ----------------------------------------------------
  key_f <- "sea_pmedesfh_lag12"
  tl <- train_yr[["land"]][tax_yr <= hist_to]
  ti <- train_yr[["impr"]][tax_yr <= hist_to]
  # growth-mode delta frames carry no price level; the improvement LEVEL
  # frame keeps tax_yr and the six levels in both modes.
  price_yr <- if (key_f %in% names(tl)) tl[, .(tax_yr, price = get(key_f))] else {
    p_lvl <- file.path(cache_dir, "model_data_impr_level_model.rds")
    if (file.exists(p_lvl)) {
      d <- read_panel_cols(p_lvl, paste0("^", key_f, "$"))
      out <- if (key_f %in% names(d))
        yearly_values(d, key_f)[tax_yr <= hist_to, .(tax_yr, price = get(key_f))]
      else NULL
      rm(d); gc(verbose = FALSE)
      out
    } else NULL
  }
  nrow_l <- list()
  if (!is.null(price_yr) && nrow(price_yr)) {
    hist <- merge(price_yr,
                  merge(tl[, .(tax_yr, mean_target_land = delta_log_land)],
                        ti[, .(tax_yr, mean_target_impr = delta_log_impr)],
                        by = "tax_yr", all = TRUE),
                  by = "tax_yr", all.x = TRUE)
    hist <- hist[!is.na(price)]
    for (sc in names(fc_yr)) {
      fy <- fc_yr[[sc]]
      if (!key_f %in% names(fy)) next
      for (i in seq_len(nrow(fy))) {
        v <- fy[[key_f]][i]
        if (is.na(v)) next
        j <- which.min(abs(hist[["price"]] - v))
        pr <- pred_yr[[sc]]
        pr <- if (is.null(pr)) NULL else pr[tax_yr == fy[["tax_yr"]][i]]
        nrow_l[[length(nrow_l) + 1L]] <- data.table(
          scenario = sc, fcst_tax_yr = fy[["tax_yr"]][i],
          fcst_price_lag12 = v,
          nearest_train_tax_yr = hist[["tax_yr"]][j],
          nearest_price_lag12 = hist[["price"]][j],
          nearest_mean_target_land_delta = hist[["mean_target_land"]][j],
          nearest_mean_target_impr_delta = hist[["mean_target_impr"]][j],
          fcst_mean_pred_land_delta = if (length(pr) && nrow(pr)) pr[["pred_land_delta"]][1] else NA_real_,
          fcst_mean_pred_impr_delta = if (length(pr) && nrow(pr)) pr[["pred_impr_delta"]][1] else NA_real_)
      }
    }
  } else {
    cat("NOTE: no training-year ", key_f, " found - nearest-year table skipped.\n",
        sep = "")
  }
  ny <- rbindlist(nrow_l, fill = TRUE)

  # ---- write -------------------------------------------------------------------
  f1 <- file.path(out_dir, paste0("res_driver_audit_features", sfx, ".csv"))
  f2 <- file.path(out_dir, paste0("res_driver_audit_nearest_year", sfx, ".csv"))
  fwrite(ft, f1)
  fwrite(ny, f2)

  # ---- plain-English summary ---------------------------------------------------
  say("=== RESIDENTIAL DRIVER AUDIT", if (nzchar(sfx)) paste0(" [", tag, "]") else "", " ===")
  for (k in names(bst)) {
    g <- unique(ft[model == k, .(feature, group, gain_pct)])
    cat(sprintf("%s delta (%s): NWMLS features carry %.1f%% of gain, econ %.1f%%.\n",
                k, bst[[k]][["mode"]],
                g[group == "nwmls", sum(gain_pct)], g[group == "econ", sum(gain_pct)]))
    top <- g[order(-gain_pct)][1:min(3L, .N)]
    cat("  top drivers: ", paste0(top[["feature"]], " ", sprintf("%.1f%%", top[["gain_pct"]]),
                                  collapse = ", "), "\n", sep = "")
  }
  fl <- ft[n_sig > 0 & gain_pct >= 1]
  if (nrow(fl)) {
    cat("\nFlagged drivers with >= 1% gain (any signature):\n")
    print(fl[, .(model, scenario, feature, gain_pct = round(gain_pct, 1),
                 mono = round(mono, 2), out_of_range = sig_out_of_range,
                 no_splits = sig_no_splits,
                 below = round(share_fc_below_all_splits, 2),
                 above = round(share_fc_above_all_splits, 2))])
  } else {
    cat("\nNo driver with >= 1% gain shows any of the three signatures.\n")
  }
  if (nrow(ny)) {
    cat("\nNearest training year by 12-month mean median SFH price:\n")
    print(ny[, .(scenario, fcst_tax_yr,
                 fcst_price = round(fcst_price_lag12),
                 looks_like = nearest_train_tax_yr,
                 that_yr_land = round(nearest_mean_target_land_delta, 4),
                 that_yr_impr = round(nearest_mean_target_impr_delta, 4),
                 pred_land = round(fcst_mean_pred_land_delta, 4),
                 pred_impr = round(fcst_mean_pred_impr_delta, 4))])
    for (sc in unique(ny[["scenario"]])) {
      z <- ny[scenario == sc]
      cat(sprintf("  %s: the model treats %s.\n", sc,
                  paste0("TY", z[["fcst_tax_yr"]], " like TY",
                         z[["nearest_train_tax_yr"]], collapse = ", ")))
    }
    cat("If scenarios map to different training years, the forecast differences\n",
        "between them come from those years' history, not from the price path.\n",
        sep = "")
  }
  say("CSVs: ", f1, " | ", f2)
  invisible(list(features = ft, nearest_year = ny))
}

if (!isTRUE(get0("RES_AUDIT_DEFINE_ONLY", envir = .GlobalEnv,
                 ifnotfound = FALSE)))
  res_driver_audit()
