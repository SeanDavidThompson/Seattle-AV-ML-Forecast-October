# 06_forecast_av_2026_2031_sequential.R ----------------------------------
# Forecast land + improvements AV for 2026–2031 sequentially (recursive).
#
# Strategy per year, per component (delta models only):
#   Predict delta_log = f(lag, parcel features, exogenous indicators)
#   log_AV[t] = log_AV[t-1] + delta_log[t]
#
# Runs over all three scenario input panels: baseline, optimistic, pessimistic.
# Each scenario is saved independently to cache + wrangled.
# -------------------------------------------------------------------------

cache_dir  <- get("cache_dir",  envir = .GlobalEnv)
model_dir  <- get("model_dir",  envir = .GlobalEnv)
output_dir <- get("output_dir", envir = .GlobalEnv)
scenario   <- get("scenario",   envir = .GlobalEnv)

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Set options(ml_forecast_verbose = TRUE) before sourcing to enable debug output
verbose <- getOption("ml_forecast_verbose", default = FALSE)

# =========================================================================
# 0) Prediction helpers
# =========================================================================

# Safely expand newdata through a caret dummyVars object, matching training
# factor levels and column types exactly.
safe_predict_dummyvars <- function(dv, newdata, train_df, seed_n = 100) {
  response  <- all.vars(dv$form)[1]
  term_vars <- setdiff(all.vars(dv$terms), response)

  nd <- as.data.frame(newdata)
  for (m in setdiff(term_vars, names(nd))) nd[[m]] <- NA

  # Small seed from training data anchors factor levels for predict.dummyVars
  seed_n  <- min(seed_n, nrow(train_df))
  seed_df <- as.data.frame(train_df[sample.int(nrow(train_df), seed_n),
                                    term_vars, drop = FALSE])

  # Enforce stored factor levels
  if (!is.null(dv$lvls)) {
    for (v in intersect(names(dv$lvls), term_vars)) {
      lvls         <- dv$lvls[[v]]
      nd[[v]]      <- factor(as.character(nd[[v]]),      levels = lvls)
      seed_df[[v]] <- factor(as.character(seed_df[[v]]), levels = lvls)
    }
  }

  # Coerce types to match training data
  for (v in intersect(term_vars, names(train_df))) {
    tr <- train_df[[v]]
    if (is.numeric(tr) || is.integer(tr)) {
      nd[[v]]      <- as.numeric(nd[[v]])
      seed_df[[v]] <- as.numeric(seed_df[[v]])
    } else if (is.logical(tr)) {
      nd[[v]]      <- as.logical(nd[[v]])
      seed_df[[v]] <- as.logical(seed_df[[v]])
      # ensure seed has both levels so dummyVars doesn't collapse the column
      if (length(unique(na.omit(seed_df[[v]]))) < 2)
        seed_df[[v]][1] <- !isTRUE(seed_df[[v]][1])
    } else if (is.factor(tr)) {
      nd[[v]]      <- factor(as.character(nd[[v]]),      levels = levels(tr))
      seed_df[[v]] <- factor(as.character(seed_df[[v]]), levels = levels(tr))
    } else {
      lvls         <- sort(unique(as.character(tr)))
      nd[[v]]      <- factor(as.character(nd[[v]]),      levels = lvls)
      seed_df[[v]] <- factor(as.character(seed_df[[v]]), levels = lvls)
    }
  }

  nd      <- nd[,      term_vars, drop = FALSE]
  seed_df <- seed_df[, term_vars, drop = FALSE]

  # predict.dummyVars checks for ALL formula variables including the response.
  # Add a dummy response column (value irrelevant — only predictors are encoded).
  nd[[response]]      <- NA_real_
  seed_df[[response]] <- NA_real_

  mm <- predict(dv, newdata = rbind(seed_df, nd))
  mm[(seed_n + 1):(seed_n + nrow(nd)), , drop = FALSE]
}


# Predict from a LightGBM + dummyVars pair.
predict_lgbm_safe <- function(lgb_model, dv, feature_names,
                               newdata, train_df, seed_n = 2000) {
  mm <- safe_predict_dummyvars(dv, newdata, train_df, seed_n = seed_n)
  mm <- as.matrix(mm)

  if (ncol(mm) == 0) {
    warning("dummyVars produced 0 columns — returning NA.")
    return(rep(NA_real_, nrow(mm)))
  }

  # Pad columns the model expects but dummyVars didn't produce
  missing_cols <- setdiff(feature_names, colnames(mm))
  if (length(missing_cols) > 0) {
    mm <- cbind(mm, matrix(0, nrow(mm), length(missing_cols),
                           dimnames = list(NULL, missing_cols)))
  }
  mm <- mm[, feature_names, drop = FALSE]

  if (verbose) {
    message("  matrix: ", nrow(mm), " x ", ncol(mm),
            " | padded: ", length(missing_cols),
            " | NA rate: ", round(mean(is.na(mm)), 4))
  }

  preds <- as.numeric(predict(lgb_model, mm))

  if (length(preds) != nrow(mm)) {
    warning("LightGBM returned ", length(preds), " preds for ",
            nrow(mm), " rows — returning NA.")
    return(rep(NA_real_, nrow(mm)))
  }
  preds
}


# Predict in chunks to manage memory on large panels.
predict_in_chunks <- function(pred_fn, newdata, chunk_size = 50000) {
  n   <- nrow(newdata)
  out <- rep(NA_real_, n)
  idx <- split(seq_len(n), ceiling(seq_len(n) / chunk_size))
  for (i in seq_along(idx)) {
    if (verbose) message("  chunk ", i, "/", length(idx))
    out[idx[[i]]] <- pred_fn(newdata[idx[[i]], , drop = FALSE])
    gc()
  }
  out
}


# Align panel column types to a training data frame for a given set of columns.
align_to_train <- function(panel_dt, train_df, cols) {
  for (cn in intersect(cols, intersect(names(train_df), names(panel_dt)))) {
    tr <- train_df[[cn]]
    if (is.factor(tr)) {
      panel_dt[, (cn) := factor(as.character(get(cn)), levels = levels(tr))]
    } else if (is.numeric(tr)) {
      panel_dt[, (cn) := as.numeric(get(cn))]
    } else if (is.logical(tr)) {
      panel_dt[, (cn) := toupper(trimws(as.character(get(cn)))) %in%
                          c("Y", "YES", "TRUE", "1")]
    } else {
      panel_dt[, (cn) := as.character(get(cn))]
    }
  }
  panel_dt
}


# Median-impute numeric NAs; coerce factor NAs to "Unknown" for dv term vars.
impute_for_prediction <- function(newdata, train_df, dv) {
  term_vars <- intersect(all.vars(dv$terms), names(newdata))
  if (length(term_vars) == 0) return(newdata)
  for (cn in term_vars) {
    if (!cn %in% names(train_df)) next
    tr <- train_df[[cn]]
    if (is.numeric(tr)) {
      newdata[[cn]][is.na(newdata[[cn]])] <- median(tr, na.rm = TRUE)
    } else if (is.factor(tr)) {
      newdata[[cn]] <- factor(as.character(newdata[[cn]]), levels = levels(tr))
      newdata[[cn]] <- forcats::fct_na_value_to_level(newdata[[cn]], level = "Unknown")
    }
  }
  newdata
}


# Drop predictors with <2 stored factor levels from a dummyVars object.
drop_single_level_dv_vars <- function(dv) {
  if (is.null(dv$lvls)) return(dv)
  bad <- names(dv$lvls)[sapply(dv$lvls, length) < 2]
  if (length(bad) == 0) return(dv)
  message("  Dropping dv vars with <2 levels: ", paste(bad, collapse = ", "))
  dv$vars <- setdiff(unlist(dv$vars, use.names = FALSE), bad)
  dv$lvls[bad] <- NULL
  if (!is.null(dv$facVars)) dv$facVars <- setdiff(dv$facVars, bad)
  dv
}

# =========================================================================
# 1) Load cached object to .GlobalEnv if not already present
# =========================================================================
load_cache_if_missing <- function(obj_name, path) {
  if (exists(obj_name, envir = .GlobalEnv)) return(invisible(NULL))
  if (!file.exists(path)) stop("Missing cache: ", path)
  assign(obj_name, readRDS(path), envir = .GlobalEnv)
  message("✅ loaded: ", obj_name)
}

# =========================================================================
# 2) Load delta models + training frames (once, shared across scenarios)
# =========================================================================
latest_model_file <- function(prefix, dir = model_dir) {
  files <- list.files(dir,
                      pattern    = paste0("^", prefix, "(_|\\.).*\\.rds$"),
                      full.names = TRUE)
  if (length(files) == 0) return(NULL)
  files[which.max(file.info(files)$mtime)]
}

load_model <- function(obj_name, prefix) {
  if (exists(obj_name, envir = .GlobalEnv)) return(invisible(NULL))
  f <- latest_model_file(prefix)
  if (is.null(f)) stop("No model file found for: ", obj_name,
                       "\nRun with model_replicate=TRUE to train models.")
  assign(obj_name, readRDS(f), envir = .GlobalEnv)
  message("✅ loaded: ", obj_name, " (", basename(f), ")")
}

load_model("lgb_land_delta_cv", "lgb_land_delta_cv")
load_model("dv_land_delta",     "dv_land_delta")
load_model("lgb_impr_delta_cv", "lgb_impr_delta_cv")
load_model("dv_impr_delta",     "dv_impr_delta")

# NWMLS feature mode: the boosters must have been trained in the run's mode
# (nwmls_features, default "level"; unstamped pre-2026-10 models = "level").
if (!exists("nwmls_assert_model_mode", mode = "function"))
  source(here::here("scripts", "ml", "nwmls_features.R"))
nwmls_assert_model_mode(
  list(lgb_land_delta_cv = lgb_land_delta_cv,
       lgb_impr_delta_cv = lgb_impr_delta_cv),
  nwmls_run_mode(), where = "Step 6 (residential)")

# Expose booster + feature aliases
lgb_land_delta_model    <- lgb_land_delta_cv$model
lgb_land_delta_features <- lgb_land_delta_cv$x_cols
lgb_impr_delta_model    <- lgb_impr_delta_cv$model
lgb_impr_delta_features <- lgb_impr_delta_cv$x_cols

# Fix caret serialisation quirk + clean single-level vars
for (.nm in c("dv_land_delta", "dv_impr_delta")) {
  obj     <- get(.nm, envir = .GlobalEnv)
  obj$sep <- ""
  assign(.nm, drop_single_level_dv_vars(obj), envir = .GlobalEnv)
}

# Training frames
load_cache_if_missing("model_data_land_delta_model",
                      file.path(cache_dir, "model_data_land_delta_model.rds"))
load_cache_if_missing("model_data_impr_delta_model",
                      file.path(cache_dir, "model_data_impr_delta_model.rds"))

train_land_df <- model_data_land_delta_model
train_impr_df <- model_data_impr_delta_model

# Predictor column sets (resolved against panel inside the loop)
pred_cols_land_base <- setdiff(
  names(train_land_df),
  c("delta_log_land", "log_appr_land_val", "log_land_filled")
)
pred_cols_impr_base <- setdiff(
  names(train_impr_df),
  c("delta_log_impr", "log_appr_imps_val", "log_impr_filled")
)

# =========================================================================
# 3) Load extended panel for the current scenario
# =========================================================================
# main_ml.R saves the residential extended panel as:
#   panel_tbl_2006_2031_inputs_<scenario>_res  (in GlobalEnv)
# or on disk as:
#   panel_tbl_2006_2031_inputs_<scenario>_res.rds
ext_name       <- paste0("panel_tbl_2006_2031_inputs_", scenario, "_res")
ext_cache      <- file.path(cache_dir, paste0(ext_name, ".rds"))
ext_cache_nosuffix <- file.path(cache_dir,
                                paste0("panel_tbl_2006_2031_inputs_", scenario, ".rds"))

if (exists(ext_name, envir = .GlobalEnv)) {
  panel_all <- as.data.table(get(ext_name, envir = .GlobalEnv))
} else if (file.exists(ext_cache)) {
  panel_all <- as.data.table(readRDS(ext_cache))
  message("Loaded residential extended panel from cache: ", basename(ext_cache))
} else if (file.exists(ext_cache_nosuffix)) {
  panel_all <- as.data.table(readRDS(ext_cache_nosuffix))
  message("Loaded residential extended panel from legacy cache: ", basename(ext_cache_nosuffix))
} else {
  stop("Extended residential panel not found. Run 05_extend_panel_2026_2031.R first.")
}

# =========================================================================
# 4) Forecast — single scenario pass
# =========================================================================
all_method_counts <- list()

for (scenario_name in scenario) {  # single iteration — kept for structure

  message("\n", strrep("=", 60))
  message("SCENARIO: ", toupper(scenario_name))
  message(strrep("=", 60))

  # Drop data.table join shadow columns if any
  shadow_cols <- grep("^i\\.", names(panel_all), value = TRUE)
  if (length(shadow_cols) > 0) {
    panel_all[, (shadow_cols) := NULL]
    message("Dropped ", length(shadow_cols), " shadow columns (i.*)")
  }

  stopifnot(all(c("parcel_id", "tax_yr") %in% names(panel_all)))
  setorder(panel_all, parcel_id, tax_yr)
  setkey(panel_all, NULL)

  # Determine history boundary and forecast range dynamically
  hist_max_yr <- max(panel_all[!is.na(total_assessed_filled), tax_yr], na.rm = TRUE)
  # seq_len() form: a one-year horizon (hist_max_yr + 1 == forecast_end) gives
  # one year, and hist_max_yr >= forecast_end gives none, never a descending
  # sequence the way `:` would.
  .fcst_end   <- as.integer(get0("forecast_end", envir = .GlobalEnv,
                                 ifnotfound = 2031L))
  fcst_years  <- hist_max_yr + seq_len(max(0L, .fcst_end - hist_max_yr))
  message("History max year: ", hist_max_yr,
          " | Forecasting: ", paste(fcst_years, collapse = ", "))

  # ---- Forecast-year feature check --------------------------------------
  # Every raw (non-dummy) feature the delta boosters split on must be present
  # and not all-NA in each forecast year; otherwise impute_for_prediction()
  # quietly gives the whole year the training median.  The lag columns are
  # rebuilt inside the year loop, so they are not checked here.  growth
  # mode: stop(); level mode: warning() (NWMLS_GROWTH_DECISION.md).
  .used_feats <- function(cv, train_df) {
    f <- intersect(cv$x_cols, names(train_df))
    imp <- tryCatch(lgb.importance(cv$model)$Feature, error = function(e) NULL)
    if (length(imp)) f <- intersect(f, imp)
    f
  }
  nwmls_assert_forecast_features(
    panel_all,
    features = grep("^log_appr_(land|imps)_val_lag[0-9]+$",
                    union(.used_feats(lgb_land_delta_cv, train_land_df),
                          .used_feats(lgb_impr_delta_cv, train_impr_df)),
                    value = TRUE, invert = TRUE),
    years = fcst_years, mode = nwmls_run_mode(),
    where = "Step 6 (residential)")

  # ---- Seed log-scale columns -------------------------------------------
  panel_all[, appr_land_val_filled := as.numeric(appr_land_val_filled)]
  panel_all[, appr_imps_val_filled := as.numeric(appr_imps_val_filled)]

  panel_all[, appr_land_val_base :=
              fifelse(!is.na(appr_land_val_filled),
                      appr_land_val_filled, appr_land_val)]
  panel_all[, appr_imps_val_base :=
              fifelse(!is.na(appr_imps_val_filled),
                      appr_imps_val_filled, appr_imps_val)]

  panel_all[, log_land_filled :=
              fifelse(appr_land_val_base > 0, log(appr_land_val_base), NA_real_)]
  panel_all[, log_impr_filled :=
              fifelse(appr_imps_val_base > 0, log(appr_imps_val_base), NA_real_)]

  panel_all[, land_method := fifelse(
    tax_yr <= hist_max_yr & !is.na(log_land_filled), "observed", NA_character_)]
  panel_all[, impr_method := fifelse(
    tax_yr <= hist_max_yr & !is.na(log_impr_filled), "observed", NA_character_)]

  # ---- Sequential forecast ----------------------------------------------
  for (yr in fcst_years) {
    message("\n--- tax_yr = ", yr, " ---")
    idx_yr <- which(panel_all$tax_yr == yr)
    if (length(idx_yr) == 0) { message("No rows — skipping."); next }

    # Rebuild lags from the just-updated log columns (the recursive step)
    panel_all[, log_land_filled_lag1 := shift(log_land_filled, 1L, type = "lag"),
              by = parcel_id]
    panel_all[, log_impr_filled_lag1 := shift(log_impr_filled, 1L, type = "lag"),
              by = parcel_id]

    # Lag aliases expected by the trained models
    panel_all[, log_appr_land_val_lag1 := shift(log_land_filled, 1L, type = "lag"),
              by = parcel_id]
    panel_all[, log_appr_land_val_lag2 := shift(log_land_filled, 2L, type = "lag"),
              by = parcel_id]
    panel_all[, log_appr_imps_val_lag1 := shift(log_impr_filled, 1L, type = "lag"),
              by = parcel_id]
    panel_all[, log_appr_imps_val_lag2 := shift(log_impr_filled, 2L, type = "lag"),
              by = parcel_id]

    message("  rows=", length(idx_yr),
            " | land_NA=", sum(is.na(panel_all$log_land_filled[idx_yr])),
            " | impr_NA=", sum(is.na(panel_all$log_impr_filled[idx_yr])))

    # ------------------------------------------------------------------
    # ACTUALS ANCHOR — use reported area growth instead of ML
    # ------------------------------------------------------------------
    # If area_report_actuals (Step 0) contains residential population-basis
    # growth for this assessment year, apply it directly:
    #   log_AV[t] = log_AV[t-1] + log(1 + actual_pct_change_area)
    # to BOTH land and impr, so parcel totals grow exactly by the reported
    # rate.  Parcels in areas without a report fall through to the ML delta
    # blocks below (which only fill rows still NA).
    if (exists("area_report_actuals", envir = .GlobalEnv)) {
      act <- data.table::as.data.table(
        get("area_report_actuals", envir = .GlobalEnv))
      # Reports for assessment year A describe the A -> A+1 tax-roll change
      # (1/1/A revalue posts to the A+1 tax roll), so they anchor tax_yr A+1.
      act <- act[prop_type == "res" & basis == "population" &
                   assessment_yr + 1L == yr,
                 .(area_int    = suppressWarnings(as.integer(area)),
                   dlog_actual = log1p(pct_change))]
      act <- act[!is.na(area_int) & is.finite(dlog_actual)]

      if (nrow(act) > 0 && "area" %in% names(panel_all)) {
        panel_all[, area_int_tmp :=
                    suppressWarnings(as.integer(as.character(area)))]
        panel_all[, dlog_actual_tmp := NA_real_]
        panel_all[act, on = .(area_int_tmp = area_int),
                  dlog_actual_tmp := i.dlog_actual]

        rows_act_land <- idx_yr[
          !is.na(panel_all$dlog_actual_tmp[idx_yr]) &
            !is.na(panel_all$log_land_filled_lag1[idx_yr]) &
            is.na(panel_all$log_land_filled[idx_yr])
        ]
        if (length(rows_act_land) > 0) {
          panel_all[rows_act_land,
                    log_land_filled := log_land_filled_lag1 + dlog_actual_tmp]
          panel_all[rows_act_land,
                    appr_land_val_filled := exp(log_land_filled)]
          panel_all[rows_act_land, land_method := "actual"]
        }

        rows_act_impr <- idx_yr[
          !is.na(panel_all$dlog_actual_tmp[idx_yr]) &
            !is.na(panel_all$log_impr_filled_lag1[idx_yr]) &
            is.na(panel_all$log_impr_filled[idx_yr])
        ]
        if (length(rows_act_impr) > 0) {
          panel_all[rows_act_impr,
                    log_impr_filled := log_impr_filled_lag1 + dlog_actual_tmp]
          panel_all[rows_act_impr,
                    appr_imps_val_filled := exp(log_impr_filled)]
          panel_all[rows_act_impr, impr_method := "actual"]
        }

        n_areas_hit <- panel_all[idx_yr][!is.na(dlog_actual_tmp),
                                         data.table::uniqueN(area_int_tmp)]
        message("  actuals anchor (", yr, "): land=", length(rows_act_land),
                " | impr=", length(rows_act_impr),
                " rows across ", n_areas_hit,
                " areas; unmatched rows fall back to ML")
        panel_all[, c("area_int_tmp", "dlog_actual_tmp") := NULL]
      } else if (nrow(act) > 0) {
        message("  actuals anchor: 'area' column missing from panel — ",
                "using ML for ", yr)
      }
    }

    # ------------------------------------------------------------------
    # LAND delta
    # ------------------------------------------------------------------
    rows_land <- idx_yr[
      !is.na(panel_all$log_land_filled_lag1[idx_yr]) &
        is.na(panel_all$log_land_filled[idx_yr])
    ]

    if (length(rows_land) > 0) {
      pc      <- intersect(pred_cols_land_base, names(panel_all))
      dv_vars <- setdiff(all.vars(dv_land_delta$terms),
                         all.vars(dv_land_delta$form)[1])
      panel_all <- align_to_train(panel_all, train_land_df, union(pc, dv_vars))

      nd <- impute_for_prediction(
        as.data.frame(panel_all[rows_land, ..pc]),
        train_land_df, dv_land_delta
      )
      preds <- predict_in_chunks(
        function(df) predict_lgbm_safe(
          lgb_land_delta_model, dv_land_delta, lgb_land_delta_features,
          df, train_land_df
        ), nd
      )
      panel_all[rows_land, log_land_filled      := log_land_filled_lag1 + preds]
      panel_all[rows_land, appr_land_val_filled  := exp(log_land_filled)]
      panel_all[rows_land, land_method           := "delta"]
      message("  land: ", length(rows_land), " rows predicted")
    } else {
      message("  land: no eligible rows (lag missing or already filled)")
    }

    # ------------------------------------------------------------------
    # IMPROVEMENTS delta
    # ------------------------------------------------------------------
    rows_impr <- idx_yr[
      !is.na(panel_all$log_impr_filled_lag1[idx_yr]) &
        is.na(panel_all$log_impr_filled[idx_yr])
    ]

    if (length(rows_impr) > 0) {
      pc      <- intersect(pred_cols_impr_base, names(panel_all))
      dv_vars <- setdiff(all.vars(dv_impr_delta$terms),
                         all.vars(dv_impr_delta$form)[1])
      panel_all <- align_to_train(panel_all, train_impr_df, union(pc, dv_vars))

      nd <- impute_for_prediction(
        as.data.frame(panel_all[rows_impr, ..pc]),
        train_impr_df, dv_impr_delta
      )
      preds <- predict_in_chunks(
        function(df) predict_lgbm_safe(
          lgb_impr_delta_model, dv_impr_delta, lgb_impr_delta_features,
          df, train_impr_df
        ), nd
      )
      tmp <- panel_all$log_impr_filled_lag1[rows_impr] + preds
      set(panel_all, rows_impr, "log_impr_filled",      tmp)
      set(panel_all, rows_impr, "appr_imps_val_filled",  exp(tmp))
      panel_all[rows_impr, impr_method := "delta"]
      message("  impr: ", length(rows_impr), " rows predicted")
    } else {
      message("  impr: no eligible rows (lag missing or already filled)")
    }

    # ------------------------------------------------------------------
    # Totals
    # ------------------------------------------------------------------
    # LAND-ONLY PARCELS.  Unlike the commercial and condo tracks, this script
    # has no improvement LEVEL fallback: rows_impr requires a non-NA
    # log_impr_filled_lag1, and a parcel with no improvement history never has
    # one.  So no buildings are invented here - the delta model simply never
    # fires for them.
    #
    # The consequence is different: appr_imps_val_filled stays NA, and
    # land + NA = NA, so the parcel drops out of total_assessed_filled
    # entirely rather than carrying its land value.  Coalescing to 0 keeps
    # land-only parcels in the population at their correct value.
    #
    # Only parcels with land history and NO improvement history are treated
    # this way.  A parcel whose improvement value merely failed to predict
    # this year is left as NA so it stays visible.
    if (isTRUE(get0("forecast_gate_land_only",
                    envir = .GlobalEnv, ifnotfound = TRUE))) {
      if (!exists(".res_land_only_ids", inherits = FALSE)) {
        .rl <- panel_all[tax_yr <= hist_max_yr, .(
          ever_imps = any(!is.na(appr_imps_val_base) & appr_imps_val_base > 0),
          ever_land = any(!is.na(appr_land_val_base) & appr_land_val_base > 0)
        ), by = parcel_id]
        .res_land_only_ids <- .rl[ever_imps == FALSE & ever_land == TRUE,
                                  parcel_id]
        message("  land-only: ", scales::comma(length(.res_land_only_ids)),
                " of ", scales::comma(nrow(.rl)),
                " parcels have land but no improvement history through ",
                hist_max_yr, " - improvements treated as 0 in totals")
        rm(.rl)
      }
      if (length(.res_land_only_ids)) {
        .lo_rows <- idx_yr[
          panel_all$parcel_id[idx_yr] %in% .res_land_only_ids &
            is.na(panel_all$appr_imps_val_filled[idx_yr])
        ]
        if (length(.lo_rows) > 0) {
          set(panel_all, .lo_rows, "appr_imps_val_filled", 0)
          panel_all[.lo_rows, impr_method := "land_only"]
          message("  land-only: ", length(.lo_rows),
                  " rows set to 0 improvements")
        }
        rm(.lo_rows)
      }
    }

    panel_all[idx_yr,
              total_assessed_filled := appr_land_val_filled + appr_imps_val_filled]
    panel_all[idx_yr,
              log_total_assessed_filled := fifelse(
                total_assessed_filled > 0, log(total_assessed_filled), NA_real_)]

    message("  total NA=",
            sum(is.na(panel_all$total_assessed_filled[idx_yr])))
  }

  # ---- Method summary for this scenario ---------------------------------
  method_counts <- as_tibble(panel_all) |>
    filter(tax_yr > hist_max_yr) |>
    group_by(tax_yr) |>
    summarise(
      n           = n(),
      land_actual = sum(land_method == "actual", na.rm = TRUE),
      land_delta  = sum(land_method == "delta", na.rm = TRUE),
      land_na     = sum(is.na(land_method)),
      impr_actual = sum(impr_method == "actual", na.rm = TRUE),
      impr_delta  = sum(impr_method == "delta", na.rm = TRUE),
      impr_na     = sum(is.na(impr_method)),
      .groups = "drop"
    ) |>
    mutate(scenario = scenario_name)

  all_method_counts[[scenario_name]] <- method_counts
  print(method_counts, n = 100)

  # ---- Write outputs ----------------------------------------------------
  panel_out <- as_tibble(panel_all)

  rds_path <- file.path(
    cache_dir,
    paste0("panel_tbl_2006_2031_forecasted_", scenario_name, "_res.rds")
  )
  saveRDS(panel_out, rds_path)
  message("💾 cached: ", basename(rds_path))

  parquet_path <- file.path(
    output_dir,
    paste0("parcel_year_panel_2006_2031_forecasted_", scenario_name, "_res.parquet")
  )
  arrow::write_parquet(panel_all, parquet_path)
  message("💾 parquet: ", basename(parquet_path))
}

# =========================================================================
# 5) Combined method summary across all scenarios
# =========================================================================
all_method_counts <- bind_rows(all_method_counts)
message("\n--- Method counts, all scenarios ---")
print(all_method_counts, n = 200)

assign("panel_tbl_forecasted_res", panel_out, envir = .GlobalEnv)
message("\n06_forecast_av_2026_2031_sequential.R complete (scenario = ", scenario, ").")
