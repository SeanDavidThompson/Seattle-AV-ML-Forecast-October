###############################################################################
# NWMLS directional sensitivity -- residential
#
# The question: if house prices come in lower, does our forecast come in
# lower?  Re-run the residential Step 6 forecast on already-trained models
# with the baseline NWMLS median SFH price multiplied by 0.95 / 1.00 / 1.05
# in the FORECAST months only (after the last observed month, detected from
# the export, never hard-coded).  Everything else is the baseline run.
#
#   PASS  residential total AV is ordered -5% < base < +5% in every year
#         2028-2031.  (TY2027 is mostly anchored to the KCA area reports and
#         is shown but not judged.)
#
# Works for either nwmls_features mode: the mode is read from the models'
# stamp, and the derived NWMLS columns for that mode are rebuilt from the
# shocked monthly series.
#
# Read-only on production: models are read from model_dir, the extended
# baseline panel and training frames from cache_dir.  Every write goes under
# scratch_dir (refused if it is a production directory).  Writes one CSV to
# out_dir.  Nothing is trained.
#
# Run (fresh R session, repo root as working directory):
#   source("ad_hoc/nwmls_sensitivity.R")      # defines + runs on defaults
# or, to point at the growth A/B directories:
#   NWMLS_SENS_DEFINE_ONLY <- TRUE
#   source("ad_hoc/nwmls_sensitivity.R")
#   run_nwmls_sensitivity(model_dir   = "data/model_nwmls_growth",
#                         cache_dir   = "data/cache_nwmls_growth",
#                         scratch_dir = "data/scratch_nwmls_sens_growth")
#
# STYLE: no $ operator - it gets stripped in transit. Use x[["name"]].
###############################################################################

suppressPackageStartupMessages({ library(data.table); library(here) })

MAIN_ML_DEFINE_ONLY <- TRUE
source(here::here("scripts", "ml", "main_ml.R"))   # CFG + nwmls_features.R

# Residential total AV, the av_fcst_summary() definition: filled value where
# present, else observed; land + improvements, NA counted as 0.
sens_res_av <- function(dt) {
  dt <- as.data.table(dt)
  lf <- if ("appr_land_val_filled" %in% names(dt))
    fifelse(!is.na(dt[["appr_land_val_filled"]]), dt[["appr_land_val_filled"]],
            dt[["appr_land_val"]]) else dt[["appr_land_val"]]
  im <- if ("appr_imps_val_filled" %in% names(dt))
    fifelse(!is.na(dt[["appr_imps_val_filled"]]), dt[["appr_imps_val_filled"]],
            dt[["appr_imps_val"]]) else dt[["appr_imps_val"]]
  data.table(parcel_id = dt[["parcel_id"]], tax_yr = as.integer(dt[["tax_yr"]]),
             av = fifelse(is.na(lf), 0, as.numeric(lf)) +
                  fifelse(is.na(im), 0, as.numeric(im)))
}

# Total AV by year, and matched-parcel growth: parcels with AV > 0 in both
# t-1 and t, sum(t) / sum(t-1) - 1.
sens_summarise <- function(av, years) {
  tot <- av[tax_yr %in% years, .(total_av = sum(av)), by = tax_yr]
  prev <- av[av > 0, .(parcel_id, tax_yr = tax_yr + 1L, av_prev = av)]
  m <- merge(av[av > 0 & tax_yr %in% years], prev, by = c("parcel_id", "tax_yr"))
  g <- m[, .(matched_growth = sum(av) / sum(av_prev) - 1, n_matched = .N),
         by = tax_yr]
  out <- merge(tot, g, by = "tax_yr", all.x = TRUE)
  setorder(out, tax_yr)
  out
}

sens_latest <- function(dir, prefix) {
  f <- list.files(dir, pattern = paste0("^", prefix, "(_|\\.).*\\.rds$"),
                  full.names = TRUE)
  if (!length(f)) stop("no ", prefix, "* in ", dir)
  f[which.max(file.info(f)[["mtime"]])]
}

run_nwmls_sensitivity <- function(
    model_dir      = CFG[["model_dir"]],
    cache_dir      = CFG[["cache_dir"]],
    scratch_dir    = NULL,
    shocks         = c(minus5 = 0.95, base = 1.00, plus5 = 1.05),
    nwmls_dir      = here::here("data", "nwmls"),
    forecast_start = CFG[["forecast_start"]],
    forecast_end   = CFG[["forecast_end"]],
    judge_years    = 2028:2031,
    use_area_actuals = TRUE,
    out_dir        = here::here("output", "exhibits")) {

  t0 <- Sys.time()
  forecast_start <- as.integer(forecast_start)
  forecast_end   <- as.integer(forecast_end)
  stopifnot(names(shocks) %in% c("minus5", "base", "plus5"),
            length(shocks) == 3L)

  # ---- models and mode ------------------------------------------------------
  f_land <- sens_latest(model_dir, "lgb_land_delta_cv")
  f_impr <- sens_latest(model_dir, "lgb_impr_delta_cv")
  lgb_land_delta_cv <- readRDS(f_land)
  lgb_impr_delta_cv <- readRDS(f_impr)
  mode <- nwmls_model_mode(lgb_land_delta_cv)
  if (nwmls_model_mode(lgb_impr_delta_cv) != mode)
    stop("land and improvement delta models disagree on nwmls_features: ",
         mode, " vs ", nwmls_model_mode(lgb_impr_delta_cv))
  cat("\nModels (", mode, " mode):\n  ", basename(f_land), "\n  ",
      basename(f_impr), "\n", sep = "")

  # ---- scratch dir: never a production location ----------------------------
  if (is.null(scratch_dir))
    scratch_dir <- here::here("data", paste0("scratch_nwmls_sens_", mode))
  dir.create(scratch_dir, recursive = TRUE, showWarnings = FALSE)
  np <- function(p) normalizePath(p, winslash = "/", mustWork = FALSE)
  protected <- unique(np(c(cache_dir, model_dir, CFG[["cache_dir"]],
                           CFG[["model_dir"]], CFG[["output_dir"]],
                           here::here("data", "cache"),
                           here::here("data", "model"),
                           here::here("data", "outputs"), nwmls_dir)))
  if (np(scratch_dir) %in% protected)
    stop("scratch_dir must not be a production directory: ", scratch_dir)
  cat("Scratch: ", np(scratch_dir), "\n", sep = "")

  # ---- monthly series, observed boundary -----------------------------------
  raw <- nwmls_read_raw("baseline", nwmls_dir)
  cat("NWMLS export: ", basename(raw[["file"]]), "\n", sep = "")
  last_obs <- nwmls_last_observed_month(raw[["data"]])
  cat("Last observed month: ", format(last_obs, "%Y-%m"),
      " -> shocking months after it\n", sep = "")

  # ---- extended baseline panel (read once) ---------------------------------
  ext_file <- file.path(cache_dir, paste0("panel_tbl_", forecast_start, "_",
                                          forecast_end, "_inputs_baseline_res.rds"))
  if (!file.exists(ext_file))
    ext_file <- file.path(cache_dir, "panel_tbl_2006_2031_inputs_baseline_res.rds")
  if (!file.exists(ext_file))
    stop("no residential extended baseline panel in ", cache_dir)
  cat("Extended panel: ", basename(ext_file), " ...\n", sep = "")
  ext <- as.data.table(readRDS(ext_file))
  fut <- which(ext[["tax_yr"]] >= forecast_start & ext[["tax_yr"]] <= forecast_end)

  # The unshocked rebuild must reproduce what the extend put in the panel;
  # otherwise the export on disk is not the one the panel was built from.
  base_ann <- nwmls_build_annual(raw[["data"]])[["annual"]]
  chk <- intersect(NWMLS_LEVEL_COLS, names(ext))
  for (cn in chk) {
    v_panel <- ext[fut, .(v = get(cn)[1L]), by = tax_yr]
    v_ann   <- base_ann[[cn]][match(v_panel[["tax_yr"]], base_ann[["tax_yr"]])]
    d <- max(abs(v_panel[["v"]] / v_ann - 1), na.rm = TRUE)
    if (!is.finite(d) || d > 1e-9)
      stop("NWMLS export ", basename(raw[["file"]]), " does not reproduce ", cn,
           " in ", basename(ext_file), " (max rel diff ", signif(d, 3), ").  ",
           "The extended panel was built from a different export vintage.")
  }
  cat("Unshocked rebuild reproduces the panel's NWMLS levels exactly.\n")

  # ---- what Step 6 needs in .GlobalEnv -------------------------------------
  ge <- .GlobalEnv
  assign("kca_date_data_extracted", CFG[["kca_date_data_extracted"]], envir = ge)
  source(here::here("scripts", "ml", "00_init.R"), local = ge)
  assign("lgb_land_delta_cv", lgb_land_delta_cv, envir = ge)
  assign("lgb_impr_delta_cv", lgb_impr_delta_cv, envir = ge)
  assign("dv_land_delta", readRDS(sens_latest(model_dir, "dv_land_delta")), envir = ge)
  assign("dv_impr_delta", readRDS(sens_latest(model_dir, "dv_impr_delta")), envir = ge)
  for (nm in c("model_data_land_delta_model", "model_data_impr_delta_model")) {
    p <- file.path(cache_dir, paste0(nm, ".rds"))
    if (!file.exists(p)) stop("missing training frame ", p)
    assign(nm, readRDS(p), envir = ge)
  }
  if (exists("area_report_actuals", envir = ge)) rm("area_report_actuals", envir = ge)
  if (isTRUE(use_area_actuals)) {
    p <- file.path(cache_dir, paste0("area_report_actuals_", forecast_start - 1L, ".rds"))
    if (file.exists(p)) {
      assign("area_report_actuals", readRDS(p), envir = ge)
      cat("Area-report anchors: ", basename(p), " (as in production)\n", sep = "")
    } else {
      cat("NOTE: ", basename(p), " not in cache_dir - running without anchors\n", sep = "")
    }
  }
  assign("nwmls_features", mode,           envir = ge)
  assign("scenario",       "baseline",     envir = ge)
  assign("forecast_start", forecast_start, envir = ge)
  assign("forecast_end",   forecast_end,   envir = ge)
  assign("model_dir",      model_dir,      envir = ge)

  need_cols <- setdiff(names(base_ann), "tax_yr")
  res <- list()
  for (tag in names(shocks)) {
    fac <- shocks[[tag]]
    cat("\n=== ", tag, " (price x ", fac, " after ", format(last_obs, "%Y-%m"),
        ") ===\n", sep = "")
    ann <- nwmls_build_annual(
      nwmls_shock_price(raw[["data"]], fac, last_obs))[["annual"]]
    # Overwrite every NWMLS column in the forecast rows, as 05_extend does
    m <- match(ext[["tax_yr"]][fut], ann[["tax_yr"]])
    for (cn in need_cols) {
      if (!cn %in% names(ext)) ext[, (cn) := NA_real_]
      set(ext, i = fut, j = cn, value = ann[[cn]][m])
    }
    d <- file.path(scratch_dir, tag)
    assign("cache_dir",  file.path(d, "cache"),   envir = ge)
    assign("output_dir", file.path(d, "outputs"), envir = ge)
    dir.create(file.path(d, "cache"), recursive = TRUE, showWarnings = FALSE)
    assign("panel_tbl_2006_2031_inputs_baseline_res", ext, envir = ge)
    source(here::here("scripts", "ml", "06_forecast_av_2026_2031_sequential.R"),
           local = ge)
    fc <- get("panel_tbl_forecasted_res", envir = ge)
    s <- sens_summarise(sens_res_av(fc), (forecast_start - 1L):forecast_end)
    s[, case := tag]
    res[[tag]] <- s
    rm("panel_tbl_forecasted_res", envir = ge)
    rm(fc); gc(verbose = FALSE)
  }
  rm("panel_tbl_2006_2031_inputs_baseline_res", envir = ge)

  long <- rbindlist(res)
  wide <- dcast(long, tax_yr ~ case,
                value.var = c("total_av", "matched_growth"))
  wide[, `:=`(pct_minus5_vs_base = total_av_minus5 / total_av_base - 1,
              pct_plus5_vs_base  = total_av_plus5  / total_av_base - 1)]
  ok_yr <- wide[tax_yr %in% judge_years,
                total_av_minus5 < total_av_base & total_av_base < total_av_plus5]
  pass <- length(ok_yr) == length(judge_years) && all(ok_yr)
  wide[, nwmls_features := mode]

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_csv <- file.path(out_dir, paste0("nwmls_sensitivity_", mode, ".csv"))
  fwrite(wide, out_csv)

  cat("\n=== Residential total AV, NWMLS price -5% / base / +5% (", mode,
      " models) ===\n", sep = "")
  show <- wide[, .(tax_yr,
                   minus5_bn = round(total_av_minus5 / 1e9, 2),
                   base_bn   = round(total_av_base   / 1e9, 2),
                   plus5_bn  = round(total_av_plus5  / 1e9, 2),
                   minus5_vs_base = sprintf("%+.3f%%", 100 * pct_minus5_vs_base),
                   plus5_vs_base  = sprintf("%+.3f%%", 100 * pct_plus5_vs_base),
                   g_minus5 = sprintf("%.2f%%", 100 * matched_growth_minus5),
                   g_base   = sprintf("%.2f%%", 100 * matched_growth_base),
                   g_plus5  = sprintf("%.2f%%", 100 * matched_growth_plus5))]
  print(show)
  bad <- wide[tax_yr %in% judge_years][!ok_yr, tax_yr]
  cat("\n", if (pass) "PASS" else "FAIL", "  [", mode, "]  total AV ordered ",
      "-5% < base < +5% in every year ", min(judge_years), "-", max(judge_years),
      if (!pass) paste0(" -- fails in ", paste(bad, collapse = ", ")) else "",
      "\n", sep = "")
  cat("CSV: ", out_csv, "\nElapsed: ",
      round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), " min\n",
      sep = "")
  invisible(list(table = wide, pass = pass, mode = mode))
}

if (!isTRUE(get0("NWMLS_SENS_DEFINE_ONLY", envir = .GlobalEnv,
                 ifnotfound = FALSE)))
  run_nwmls_sensitivity()
