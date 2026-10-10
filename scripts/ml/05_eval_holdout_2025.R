# scripts/ml/05_eval_holdout_2025.R ----------------------------
# Evaluate LightGBM delta models on 2023–2025 observed parcels.
# Cache-first: if diagnostics_replicate=FALSE and outputs exist, loads and exits.
# When called from main_ml.R, models are already loaded — no re-sourcing needed.
# ---------------------------------------------------------------------

message("Running 05_eval_holdout_2025.R ...")

# ---- 0) Config ----------------------------------------------------------
kca_date_data_extracted <- get0("kca_date_data_extracted", envir = .GlobalEnv,
                                ifnotfound = "2025-10-10")
diagnostics_replicate   <- get0("diagnostics_replicate",   envir = .GlobalEnv,
                                ifnotfound = TRUE)
cache_dir <- get0("cache_dir", envir = .GlobalEnv,
                  ifnotfound = here("data", "cache"))
model_dir <- get0("model_dir", envir = .GlobalEnv,
                  ifnotfound = here("data", "model"))

out_dir <- file.path(
  get0("output_dir", envir = .GlobalEnv, ifnotfound = here("data", "outputs")),
  "eval_2025"
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Residential paths
metrics_path_land <- file.path(out_dir, paste0("metrics_land_2025_", kca_date_data_extracted, ".csv"))
metrics_path_impr <- file.path(out_dir, paste0("metrics_impr_2025_", kca_date_data_extracted, ".csv"))
pred_path_land    <- file.path(out_dir, paste0("pred_vs_actual_land_2025_", kca_date_data_extracted, ".csv"))
pred_path_impr    <- file.path(out_dir, paste0("pred_vs_actual_impr_2025_", kca_date_data_extracted, ".csv"))
# Commercial paths
metrics_path_com_land <- file.path(out_dir, paste0("metrics_com_land_2025_", kca_date_data_extracted, ".csv"))
metrics_path_com_impr <- file.path(out_dir, paste0("metrics_com_impr_2025_", kca_date_data_extracted, ".csv"))
# Condo paths
metrics_path_condo_land <- file.path(out_dir, paste0("metrics_condo_land_2025_", kca_date_data_extracted, ".csv"))
metrics_path_condo_impr <- file.path(out_dir, paste0("metrics_condo_impr_2025_", kca_date_data_extracted, ".csv"))

# ---- 0.5) Cache-first exit ----------------------------------------------
if (!diagnostics_replicate &&
    file.exists(metrics_path_land) &&
    file.exists(metrics_path_impr) &&
    file.exists(pred_path_land)    &&
    file.exists(pred_path_impr)) {

  message("diagnostics_replicate=FALSE: loading cached 2025 evaluation outputs...")

  metrics_land <- read_csv(metrics_path_land, show_col_types = FALSE)
  metrics_impr <- read_csv(metrics_path_impr, show_col_types = FALSE)
  metrics_all  <- bind_rows(metrics_land, metrics_impr)

  # Load commercial metrics if available
  if (file.exists(metrics_path_com_land) && file.exists(metrics_path_com_impr)) {
    metrics_com <- bind_rows(
      read_csv(metrics_path_com_land, show_col_types = FALSE),
      read_csv(metrics_path_com_impr, show_col_types = FALSE)
    )
    metrics_all <- bind_rows(metrics_all, metrics_com)
  }

  # Load condo metrics if available
  if (file.exists(metrics_path_condo_land) && file.exists(metrics_path_condo_impr)) {
    metrics_condo <- bind_rows(
      read_csv(metrics_path_condo_land, show_col_types = FALSE),
      read_csv(metrics_path_condo_impr, show_col_types = FALSE)
    )
    metrics_all <- bind_rows(metrics_all, metrics_condo)
  }

  assign("metrics_all",  metrics_all,  envir = .GlobalEnv)

  land_results <- read_csv(pred_path_land, show_col_types = FALSE)
  impr_results <- read_csv(pred_path_impr, show_col_types = FALSE)
  assign("land_results", land_results, envir = .GlobalEnv)
  assign("impr_results", impr_results, envir = .GlobalEnv)

  message("✅ Loaded cached eval results from: ", out_dir)
  return(invisible(list(metrics_all  = metrics_all,
                        land_results = land_results,
                        impr_results = impr_results)))
}

# ---- 1) Guard: source 00_init.R only if helpers aren't already loaded ---
# When called from main_ml.R, 00_init.R has already been sourced.
if (!exists("train_lgbm_log_model", envir = .GlobalEnv)) {
  assign("kca_date_data_extracted", kca_date_data_extracted, envir = .GlobalEnv)
  source(here("scripts", "ml", "00_init.R"))
}

# ---- 2) Load panel_tbl from cache if missing ----------------------------
if (!exists("panel_tbl", envir = .GlobalEnv)) {
  panel_path <- file.path(cache_dir, "panel_tbl.rds")
  if (!file.exists(panel_path)) stop(
    "Missing panel cache: ", panel_path,
    "\nRun run_main_ml(replicate=TRUE) once to build panel cache."
  )
  assign("panel_tbl", readRDS(panel_path), envir = .GlobalEnv)
  message("✅ loaded panel_tbl from cache")
}

# ---- 3) Load models if not already in memory ----------------------------
# Models are loaded by main_ml.R step 3 when called from the orchestrator.
# When running standalone, fall back to finding the newest file by prefix.

.latest_model_file <- function(prefix) {
  pat   <- paste0("^", prefix, ".*\\.rds$")
  files <- list.files(model_dir, pattern = pat, full.names = TRUE)
  if (length(files) == 0) return(NULL)
  files[which.max(file.info(files)$mtime)]
}

.load_if_missing <- function(obj_name, prefix, required = TRUE) {
  if (exists(obj_name, envir = .GlobalEnv)) return(invisible(NULL))
  f <- .latest_model_file(prefix)
  if (is.null(f)) {
    if (required) stop(
      "No model file found for: ", obj_name,
      "\nExpected in: ", model_dir,
      "\nRun run_main_ml(model_replicate=TRUE) to train models."
    )
    return(invisible(NULL))
  }
  assign(obj_name, readRDS(f), envir = .GlobalEnv)
  message("✅ loaded ", obj_name, " from: ", basename(f))
}

.load_if_missing("lgb_land_delta_cv", "lgb_land_delta_cv")
.load_if_missing("dv_land_delta",     "dv_land_delta")
.load_if_missing("lgb_impr_delta_cv", "lgb_impr_delta_cv")
.load_if_missing("dv_impr_delta",     "dv_impr_delta")

# NWMLS feature mode: the residential boosters must have been trained in the
# run's mode (nwmls_features, default "level"; unstamped = "level").
if (!exists("nwmls_assert_model_mode", mode = "function"))
  source(here::here("scripts", "ml", "nwmls_features.R"))
nwmls_assert_model_mode(
  list(lgb_land_delta_cv = lgb_land_delta_cv,
       lgb_impr_delta_cv = lgb_impr_delta_cv),
  nwmls_run_mode(), where = "Step 5a eval (residential)")

# Commercial models (optional — eval skipped if absent)
.load_if_missing("lgb_com_land_delta_cv", "lgb_comm_land_delta_cv", required = FALSE)
.load_if_missing("dv_com_land_delta",     "dv_comm_land_delta",     required = FALSE)
.load_if_missing("lgb_com_impr_delta_cv", "lgb_comm_impr_delta_cv", required = FALSE)
.load_if_missing("dv_com_impr_delta",     "dv_comm_impr_delta",     required = FALSE)

# Condo models (optional — eval skipped if absent)
.load_if_missing("lgb_condo_land_delta_cv", "lgb_condo_land_delta_cv", required = FALSE)
.load_if_missing("dv_condo_land_delta",     "dv_condo_land_delta",     required = FALSE)
.load_if_missing("lgb_condo_impr_delta_cv", "lgb_condo_impr_delta_cv", required = FALSE)
.load_if_missing("dv_condo_impr_delta",     "dv_condo_impr_delta",     required = FALSE)

# Expose booster + feature aliases — residential
if (!exists("lgb_land_delta_model", envir = .GlobalEnv)) {
  assign("lgb_land_delta_model",    lgb_land_delta_cv$model,  envir = .GlobalEnv)
  assign("lgb_land_delta_features", lgb_land_delta_cv$x_cols, envir = .GlobalEnv)
}
if (!exists("lgb_impr_delta_model", envir = .GlobalEnv)) {
  assign("lgb_impr_delta_model",    lgb_impr_delta_cv$model,  envir = .GlobalEnv)
  assign("lgb_impr_delta_features", lgb_impr_delta_cv$x_cols, envir = .GlobalEnv)
}

# Expose booster + feature aliases — commercial (if loaded)
if (exists("lgb_com_land_delta_cv", envir = .GlobalEnv) &&
    !exists("lgb_com_land_delta_model", envir = .GlobalEnv)) {
  assign("lgb_com_land_delta_model",    lgb_com_land_delta_cv$model,  envir = .GlobalEnv)
  assign("lgb_com_land_delta_features", lgb_com_land_delta_cv$x_cols, envir = .GlobalEnv)
}
if (exists("lgb_com_impr_delta_cv", envir = .GlobalEnv) &&
    !exists("lgb_com_impr_delta_model", envir = .GlobalEnv)) {
  assign("lgb_com_impr_delta_model",    lgb_com_impr_delta_cv$model,  envir = .GlobalEnv)
  assign("lgb_com_impr_delta_features", lgb_com_impr_delta_cv$x_cols, envir = .GlobalEnv)
}

# Expose booster + feature aliases — condo (if loaded)
if (exists("lgb_condo_land_delta_cv", envir = .GlobalEnv) &&
    !exists("lgb_condo_land_delta_model", envir = .GlobalEnv)) {
  assign("lgb_condo_land_delta_model",    lgb_condo_land_delta_cv$model,  envir = .GlobalEnv)
  assign("lgb_condo_land_delta_features", lgb_condo_land_delta_cv$x_cols, envir = .GlobalEnv)
}
if (exists("lgb_condo_impr_delta_cv", envir = .GlobalEnv) &&
    !exists("lgb_condo_impr_delta_model", envir = .GlobalEnv)) {
  assign("lgb_condo_impr_delta_model",    lgb_condo_impr_delta_cv$model,  envir = .GlobalEnv)
  assign("lgb_condo_impr_delta_features", lgb_condo_impr_delta_cv$x_cols, envir = .GlobalEnv)
}

# Training frame for land (used by predict_log_land_lgb type alignment)
if (!exists("model_data_land_delta_model", envir = .GlobalEnv)) {
  p <- file.path(cache_dir, "model_data_land_delta_model.rds")
  if (file.exists(p)) {
    assign("model_data_land_delta_model", readRDS(p), envir = .GlobalEnv)
    message("✅ loaded training frame: model_data_land_delta_model")
  }
}
train_land_df <- model_data_land_delta_model

if (!exists("model_data_impr_delta_model", envir = .GlobalEnv)) {
  p <- file.path(cache_dir, "model_data_impr_delta_model.rds")
  if (file.exists(p)) {
    assign("model_data_impr_delta_model", readRDS(p), envir = .GlobalEnv)
    message("✅ loaded training frame: model_data_impr_delta_model")
  }
}
train_impr_df <- model_data_impr_delta_model

# Commercial training frames (optional)
if (!exists("model_data_comm_land_delta", envir = .GlobalEnv)) {
  p <- file.path(cache_dir, "model_data_comm_land_delta.rds")
  if (file.exists(p)) {
    assign("model_data_comm_land_delta", readRDS(p), envir = .GlobalEnv)
    message("✅ loaded training frame: model_data_comm_land_delta")
  }
}
if (!exists("model_data_comm_impr_delta", envir = .GlobalEnv)) {
  p <- file.path(cache_dir, "model_data_comm_impr_delta.rds")
  if (file.exists(p)) {
    assign("model_data_comm_impr_delta", readRDS(p), envir = .GlobalEnv)
    message("✅ loaded training frame: model_data_comm_impr_delta")
  }
}

# Condo training frames (optional)
if (!exists("model_data_condo_land_delta", envir = .GlobalEnv)) {
  p <- file.path(cache_dir, "model_data_condo_land_delta.rds")
  if (file.exists(p)) {
    assign("model_data_condo_land_delta", readRDS(p), envir = .GlobalEnv)
    message("✅ loaded training frame: model_data_condo_land_delta")
  }
}
if (!exists("model_data_condo_impr_delta", envir = .GlobalEnv)) {
  p <- file.path(cache_dir, "model_data_condo_impr_delta.rds")
  if (file.exists(p)) {
    assign("model_data_condo_impr_delta", readRDS(p), envir = .GlobalEnv)
    message("✅ loaded training frame: model_data_condo_impr_delta")
  }
}

# ---- 4) Local prediction helpers ----------------------------------------

# Predict land delta → reconstruct log level
predict_log_land_lgb <- function(df) {
  df <- prep_for_dummyvars(df, dv_land_delta, train_land_df)
  pred_delta <- predict_lgbm_log(
    lgb_model     = lgb_land_delta_model,
    dv            = dv_land_delta,
    feature_names = lgb_land_delta_features,
    newdata       = df
  )
  df$log_appr_land_val_lag1 + pred_delta
}

# Predict impr delta → reconstruct log level
predict_log_impr_lgb <- function(newdata) {
  nd <- prep_for_dummyvars(newdata, dv_impr_delta, train_impr_df)
  pred_delta <- predict_lgbm_log(
    lgb_impr_delta_model, dv_impr_delta, lgb_impr_delta_features, nd
  )
  nd$log_appr_imps_val_lag1 + pred_delta
}

# ---- safe_predict_delta: mirrors 06_forecast_av_2026_2031_sequential_comm.R --
# Commercial and condo delta models predict log-CHANGE (delta), matching the
# residential pattern. The lag1 anchor is added back here to reconstruct the
# log-level prediction, identical to the production forecast scripts.
.safe_predict_delta <- function(cv_obj, data_dt, lag_col) {
  x_cols <- cv_obj$x_cols
  model  <- cv_obj$model
  dt     <- data.table::as.data.table(data_dt)

  # Add any missing feature columns as NA
  for (col in setdiff(x_cols, names(dt)))
    dt[, (col) := NA_real_]

  # Integer-encode character/factor columns
  for (col in x_cols) {
    v <- dt[[col]]
    if (is.character(v) || is.factor(v))
      dt[, (col) := as.integer(as.factor(v))]
  }

  x_mat <- as.matrix(dt[, x_cols, with = FALSE])
  x_mat[!is.finite(x_mat)] <- NA_real_
  pred_delta <- as.numeric(predict(model, x_mat))
  # Reconstruct log level: lag1 + predicted change
  dt[[lag_col]] + pred_delta
}

# Commercial helpers (only defined if models loaded)
if (exists("lgb_com_land_delta_cv", envir = .GlobalEnv)) {
  predict_log_com_land_lgb <- function(df) {
    .safe_predict_delta(lgb_com_land_delta_cv,
                        data.table::as.data.table(df),
                        "log_appr_land_val_lag1")
  }
}
if (exists("lgb_com_impr_delta_cv", envir = .GlobalEnv)) {
  predict_log_com_impr_lgb <- function(df) {
    .safe_predict_delta(lgb_com_impr_delta_cv,
                        data.table::as.data.table(df),
                        "log_appr_imps_val_lag1")
  }
}

# Condo helpers (only defined if models loaded)
if (exists("lgb_condo_land_delta_cv", envir = .GlobalEnv)) {
  predict_log_condo_land_lgb <- function(df) {
    .safe_predict_delta(lgb_condo_land_delta_cv,
                        data.table::as.data.table(df),
                        "log_appr_land_val_lag1")
  }
}
if (exists("lgb_condo_impr_delta_cv", envir = .GlobalEnv)) {
  predict_log_condo_impr_lgb <- function(df) {
    .safe_predict_delta(lgb_condo_impr_delta_cv,
                        data.table::as.data.table(df),
                        "log_appr_imps_val_lag1")
  }
}

calc_metrics <- function(actual_log, pred_log, mape_floor = 20000) {
  rmse_log <- sqrt(mean((actual_log - pred_log)^2, na.rm = TRUE))
  mae_log  <- mean(abs(actual_log - pred_log), na.rm = TRUE)

  actual <- exp(actual_log)
  pred   <- exp(pred_log)

  rmse <- sqrt(mean((actual - pred)^2, na.rm = TRUE))
  mae  <- mean(abs(actual - pred), na.rm = TRUE)

  denom <- pmax(actual, mape_floor)
  mape  <- mean(abs(actual - pred) / denom, na.rm = TRUE)
  wape  <- sum(abs(actual - pred), na.rm = TRUE) / sum(actual, na.rm = TRUE)

  tibble(RMSE_log = rmse_log, MAE_log = mae_log,
         RMSE = rmse, MAE = mae, MAPE = mape, WAPE = wape)
}

predict_land_by_year <- function(panel_tbl, year) {
  panel_tbl %>%
    mutate(tax_yr = as.integer(tax_yr)) %>%
    group_by(parcel_id) %>%
    arrange(tax_yr) %>%
    mutate(
      log_appr_land_val      = log(appr_land_val),
      log_appr_land_val_lag1 = lag(log_appr_land_val, 1),
      log_appr_land_val_lag2 = lag(log_appr_land_val, 2)
    ) %>%
    ungroup() %>%
    filter(tax_yr == year, !is.na(appr_land_val), appr_land_val > 0,
           !is.na(log_appr_land_val_lag1)) %>%
    mutate(
      pred_log_lgb_land = predict_log_land_lgb(.),
      pred_land_val     = exp(pred_log_lgb_land)
    )
}

predict_impr_by_year <- function(panel_tbl, year) {
  panel_tbl %>%
    mutate(tax_yr = as.integer(tax_yr)) %>%
    group_by(parcel_id) %>%
    arrange(tax_yr) %>%
    mutate(
      log_appr_imps_val      = log(appr_imps_val),
      log_appr_imps_val_lag1 = lag(log_appr_imps_val, 1)
    ) %>%
    ungroup() %>%
    filter(tax_yr == year, !is.na(appr_imps_val), appr_imps_val > 0,
           !is.na(log_appr_imps_val_lag1)) %>%
    mutate(
      pred_log_lgb_impr = predict_log_impr_lgb(.),
      pred_impr_val     = exp(pred_log_lgb_impr)
    )
}

# ---- 5) Pull holdout observations for 2025 ------------------------------
panel_tbl <- panel_tbl %>% mutate(tax_yr = as.integer(tax_yr))

land_2025_obs <- panel_tbl %>%
  group_by(parcel_id) %>%
  arrange(tax_yr) %>%
  mutate(
    log_appr_land_val      = log(appr_land_val),
    log_appr_land_val_lag1 = lag(log_appr_land_val, 1),
    log_appr_land_val_lag2 = lag(log_appr_land_val, 2)
  ) %>%
  ungroup() %>%
  filter(tax_yr == 2025, !is.na(appr_land_val), appr_land_val > 0,
         !is.na(log_appr_land_val_lag1))

impr_2025_obs <- panel_tbl %>%
  group_by(parcel_id) %>%
  arrange(tax_yr) %>%
  mutate(
    log_appr_imps_val      = log(appr_imps_val),
    log_appr_imps_val_lag1 = lag(log_appr_imps_val, 1)
  ) %>%
  ungroup() %>%
  filter(tax_yr == 2025, !is.na(appr_imps_val), appr_imps_val > 0,
         !is.na(log_appr_imps_val_lag1))

# ---- 6) Predict 2023–2025 -----------------------------------------------
land_2023 <- predict_land_by_year(panel_tbl, 2023)
land_2024 <- predict_land_by_year(panel_tbl, 2024)
land_2025 <- predict_land_by_year(panel_tbl, 2025)

impr_2023 <- predict_impr_by_year(panel_tbl, 2023)
impr_2024 <- predict_impr_by_year(panel_tbl, 2024)
impr_2025 <- predict_impr_by_year(panel_tbl, 2025)

# ---- 7) 2025 holdout predictions (for metrics) --------------------------
land_results <- land_2025_obs %>%
  mutate(pred_log_lgb_land = predict_log_land_lgb(.))

impr_results <- impr_2025_obs %>%
  mutate(pred_log_lgb_impr = predict_log_impr_lgb(.))

# ---- 8) Aggregate AV comparison -----------------------------------------
agg_total_av <- function(land_df, impr_df, year) {
  tibble(
    tax_yr          = year,
    actual_land_av  = sum(land_df$appr_land_val,  na.rm = TRUE),
    actual_impr_av  = sum(impr_df$appr_imps_val,  na.rm = TRUE),
    actual_total_av = actual_land_av + actual_impr_av,
    ml_land_av      = sum(land_df$pred_land_val,  na.rm = TRUE),
    ml_impr_av      = sum(impr_df$pred_impr_val,  na.rm = TRUE),
    ml_total_av     = ml_land_av + ml_impr_av
  )
}

res_total_av <- bind_rows(
  agg_total_av(land_2023, impr_2023, 2023),
  agg_total_av(land_2024, impr_2024, 2024),
  agg_total_av(land_2025, impr_2025, 2025)
)

# ---- 9) Metrics ---------------------------------------------------------
metrics_land <- calc_metrics(
  land_results$log_appr_land_val,
  land_results$pred_log_lgb_land
) %>% mutate(Model = "LGB_land", Target = "land", Track = "Residential")

metrics_impr <- calc_metrics(
  impr_results$log_appr_imps_val,
  impr_results$pred_log_lgb_impr
) %>% mutate(Model = "LGB_impr", Target = "impr", Track = "Residential")

metrics_all <- bind_rows(metrics_land, metrics_impr) %>%
  select(Target, Model, everything())

# Full combined summary printed in Section D after com + condo metrics are added

# ---- 10) Save outputs ---------------------------------------------------
write_csv(metrics_land, metrics_path_land)
write_csv(metrics_impr, metrics_path_impr)
write_csv(land_results, pred_path_land)
write_csv(impr_results, pred_path_impr)

assign("metrics_all",  metrics_all,  envir = .GlobalEnv)
assign("land_results", land_results, envir = .GlobalEnv)
assign("impr_results", impr_results, envir = .GlobalEnv)

# ============================================================================
# SECTION B — Commercial holdout eval (2025)
# Requires: panel_tbl_retro_com loaded in Step 4b by main_ml.R.
# Skipped gracefully if models or panel are unavailable.
# ============================================================================
if (exists("predict_log_com_land_lgb") && exists("predict_log_com_impr_lgb")) {

  panel_com <- get0("panel_tbl_retro_com", envir = .GlobalEnv)
  if (is.null(panel_com)) {
    com_cache <- file.path(cache_dir, "panel_tbl_retro_com.rds")
    if (file.exists(com_cache)) {
      panel_com <- readRDS(com_cache)
      message("  Loaded panel_tbl_retro_com from cache for eval.")
    }
  }

  if (!is.null(panel_com)) {
    setDT(panel_com)
    setkeyv(panel_com, c("parcel_id", "tax_yr"))

    # Always recompute lags fresh from the level columns rather than trusting
    # whatever was cached -- stale/inconsistent retro panel caches can have lag
    # columns populated with all-NA or garbage values from earlier broken builds.
    # Derive log levels first if absent or all-NA.
    if (!"log_appr_land_val" %in% names(panel_com) ||
        all(is.na(panel_com$log_appr_land_val)))
      panel_com[appr_land_val > 0, log_appr_land_val := log(appr_land_val)]
    if (!"log_appr_imps_val" %in% names(panel_com) ||
        all(is.na(panel_com$log_appr_imps_val)))
      panel_com[appr_imps_val > 0, log_appr_imps_val := log(appr_imps_val)]

    # Guard: if log_appr_land_val is still all-NA the retro cache is from before
    # the AV rejoin fix. Attempt an inline fix using av_history_cln.
    if (all(is.na(panel_com$log_appr_land_val))) {
      message("  ⚠  log_appr_land_val all-NA in commercial retro panel — retro cache pre-dates")
      message("     AV rejoin fix. Re-join av_history_cln inline for eval ...")
      av_cache_path <- file.path(cache_dir, "av_history_cln.rds")
      if (!exists("av_history_cln", envir = .GlobalEnv) && file.exists(av_cache_path))
        assign("av_history_cln", readRDS(av_cache_path), envir = .GlobalEnv)
      if (exists("av_history_cln", envir = .GlobalEnv)) {
        av_fix <- data.table::as.data.table(av_history_cln)
        av_fix[, parcel_id := gsub("-", "", parcel_id)]
        av_fix <- av_fix[parcel_id %in% unique(panel_com$parcel_id),
                         .(parcel_id, tax_yr, appr_land_val, appr_imps_val)]
        panel_com[av_fix, on = .(parcel_id, tax_yr),
                  `:=`(appr_land_val = i.appr_land_val, appr_imps_val = i.appr_imps_val)]
        panel_com[appr_land_val > 0, log_appr_land_val := log(appr_land_val)]
        panel_com[appr_imps_val  > 0, log_appr_imps_val := log(appr_imps_val)]
        rm(av_fix)
        message("     Inline AV re-join complete for commercial eval panel.")
      } else {
        message("  ⚠  av_history_cln unavailable — commercial eval skipped.")
        panel_com <- NULL
      }
    }

    # Always recompute lags from the (now correct) level columns.
    # Force-overwrite any stale cached lag values.
    if (!is.null(panel_com)) {
      setorderv(panel_com, c("parcel_id", "tax_yr"))
      panel_com[, log_appr_land_val_lag1 := shift(log_appr_land_val, 1L), by = parcel_id]
      panel_com[, log_appr_land_val_lag2 := shift(log_appr_land_val, 2L), by = parcel_id]
      panel_com[, log_appr_imps_val_lag1 := shift(log_appr_imps_val, 1L), by = parcel_id]

      # Sanity check: lag values should be in log-dollar range (~8–18)
      lag_check <- panel_com[!is.na(log_appr_land_val_lag1), log_appr_land_val_lag1]
      if (length(lag_check) > 0 && (median(lag_check) < 5 || median(lag_check) > 25)) {
        message("  ⚠  log_appr_land_val_lag1 median = ", round(median(lag_check), 2),
                " — out of expected range [5, 25]. Check panel AV columns.")
      }
    }

    com_land_2025 <- panel_com[
      tax_yr == 2025 & !is.na(appr_land_val) & appr_land_val > 0 &
        !is.na(log_appr_land_val_lag1)
    ]
    com_impr_2025 <- panel_com[
      tax_yr == 2025 & !is.na(appr_imps_val) & appr_imps_val > 0 &
        !is.na(log_appr_imps_val_lag1)
    ]

    if (nrow(com_land_2025) > 0) {
      com_land_2025[, pred_log_com_land := predict_log_com_land_lgb(com_land_2025)]
      metrics_com_land <- calc_metrics(
        com_land_2025$log_appr_land_val,
        com_land_2025$pred_log_com_land
      ) %>% mutate(Model = "LGB_com_land", Target = "com_land", Track = "Commercial")
      write_csv(metrics_com_land, metrics_path_com_land)
      message("  Commercial land 2025 holdout — RMSE_log: ",
              round(metrics_com_land$RMSE_log, 4),
              "  WAPE: ", round(metrics_com_land$WAPE, 4))
    } else {
      metrics_com_land <- NULL
      message("  ⚠  No commercial land 2025 holdout rows — skipping.")
    }

    if (nrow(com_impr_2025) > 0) {
      com_impr_2025[, pred_log_com_impr := predict_log_com_impr_lgb(com_impr_2025)]
      metrics_com_impr <- calc_metrics(
        com_impr_2025$log_appr_imps_val,
        com_impr_2025$pred_log_com_impr
      ) %>% mutate(Model = "LGB_com_impr", Target = "com_impr", Track = "Commercial")
      write_csv(metrics_com_impr, metrics_path_com_impr)
      message("  Commercial impr 2025 holdout — RMSE_log: ",
              round(metrics_com_impr$RMSE_log, 4),
              "  WAPE: ", round(metrics_com_impr$WAPE, 4))
    } else {
      metrics_com_impr <- NULL
      message("  ⚠  No commercial impr 2025 holdout rows — skipping.")
    }

    metrics_all <- bind_rows(metrics_all,
                             metrics_com_land,
                             metrics_com_impr)
    assign("metrics_all", metrics_all, envir = .GlobalEnv)

  } else {
    message("  ⚠  panel_tbl_retro_com not available — skipping commercial holdout eval.")
  }

} else {
  message("  Commercial models not loaded — skipping commercial holdout eval.")
}

# ============================================================================
# SECTION C — Condo holdout eval (2025)
# Requires: panel_tbl_retro_condo loaded in Step 4c by main_ml.R.
# Condo parcel_ids are stored in no-dash format after the panel fix.
# ============================================================================
if (exists("predict_log_condo_land_lgb") && exists("predict_log_condo_impr_lgb")) {

  panel_condo <- get0("panel_tbl_retro_condo", envir = .GlobalEnv)
  if (is.null(panel_condo)) {
    condo_cache <- file.path(cache_dir, "panel_tbl_retro_condo.rds")
    if (file.exists(condo_cache)) {
      panel_condo <- readRDS(condo_cache)
      message("  Loaded panel_tbl_retro_condo from cache for eval.")
    }
  }

  if (!is.null(panel_condo)) {
    setDT(panel_condo)
    setkeyv(panel_condo, c("parcel_id", "tax_yr"))

    # Always recompute lags fresh -- stale cached lag values cause garbage metrics.
    if (!"log_appr_land_val" %in% names(panel_condo) ||
        all(is.na(panel_condo$log_appr_land_val)))
      panel_condo[appr_land_val > 0, log_appr_land_val := log(appr_land_val)]
    if (!"log_appr_imps_val" %in% names(panel_condo) ||
        all(is.na(panel_condo$log_appr_imps_val)))
      panel_condo[appr_imps_val > 0, log_appr_imps_val := log(appr_imps_val)]

    # Guard: all-NA means retro cache pre-dates the AV rejoin fix
    if (all(is.na(panel_condo$log_appr_land_val))) {
      message("  ⚠  log_appr_land_val all-NA in condo retro panel — re-joining av_history_cln inline ...")
      av_cache_path <- file.path(cache_dir, "av_history_cln.rds")
      if (!exists("av_history_cln", envir = .GlobalEnv) && file.exists(av_cache_path))
        assign("av_history_cln", readRDS(av_cache_path), envir = .GlobalEnv)
      if (exists("av_history_cln", envir = .GlobalEnv)) {
        av_fix <- data.table::as.data.table(av_history_cln)
        av_fix[, parcel_id := gsub("-", "", parcel_id)]
        av_fix <- av_fix[parcel_id %in% unique(panel_condo$parcel_id),
                         .(parcel_id, tax_yr, appr_land_val, appr_imps_val)]
        panel_condo[av_fix, on = .(parcel_id, tax_yr),
                    `:=`(appr_land_val = i.appr_land_val, appr_imps_val = i.appr_imps_val)]
        panel_condo[appr_land_val > 0, log_appr_land_val := log(appr_land_val)]
        panel_condo[appr_imps_val  > 0, log_appr_imps_val := log(appr_imps_val)]
        rm(av_fix)
        message("     Inline AV re-join complete for condo eval panel.")
      } else {
        message("  ⚠  av_history_cln unavailable — condo eval skipped.")
        panel_condo <- NULL
      }
    }

    # Force-recompute lags from the corrected level columns.
    if (!is.null(panel_condo)) {
      setorderv(panel_condo, c("parcel_id", "tax_yr"))
      panel_condo[, log_appr_land_val_lag1 := shift(log_appr_land_val, 1L), by = parcel_id]
      panel_condo[, log_appr_land_val_lag2 := shift(log_appr_land_val, 2L), by = parcel_id]
      panel_condo[, log_appr_imps_val_lag1 := shift(log_appr_imps_val, 1L), by = parcel_id]

      lag_check <- panel_condo[!is.na(log_appr_land_val_lag1), log_appr_land_val_lag1]
      if (length(lag_check) > 0 && (median(lag_check) < 5 || median(lag_check) > 25)) {
        message("  ⚠  log_appr_land_val_lag1 median = ", round(median(lag_check), 2),
                " — out of expected range [5, 25]. Check panel AV columns.")
      }
    }

    condo_land_2025 <- panel_condo[
      tax_yr == 2025 & !is.na(appr_land_val) & appr_land_val > 0 &
        !is.na(log_appr_land_val_lag1)
    ]
    condo_impr_2025 <- panel_condo[
      tax_yr == 2025 & !is.na(appr_imps_val) & appr_imps_val > 0 &
        !is.na(log_appr_imps_val_lag1)
    ]

    if (nrow(condo_land_2025) > 0) {
      condo_land_2025[, pred_log_condo_land := predict_log_condo_land_lgb(condo_land_2025)]
      metrics_condo_land <- calc_metrics(
        condo_land_2025$log_appr_land_val,
        condo_land_2025$pred_log_condo_land
      ) %>% mutate(Model = "LGB_condo_land", Target = "condo_land", Track = "Condo")
      write_csv(metrics_condo_land, metrics_path_condo_land)
      message("  Condo land 2025 holdout — RMSE_log: ",
              round(metrics_condo_land$RMSE_log, 4),
              "  WAPE: ", round(metrics_condo_land$WAPE, 4))
    } else {
      metrics_condo_land <- NULL
      message("  ⚠  No condo land 2025 holdout rows — skipping.")
    }

    if (nrow(condo_impr_2025) > 0) {
      condo_impr_2025[, pred_log_condo_impr := predict_log_condo_impr_lgb(condo_impr_2025)]
      metrics_condo_impr <- calc_metrics(
        condo_impr_2025$log_appr_imps_val,
        condo_impr_2025$pred_log_condo_impr
      ) %>% mutate(Model = "LGB_condo_impr", Target = "condo_impr", Track = "Condo")
      write_csv(metrics_condo_impr, metrics_path_condo_impr)
      message("  Condo impr 2025 holdout — RMSE_log: ",
              round(metrics_condo_impr$RMSE_log, 4),
              "  WAPE: ", round(metrics_condo_impr$WAPE, 4))
    } else {
      metrics_condo_impr <- NULL
      message("  ⚠  No condo impr 2025 holdout rows — skipping.")
    }

    metrics_all <- bind_rows(metrics_all,
                             metrics_condo_land,
                             metrics_condo_impr)
    assign("metrics_all", metrics_all, envir = .GlobalEnv)

  } else {
    message("  ⚠  panel_tbl_retro_condo not available — skipping condo holdout eval.")
  }

} else {
  message("  Condo models not loaded — skipping condo holdout eval.")
}

# ============================================================================
# SECTION D — Combined summary across all tracks
# ============================================================================
message("\n=== 2025 Holdout Eval — All Tracks ===")
metrics_all_print <- metrics_all %>%
  mutate(Track = coalesce(
    get0("Track"),
    case_when(
      grepl("com",   Target) ~ "Commercial",
      grepl("condo", Target) ~ "Condo",
      TRUE                   ~ "Residential"
    )
  )) %>%
  select(Track, Target, Model, RMSE_log, MAE_log, MAPE, WAPE) %>%
  arrange(Track, Target)
print(metrics_all_print)

message("✅ 2025 evaluation complete. Outputs written to: ", out_dir)
message("05_eval_holdout_2025.R loaded")
