# scripts/ml/03_model_land.R ------------------------------------------
# Panel LAND Models (LightGBM only)
# - DELTA model (log change, requires lag)
# - LEVEL model (fallback for cold-start parcels)

message("Running 03_model_land.R (LightGBM delta + level rolling CV) ...")

# Read from the full residential panel (includes spatial distance columns
# added by xx_combine_res_comm_condo_panel.R).
# Fall back chain: panel_tbl_res → panel_tbl_res_backbone → panel_tbl
#
# cache_dir / model_dir come from run_main_ml() via .GlobalEnv.  The literals
# here("data","cache") / here("data","model") used to be hardcoded in this
# script, which ignored a caller-supplied model_dir: any run with a
# non-default model_dir still wrote stamped land models into data/model/,
# where latest_model_file() (newest mtime) would serve them to the next
# production run.  The defaults reproduce the old literals for standalone use.
cache_dir <- get0("cache_dir", envir = .GlobalEnv, ifnotfound = here("data", "cache"))
model_dir <- get0("model_dir", envir = .GlobalEnv, ifnotfound = here("data", "model"))

# NWMLS feature mode (run_main_ml(nwmls_features = ...), default "level").
# nwmls_model_cols() says which sea_* columns each frame may contain; in
# "level" mode that is exactly the six levels below, so the frames are
# unchanged.  See scripts/ml/nwmls_features.R.
if (!exists("nwmls_model_cols", mode = "function"))
  source(here("scripts", "ml", "nwmls_features.R"))
.nwmls_mode       <- nwmls_run_mode()
.nwmls_delta_cols <- nwmls_model_cols(.nwmls_mode, "delta")
.nwmls_level_cols <- nwmls_model_cols(.nwmls_mode, "level")
.nwmls_all_cols   <- union(.nwmls_level_cols, .nwmls_delta_cols)
message("  nwmls_features = \"", .nwmls_mode, "\" | delta frame NWMLS cols: ",
        paste(.nwmls_delta_cols, collapse = ", "))

.panel_path <- if (file.exists(file.path(cache_dir, "panel_tbl_res.rds")))
  file.path(cache_dir, "panel_tbl_res.rds") else if (
  file.exists(file.path(cache_dir, "panel_tbl_res_backbone.rds")))
  file.path(cache_dir, "panel_tbl_res_backbone.rds") else
  file.path(cache_dir, "panel_tbl.rds")
# Prefer the panel_tbl_res object run_main_ml() Step 2 already loaded from
# that same file: identical data, and it avoids holding a second multi-GB
# copy during training.  Also what lets the backtest harness hand this
# script a panel without writing to the production cache.
if (exists("panel_tbl_res", envir = .GlobalEnv)) {
  message("  Using panel_tbl_res from .GlobalEnv")
  panel_tbl <- get("panel_tbl_res", envir = .GlobalEnv)
} else {
  message("  Reading panel from: ", basename(.panel_path))
  panel_tbl <- read_rds(.panel_path)
}
rm(.panel_path)

# Coerce to plain tibble — panel may be cached as a data.table and
# dplyr's vctrs backend can't combine factor + logical columns from a
# data.table in the select() call below.
panel_tbl <- tibble::as_tibble(panel_tbl)

# Schema-defensive: the 2026-07 KCA extract dropped CurrentZoning from
# EXTR_Parcel, so current_zoning_3 may be absent.  Synthesize a constant
# factor so downstream mutate/select/fct_lump calls work unchanged; the
# single-level column is then removed by the factor cleaner exactly as it
# was in prior runs (it has never contributed to the model).
if (!"current_zoning_3" %in% names(panel_tbl)) {
  message("  \u2139\ufe0f  current_zoning_3 missing from panel (extract schema change) — stubbing constant factor")
  panel_tbl$current_zoning_3 <- factor("Unknown")
}

# The dist_to_* columns are legacy features no longer produced by the import;
# they only ever appeared as all-NA schema padding on the combined-panel path
# and were removed by the zero-variance cleaner every run.  The res-only
# assembly path omits them entirely, so stub them (NA -> zero-variance ->
# dropped, exactly as before).
for (.opt_num in c("dist_to_public_km", "dist_to_private_km",
                   "dist_to_lightrail_km")) {
  if (!.opt_num %in% names(panel_tbl)) {
    message("  \u2139\ufe0f  ", .opt_num,
            " missing from panel (legacy feature) — stubbing NA")
    panel_tbl[[.opt_num]] <- NA_real_
  }
}

# In "growth" mode a panel cached before nwmls_features existed has no growth
# columns.  main_ml.R joins them before Step 3; standalone, stop rather than
# let the predictor filter drop them silently.
.nwmls_absent <- setdiff(.nwmls_all_cols, names(panel_tbl))
if (.nwmls_mode == "growth" && length(.nwmls_absent))
  stop("nwmls_features = \"growth\" but the residential panel lacks: ",
       paste(.nwmls_absent, collapse = ", "),
       ".  Run through run_main_ml(nwmls_features = \"growth\"), which joins ",
       "them (nwmls_ensure_panel_cols()).", call. = FALSE)
rm(.nwmls_absent)

# ------------------------------------------------------------------
# 1) Build base frame with logs + lags + delta (NO drop_na yet)
# ------------------------------------------------------------------
# Filter to training-eligible parcels only.
# train_res = 1 excludes land-only parcels and model_res=0 exclusions
# (exempt, incomplete, non-SFR HBU) from the training set so they do not
# bias model coefficients.  Parcels without train_res (old cached panels)
# fall back to the full set with a warning.
if ("train_res" %in% names(panel_tbl)) {
  n_excl_land <- sum(panel_tbl$train_res == 0L, na.rm = TRUE)
  message("  03_model_land: ", n_excl_land,
          " parcel-years excluded from training (train_res=0)")
  panel_tbl_train <- panel_tbl %>% filter(train_res == 1L)
} else {
  warning("train_res not found in panel_tbl — training on full panel. ",
          "Re-run 02_transfrm.R to add train_res.")
  panel_tbl_train <- panel_tbl
}

# Backtest: train_through_year (run_main_ml(), .GlobalEnv) truncates the
# training frame to tax_yr <= T before lags are computed.  NULL = no filter.
.tty <- get0("train_through_year", envir = .GlobalEnv, ifnotfound = NULL)
if (!is.null(.tty)) {
  .n0 <- nrow(panel_tbl_train)
  panel_tbl_train <- panel_tbl_train %>% filter(tax_yr <= .tty)
  message("  03_model_land: train_through_year = ", .tty, " — ",
          nrow(panel_tbl_train), " of ", .n0, " rows kept")
  rm(.n0)
}
rm(.tty)

model_data_land_base <- panel_tbl_train %>%
  filter(tax_yr > 2006) %>%
  mutate(
    appr_land_val     = if_else(appr_land_val <= 0, NA_real_, appr_land_val),
    log_appr_land_val = log(appr_land_val)
  ) %>%
  group_by(parcel_id) %>%
  arrange(tax_yr) %>%
  mutate(
    log_appr_land_val_lag1 = lag(log_appr_land_val, 1),
    log_appr_land_val_lag2 = lag(log_appr_land_val, 2),
    delta_log_land         = log_appr_land_val - log_appr_land_val_lag1
  ) %>%
  ungroup() %>%
  mutate(
    area                 = as.factor(area),
    current_zoning_3     = as.factor(current_zoning_3),
    hbu_as_if_vacant_desc= as.factor(hbu_as_if_vacant_desc),
    tax_yr               = as.integer(tax_yr)
  ) %>%
  select(
    area, current_zoning_3, hbu_as_if_vacant_desc,
    unbuildable, sq_ft_lot,
    nuisance_score,
    mt_rainier, olympics, cascades, territorial, seattle_skyline,
    puget_sound, lake_washington,
    seismic_hazard, landslide_hazard, steep_slope_hazard,
    traffic_noise, airport_noise, power_lines, other_nuisances,
    contamination, historic_site,
    dist_to_public_km, dist_to_private_km, dist_to_lightrail_km,
    tax_yr,
    log_appr_land_val,
    log_appr_land_val_lag1,
    log_appr_land_val_lag2,
    delta_log_land,
    # NWMLS: "level" mode = sea_pmedesfh_lag12, sea_pmedesfh_lag6,
    # sea_sesfh_lag6, sea_sesfh_lag12, sea_alesfh_lag6, sea_alesfh_lag12
    # (same columns, same order as before nwmls_features existed).
    all_of(.nwmls_all_cols),
#    sea_spesfh_lag6,
 #   sea_spesfh_lag12,
    
    econ_employment_thous_yoy_lag1,
    econ_services_providing_yoy_lag1,
    econ_population_thous_yoy_lag1,
    econ_wholesale_and_retail_trade_yoy_lag1,
    econ_housing_permits_thous_yoy_lag1,
    econ_seattle_msa_s_p_corelogic_case_shilller_home_price_index_yoy_lag1,
    # econ_seattle_msa_cpi_u_1982_1984_100_yoy_lag1,  # excluded: CPI hotter in pessimistic → artificially inflates pessimistic AV
    
    econ_employment_thous_lvl_lag1,
    econ_services_providing_lvl_lag1,
    econ_population_thous_lvl_lag1,
    econ_seattle_msa_s_p_corelogic_case_shilller_home_price_index_lvl_lag1
    # econ_seattle_msa_cpi_u_1982_1984_100_lvl_lag1   # excluded: see above
  )

# ------------------------------------------------------------------
# 2) Split into DELTA and LEVEL training frames
# ------------------------------------------------------------------
model_data_land_delta <- model_data_land_base %>%
  drop_na(log_appr_land_val, log_appr_land_val_lag1) %>% 
  select(-contains("_lvl"))


model_data_land_level <- model_data_land_base %>%
  drop_na(log_appr_land_val) %>% 
  select(-contains("_yoy"))

# NWMLS columns this mode keeps out of each frame.  Empty in "level" mode.
# "growth": the delta frame drops the six levels (they read as period labels
# in a delta model); the level frame drops sea_mos_sfh_lag12 (the _yoy
# columns are already gone via -contains("_yoy")).
.nwmls_drop_delta <- setdiff(.nwmls_all_cols, .nwmls_delta_cols)
.nwmls_drop_level <- setdiff(.nwmls_all_cols, .nwmls_level_cols)
if (length(.nwmls_drop_delta))
  model_data_land_delta <- model_data_land_delta %>% select(-any_of(.nwmls_drop_delta))
if (length(.nwmls_drop_level))
  model_data_land_level <- model_data_land_level %>% select(-any_of(.nwmls_drop_level))


# ------------------------------------------------------------------
# 3) Shared helpers (same as improvements)
# ------------------------------------------------------------------
drop_single_level_and_nzv <- function(df, outcome_cols) {
  
  single_level_facs <- names(which(sapply(df, function(x)
    is.factor(x) && nlevels(x) < 2)))
  
  if (length(single_level_facs) > 0) {
    message("Dropping single-level factor(s): ",
            paste(single_level_facs, collapse = ", "))
    df <- df[, setdiff(names(df), single_level_facs)]
  }
  
  predictor_names <- setdiff(names(df), outcome_cols)
  
  nzv_info <- caret::nearZeroVar(
    df[, predictor_names, drop = FALSE],
    saveMetrics = TRUE
  )
  
  if (any(nzv_info$zeroVar)) {
    zero_var_cols <- rownames(nzv_info)[nzv_info$zeroVar]
    message("Dropping zero-variance predictor(s): ",
            paste(zero_var_cols, collapse = ", "))
    df <- df[, setdiff(names(df), zero_var_cols)]
  }
  
  df
}

impute_predictors <- function(df, outcome_cols) {
  
  pred_cols <- setdiff(names(df), outcome_cols)
  
  # Coerce to plain data.frame before subsetting — tibble [, cols, drop=FALSE]
  # returns a list from sapply rather than a logical vector, causing a crash.
  df_plain <- as.data.frame(df)
  num_cols <- pred_cols[sapply(df_plain[, pred_cols, drop=FALSE], is.numeric)]
  fct_cols <- pred_cols[sapply(df_plain[, pred_cols, drop=FALSE], is.factor)]
  
  df %>%
    mutate(across(all_of(num_cols),
                  ~ if_else(is.na(.x), median(.x, na.rm = TRUE), .x))) %>%
    mutate(across(all_of(fct_cols),
                  ~ forcats::fct_na_value_to_level(.x, level = "Unknown")))
}

# ------------------------------------------------------------------
# 4) DELTA MODEL (requires lag)
# ------------------------------------------------------------------
land_folds_delta <- make_rolling_year_folds(model_data_land_delta$tax_yr)

model_data_land_delta <- drop_single_level_and_nzv(
  model_data_land_delta,
  outcome_cols = c("log_appr_land_val", "delta_log_land")
)

model_data_land_delta <- impute_predictors(
  model_data_land_delta,
  outcome_cols = c("delta_log_land", "log_appr_land_val")
)

train_land_delta <- model_data_land_delta %>%
  select(-log_appr_land_val, -tax_yr)

dv_land_delta <- caret::dummyVars(
  delta_log_land ~ .,
  data = train_land_delta,
  fullRank = TRUE
)

lgb_land_delta_cv <- train_lgbm_log_model(
  df      = train_land_delta,
  outcome = "delta_log_land",
  folds   = land_folds_delta,
  seed    = 123
)

lgb_land_delta_model    <- lgb_land_delta_cv$model
lgb_land_delta_features <- lgb_land_delta_cv$x_cols

message("LightGBM LAND DELTA rolling CV RMSE (delta): ",
        round(lgb_land_delta_cv$cv_rmse, 4))

# ------------------------------------------------------------------
# 5) LEVEL MODEL (fallback, no lag predictors)
# ------------------------------------------------------------------
land_folds_level <- make_rolling_year_folds(model_data_land_level$tax_yr)

model_data_land_level <- model_data_land_level %>%
  select(-delta_log_land, -log_appr_land_val_lag1, -log_appr_land_val_lag2, -tax_yr)

model_data_land_level <- drop_single_level_and_nzv(
  model_data_land_level,
  outcome_cols = c("log_appr_land_val")
)

model_data_land_level <- impute_predictors(
  model_data_land_level,
  outcome_cols = c("log_appr_land_val")
)

train_land_level <- model_data_land_level

dv_land_level <- caret::dummyVars(
  log_appr_land_val ~ .,
  data = train_land_level,
  fullRank = TRUE
)



lgb_land_level_cv <- train_lgbm_log_model(
  df      = train_land_level,
  outcome = "log_appr_land_val",
  folds   = land_folds_level,
  seed    = 123
)

lgb_land_level_model    <- lgb_land_level_cv$model
lgb_land_level_features <- lgb_land_level_cv$x_cols

message("LightGBM LAND LEVEL rolling CV RMSE (log level): ",
        round(lgb_land_level_cv$cv_rmse, 4))


# ------------------------------------------------------------------
# 5.5) SAVE artifacts for downstream forecast scripts
# ------------------------------------------------------------------
dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")  # avoids overwriting

# Record the NWMLS feature mode on the artifacts: Steps 4 and 6 refuse to run
# a model trained in a different mode (nwmls_assert_model_mode()).
lgb_land_delta_cv <- nwmls_stamp_mode(lgb_land_delta_cv, .nwmls_mode)
lgb_land_level_cv <- nwmls_stamp_mode(lgb_land_level_cv, .nwmls_mode)

# Save delta artifacts (required for 07_forecast... land delta branch)
saveRDS(lgb_land_delta_cv,
        file.path(model_dir, paste0("lgb_land_delta_cv_", stamp, ".rds")))
saveRDS(dv_land_delta,
        file.path(model_dir, paste0("dv_land_delta_", stamp, ".rds")))

# Save level artifacts too (optional but good hygiene)
saveRDS(lgb_land_level_cv,
        file.path(model_dir, paste0("lgb_land_level_cv_", stamp, ".rds")))
saveRDS(dv_land_level,
        file.path(model_dir, paste0("dv_land_level_", stamp, ".rds")))

# Save training frames to cache (07 uses cache for type alignment / medians)
saveRDS(model_data_land_level,
        file.path(cache_dir, "model_data_land_model.rds"))
saveRDS(model_data_land_delta,
        file.path(cache_dir, "model_data_land_delta_model.rds"))

message("✅ Saved land delta + level artifacts with stamp: ", stamp)



# ---- Feature importance by group ----------------------------------------
.classify_feat <- function(feat) dplyr::case_when(
  grepl("^econ_", feat) ~ "econ",
  grepl("^sea_",  feat) ~ "nwmls",
  grepl("^permits_|^val_last_|^sqft_last_|^units_last_", feat) ~ "permits",
  TRUE ~ "parcel"
)

for (.wm in c("delta", "level")) {
  .cv  <- if (.wm == "delta") lgb_land_delta_cv else lgb_land_level_cv
  .imp <- data.table::as.data.table(lgb.importance(.cv$model))
  if (nrow(.imp) == 0) next
  .imp[, group := .classify_feat(Feature)]
  .grp <- .imp[, .(total_gain = sum(Gain), n_features = .N, avg_gain = mean(Gain)),
               by = group][order(-total_gain)]
  message("\n", .wm, " model — feature gain by group:")
  print(tibble::as_tibble(.grp))
  .top <- .imp[, .SD[order(-Gain)][seq_len(min(5L, .N))], by = group]
  message("\n", .wm, " model — top features by group:")
  print(tibble::as_tibble(.top[, .(group, feature = Feature, gain = Gain)]))
}

message("03_model_land.R loaded (LightGBM delta + level)")
