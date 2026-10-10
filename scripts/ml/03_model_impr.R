# scripts/ml/03_model_impr.R ------------------------------------------
# Panel Improvements Model (LightGBM only, delta formulation + rolling CV)
# PLUS: Level model (log_appr_imps_val) trained in parallel for fallback fills

message("Running 03_model_impr.R (LightGBM delta + level rolling CV) ...")


# Read from the full residential panel (includes spatial distance columns
# added by xx_combine_res_comm_condo_panel.R).
# Fall back chain: panel_tbl_res → panel_tbl_res_backbone → panel_tbl
# cache_dir / model_dir come from run_main_ml() via .GlobalEnv; the defaults
# reproduce the literals that used to be hardcoded here (see 03_model_land.R).
cache_dir <- get0("cache_dir", envir = .GlobalEnv, ifnotfound = here("data", "cache"))
model_dir <- get0("model_dir", envir = .GlobalEnv, ifnotfound = here("data", "model"))

.panel_path <- if (file.exists(file.path(cache_dir, "panel_tbl_res.rds")))
  file.path(cache_dir, "panel_tbl_res.rds") else if (
  file.exists(file.path(cache_dir, "panel_tbl_res_backbone.rds")))
  file.path(cache_dir, "panel_tbl_res_backbone.rds") else
  file.path(cache_dir, "panel_tbl.rds")
# Prefer the panel_tbl_res object run_main_ml() Step 2 already loaded from
# that same file (identical data, no second multi-GB copy; see 03_model_land.R).
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

# ---- predictors ----
permit_predictors <- c(
  "permits_last_1yr","permits_last_3yr","permits_last_5yr",
  "val_last_3yr","val_last_5yr",
  "sqft_last_3yr","units_last_3yr",
  "any_newconst","years_since_newconst",
  "log_val_last_3yr","log_sqft_last_3yr"
)

base_impr_predictors <- c(
  "area","current_zoning_3",
  "total_living_sqft","n_res_units","max_stories",
  "avg_living_sqft_per_unit","avg_bldg_grade","condition_mode",
  "avg_fin_bsmt_grade","max_bedrooms","max_bath_full","max_bath_half",
  "yr_built_last","yr_renovated_last","fin_basement_sqft",
  "garage_attached_sqft","deck_sqft","fp_single_story","fp_multi_story",
  "water_system","sewer_system",
  "tax_yr", "log_appr_land_val"
)

# NWMLS feature mode (run_main_ml(nwmls_features = ...), default "level").
# "level" mode: exactly the six levels this list always held, in this order:
#   sea_pmedesfh_lag12, sea_pmedesfh_lag6, sea_sesfh_lag6, sea_sesfh_lag12,
#   sea_alesfh_lag6, sea_alesfh_lag12
# "growth" mode adds the _yoy columns + sea_mos_sfh_lag12 for the delta frame;
# the per-frame drops are below.  See scripts/ml/nwmls_features.R.
if (!exists("nwmls_model_cols", mode = "function"))
  source(here("scripts", "ml", "nwmls_features.R"))
.nwmls_mode       <- nwmls_run_mode()
.nwmls_delta_cols <- nwmls_model_cols(.nwmls_mode, "delta")
.nwmls_level_cols <- nwmls_model_cols(.nwmls_mode, "level")
nwmls_predictors  <- union(.nwmls_level_cols, .nwmls_delta_cols)
message("  nwmls_features = \"", .nwmls_mode, "\" | delta frame NWMLS cols: ",
        paste(.nwmls_delta_cols, collapse = ", "))

econ_prod_delta <- c(
  "econ_employment_thous_yoy_lag1",
  "econ_services_providing_yoy_lag1",
  "econ_population_thous_yoy_lag1",
  "econ_wholesale_and_retail_trade_yoy_lag1",
  "econ_housing_permits_thous_yoy_lag1",
  "econ_seattle_msa_s_p_corelogic_case_shilller_home_price_index_yoy_lag1"
  # "econ_seattle_msa_cpi_u_1982_1984_100_yoy_lag1"  # excluded: CPI hotter in pessimistic → artificially inflates pessimistic AV
)

econ_prod_level <- c(
  "econ_employment_thous_lvl_lag1",
  "econ_services_providing_lvl_lag1",
  "econ_population_thous_lvl_lag1",
  "econ_seattle_msa_s_p_corelogic_case_shilller_home_price_index_lvl_lag1"
  # "econ_seattle_msa_cpi_u_1982_1984_100_lvl_lag1"  # excluded: see above
)

# KCA permit history -> kcap_*.  Additive to permit_predictors above, not a
# replacement: three of the eight are the same construct as an SDCI feature but
# over a countywide universe and a different valuation basis, and the other
# five (remodel, demolition, description) have no SDCI analog at all.  Both
# sets go in so the gain report can decide what to drop.  Defined by
# xx_kca_permits_to_panel.R; absent when use_kca_permits = FALSE.
kcap_predictors <- get0("kcap_predictors", ifnotfound = character(0))
kcap_predictors <- kcap_predictors[kcap_predictors %in% names(panel_tbl)]

# In "growth" mode a panel cached before nwmls_features existed has no growth
# columns.  main_ml.R joins them before Step 3; standalone, stop rather than
# let the predictor filter drop them silently.
.nwmls_absent <- setdiff(nwmls_predictors, names(panel_tbl))
if (.nwmls_mode == "growth" && length(.nwmls_absent))
  stop("nwmls_features = \"growth\" but the residential panel lacks: ",
       paste(.nwmls_absent, collapse = ", "),
       ".  Run through run_main_ml(nwmls_features = \"growth\"), which joins ",
       "them (nwmls_ensure_panel_cols()).", call. = FALSE)
rm(.nwmls_absent)

permit_predictors <- permit_predictors[permit_predictors %in% names(panel_tbl)]
impr_predictors   <- c(base_impr_predictors, permit_predictors, kcap_predictors,
                       nwmls_predictors, econ_prod_delta, econ_prod_level)
impr_predictors   <- impr_predictors[impr_predictors %in% names(panel_tbl)]

# ------------------------------------------------------------------
# 1) Build base frame with logs + lags + delta (NO drop_na yet)
# ------------------------------------------------------------------
# Filter to training-eligible parcels only.
# train_res = 1 excludes land-only parcels and model_res=0 exclusions
# (exempt, incomplete, non-SFR HBU) from the training set so they do not
# bias model coefficients.  Parcels without train_res (old cached panels)
# fall back to the full set with a warning.
if ("train_res" %in% names(panel_tbl)) {
  n_excl_impr <- sum(panel_tbl$train_res == 0L, na.rm = TRUE)
  message("  03_model_impr: ", n_excl_impr,
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
  message("  03_model_impr: train_through_year = ", .tty, " — ",
          nrow(panel_tbl_train), " of ", .n0, " rows kept")
  rm(.n0)
}
rm(.tty)

model_data_impr_base <- panel_tbl_train %>%
  mutate(
    appr_imps_val     = if_else(appr_imps_val <= 0, NA_real_, appr_imps_val),
    log_appr_imps_val = log(appr_imps_val)
  ) %>%
  group_by(parcel_id) %>%
  arrange(tax_yr) %>%
  mutate(
    log_appr_imps_val_lag1 = lag(log_appr_imps_val, 1),
    log_appr_imps_val_lag2 = lag(log_appr_imps_val, 2),
    delta_log_impr         = log_appr_imps_val - log_appr_imps_val_lag1
  ) %>%
  ungroup() %>%
  mutate(
    area = as.factor(area),
    current_zoning_3 = as.factor(current_zoning_3),
    tax_yr = as.integer(tax_yr)
  ) %>%
  select(
    all_of(impr_predictors),
    log_appr_imps_val,         # level outcome
    log_appr_imps_val_lag1,
    log_appr_imps_val_lag2,
    delta_log_impr             # delta outcome
  )

# model_data_impr_base <- model_data_impr_base %>% filter(tax_yr > 2014)

# ------------------------------------------------------------------
# 2) Split into DELTA and LEVEL training frames
#    - delta requires lag1
#    - level requires only level outcome
# ------------------------------------------------------------------
model_data_impr_delta <- model_data_impr_base %>%
  drop_na(log_appr_imps_val, log_appr_imps_val_lag1) %>%
  mutate(current_zoning_3 = fct_lump_min(current_zoning_3, min = 500)) %>% 
  select(-contains("_lvl"))

model_data_impr_level <- model_data_impr_base %>%
  drop_na(log_appr_imps_val) %>%
  mutate(current_zoning_3 = fct_lump_min(current_zoning_3, min = 500)) %>% 
  select(-contains("_yoy"))

# NWMLS columns this mode keeps out of each frame.  Empty in "level" mode.
# "growth": the delta frame drops the six levels; the level frame drops
# sea_mos_sfh_lag12 (the _yoy columns are already gone via -contains("_yoy")).
.nwmls_drop_delta <- setdiff(nwmls_predictors, .nwmls_delta_cols)
.nwmls_drop_level <- setdiff(nwmls_predictors, .nwmls_level_cols)
if (length(.nwmls_drop_delta))
  model_data_impr_delta <- model_data_impr_delta %>% select(-any_of(.nwmls_drop_delta))
if (length(.nwmls_drop_level))
  model_data_impr_level <- model_data_impr_level %>% select(-any_of(.nwmls_drop_level))

# ------------------------------------------------------------------
# 3) Feature cleaning helper (drop single-level factors + NZV) - reused
# ------------------------------------------------------------------
drop_single_level_and_nzv <- function(df, outcome_cols) {
  
  # drop single-level factors
  single_level_facs <- names(which(sapply(df, function(x) is.factor(x) && nlevels(x) < 2)))
  if (length(single_level_facs) > 0) {
    message("Dropping single-level factor(s): ", paste(single_level_facs, collapse = ", "))
    df <- df[, setdiff(names(df), single_level_facs)]
  }
  
  predictor_names <- setdiff(names(df), outcome_cols)
  
  nzv_info <- caret::nearZeroVar(
    df[, predictor_names, drop = FALSE],
    saveMetrics = TRUE
  )
  if (any(nzv_info$zeroVar)) {
    zero_var_cols <- rownames(nzv_info)[nzv_info$zeroVar]
    message("Dropping zero-variance predictor(s): ", paste(zero_var_cols, collapse = ", "))
    df <- df[, setdiff(names(df), zero_var_cols)]
  }
  
  df
}

# ------------------------------------------------------------------
# 4) Impute remaining predictor NAs helper - reused
# ------------------------------------------------------------------
impute_predictors <- function(df, outcome_cols) {
  
  pred_cols <- setdiff(names(df), outcome_cols)
  
  # Coerce to plain data.frame before subsetting — tibble [, cols, drop=FALSE]
  # returns a list from sapply rather than a logical vector, causing a crash.
  df_plain <- as.data.frame(df)
  num_cols <- pred_cols[sapply(df_plain[, pred_cols, drop=FALSE], is.numeric)]
  fct_cols <- pred_cols[sapply(df_plain[, pred_cols, drop=FALSE], is.factor)]
  
  df %>%
    mutate(across(all_of(num_cols), ~ if_else(is.na(.x), median(.x, na.rm=TRUE), .x))) %>%
    mutate(across(all_of(fct_cols), ~ forcats::fct_na_value_to_level(.x, level = "Unknown")))
}

# ------------------------------------------------------------------
# 5) DELTA MODEL (your existing logic, unchanged except naming)
# ------------------------------------------------------------------

# IMPORTANT: keep lag1/lag2 for delta, drop level outcome to avoid leakage

model_data_impr_delta <- drop_single_level_and_nzv(
  model_data_impr_delta,
  outcome_cols = c("log_appr_imps_val", "delta_log_impr")
)

model_data_impr_delta <- impute_predictors(
  model_data_impr_delta,
  outcome_cols = c("delta_log_impr", "log_appr_imps_val")
)

impr_folds_delta <- make_rolling_year_folds(model_data_impr_delta$tax_yr)

train_impr_delta <- model_data_impr_delta %>%
  select(-log_appr_imps_val, -tax_yr)

dv_impr <- caret::dummyVars(
  delta_log_impr ~ .,
  data = train_impr_delta,
  fullRank = TRUE
)

lgb_impr_cv <- train_lgbm_log_model(
  df      = train_impr_delta,
  outcome = "delta_log_impr",
  folds   = impr_folds_delta,
  seed    = 123
)

lgb_impr_model    <- lgb_impr_cv$model
lgb_impr_features <- lgb_impr_cv$x_cols

message(
  "LightGBM IMPROVEMENT delta rolling CV RMSE (delta): ",
  round(lgb_impr_cv$cv_rmse, 4)
)

# ---- cache DELTA model + encoder + training frame ----
dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)

stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

# Record the NWMLS feature mode on the artifact: Steps 4 and 6 refuse to run
# a model trained in a different mode (nwmls_assert_model_mode()).
lgb_impr_cv <- nwmls_stamp_mode(lgb_impr_cv, .nwmls_mode)

saveRDS(
  lgb_impr_cv,
  file.path(model_dir, paste0("lgb_impr_delta_cv_", stamp, ".rds"))
)

saveRDS(
  dv_impr,
  file.path(model_dir, paste0("dv_impr_delta_", stamp, ".rds"))
)

# keep a delta training frame for 07 type-alignment + medians
saveRDS(
  model_data_impr_delta,
  file.path(cache_dir, "model_data_impr_delta_model.rds")
)

message("💾 cached improvements DELTA model + dv_impr_delta + training frame")


# reconstruct level preds for RMSE on log level (ENCODING-CORRECT)
X_all_delta <- as.matrix(predict(dv_impr, newdata = train_impr_delta))

# align columns to what the model expects
missing_cols <- setdiff(lgb_impr_features, colnames(X_all_delta))
if (length(missing_cols) > 0) {
  X_all_delta <- cbind(
    X_all_delta,
    matrix(0, nrow(X_all_delta), length(missing_cols),
           dimnames = list(NULL, missing_cols))
  )
}
X_all_delta <- X_all_delta[, lgb_impr_features, drop = FALSE]

pred_delta_lgb <- as.numeric(predict(lgb_impr_model, X_all_delta))
pred_log_lgb   <- model_data_impr_delta$log_appr_imps_val_lag1 + pred_delta_lgb

lgb_rmse_level_from_delta <- sqrt(mean(
  (model_data_impr_delta$log_appr_imps_val - pred_log_lgb)^2,
  na.rm = TRUE
))

rmse_comparison_impr_delta <- data.frame(
  Model = "LightGBM (delta rolling CV, reconstructed level)",
  RMSE  = lgb_rmse_level_from_delta
)
print(rmse_comparison_impr_delta)


# ------------------------------------------------------------------
# 6) LEVEL MODEL (NEW): predict log_appr_imps_val directly (rolling CV)
#    This is the fallback model for missing-lag cases in retrofit.
#    Key choice: do NOT require lag1 in training, so it can predict without lag1.
# ------------------------------------------------------------------


# remove delta (leakage risk because it embeds lag1)
model_data_impr_level <- model_data_impr_level %>%
  select(-delta_log_impr)

# drop single-level + NZV
model_data_impr_level <- drop_single_level_and_nzv(
  model_data_impr_level,
  outcome_cols = c(
    "log_appr_imps_val",
    "log_appr_imps_val_lag1",
    "log_appr_imps_val_lag2"
  )
)

model_data_impr_level <- impute_predictors(
  model_data_impr_level,
  outcome_cols = c("log_appr_imps_val")
)

impr_folds_level <- make_rolling_year_folds(model_data_impr_level$tax_yr)

train_impr_level <- model_data_impr_level %>%
  select(
    -log_appr_imps_val_lag1,
    -log_appr_imps_val_lag2,
    -tax_yr
  )

dv_impr_level <- caret::dummyVars(
  log_appr_imps_val ~ .,
  data = train_impr_level,
  fullRank = TRUE
)

lgb_impr_level_cv <- train_lgbm_log_model(
  df      = train_impr_level,
  outcome = "log_appr_imps_val",
  folds   = impr_folds_level,
  seed    = 123
)

lgb_impr_level_model    <- lgb_impr_level_cv$model
lgb_impr_level_features <- lgb_impr_level_cv$x_cols

message(
  "LightGBM IMPROVEMENT LEVEL rolling CV RMSE (log level): ",
  round(lgb_impr_level_cv$cv_rmse, 4)
)

# (optional) in-sample encoded RMSE sanity check using the SAME encoding schema
X_all_level <- as.matrix(predict(dv_impr_level, newdata = train_impr_level))

missing_cols_lvl <- setdiff(lgb_impr_level_features, colnames(X_all_level))
if (length(missing_cols_lvl) > 0) {
  X_all_level <- cbind(
    X_all_level,
    matrix(0, nrow(X_all_level), length(missing_cols_lvl),
           dimnames = list(NULL, missing_cols_lvl))
  )
}
X_all_level <- X_all_level[, lgb_impr_level_features, drop = FALSE]

pred_log_lvl <- as.numeric(predict(lgb_impr_level_model, X_all_level))
y_level <- model_data_impr_level$log_appr_imps_val

rmse_lvl_insample <- sqrt(mean((y_level - pred_log_lvl)^2, na.rm = TRUE))

message("LEVEL model in-sample RMSE (sanity check; should be <= CV RMSE): ",
        round(rmse_lvl_insample, 4))

rmse_comparison_impr_level <- data.frame(
  Model = "LightGBM (level rolling CV)",
  RMSE  = lgb_impr_level_cv$cv_rmse
)
print(rmse_comparison_impr_level)


lgb_impr_level_cv <- nwmls_stamp_mode(lgb_impr_level_cv, .nwmls_mode)

saveRDS(
  lgb_impr_level_cv,
  file.path(model_dir, paste0("lgb_impr_level_cv_", stamp, ".rds"))
)

saveRDS(
  dv_impr_level,
  file.path(model_dir, paste0("dv_impr_level_", stamp, ".rds"))
)

saveRDS(
  model_data_impr_level,
  file.path(cache_dir, "model_data_impr_level_model.rds")
)

message("💾 cached improvements LEVEL model + dv_impr_level + training frame")


# Alias expected by main_ml.R
lgb_impr_delta_cv      <- lgb_impr_cv
lgb_impr_delta_model   <- lgb_impr_model
lgb_impr_delta_features <- lgb_impr_features


# ---- Feature importance by group ----------------------------------------
.classify_feat <- function(feat) dplyr::case_when(
  grepl("^econ_", feat) ~ "econ",
  grepl("^sea_",  feat) ~ "nwmls",
  grepl("^permits_|^val_last_|^sqft_last_|^units_last_", feat) ~ "permits",
  TRUE ~ "parcel"
)

for (.wm in c("delta", "level")) {
  .cv  <- if (.wm == "delta") lgb_impr_cv else lgb_impr_level_cv
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

message("03_model_impr.R loaded (LightGBM delta + level, dv_impr + dv_impr_level built)")
