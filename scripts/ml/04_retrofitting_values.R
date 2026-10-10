# 04_retrofitting_values.R  ---------------------------------------------
# Fill missing land and improvements AV in the parcel-year panel using LightGBM only.
# Cache-first: can be sourced cold without rerunning ETL or models.

# -------------------------------------------------------------------
# 0) CACHE-FIRST LOADING
# -------------------------------------------------------------------

cache_dir <- get0("cache_dir",  envir = .GlobalEnv, ifnotfound = here("data", "cache"))
model_dir <- get0("model_dir",  envir = .GlobalEnv, ifnotfound = here("data", "model"))

model_data_impr_level_path <- file.path(cache_dir, "model_data_impr_level_model.rds")
if (!exists("model_data_impr_level_model", envir=.GlobalEnv) && file.exists(model_data_impr_level_path)) {
  model_data_impr_level_model <- readRDS(model_data_impr_level_path)
  message("✅ loaded cached: model_data_impr_level_model")
}





# helper: load object from cache if missing in env
load_cache_if_missing <- function(obj_name, path) {
  if (!exists(obj_name, envir = .GlobalEnv)) {
    if (!file.exists(path)) stop("Missing cache file for ", obj_name, ": ", path)
    assign(obj_name, readRDS(path), envir = .GlobalEnv)
    message("✅ loaded cached: ", obj_name)
  }
}

# -------------------------------------------------------------------
# helper: force newdata to match dummyVars training schema
# -------------------------------------------------------------------
# -------------------------------------------------------------------
# FIX 2 (improved): seed-row-safe dummyVars prediction (handles char/logical)
# -------------------------------------------------------------------
safe_predict_dummyvars <- function(dv_obj, newdata, train_df = NULL) {
  
  dv_obj$vars <- unlist(dv_obj$vars, use.names = FALSE)
  
  nd <- as.data.frame(newdata)
  if (nrow(nd) == 0) stop("safe_predict_dummyvars: newdata has 0 rows")
  
  # ensure all dv predictors exist
  missing <- setdiff(dv_obj$vars, names(nd))
  if (length(missing) > 0) {
    for (m in missing) nd[[m]] <- NA
  }
  
  # enforce training levels
  for (v in names(dv_obj$lvls)) {
    if (!v %in% names(nd)) next
    lvls <- dv_obj$lvls[[v]]
    x <- as.character(nd[[v]])
    x[!x %in% lvls] <- NA_character_
    nd[[v]] <- factor(x, levels = lvls)
  }
  
  # remaining character/logical vars
  if (!is.null(train_df)) {
    other_cat <- setdiff(
      intersect(dv_obj$vars, names(nd)),
      names(dv_obj$lvls)
    )
    
    if (length(other_cat) > 0) {
      other_cat <- other_cat[
        sapply(nd[, other_cat, drop = FALSE],
               function(x) is.character(x) || is.logical(x))
      ]
      
      for (v in other_cat) {
        tr_lvls <- sort(unique(na.omit(as.character(train_df[[v]]))))
        if (length(tr_lvls) < 2) tr_lvls <- c(tr_lvls, "__DUMMY__")
        x <- as.character(nd[[v]])
        x[!x %in% tr_lvls] <- NA_character_
        nd[[v]] <- factor(x, levels = tr_lvls)
      }
    }
  }
  
  # seed row
  seed <- nd[1, , drop = FALSE]
  for (v in intersect(dv_obj$vars, names(nd))) {
    if (!is.factor(nd[[v]])) next
    obs <- unique(na.omit(as.character(nd[[v]])))
    if (length(obs) < 2) {
      lvls <- levels(nd[[v]])
      if (length(lvls) >= 2) {
        base <- if (length(obs) == 1) obs[1] else lvls[1]
        alt  <- lvls[lvls != base][1]
        seed[[v]] <- factor(alt, levels = lvls)
      }
    }
  }
  
  mm <- predict(dv_obj, newdata = rbind(nd, seed))
  mm[seq_len(nrow(nd)), , drop = FALSE]
}


predict_lgbm_log_safe <- function(lgb_model, dv, feature_names, newdata, train_df = NULL) {
  mm <- safe_predict_dummyvars(dv, newdata, train_df = train_df)
  mm <- as.matrix(mm)

  missing_cols <- setdiff(feature_names, colnames(mm))
  if (length(missing_cols) > 0) {
    mm <- cbind(
      mm,
      matrix(0, nrow(mm), length(missing_cols),
             dimnames = list(NULL, missing_cols))
    )
  }
  mm <- mm[, feature_names, drop = FALSE]

  as.numeric(predict(lgb_model, mm))
}


# helper: find latest stamped model file by prefix
latest_model_file <- function(prefix, dir = model_dir) {
  files <- list.files(dir, pattern = paste0("^", prefix, ".*\\.rds$"), full.names = TRUE)
  if (length(files) == 0) return(NULL)
  files[which.max(file.info(files)$mtime)]
}

# ---- DATA: load panel_tbl if needed ----
if (!exists("panel_tbl", envir=.GlobalEnv) && !exists("panel", envir=.GlobalEnv)) {
  load_cache_if_missing("panel_tbl", file.path(cache_dir, "panel_tbl.rds"))
}


# ---- MODELS: map current pipeline names → names expected by this script ----
# Current pipeline produces: lgb_land_delta_cv / lgb_impr_delta_cv (delta models)
# and lgb_land_level_cv / lgb_impr_level_cv (level fallbacks).
# This script uses lgb_land_cv / lgb_impr_cv for delta, and
# lgb_impr_level_cv for the level fallback — set aliases if needed.

if (!exists("lgb_land_cv", envir = .GlobalEnv)) {
  if (exists("lgb_land_delta_cv", envir = .GlobalEnv)) {
    lgb_land_cv <- lgb_land_delta_cv
  } else {
    f <- latest_model_file("lgb_land_delta_cv")
    if (is.null(f)) stop("No lgb_land_delta_cv model found in ", model_dir)
    lgb_land_cv <- readRDS(f); message("✅ loaded: ", basename(f))
  }
}
if (!exists("lgb_impr_cv", envir = .GlobalEnv)) {
  if (exists("lgb_impr_delta_cv", envir = .GlobalEnv)) {
    lgb_impr_cv <- lgb_impr_delta_cv
  } else {
    f <- latest_model_file("lgb_impr_delta_cv")
    if (is.null(f)) stop("No lgb_impr_delta_cv model found in ", model_dir)
    lgb_impr_cv <- readRDS(f); message("✅ loaded: ", basename(f))
  }
}
if (!exists("dv_land", envir = .GlobalEnv)) {
  if (exists("dv_land_delta", envir = .GlobalEnv)) {
    dv_land <- dv_land_delta
  } else {
    f <- latest_model_file("dv_land_delta")
    if (is.null(f)) stop("No dv_land_delta found in ", model_dir)
    dv_land <- readRDS(f); message("✅ loaded: ", basename(f))
  }
}
if (!exists("dv_impr", envir = .GlobalEnv)) {
  if (exists("dv_impr_delta", envir = .GlobalEnv)) {
    dv_impr <- dv_impr_delta
  } else {
    f <- latest_model_file("dv_impr_delta")
    if (is.null(f)) stop("No dv_impr_delta found in ", model_dir)
    dv_impr <- readRDS(f); message("✅ loaded: ", basename(f))
  }
}
if (!exists("lgb_impr_level_cv", envir = .GlobalEnv)) {
  f <- latest_model_file("lgb_impr_level_cv")
  if (!is.null(f)) { lgb_impr_level_cv <- readRDS(f); message("✅ loaded: ", basename(f)) }
}
if (!exists("dv_impr_level", envir = .GlobalEnv)) {
  if (exists("dv_impr_level", envir = .GlobalEnv)) NULL else {
    f <- latest_model_file("dv_impr_level")
    if (!is.null(f)) { dv_impr_level <- readRDS(f); message("✅ loaded: ", basename(f)) }
  }
}

# NWMLS feature mode: the residential boosters must have been trained in the
# run's mode (nwmls_features, default "level"; unstamped = "level").
if (!exists("nwmls_assert_model_mode", mode = "function"))
  source(here::here("scripts", "ml", "nwmls_features.R"))
nwmls_assert_model_mode(
  c(list(lgb_land_cv = lgb_land_cv, lgb_impr_cv = lgb_impr_cv),
    if (exists("lgb_impr_level_cv"))
      list(lgb_impr_level_cv = lgb_impr_level_cv)),
  nwmls_run_mode(), where = "Step 4 retrofit (residential)")

# Expose booster + feature names
if (!exists("lgb_land_model",         envir = .GlobalEnv)) lgb_land_model         <- lgb_land_cv$model
if (!exists("lgb_impr_model",         envir = .GlobalEnv)) lgb_impr_model         <- lgb_impr_cv$model
if (!exists("lgb_land_features",      envir = .GlobalEnv)) lgb_land_features      <- lgb_land_cv$x_cols
if (!exists("lgb_impr_features",      envir = .GlobalEnv)) lgb_impr_features      <- lgb_impr_cv$x_cols
if (exists("lgb_impr_level_cv", envir = .GlobalEnv)) {
  if (!exists("lgb_impr_level_model",    envir = .GlobalEnv)) lgb_impr_level_model    <- lgb_impr_level_cv$model
  if (!exists("lgb_impr_level_features", envir = .GlobalEnv)) lgb_impr_level_features <- lgb_impr_level_cv$x_cols
}

# ---- OPTIONAL: training frames (only if you’ve cached them) ----
model_data_land_path <- file.path(cache_dir, "model_data_land_model.rds")
model_data_impr_path <- file.path(cache_dir, "model_data_impr_delta_model.rds")

if (!exists("model_data_land_model", envir=.GlobalEnv) && file.exists(model_data_land_path)) {
  model_data_land_model <- readRDS(model_data_land_path)
  message("✅ loaded cached: model_data_land_model")
}
if (!exists("model_data_impr_delta_model", envir=.GlobalEnv) && file.exists(model_data_impr_path)) {
  model_data_impr_delta_model <- readRDS(model_data_impr_path)
  message("✅ loaded cached: model_data_impr_delta_model")
}

# -------------------------------------------------------------------
# 1) Grab panel + training frames into local vars
# -------------------------------------------------------------------

# panel object: prefer panel_tbl, fallback to panel
if (exists("panel_tbl")) {
  panel <- as.data.table(panel_tbl)
} else if (exists("panel")) {
  panel <- as.data.table(panel)
} else {
  stop("Need panel_tbl or panel in environment.")
}

# training frames
if (exists("model_data_land_model")) {
  train_land_df <- model_data_land_model
} else if (exists("model_data_model")) {
  train_land_df <- model_data_model
} else {
  stop("Need model_data_land_model (preferred) or model_data_model.")
}

# Accept either the new name (model_data_impr_delta_model) or old name (model_data_impr_model)
if (exists("model_data_impr_delta_model")) {
  train_impr_df <- model_data_impr_delta_model
} else if (exists("model_data_impr_model")) {
  train_impr_df <- model_data_impr_model
} else {
  stop("Need model_data_impr_delta_model (or legacy model_data_impr_model).")
}

stopifnot(
  exists("dv_land"),
  exists("dv_impr"),
  exists("lgb_land_model"),
  exists("lgb_impr_model"),
  exists("lgb_land_features"),
  exists("lgb_impr_features")
)

train_impr_level_df <- if (exists("model_data_impr_level_model")) model_data_impr_level_model else train_impr_df  # level frame optional; falls back to delta frame

train_impr_delta <- train_impr_df
train_impr_level <- train_impr_level_df

# -------------------------------------------------------------------
# helpers
# -------------------------------------------------------------------

# align panel col types to training data
align_panel_to_train <- function(panel_dt, train_df, pred_cols) {
  for (cn in pred_cols) {
    if (!cn %in% names(train_df) || !cn %in% names(panel_dt)) next
    train_col <- train_df[[cn]]
    
    if (is.factor(train_col)) {
      panel_dt[, (cn) := as.character(get(cn))]
      panel_dt[, (cn) := factor(get(cn), levels = levels(train_col))]
    } else if (is.numeric(train_col)) {
      panel_dt[, (cn) := as.numeric(get(cn))]
    } else if (is.logical(train_col)) {
      panel_dt[, (cn) := {
        x <- toupper(trimws(as.character(get(cn))))
        x %in% c("Y","YES","TRUE","1")
      }]
    } else {
      panel_dt[, (cn) := as.character(get(cn))]
    }
  }
  panel_dt
}

# impute predictors the same way as training
impute_for_prediction <- function(newdata, train_df, dv_obj) {
  cat("impute_for_prediction called\n")
  
  dv_vars <- intersect(
    unlist(dv_obj$vars, use.names = FALSE),
    names(newdata)
  )
  
  if (length(dv_vars) == 0) return(newdata)
  
  # numeric cols: median if in training, else 0
  num_cols <- dv_vars[
    sapply(newdata[, dv_vars, drop = FALSE], is.numeric) |
      sapply(train_df[, dv_vars, drop = FALSE], is.numeric)
  ]
  
  for (cn in num_cols) {
    if (cn %in% names(train_df) && is.numeric(train_df[[cn]])) {
      med <- median(train_df[[cn]], na.rm = TRUE)
      newdata[[cn]][is.na(newdata[[cn]])] <- med
    } else {
      newdata[[cn]][is.na(newdata[[cn]])] <- 0
    }
  }
  
  # factor cols
  fct_cols <- dv_vars[sapply(newdata[, dv_vars, drop = FALSE], is.factor)]
  for (cn in fct_cols) {
    if (!"Unknown" %in% levels(newdata[[cn]])) {
      levels(newdata[[cn]]) <- c(levels(newdata[[cn]]), "Unknown")
    }
    newdata[[cn]] <- forcats::fct_na_value_to_level(newdata[[cn]], level = "Unknown")
  }
  
  newdata
}


# -------------------------------------------------------------------
# 2) LAND retrofitting (log_appr_land_val)  [LEVEL MODEL]
# -------------------------------------------------------------------

# ---- BEFORE land prediction ----
dv_land_use <- rebuild_dv_clean(train_land_df, "log_appr_land_val")

pred_cols_land_raw <- setdiff(names(train_land_df), "log_appr_land_val")
pred_cols_land_raw <- intersect(pred_cols_land_raw, names(panel))

panel <- align_panel_to_train(panel, train_land_df, pred_cols_land_raw)

panel[, log_appr_land_val := log(appr_land_val)]
panel[is.infinite(log_appr_land_val) | is.nan(log_appr_land_val),
      log_appr_land_val := NA_real_]

miss_land_idx  <- is.na(panel$log_appr_land_val)
miss_land_rows <- which(miss_land_idx)

nd_land <- panel[miss_land_idx, ..pred_cols_land_raw] |> as.data.frame()

# hard schema match BEFORE impute / predict
nd_land <- prep_for_dummyvars(nd_land, dv_land_use, train_land_df)

# impute numerics + NA factors the same way as training
nd_land <- impute_for_prediction(nd_land, train_land_df, dv_land_use)

keep_land <- rep(TRUE, nrow(nd_land))  # after impute, everything should be usable

panel[, pred_log_land := NA_real_]

predict_in_chunks <- function(fun, newdata, chunk_size = 50000) {
  n <- nrow(newdata)
  out <- numeric(n)
  idx <- split(seq_len(n), ceiling(seq_len(n) / chunk_size))
  
  for (i in seq_along(idx)) {
    message("Predicting chunk ", i, "/", length(idx))
    out[idx[[i]]] <- fun(newdata[idx[[i]], , drop = FALSE])
    gc()
  }
  out
}

pred_vals_land <- predict_in_chunks(
  function(df) {
    predict_lgbm_log_safe(
      lgb_model     = lgb_land_model,
      dv            = dv_land_use,
      feature_names = lgb_land_features,
      newdata       = df,
      train_df      = train_land_df
    )
  },
  nd_land[keep_land, , drop = FALSE]
)

panel[miss_land_idx, pred_log_land := pred_vals_land]
panel[, pred_land_val := fifelse(is.na(pred_log_land), NA_real_, exp(pred_log_land))]
panel[, appr_land_val_filled := appr_land_val]
panel[miss_land_idx & !is.na(pred_land_val), appr_land_val_filled := pred_land_val]

message("LAND (LGBM): Missing outcomes: ", length(miss_land_rows))
message("LAND (LGBM): Predicted (after impute/align): ", sum(!is.na(panel$pred_log_land) & miss_land_idx))
message("LAND (LGBM): Still missing after predict: ",
        sum(miss_land_idx & is.na(panel$pred_log_land)))

# -------------------------------------------------------------------
# 3) IMPROVEMENTS retrofitting (delta or level depending on training)
# -------------------------------------------------------------------

# detect delta model
is_delta_impr <- "delta_log_impr" %in% names(train_impr_df) ||
  "delta_log_impr" %in% lgb_impr_features

outcome_impr <- if (is_delta_impr) "delta_log_impr" else "log_appr_imps_val"

# Columns to use for prediction
pred_cols_impr_raw <- setdiff(names(train_impr_df), c(outcome_impr, "log_appr_imps_val"))
pred_cols_impr_raw <- intersect(pred_cols_impr_raw, names(panel))

# compute logs + lag1
panel[, appr_imps_val := fifelse(appr_imps_val <= 0, NA_real_, appr_imps_val)]
panel[, log_appr_imps_val := log(appr_imps_val)]
panel[is.infinite(log_appr_imps_val) | is.nan(log_appr_imps_val),
      log_appr_imps_val := NA_real_]

setorder(panel, parcel_id, tax_yr)
panel[, log_appr_imps_val_lag1 := shift(log_appr_imps_val, 1L, type = "lag"),
      by = parcel_id]

miss_impr_idx  <- is.na(panel$log_appr_imps_val)
miss_impr_rows <- which(miss_impr_idx)

# ---- Land-only guard -------------------------------------------------------
# Parcels flagged is_land_only=1 have no structure: their improvement AV is
# legitimately zero, not missing.  Exclude them from the improvement model
# entirely so the imputer cannot fabricate a nonzero improvement value.
if ("is_land_only" %in% names(panel)) {
  n_land_excl <- sum(miss_impr_idx & panel$is_land_only == 1L, na.rm = TRUE)
  miss_impr_idx  <- miss_impr_idx & (is.na(panel$is_land_only) | panel$is_land_only == 0L)
  miss_impr_rows <- which(miss_impr_idx)
  message("  IMPR: ", n_land_excl,
          " land-only parcels excluded from improvement prediction")
}

# rows eligible for DELTA model require lag1
keep_impr <- !is.na(panel$log_appr_imps_val_lag1)[miss_impr_idx]

panel[, pred_log_impr := NA_real_]

# -------------------------------------------------------
# Helper: build safe newdata for dummyVars
# -------------------------------------------------------
build_safe_newdata_for_dv <- function(dv_obj, panel_dt, idx, train_df) {
  nd <- as.data.frame(panel_dt[idx, , drop = FALSE])
  
  # Ensure all dv variables exist
  all_vars <- unlist(dv_obj$vars, use.names = FALSE)
  for (v in all_vars) {
    if (!v %in% names(nd)) nd[[v]] <- NA
  }
  
  # Enforce factor levels from dv_obj$lvls
  if (!is.null(dv_obj$lvls)) {
    for (v in names(dv_obj$lvls)) {
      lvls <- dv_obj$lvls[[v]]
      if (!v %in% names(nd)) nd[[v]] <- factor(NA, levels = lvls)
      else {
        x <- nd[[v]]
        if (!is.factor(x)) x <- factor(as.character(x), levels = lvls)
        else x <- factor(as.character(x), levels = lvls)
        nd[[v]] <- x
      }
    }
  }
  
  # Enforce types from training
  for (v in all_vars) {
    train_col <- train_df[[v]]
    if (is.factor(train_col)) nd[[v]] <- factor(nd[[v]], levels = levels(train_col))
    else if (is.numeric(train_col)) nd[[v]] <- as.numeric(nd[[v]])
    else if (is.character(train_col)) nd[[v]] <- as.character(nd[[v]])
    else if (is.logical(train_col)) nd[[v]] <- as.logical(nd[[v]])
  }
  
  nd
}

all_dv_vars <- unique(c(
  unlist(dv_impr$vars, use.names = FALSE),
  names(dv_impr$lvls)  # in case factor levels exist
))

all_lags <- grep("log_appr_imps_val_lag", unlist(dv_impr$vars), value = TRUE)
setorder(panel, parcel_id, tax_yr)
for (lag_col in all_lags) {
  lag_n <- as.integer(gsub(".*lag", "", lag_col))
  panel[, (lag_col) := shift(log_appr_imps_val, lag_n), by = parcel_id]
}


# -------------------------------------------------------
# 1) DELTA MODEL (log delta)
# -------------------------------------------------------
if (any(keep_impr)) {
  
  # --- Safe DV vars: only those in training
  dv_vars_safe <- intersect(unlist(dv_impr$vars, use.names = FALSE), names(train_impr_delta))
  dv_impr_safe <- dv_impr
  dv_impr_safe$vars <- dv_vars_safe
  if (!is.null(dv_impr_safe$lvls)) {
    dv_impr_safe$lvls <- dv_impr_safe$lvls[names(dv_impr_safe$lvls) %in% dv_vars_safe]
  }
  
  # --- Fully safe newdata builder
  build_safe_newdata_for_dv <- function(dv_obj, panel_dt, idx, train_df) {
    nd <- as.data.frame(panel_dt[idx, , drop = FALSE])
    all_vars <- unique(c(unlist(dv_obj$vars, use.names = FALSE),
                         names(dv_obj$lvls)))
    
    for (v in all_vars) {
      if (!v %in% names(nd)) nd[[v]] <- NA
    }
    
    # enforce factor levels from dv_obj$lvls
    if (!is.null(dv_obj$lvls)) {
      for (v in names(dv_obj$lvls)) {
        lvls <- dv_obj$lvls[[v]]
        if (!v %in% names(nd)) nd[[v]] <- factor(NA, levels = lvls)
        else nd[[v]] <- factor(as.character(nd[[v]]), levels = lvls)
      }
    }
    
    # enforce types from training
    for (v in all_vars) {
      if (!v %in% names(nd)) next
      train_col <- train_df[[v]]
      if (is.factor(train_col)) nd[[v]] <- factor(nd[[v]], levels = levels(train_col))
      else if (is.numeric(train_col)) nd[[v]] <- as.numeric(nd[[v]])
      else if (is.character(train_col)) nd[[v]] <- as.character(nd[[v]])
      else if (is.logical(train_col)) nd[[v]] <- as.logical(nd[[v]])
    }
    
    nd
  }
  
  # --- Build fully safe newdata
  nd_impr_delta_safe <- build_safe_newdata_for_dv(dv_impr_safe, panel, miss_impr_idx, train_impr_delta)
  
  # --- DummyVars + impute
  nd_impr_delta_safe <- prep_for_dummyvars(nd_impr_delta_safe, dv_impr_safe, train_impr_delta)
  nd_impr_delta_safe <- impute_for_prediction(
    nd_impr_delta_safe,
    train_impr_delta[, dv_vars_safe, drop = FALSE],
    dv_impr_safe
  )
  
  # --- Predict in chunks
  pred_vals_impr_delta <- predict_in_chunks(
    function(df) {
      predict_lgbm_log_safe(
        lgb_model     = lgb_impr_model,
        dv            = dv_impr_safe,
        feature_names = lgb_impr_features,
        newdata       = df,
        train_df      = train_impr_delta[, dv_vars_safe, drop = FALSE]
      )
    },
    nd_impr_delta_safe[keep_impr, , drop = FALSE]
  )
  
  # --- Add lag1 back
  panel[miss_impr_rows[keep_impr], pred_log_impr :=
          panel[miss_impr_rows[keep_impr], log_appr_imps_val_lag1] +
          pred_vals_impr_delta]
}

# -------------------------------------------------------
# 2) LEVEL FALLBACK MODEL for remaining missing rows
# -------------------------------------------------------
remaining_idx  <- miss_impr_idx & is.na(panel$pred_log_impr)
remaining_rows <- which(remaining_idx)

if (length(remaining_rows) > 0) {
  
  train_impr_level_df <- train_impr_level
  
  dv_level_safe <- dv_impr_level
  dv_level_safe$vars <- intersect(unlist(dv_level_safe$vars, use.names = FALSE), names(train_impr_level_df))
  if (!is.null(dv_level_safe$lvls)) {
    dv_level_safe$lvls <- dv_level_safe$lvls[names(dv_level_safe$lvls) %in% dv_level_safe$vars]
  }
  
  nd_impr_lvl_safe <- build_safe_newdata_for_dv(dv_level_safe, panel, remaining_idx, train_impr_level_df)
  
  nd_impr_lvl_safe <- prep_for_dummyvars(nd_impr_lvl_safe, dv_level_safe, train_impr_level_df)
  nd_impr_lvl_safe <- impute_for_prediction(nd_impr_lvl_safe, train_impr_level_df, dv_level_safe)
  
  pred_lvl <- predict_lgbm_log_safe(
    lgb_model     = lgb_impr_level_model,
    dv            = dv_level_safe,
    feature_names = lgb_impr_level_features,
    newdata       = nd_impr_lvl_safe,
    train_df      = train_impr_level_df
  )
  
  panel[remaining_rows, pred_log_impr := pred_lvl]
}

# -------------------------------------------------------
# 3) Fill final improvements value
# -------------------------------------------------------
panel[, pred_impr_val := fifelse(is.na(pred_log_impr), NA_real_, exp(pred_log_impr))]
panel[, appr_imps_val_filled := appr_imps_val]
panel[miss_impr_idx & !is.na(pred_impr_val), appr_imps_val_filled := pred_impr_val]

# Zero out improvement AV for land-only parcels.
# appr_imps_val is already 0/NA for them, but if any slipped through the
# guard above (e.g. is_land_only added after panel was built), make it
# explicit here so total_assessed_filled is never inflated.
if ("is_land_only" %in% names(panel)) {
  n_zeroed <- sum(panel$is_land_only == 1L & !is.na(panel$appr_imps_val_filled) &
                  panel$appr_imps_val_filled > 0, na.rm = TRUE)
  panel[is_land_only == 1L, appr_imps_val_filled := 0]
  if (n_zeroed > 0)
    message("  IMPR: ", n_zeroed,
            " land-only parcel-years zeroed out in appr_imps_val_filled")
}

message("IMPR (LGBM): Missing outcomes: ", length(miss_impr_rows))
message("IMPR (LGBM): Predicted (after impute/align): ", sum(!is.na(panel$pred_log_impr) & miss_impr_idx))
message("IMPR (LGBM): Still missing after predict: ", sum(miss_impr_idx & is.na(panel$pred_log_impr)))

# -------------------------------------------------------------------
# 4) Combined filled totals + logs
# -------------------------------------------------------------------

panel[, total_assessed_filled := appr_land_val_filled + appr_imps_val_filled]
panel[, log_total_assessed_filled := log(total_assessed_filled)]
panel[is.infinite(log_total_assessed_filled) | is.nan(log_total_assessed_filled),
      log_total_assessed_filled := NA_real_]

# -------------------------------------------------------------------
# 5) Write out + plots (your plots mostly unchanged)
# -------------------------------------------------------------------

# Write parquet to output_dir if available, skip silently if not
tryCatch({
  out_dir_retro <- get0("output_dir", envir = .GlobalEnv, ifnotfound = here("data", "outputs"))
  dir.create(out_dir_retro, recursive = TRUE, showWarnings = FALSE)
  out_parquet <- file.path(out_dir_retro,
                           paste0("parcel_year_panel_2000_2025_", Sys.Date(), ".parquet"))
  arrow::write_parquet(panel, out_parquet)
  message("💾 parquet: ", basename(out_parquet))
}, error = function(e) {
  message("  ℹ️  parquet write skipped: ", e$message)
})

panel_tbl <- as_tibble(panel)

#write_csv(panel_tbl %>% slice_sample(n = 2000),
 #         here("data","wrangled","panel_tbl.csv"))

panel_tbl %>%
  group_by(tax_yr) %>%
  summarise(
    n_rows           = n(),
    n_na_land        = sum(is.na(appr_land_val_filled)),
    n_na_impr        = sum(is.na(appr_imps_val_filled)),
    n_na_tot_filled  = sum(is.na(total_assessed_filled)),
    .groups = "drop"
  ) %>%
  print(n = 26)

# -------------------------------------------------------------------
# Cache retrofitted panel_tbl for downstream scripts
# -------------------------------------------------------------------
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
# Assign panel_tbl_retro to GlobalEnv so main_ml.R can pick it up
assign("panel_tbl_retro", panel_tbl, envir = .GlobalEnv)

retro_cache_path <- file.path(cache_dir, "panel_tbl_retro.rds")
saveRDS(panel_tbl, retro_cache_path)
message("💾 cached retrofitted panel_tbl to: ", retro_cache_path)

message("04_retrofitting_values.R loaded (LightGBM only, cache-first).")
