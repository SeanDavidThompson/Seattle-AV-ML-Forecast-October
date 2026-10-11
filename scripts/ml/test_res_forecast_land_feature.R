# test_res_forecast_land_feature.R ---------------------------------------------------
# 06_forecast_av_2026_2031_sequential.R on synthetic data with a fake booster
# (no LightGBM needed), both nwmls_features modes:
#   - current-year land (log_appr_land_val), an improvement predictor, is
#     filled from log_land_filled in every forecast year, so the improvement
#     prediction varies with each parcel's land instead of getting the
#     training median;
#   - the pre-check does not trip on log_appr_land_val (the loop fills it);
#   - the in-loop check stops when an improvement feature is still all-NA
#     right before predict().
# Needs data.table, dplyr, tibble, caret, scales, here, arrow (any version:
# only write_parquet is called).
#
#   Rscript scripts/ml/test_res_forecast_land_feature.R
# -----------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(data.table); library(dplyr); library(tibble); library(here)
  library(caret); library(scales)
})
source(here::here("scripts", "ml", "nwmls_features.R"))

# Fake booster: delta = a + b * feature, feature chosen per model.
predict.fakebst <- function(object, newdata, ...)
  attr(object, "a") + attr(object, "b") * (as.numeric(newdata[, attr(object, "f")]) - attr(object, "c"))

test_dir_06 <- file.path(tempdir(), "res_forecast_land_feature")
dir.create(test_dir_06, showWarnings = FALSE)   # not "tmp": 06 assigns its own tmp

mk_model <- function(outcome, feats, f, a, b, c, mode) {
  set.seed(4)
  tr <- data.table(area = factor(sample(c("1", "2"), 300, TRUE)))
  for (x in feats) tr[, (x) := rnorm(300, 12.5, .5)]
  tr[, (outcome) := rnorm(300, 0, .02)]
  form <- as.formula(paste(outcome, "~ ."))
  dv <- dummyVars(form, data = tr, fullRank = TRUE)
  X <- model.matrix(form, data = tr)[, -1]
  cv <- nwmls_stamp_mode(list(model = structure(list(), class = "fakebst", f = f, a = a, b = b, c = c),
                              x_cols = colnames(X)), mode)
  list(cv = cv, dv = dv, tr = as_tibble(tr))
}

mk_panel <- function() {
  set.seed(5)
  ids <- sprintf("%04d", 1:40)
  p <- CJ(parcel_id = ids, tax_yr = 2020:2031)
  p[, area := factor(sample(c("1", "2"), .N, TRUE))]
  land0 <- exp(runif(40, 11.5, 13.5))[match(p$parcel_id, ids)]   # parcel land varies
  p[, appr_land_val := fifelse(tax_yr <= 2026, land0, NA_real_)]
  p[, appr_imps_val := fifelse(tax_yr <= 2026, 4e5, NA_real_)]
  p[, `:=`(appr_land_val_filled = appr_land_val, appr_imps_val_filled = appr_imps_val)]
  p[, total_assessed_filled := appr_land_val_filled + appr_imps_val_filled]
  p[, log_appr_land_val := fifelse(tax_yr <= 2026, log(appr_land_val), NA_real_)]  # as 05_extend leaves it
  p[, sea_x := 0.02]
  p[, econ_y := 0.01]
  p
}

run06 <- function(mode, panel) {
  for (o in ls(.GlobalEnv, pattern = "^(lgb_|dv_|model_data_|panel_tbl_)"))
    rm(list = o, envir = .GlobalEnv)
  ml <- mk_model("delta_log_land", c("sea_x", "log_appr_land_val_lag1"), "sea_x",
                 0.03, 0, 0, mode)
  mi <- mk_model("delta_log_impr", c("log_appr_land_val", "econ_y", "log_appr_imps_val_lag1"),
                 "log_appr_land_val", 0.02, 0.05, 12.5, mode)
  assign("lgb_land_delta_cv", ml[["cv"]], .GlobalEnv); assign("dv_land_delta", ml[["dv"]], .GlobalEnv)
  assign("model_data_land_delta_model", ml[["tr"]], .GlobalEnv)
  assign("lgb_impr_delta_cv", mi[["cv"]], .GlobalEnv); assign("dv_impr_delta", mi[["dv"]], .GlobalEnv)
  assign("model_data_impr_delta_model", mi[["tr"]], .GlobalEnv)
  assign("panel_tbl_2006_2031_inputs_baseline_res", panel, .GlobalEnv)
  assign("cache_dir", test_dir_06, .GlobalEnv); assign("model_dir", test_dir_06, .GlobalEnv)
  assign("output_dir", test_dir_06, .GlobalEnv); assign("scenario", "baseline", .GlobalEnv)
  assign("forecast_end", 2031L, .GlobalEnv); assign("nwmls_features", mode, .GlobalEnv)
  if (exists("area_report_actuals", envir = .GlobalEnv)) rm("area_report_actuals", envir = .GlobalEnv)
  suppressMessages(source(here::here("scripts", "ml", "06_forecast_av_2026_2031_sequential.R"),
                          local = .GlobalEnv))
  as.data.table(get("panel_tbl_forecasted_res", envir = .GlobalEnv))
}

for (mode in c("level", "growth")) {
  out <- run06(mode, mk_panel())
  f <- out[tax_yr >= 2027]
  # land filled into the improvement predictor every forecast year
  stopifnot(identical(attr(get("panel_tbl_forecasted_res", envir = .GlobalEnv),
                            "nwmls_features"), mode),           # 06 stamps its mode
            all(!is.na(f[["log_appr_land_val"]])),
            isTRUE(all.equal(f[["log_appr_land_val"]], f[["log_land_filled"]])))
  # improvement delta follows each parcel's land: 0.02 + 0.05 * (land - 12.5)
  d <- f[, .(parcel_id, tax_yr, dimp = log_impr_filled - log_impr_filled_lag1,
             expect = 0.02 + 0.05 * (log_land_filled - 12.5))]
  stopifnot(max(abs(d[["dimp"]] - d[["expect"]])) < 1e-10,
            d[tax_yr == 2027, uniqueN(round(dimp, 10))] > 30)   # not one median value
  cat("PASS  ", mode, ": log_appr_land_val = log_land_filled in 2027-2031; ",
      "improvement delta varies with parcel land\n", sep = "")
}

# in-loop check: an improvement feature all-NA in 2029.  growth: the
# pre-check stops first; level: the pre-check only warns, so the in-loop
# check is what stops.
p <- mk_panel(); p[tax_yr == 2029, econ_y := NA_real_]
err <- tryCatch({ run06("growth", copy(p)); "no error" }, error = conditionMessage)
stopifnot(grepl("ALL-NA in: econ_y \\(2029\\)", err))
w <- character(0)
err <- withCallingHandlers(
  tryCatch({ run06("level", copy(p)); "no error" }, error = conditionMessage),
  warning = function(x) { w <<- c(w, conditionMessage(x)); invokeRestart("muffleWarning") })
stopifnot(any(grepl("ALL-NA in: econ_y \\(2029\\)", w)),
          grepl("tax_yr 2029: improvement booster feature\\(s\\) missing or all-NA.*econ_y", err))
cat("PASS  all-NA improvement feature: growth stops in the pre-check; level warns, then the in-loop check stops at 2029\n")

cat("\nAll residential forecast land-feature checks passed.\n")
