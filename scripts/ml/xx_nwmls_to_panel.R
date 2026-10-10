# scripts/ml/xx_nwmls_to_panel.R -------------------------------------------
# Join NWMLS SFH market indicators to panel_tbl (residential backbone).
# Reads from the EViews Excel export (preferred) or legacy CSV (fallback).
# Reads `scenario` from .GlobalEnv (set by main_ml.R).
#
# File: data/nwmls/YYYY-MM_nwmls_housing_forecast_<scenario>.xlsx, newest
# YYYY-MM prefix wins (nwmls_resolve_file()); all scenarios must agree.
#
# Excel layout (from EViews tbl_export.save):
#   Row 1: col A blank, col B onward = series names
#   Row 2: blank
#   Row 3+: col A = date string "2005M01", col B onward = values
#
# Gracefully handles missing KC SFH columns.
#
# The read / lag / December-snapshot code lives in nwmls_features.R
# (nwmls_read_raw, nwmls_build_annual) so the sensitivity test and the growth
# cache rebuild use the same code.  Output columns, in order:
#   tax_yr, the six sea_* levels, the six k_* levels (if present),
#   then the growth columns sea_*_lag{6,12}_yoy and sea_mos_sfh_lag12.
# The growth columns are added in BOTH nwmls_features modes; which ones a
# model may use is decided in 03_model_*.R via nwmls_model_cols().
# -------------------------------------------------------------------------

if (!exists("nwmls_build_annual", mode = "function"))
  source(here("scripts", "ml", "nwmls_features.R"))

scenario <- get0("scenario", envir = .GlobalEnv, ifnotfound = "baseline")
message("xx_nwmls_to_panel.R: scenario = ", scenario)

# ---- Locate and read the NWMLS data file --------------------------------
# File choice = newest YYYY-MM vintage prefix in the name (not modified
# time); stops if the three scenarios resolve to different vintages.
nwmls_dir <- here("data", "nwmls")
nwmls_vintages <- nwmls_check_vintages(nwmls_dir)
for (.i in seq_len(NROW(nwmls_vintages)))
  message("  NWMLS ", nwmls_vintages[["scenario"]][.i], " -> ",
          nwmls_vintages[["file"]][.i])
nwmls_raw <- nwmls_read_raw(scenario, nwmls_dir)[["data"]]

# ---- Detect columns, rolling windows, December snapshot → next tax_yr ---
nwmls_built   <- nwmls_build_annual(nwmls_raw)
nwmls_av_year <- nwmls_built[["annual"]]
has_kc_sfh    <- nwmls_built[["has_kc"]]
rm(nwmls_built)

# ---- Cache forecast years -----------------------------------------------
cache_dir <- get0("cache_dir", envir = .GlobalEnv,
                  ifnotfound = here("data", "cache"))
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

nwmls_fcst_out <- nwmls_fcst_slice(
  nwmls_av_year,
  forecast_start = get0("forecast_start", envir = .GlobalEnv, ifnotfound = 2027L),
  forecast_end   = get0("forecast_end",   envir = .GlobalEnv, ifnotfound = 2032L))

nwmls_fcst_path <- nwmls_fcst_cache_path(cache_dir, scenario)
saveRDS(nwmls_fcst_out, nwmls_fcst_path)
message("\U0001f4be NWMLS forecast cached to: ", basename(nwmls_fcst_path))

# ---- Join to panel_tbl --------------------------------------------------
panel_tbl <- panel_tbl %>%
  left_join(nwmls_av_year, by = "tax_yr")

message("xx_nwmls_to_panel.R loaded (scenario = ", scenario,
        " | KC SFH = ", has_kc_sfh, ")")
