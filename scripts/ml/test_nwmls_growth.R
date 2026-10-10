# test_nwmls_growth.R ------------------------------------------------------------
# Synthetic-data checks for the nwmls_features flag (scripts/ml/nwmls_features.R):
# growth columns, per-mode column sets, the hard stops, and the sensitivity
# shock.  No pipeline data, no LightGBM.
#
#   Rscript scripts/ml/test_nwmls_growth.R
# -----------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(data.table); library(dplyr); library(here)
  library(slider); library(zoo); library(lubridate)
})
MAIN_ML_DEFINE_ONLY <- TRUE
source(here::here("scripts", "ml", "main_ml.R"))   # CFG + nwmls_features.R

expect_error <- function(expr, pattern) {
  msg <- tryCatch({ force(expr); NULL }, error = function(e) conditionMessage(e))
  if (is.null(msg)) stop("expected an error matching '", pattern, "', got none")
  if (!grepl(pattern, msg)) stop("error did not match '", pattern, "': ", msg)
  invisible(msg)
}

# ---- 0. default is "level", in CFG and in run_main_ml() ----------------------
stopifnot(identical(CFG[["nwmls_features"]], "level"))
stopifnot("nwmls_features" %in% names(formals(run_main_ml)))
cat("PASS  default: CFG$nwmls_features == \"level\" and run_main_ml() takes nwmls_features\n")

# ---- synthetic monthly export: 2005M01-2008M12, last observed 2007M06 --------
dates <- seq(as.Date("2005-01-01"), as.Date("2008-12-01"), by = "month")
n <- length(dates)
i <- seq_len(n)
raw <- tibble::tibble(
  date             = dates,
  sea_pmedesfh_fav = 400000 * 1.01^(i - 1) + 5000 * sin(i),   # price
  sea_sesfh_fav    = 500 + 100 * cos(i / 2),                  # monthly sales
  sea_alesfh_fav   = 1200 + 300 * sin(i / 3),                 # active listings
  sea_pmedesfh     = ifelse(dates <= as.Date("2007-06-01"),
                            400000 * 1.01^(i - 1) + 5000 * sin(i), NA_real_)
)
built <- suppressMessages(nwmls_build_annual(raw))
ann <- built[["annual"]]

# ---- 1. _yoy = hand-computed log growth of December trailing means -----------
dec_mean <- function(x, yr, w) {
  end <- which(dates == as.Date(sprintf("%d-12-01", yr)))
  mean(x[(end - w + 1L):end])
}
for (b in c("sea_pmedesfh", "sea_sesfh", "sea_alesfh")) for (w in c(6L, 12L)) {
  x <- raw[[paste0(b, "_fav")]]
  col <- paste0(b, "_lag", w)
  for (yr in 2006:2008) {                     # tax_yr = yr + 1
    lvl  <- dec_mean(x, yr, w)
    hand <- log(lvl / dec_mean(x, yr - 1L, w))
    stopifnot(abs(ann[[col]][ann[["tax_yr"]] == yr + 1] - lvl) < 1e-6,
              abs(ann[[paste0(col, "_yoy")]][ann[["tax_yr"]] == yr + 1] - hand) < 1e-12)
  }
  # first December (2005 -> tax_yr 2006) has no prior year
  stopifnot(is.na(ann[[paste0(col, "_yoy")]][ann[["tax_yr"]] == 2006]))
}
mos <- dec_mean(raw[["sea_alesfh_fav"]], 2007, 12L) / dec_mean(raw[["sea_sesfh_fav"]], 2007, 12L)
stopifnot(abs(ann[["sea_mos_sfh_lag12"]][ann[["tax_yr"]] == 2008] - mos) < 1e-12)
# a gap in tax_yr gives NA, not a two-year change
gap <- nwmls_add_growth(as.data.frame(ann)[ann[["tax_yr"]] != 2007, c("tax_yr", NWMLS_LEVEL_COLS)])
stopifnot(is.na(gap[["sea_pmedesfh_lag12_yoy"]][gap[["tax_yr"]] == 2008]))
# levels first, in today's order; growth appended after
stopifnot(identical(names(ann)[1:7], c("tax_yr", NWMLS_LEVEL_COLS)),
          identical(tail(names(ann), 7), NWMLS_GROWTH_COLS))
cat("PASS  _yoy == log(Dec trailing mean[t] / [t-1]) for 3 series x 2 windows; mos; gap -> NA\n")

# ---- 2. level mode: exactly today's six columns, both frames ----------------
today <- c("sea_pmedesfh_lag12", "sea_pmedesfh_lag6", "sea_sesfh_lag6",
           "sea_sesfh_lag12", "sea_alesfh_lag6", "sea_alesfh_lag12")
stopifnot(identical(nwmls_model_cols("level", "delta"), today),
          identical(nwmls_model_cols("level", "level"), today))
cat("PASS  nwmls_model_cols(\"level\", delta|level) == today's six, same order\n")

# ---- 3. growth mode: delta has no levels, level has no _yoy / mos ------------
gd <- nwmls_model_cols("growth", "delta")
gl <- nwmls_model_cols("growth", "level")
stopifnot(!length(intersect(gd, today)),
          all(grepl("_yoy$", gd) | gd == "sea_mos_sfh_lag12"),
          length(gd) == 7L,
          identical(gl, today),
          !any(grepl("_yoy$|mos", gl)))
expect_error(nwmls_model_cols("levels", "delta"), "nwmls_features must be one of")
cat("PASS  growth: delta = 6 _yoy + mos, no levels; level = six levels, no _yoy/mos\n")

# ---- 4a. mode-mismatch stop -------------------------------------------------
m_level   <- list(model = NULL, x_cols = "a")                       # unstamped
m_growth  <- nwmls_stamp_mode(list(model = NULL, x_cols = "a"), "growth")
stopifnot(nwmls_model_mode(m_level) == "level", nwmls_model_mode(m_growth) == "growth")
nwmls_assert_model_mode(list(land = m_level, impr = m_level), "level")
nwmls_assert_model_mode(list(land = m_growth), "growth")
expect_error(nwmls_assert_model_mode(list(land = m_level, impr = m_level), "growth", "Step 6"),
             "Step 6: nwmls_features = \"growth\" but .*land = \"level\"")
expect_error(nwmls_assert_model_mode(list(land = m_growth), "level"),
             "trained with land = \"growth\"")
cat("PASS  mode mismatch stops (unstamped = level; growth model in level run; level model in growth run)\n")

# ---- 4b. all-NA / missing forecast-feature stop ------------------------------
p <- CJ(parcel_id = sprintf("%03d", 1:20), tax_yr = 2026:2031)
p[, sea_pmedesfh_lag12_yoy := 0.01]
p[, econ_x_yoy_lag1 := 0.02]
nwmls_assert_forecast_features(p, c("sea_pmedesfh_lag12_yoy", "econ_x_yoy_lag1"),
                               2027:2031, "growth")
p[tax_yr == 2029, sea_pmedesfh_lag12_yoy := NA_real_]
expect_error(nwmls_assert_forecast_features(p, "sea_pmedesfh_lag12_yoy", 2027:2031,
                                            "growth", "Step 6"),
             "ALL-NA in: sea_pmedesfh_lag12_yoy \\(2029\\)")
expect_error(nwmls_assert_forecast_features(p, "sea_mos_sfh_lag12", 2027:2031, "growth"),
             "MISSING from the panel: sea_mos_sfh_lag12")
# level mode reports the same thing as a warning and does not stop
w <- tryCatch(nwmls_assert_forecast_features(p, "sea_pmedesfh_lag12_yoy", 2027:2031, "level"),
              warning = function(w) conditionMessage(w))
stopifnot(is.character(w), grepl("ALL-NA", w))
# one parcel NA is not all-NA
p[tax_yr == 2029, sea_pmedesfh_lag12_yoy := 0.01]
p[tax_yr == 2029 & parcel_id == "001", sea_pmedesfh_lag12_yoy := NA_real_]
nwmls_assert_forecast_features(p, "sea_pmedesfh_lag12_yoy", 2027:2031, "growth")
cat("PASS  all-NA / missing forecast feature stops in growth (warns in level)\n")

# ---- 4c. stale-cache check --------------------------------------------------
tmp <- file.path(tempdir(), "nwmls_growth_test"); dir.create(tmp, showWarnings = FALSE)
cp <- nwmls_fcst_cache_path(tmp, "baseline")
saveRDS(as.data.frame(ann)[, c("tax_yr", today)], cp)          # pre-branch cache
stopifnot(identical(nwmls_ensure_fcst_cache(cp, "level"), "ok"))      # level: has its six
expect_error(nwmls_ensure_fcst_cache(cp, "growth"),
             "lacks 7 growth column\\(s\\).*nwmls_write_fcst_cache")
unlink(cp)
expect_error(nwmls_ensure_fcst_cache(cp, "growth"), "is missing")
# a rebuild that writes the right columns clears it; one that does not still stops
saveRDS(as.data.frame(ann)[, c("tax_yr", today)], cp)
st <- suppressMessages(nwmls_ensure_fcst_cache(cp, "growth",
        rebuild = function() saveRDS(nwmls_fcst_slice(ann, 2007L, 2009L), cp)))
stopifnot(identical(st, "rebuilt"), all(NWMLS_GROWTH_COLS %in% names(readRDS(cp))))
saveRDS(as.data.frame(ann)[, c("tax_yr", today)], cp)
expect_error(suppressMessages(nwmls_ensure_fcst_cache(cp, "growth", rebuild = function() NULL)),
             "still lacks")
cat("PASS  stale NWMLS cache: growth stops with the rebuild instruction, or rebuilds; level untouched\n")

# ---- 4d. panel join for growth mode -----------------------------------------
pan <- data.table(parcel_id = rep(c("a", "b"), each = 3), tax_yr = rep(2006:2008, 2))
pan <- merge(pan, as.data.table(ann)[, c("tax_yr", today), with = FALSE], by = "tax_yr")
lvl_before <- copy(pan)
pan2 <- suppressMessages(nwmls_ensure_panel_cols(pan, "growth", ann))
stopifnot(all(NWMLS_GROWTH_COLS %in% names(pan2)),
          identical(as.data.frame(pan2)[, names(lvl_before)], as.data.frame(lvl_before)),
          all(abs(pan2[tax_yr == 2008, sea_pmedesfh_lag12_yoy] -
                  ann[["sea_pmedesfh_lag12_yoy"]][ann[["tax_yr"]] == 2008]) < 1e-15))
stopifnot(identical(nwmls_ensure_panel_cols(lvl_before, "level", ann), lvl_before))
cat("PASS  growth columns joined onto a pre-branch panel by tax_yr; levels untouched; level mode no-op\n")

# ---- 5. sensitivity shock touches forecast months only ----------------------
lo <- nwmls_last_observed_month(raw)
stopifnot(identical(lo, as.Date("2007-06-01")))
sh <- nwmls_shock_price(raw, 0.95, lo)
obs <- raw[["date"]] <= lo
stopifnot(identical(sh[["sea_pmedesfh_fav"]][obs], raw[["sea_pmedesfh_fav"]][obs]),
          all(abs(sh[["sea_pmedesfh_fav"]][!obs] / raw[["sea_pmedesfh_fav"]][!obs] - 0.95) < 1e-12),
          identical(sh[["sea_sesfh_fav"]], raw[["sea_sesfh_fav"]]),
          identical(sh[["sea_alesfh_fav"]], raw[["sea_alesfh_fav"]]))
a_sh <- suppressMessages(nwmls_build_annual(sh))[["annual"]]
# tax_yr 2006-2007 use Decembers 2005-2006, all observed: identical
stopifnot(identical(a_sh[a_sh[["tax_yr"]] <= 2007, ], ann[ann[["tax_yr"]] <= 2007, ]))
# tax_yr 2009 (Dec 2008 window entirely after the boundary): level x 0.95
stopifnot(abs(a_sh[["sea_pmedesfh_lag12"]][a_sh[["tax_yr"]] == 2009] /
                ann[["sea_pmedesfh_lag12"]][ann[["tax_yr"]] == 2009] - 0.95) < 1e-12)
cat("PASS  shock: observed months unchanged, forecast months x0.95, sales/listings untouched\n")

# ---- 5b. on the real export, if this checkout has it -------------------------
june <- list.files(here::here("data", "nwmls"),
                   pattern = "^2026-06_nwmls_housing_forecast_baseline\\.xlsx$",
                   full.names = TRUE)
if (length(june)) {
  r <- suppressMessages(suppressWarnings(readxl::read_xlsx(june, col_names = TRUE)))
  names(r)[1] <- "date_str"; names(r) <- tolower(names(r))
  r[-1] <- lapply(r[-1], function(x) suppressWarnings(as.numeric(as.character(x))))
  r <- r[!is.na(r[["date_str"]]) & r[["date_str"]] != "", ]
  r[["date"]] <- as.Date(paste0(substr(r[["date_str"]], 1, 4), "-",
                                substr(r[["date_str"]], 6, 7), "-01"))
  stopifnot(identical(nwmls_last_observed_month(r), as.Date("2026-06-01")))
  cat("PASS  real 2026-06 baseline export: last observed month detected as 2026-06\n")
}

# ---- 6. file choice: newest YYYY-MM prefix, not modified time --------------
vd <- file.path(tempdir(), "nwmls_vintage_test")
mkfiles <- function(names) {
  unlink(vd, recursive = TRUE); dir.create(vd)
  for (n in names) writeLines("x", file.path(vd, n))
}
fn <- function(v, sc) sprintf("%s_nwmls_housing_forecast_%s.xlsx", v, sc)
mkfiles(c(fn("2026-03", c("baseline", "optimistic", "pessimistic")),
          fn("2026-06", c("baseline", "optimistic", "pessimistic"))))
# make the OLDER vintage the newest by modified time: the pick must ignore it
Sys.setFileTime(file.path(vd, fn("2026-03", "baseline")), Sys.time() + 3600)
Sys.setFileTime(file.path(vd, fn("2026-06", "baseline")), Sys.time() - 86400)
stopifnot(identical(basename(nwmls_resolve_file("baseline", vd)[["path"]]),
                    fn("2026-06", "baseline")))
vt <- nwmls_check_vintages(vd)
stopifnot(identical(vt[["vintage"]], rep("2026-06", 3)))
# a newer vintage for one scenario only -> scenarios disagree -> stop
writeLines("x", file.path(vd, fn("2026-09", "pessimistic")))
expect_error(nwmls_check_vintages(vd), "resolve to different vintages")
expect_error(nwmls_read_raw("baseline", vd), "resolve to different vintages")
# Excel lock file / undated copy is ignored next to dated files
mkfiles(c(fn("2026-06", c("baseline", "optimistic", "pessimistic")),
          "~$2026-06_nwmls_housing_forecast_baseline.xlsx",
          "nwmls_housing_forecast_baseline.xlsx"))
stopifnot(identical(basename(suppressMessages(nwmls_resolve_file("baseline", vd))[["path"]]),
                    fn("2026-06", "baseline")))
# a single undated file is accepted; two undated files cannot be ordered
mkfiles("nwmls_housing_forecast_baseline.xlsx")
stopifnot(identical(basename(nwmls_resolve_file("baseline", vd)[["path"]]),
                    "nwmls_housing_forecast_baseline.xlsx"))
mkfiles(c("nwmls_housing_forecast_baseline.xlsx", "old_nwmls_housing_forecast_baseline.xlsx"))
expect_error(nwmls_resolve_file("baseline", vd), "none has a YYYY-MM_ vintage prefix")
stopifnot(is.null(nwmls_resolve_file("optimistic", vd)))
cat("PASS  NWMLS file = newest YYYY-MM prefix regardless of mtime; mixed vintages stop; undated handled\n")

if (dir.exists(here::here("data", "nwmls"))) {
  vt <- nwmls_check_vintages(here::here("data", "nwmls"))
  if (!is.null(vt)) {
    print(vt, row.names = FALSE)
    stopifnot(length(unique(vt[["vintage"]])) == 1L)
    cat("PASS  data/nwmls: all scenarios resolve to vintage ", vt[["vintage"]][1], "\n", sep = "")
  }
}

cat("\nAll nwmls_growth tests passed.\n")
