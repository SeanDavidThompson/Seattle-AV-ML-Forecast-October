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

# ---- 7. growth shock: forecast months only, factor (1+g)^(k/12), reaches every year ----
mons <- seq(as.Date("2005-01-01"), as.Date("2031-12-01"), by = "month")
lo2  <- as.Date("2026-06-01")
j <- seq_along(mons)
raw2 <- tibble::tibble(
  date             = mons,
  sea_pmedesfh_fav = 400000 * 1.004^(j - 1) + 8000 * sin(j / 2),
  sea_sesfh_fav    = 500 + 100 * cos(j / 2),
  sea_alesfh_fav   = 1200 + 300 * sin(j / 3),
  sea_pmedesfh     = ifelse(mons <= lo2, 400000 * 1.004^(j - 1) + 8000 * sin(j / 2), NA_real_))
gs <- nwmls_shock_price_growth(raw2, -0.05, lo2)
k  <- (as.integer(format(mons, "%Y")) - 2026L) * 12L + as.integer(format(mons, "%m")) - 6L
fac <- gs[["sea_pmedesfh_fav"]] / raw2[["sea_pmedesfh_fav"]]
stopifnot(all(abs(fac[mons <= lo2] - 1) < 1e-15),
          all(abs(fac[mons > lo2] - 0.95^(k[mons > lo2] / 12)) < 1e-12),
          abs(fac[mons == as.Date("2027-06-01")] - 0.95) < 1e-12,
          identical(gs[["sea_sesfh_fav"]], raw2[["sea_sesfh_fav"]]),
          identical(gs[["sea_pmedesfh"]][mons <= lo2], raw2[["sea_pmedesfh"]][mons <= lo2]))
yrs_moved <- function(shocked, col) {
  b <- suppressMessages(nwmls_build_annual(raw2))[["annual"]]
  a <- suppressMessages(nwmls_build_annual(shocked))[["annual"]]
  yy <- 2027:2031
  yy[abs(a[[col]][match(yy, a[["tax_yr"]])] - b[[col]][match(yy, b[["tax_yr"]])]) > 1e-12]
}
lv <- nwmls_shock_price(raw2, 0.95, lo2)
stopifnot(identical(yrs_moved(gs, "sea_pmedesfh_lag12_yoy"), 2027:2031),
          identical(yrs_moved(gs, "sea_pmedesfh_lag6_yoy"),  2027:2031),
          identical(yrs_moved(lv, "sea_pmedesfh_lag12_yoy"), 2027:2028),
          identical(yrs_moved(lv, "sea_pmedesfh_lag6_yoy"),  2027L),
          identical(yrs_moved(lv, "sea_pmedesfh_lag12"),     2027:2031))
cat("PASS  growth shock: forecast months only, factor = (1+g)^(k/12); moves both _yoy features in\n",
    "      every year 2027-2031, while the level shock moves lag12_yoy only in 2027-2028 and\n",
    "      lag6_yoy only in 2027 (why the original test could not see the growth models)\n", sep = "")

# ---- 8. sensitivity: "level" stays the default and the old code path ---------------
NWMLS_SENS_DEFINE_ONLY <- TRUE
source(here::here("ad_hoc", "nwmls_sensitivity.R"))
fm <- formals(run_nwmls_sensitivity)
stopifnot(identical(eval(fm[["shock_type"]])[1], "level"), is.null(fm[["shocks"]]))
body_txt <- paste(readLines(here::here("ad_hoc", "nwmls_sensitivity.R")), collapse = "\n")
stopifnot(grepl("c(minus5 = 0.95, base = 1.00, plus5 = 1.05)", body_txt, fixed = TRUE),
          grepl("nwmls_shock_price(raw[[\"data\"]], fac, last_obs)", body_txt, fixed = TRUE),
          grepl("nwmls_sensitivity_\", mode, \"_\", shock_type, \".csv", body_txt, fixed = TRUE))
cat("PASS  sensitivity: shock_type defaults to \"level\" with the original 0.95/1/1.05 shock through\n",
    "      nwmls_shock_price(); CSV goes to nwmls_sensitivity_<mode>_<shock_type>.csv\n", sep = "")

# ---- 9. promotion: backup, swap, refusals, restore --------------------------------
PROMOTE_DEFINE_ONLY <- TRUE
source(here::here("ad_hoc", "promote_growth_res.R"))
proot <- file.path(tempdir(), "promote_test")
mk_fc <- function(level_av, mode) {
  d <- data.table(parcel_id = rep(sprintf("%03d", 1:5), each = 6), tax_yr = rep(2026:2031, 5))
  d[, appr_land_val_filled := level_av * 0.4 * 1.03^(tax_yr - 2026)]
  d[, appr_imps_val_filled := level_av * 0.6 * 1.03^(tax_yr - 2026)]
  x <- tibble::as_tibble(d)
  if (!is.null(mode)) attr(x, "nwmls_features") <- mode
  x
}
p_setup <- function(growth_mode = "growth", model_age_h = 10, drop = NULL) {
  unlink(proot, recursive = TRUE)
  for (dd in c("cache", "cache_nwmls_growth", "outputs", "outputs_nwmls_growth", "model_nwmls_growth"))
    dir.create(file.path(proot, "data", dd), recursive = TRUE)
  m <- nwmls_stamp_mode(list(x_cols = "a"), "growth")
  for (pfx in c("lgb_land_delta_cv_1", "lgb_impr_delta_cv_1")) {
    f <- file.path(proot, "data", "model_nwmls_growth", paste0(pfx, ".rds"))
    saveRDS(m, f); Sys.setFileTime(f, Sys.time() - model_age_h * 3600)
  }
  for (sc in c("baseline", "optimistic", "pessimistic")) {
    rds <- paste0("panel_tbl_2006_2031_forecasted_", sc, "_res.rds")
    saveRDS(mk_fc(1e6, "level"), file.path(proot, "data", "cache", rds))
    saveRDS(mk_fc(9e5, growth_mode), file.path(proot, "data", "cache_nwmls_growth", rds))
  }
  pq <- "parcel_year_panel_2006_2031_forecasted_baseline_res.parquet"
  writeLines("level parquet",  file.path(proot, "data", "outputs", pq))
  writeLines("growth parquet", file.path(proot, "data", "outputs_nwmls_growth", pq))
  for (f in drop) unlink(file.path(proot, "data", f))
}
md5 <- function(rel) unname(tools::md5sum(file.path(proot, "data", rel)))
rds_b <- "panel_tbl_2006_2031_forecasted_baseline_res.rds"
pq_b  <- "parcel_year_panel_2006_2031_forecasted_baseline_res.parquet"

p_setup()
lvl0 <- md5(file.path("cache", rds_b)); lvl_pq0 <- md5(file.path("outputs", pq_b))
out <- capture.output(bdir <- suppressMessages(promote_growth_res(proot)))
stopifnot(dir.exists(bdir), file.exists(file.path(bdir, "manifest.csv")),
          nrow(data.table::fread(file.path(bdir, "manifest.csv"))) == 4L,       # 3 rds + 1 parquet
          md5(file.path("cache", rds_b)) == md5(file.path("cache_nwmls_growth", rds_b)),
          md5(file.path("outputs", pq_b)) == md5(file.path("outputs_nwmls_growth", pq_b)),
          unname(tools::md5sum(file.path(bdir, "cache", rds_b))) == lvl0,
          file.exists(file.path(proot, "data", "cache", "_PROMOTED_GROWTH_RES.txt")),
          any(grepl("before_level_B", out)), any(grepl("-10.000%", out, fixed = TRUE)))
out <- capture.output(suppressMessages(restore_level_res(bdir, proot)))
stopifnot(md5(file.path("cache", rds_b)) == lvl0, md5(file.path("outputs", pq_b)) == lvl_pq0,
          !file.exists(file.path(proot, "data", "cache", "_PROMOTED_GROWTH_RES.txt")))
cat("PASS  promotion: level files backed up (manifest + md5), growth files swapped in, totals\n",
    "      printed before/after; restore_level_res() round-trips to the original bytes\n", sep = "")

nb <- function() length(list.files(file.path(proot, "data", "cache"), pattern = "^_level_res_backup_"))
p_setup(drop = file.path("cache_nwmls_growth", "panel_tbl_2006_2031_forecasted_optimistic_res.rds"))
expect_error(suppressMessages(promote_growth_res(proot)), "growth file\\(s\\) missing")
stopifnot(md5(file.path("cache", rds_b)) == lvl0, nb() == 0L)
p_setup(drop = file.path("outputs_nwmls_growth", pq_b))
expect_error(suppressMessages(promote_growth_res(proot)), "growth file\\(s\\) missing")
p_setup(growth_mode = "level")
expect_error(suppressMessages(promote_growth_res(proot)),
             "marked nwmls_features = \"level\", not \"growth\"")
stopifnot(md5(file.path("cache", rds_b)) == lvl0, nb() == 0L)
p_setup(growth_mode = NULL, model_age_h = 10)       # unstamped, newer than growth models: ok
invisible(capture.output(bd2 <- suppressMessages(promote_growth_res(proot))))
stopifnot(md5(file.path("cache", rds_b)) == md5(file.path("cache_nwmls_growth", rds_b)))
p_setup(growth_mode = NULL, model_age_h = -10)      # unstamped, older than the models: refuse
expect_error(suppressMessages(promote_growth_res(proot)), "not newer than the growth models")
cat("PASS  promotion refuses: missing growth rds, level parquet without a growth twin, a\n",
    "      level-marked growth file, an unstamped file older than the growth models (nothing\n",
    "      changed, no backup made); an unstamped file with growth provenance is accepted\n", sep = "")

cat("\nAll nwmls_growth tests passed.\n")
