# test_overnight_runner.R ---------------------------------------------------------
# ad_hoc/overnight_monday.R mechanics with dummy steps in a temporary copy of
# the repo layout: each step runs in its own Rscript process, logs to its own
# file, a failure skips only its dependants, and the dry run parses every
# real step script without running any.
#
#   Rscript scripts/ml/test_overnight_runner.R
# -----------------------------------------------------------------------------
suppressPackageStartupMessages(library(here))
root <- file.path(tempdir(), "overnight_runner_test")
unlink(root, recursive = TRUE)
dir.create(file.path(root, "scripts", "ml"), recursive = TRUE)
dir.create(file.path(root, "ad_hoc"))
invisible(file.copy(list.files(here::here("scripts", "ml"), full.names = TRUE),
          file.path(root, "scripts", "ml")))
invisible(file.copy(list.files(here::here("ad_hoc"), pattern = "\\.R$", full.names = TRUE),
          file.path(root, "ad_hoc")))
logs <- file.path(root, "data", "outputs_nwmls_growth", "logs")
owd <- setwd(root)

# ---- 1. dry run over the real steps ------------------------------------------
OVERNIGHT_DRY_RUN <- TRUE
out <- capture.output(source(file.path("ad_hoc", "overnight_monday.R")))
stopifnot(any(grepl("parsed OK. Nothing was run", out)),
          !file.exists(file.path(logs, "status.csv")),
          length(list.files(file.path(logs, "steps"), pattern = "\\.R$")) == 20L)
rm(OVERNIGHT_DRY_RUN)
cat("PASS  dry run: 20 real step scripts written and parsed, nothing executed\n")

# ---- 2. dummy steps: isolation, logs, dependency skipping --------------------
OVERNIGHT_STEPS_OVERRIDE <- list(
  list(id = "x1", title = "ok", deps = character(0), always = TRUE,
       code = 'cat("pid", Sys.getpid(), "\\n"); warning("w1")'),
  list(id = "x2", title = "fails", deps = "x1", always = TRUE, code = 'stop("boom")'),
  list(id = "x3", title = "needs x2", deps = "x2", always = TRUE, code = 'cat(1)'),
  list(id = "x4", title = "independent", deps = character(0), always = TRUE,
       code = 'stopifnot(!exists("OVERNIGHT_STEPS_OVERRIDE")); cat("fresh\\n")'),
  list(id = "x5", title = "needs x3", deps = "x3", always = TRUE, code = 'cat(1)'))
invisible(capture.output(source(file.path("ad_hoc", "overnight_monday.R"))))
st <- read.csv(file.path(logs, "status.csv"), stringsAsFactors = FALSE)
stopifnot(identical(st[["status"]], c("ok", "FAILED", "skipped (needs x2)", "ok",
                                      "skipped (needs x3)")))
l1 <- readLines(file.path(logs, "x1.log")); l2 <- readLines(file.path(logs, "x2.log"))
stopifnot(any(grepl("^Warning.*w1", l1)), any(grepl("STEP FAILED: boom", l2)),
          !any(grepl(paste0("pid ", Sys.getpid()), l1)),
          !file.exists(file.path(logs, "x3.log")))
cat("PASS  runner: own process per step, own log, failure skips only dependants\n")
# ---- 3. OVERNIGHT_ONLY: subset, earlier status kept, old logs moved -------------
writeLines("old x4 log", file.path(logs, "x4.log"))
OVERNIGHT_ONLY <- c("x3", "x4")
OVERNIGHT_STEPS_OVERRIDE[[2]][["code"]] <- 'cat("x2 would pass now\\n")'
invisible(capture.output(source(file.path("ad_hoc", "overnight_monday.R"))))
st <- read.csv(file.path(logs, "status.csv"), stringsAsFactors = FALSE)
stopifnot(identical(st[["status"]], c("earlier run: ok", "earlier run: FAILED", "ok", "ok",
                                      "earlier run: skipped (needs x3)")),
          !file.exists(file.path(logs, "x2.log")) || any(grepl("boom", readLines(file.path(logs, "x2.log")))),
          any(grepl("x4.log$", list.files(file.path(logs, "previous")))),
          file.exists(file.path(logs, "x3.log")))
OVERNIGHT_ONLY <- c("x4", "nope")
err <- tryCatch({ source(file.path("ad_hoc", "overnight_monday.R")); "none" }, error = conditionMessage)
stopifnot(grepl("unknown step id\\(s\\) nope", err))
rm(OVERNIGHT_ONLY, OVERNIGHT_STEPS_OVERRIDE)
cat("PASS  OVERNIGHT_ONLY: only selected steps run, unselected deps count as done,\n",
    "      earlier statuses kept, old logs moved to previous/, unknown ids refused\n", sep = "")

# ---- 4. step d reuse: stamp check on the real generated step ------------------------
md <- file.path(root, "data", "model_nwmls_growth"); dir.create(md, recursive = TRUE)
cd <- file.path(root, "data", "cache_nwmls_growth"); dir.create(cd, recursive = TRUE)
source(here::here("scripts", "ml", "nwmls_features.R"))
put <- function(name, obj, age_h) {
  f <- file.path(md, name); saveRDS(obj, f); Sys.setFileTime(f, Sys.time() - age_h * 3600)
}
g <- nwmls_stamp_mode(list(x_cols = "a"), "growth")
put("lgb_land_delta_cv_20261010_024648.rds", g, 5); put("dv_land_delta_20261010_024648.rds", 1, 5)
put("lgb_impr_delta_cv_20261010_025916.rds", g, 4); put("dv_impr_delta_20261010_025916.rds", 1, 4)
put("lgb_land_delta_cv_20261011_010101.rds", g, 1)                 # newer, wrong one
saveRDS(1, file.path(cd, "panel_tbl_2027_2031_inputs_baseline_res.rds"))
OVERNIGHT_ONLY <- "d"
OVERNIGHT_D_REUSE_MODELS <- c(land = "20261010_024648", impr = "20261010_025916")
invisible(capture.output(source(file.path("ad_hoc", "overnight_monday.R"))))
ld <- readLines(file.path(logs, "d.log"))
stopifnot(any(grepl("expected lgb_land_delta_cv_20261010_024648.rds", ld)),
          !any(grepl("reusing the cached growth extended panel", ld)))
invisible(file.rename(file.path(md, "lgb_land_delta_cv_20261011_010101.rds"),
            file.path(tempdir(), "moved_aside.rds")))
invisible(capture.output(source(file.path("ad_hoc", "overnight_monday.R"))))
ld <- readLines(file.path(logs, "d.log"))
# passes the stamp + mode checks and goes on to the forecast-only run (which
# fails here only because this scratch tree has no pipeline data)
stopifnot(any(grepl("reuse: newest lgb_impr_delta_cv = lgb_impr_delta_cv_20261010_025916.rds", ld)),
          any(grepl("reusing the cached growth extended panel", ld)))
rm(OVERNIGHT_ONLY, OVERNIGHT_D_REUSE_MODELS)
cat("PASS  step d reuse: stops when the newest model is not the named stamp; otherwise\n",
    "      verifies stamps + growth mode and runs forecast-only on the cached panel\n", sep = "")

# ---- 5. AB summary under the amended rule (2026-10-11) -----------------------------
suppressPackageStartupMessages(library(data.table))
sroot <- file.path(tempdir(), "ab_summary_test"); unlink(sroot, recursive = TRUE)
dir.create(sroot); setwd(sroot)
ids <- sprintf("%03d", 1:30)
for (m in c("cache", "cache_nwmls_growth")) {
  dir.create(file.path("data", m), recursive = TRUE)
  for (sc in c("baseline", "optimistic", "pessimistic")) {
    d <- CJ(parcel_id = ids, tax_yr = 2025:2031)
    f <- c(baseline = 1.03, optimistic = 1.04, pessimistic = 1.02)[[sc]]
    d[, appr_land_val := fifelse(tax_yr <= 2026, 3e5, NA_real_)]
    d[, appr_imps_val := fifelse(tax_yr <= 2026, 4e5, NA_real_)]
    d[, appr_land_val_filled := 3e5 * f^(tax_yr - 2026)]
    d[, appr_imps_val_filled := 4e5 * f^(tax_yr - 2026)]
    d[, total_assessed_filled := appr_land_val_filled + appr_imps_val_filled]
    d[, land_method := fifelse(tax_yr == 2027 & parcel_id <= "010", "actual",
                               fifelse(tax_yr > 2026, "delta", "observed"))]
    d[, impr_method := land_method]
    saveRDS(d, file.path("data", m, paste0("panel_tbl_2006_2031_forecasted_", sc, "_res.rds")))
  }
}
dir.create("data/model"); dir.create("data/model_nwmls_growth")
saveRDS(list(cv_rmse = 0.100), "data/model/lgb_land_delta_cv_1.rds")
saveRDS(list(cv_rmse = 0.200), "data/model/lgb_impr_delta_cv_1.rds")
saveRDS(structure(list(cv_rmse = 0.0987), nwmls_features = "growth"), "data/model_nwmls_growth/lgb_land_delta_cv_2.rds")
saveRDS(structure(list(cv_rmse = 0.2011), nwmls_features = "growth"), "data/model_nwmls_growth/lgb_impr_delta_cv_2.rds")
L <- "data/outputs_nwmls_growth/logs"; dir.create(L, recursive = TRUE)
ids_s <- c("s0", "a", "d", "e4", "e3", "f1", "f2", "g1", "g2")
write.csv(data.frame(id = ids_s, title = "t", deps = "",
                     status = c("earlier run: ok", "earlier run: ok", "earlier run: ok", "ok", "ok",
                                "earlier run: ok", "earlier run: ok", "earlier run: ok", "earlier run: ok"),
                     started = "x", elapsed_min = 30), file.path(L, "status.csv"), row.names = FALSE)
coerce <- "Warning in FUN(X[[i]], ...) : NAs introduced by coercion"
writeLines(rep(coerce, 3), file.path(L, "s0.log"))      # only in s0 among level logs
writeLines("ok", file.path(L, "a.log"))
writeLines(c(coerce, coerce), file.path(L, "f1.log"))   # growth run rebuilt its NWMLS cache
mk <- function(p, feats = TRUE) list(pass = p,
  table = data.table(tax_yr = 2026:2031, total_av_minus5 = 1:6, total_av_base = 2:7,
                     total_av_plus5 = 3:8, pct_minus5_vs_base = -0.01, pct_plus5_vs_base = 0.01),
  features = if (feats) data.table(feature = "sea_pmedesfh_lag12_yoy", tax_yr = 2028L,
                                   minus5 = -0.06, base = -0.01, plus5 = 0.03, moved = TRUE,
                                   used_by_models = TRUE, splits_crossed_minus = 4L,
                                   splits_crossed_plus = 3L) else NULL)
saveRDS(mk(FALSE, FALSE), file.path(L, "e1_result.rds"))   # old level-shock runs
saveRDS(mk(FALSE, FALSE), file.path(L, "e2_result.rds"))
saveRDS(mk(TRUE), file.path(L, "e3_result.rds"))
saveRDS(mk(TRUE), file.path(L, "e4_result.rds"))
source(file.path(root, "ad_hoc", "overnight_summary.R"))
txt <- capture.output(write_ab_summary(L))
stopifnot(any(grepl("1. Sensitivity, growth shock \\(growth models\\)  : PASS", txt)),
          any(grepl("original level shock: growth FAIL, level FAIL", txt)),
          any(grepl("4. Clean run \\(vs all level logs incl. s0\\)      : PASS", txt)),
          any(grepl("new warning lines: 0", txt)),
          any(grepl("growth steps d/f1/f2 all ok: PASS", txt)),
          any(grepl("splits_crossed_minus", txt)),
          any(grepl("feature check: n/a \\(run before the feature check existed\\)", txt)),
          tail(grep("^RULE", txt, value = TRUE), 1) == "RULE (amended 2026-10-11): MET - ship growth")
saveRDS(mk(FALSE), file.path(L, "e4_result.rds"))
txt <- capture.output(write_ab_summary(L))
stopifnot(tail(grep("^RULE", txt, value = TRUE), 1) == "RULE (amended 2026-10-11): NOT MET - ship level")
unlink(file.path(L, "e4_result.rds"))
txt <- capture.output(write_ab_summary(L))
stopifnot(tail(grep("^RULE", txt, value = TRUE), 1) == "RULE (amended 2026-10-11): NOT MET - ship level",
          any(grepl("evidence missing for criterion 1", txt)))
cat("PASS  AB summary: criterion 1 judged on e4 (growth shock) with the original shocks shown;\n",
    "      Excel-read warnings in s0 no longer count as new; earlier-run statuses accepted;\n",
    "      final line MET / NOT MET (missing e4 = not met)\n", sep = "")

setwd(owd)
cat("\nAll overnight runner checks passed.\n")
