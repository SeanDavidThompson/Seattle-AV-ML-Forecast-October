###############################################################################
# overnight_monday.R -- unattended A/B + production runs for the October 2026
# forecast.  Runs every step in its own fresh R process, one at a time, and
# writes one log per step to data/outputs_nwmls_growth/logs/.  A failed step
# skips only the steps that depend on it.
#
# Start (repo root as working directory, e.g. the .Rproj open in RStudio):
#   source("ad_hoc/overnight_monday.R")
# or from a shell (no working directory needed):
#   Rscript "<share>/ad_hoc/overnight_monday.R"
#
# Steps (dependencies in brackets):
#   s0  NWMLS forecast caches in data/cache rebuilt from the newest YYYY-MM
#       vintage (old files backed up to the log dir)                     []
#   a   level  baseline residential: extend + Step 6, so TY2027 picks up the
#       area-report anchoring in data/cache (fails if 2027 land_actual = 0)  [s0]
#   b   res_driver_audit.R on the level models                           [a]
#   c   stage_nwmls_growth.R (an existing non-empty staging dir is renamed,
#       never deleted)                                                   [a]
#   d   growth baseline residential retrain, separate dirs               [c]
#   e1  sensitivity, level models                                        [a]
#   e2  sensitivity, growth models                                       [d]
#   f1  growth optimistic residential (growth models)                    [d]
#   f2  growth pessimistic residential                                   [d]
#   g1  level optimistic residential (level models)                      [s0]
#   g2  level pessimistic residential                                    [s0]
#   h1-h6 commercial and condo, all three scenarios, existing models,
#       cached extended panels, current area-report import               []
#   i   data/outputs_nwmls_growth/AB_SUMMARY.txt                         [always]
#
# Nothing here trains commercial or condo models or touches data/nwmls,
# data/oerf or data/kca.
###############################################################################

# ---- where am I --------------------------------------------------------------
.ov_root <- local({
  # --file= is this script only when it was started with Rscript directly;
  # when something else sources it, use OVERNIGHT_ROOT or the working dir.
  fa <- grep("^--file=.*overnight_monday\\.R$", commandArgs(FALSE), value = TRUE)
  r <- if (length(fa)) dirname(dirname(normalizePath(sub("^--file=", "", fa[1]),
                                                     winslash = "/", mustWork = FALSE)))
       else get0("OVERNIGHT_ROOT", ifnotfound = getwd())
  normalizePath(r, winslash = "/", mustWork = FALSE)
})
if (!file.exists(file.path(.ov_root, "scripts", "ml", "main_ml.R")))
  stop("overnight_monday.R: cannot find scripts/ml/main_ml.R under ", .ov_root,
       ".  Open the project (or setwd) at the repo root first.")

OV_ROOT    <- .ov_root
OV_LOG_DIR <- file.path(OV_ROOT, "data", "outputs_nwmls_growth", "logs")
OV_RSCRIPT <- file.path(R.home("bin"), "Rscript")
dir.create(file.path(OV_LOG_DIR, "steps"), recursive = TRUE, showWarnings = FALSE)

# ---- step code -----------------------------------------------------------------
# Each step's body runs in a fresh process after setwd(OV_ROOT), with
# LOG_DIR defined.  Bodies are plain R text so they can be read in the
# generated logs/steps/<id>.R files.
PRE <- '
MAIN_ML_DEFINE_ONLY <- TRUE
source(file.path("scripts", "ml", "main_ml.R"))
G <- list(cache_dir  = here::here("data", "cache_nwmls_growth"),
          model_dir  = here::here("data", "model_nwmls_growth"),
          output_dir = here::here("data", "outputs_nwmls_growth"))
res_args <- function(scenario, mode, train = FALSE)
  list(scenario = scenario, prop_scope = "res", nwmls_features = mode,
       model_replicate = train, forecast_only = FALSE, extend_replicate = TRUE,
       panel_replicate = FALSE, retrofit_replicate = FALSE,
       diagnostics_replicate = FALSE, use_area_actuals = TRUE)
anchor_counts <- function(cache_dir, scenario, yr = 2027L) {
  f <- file.path(cache_dir, paste0("panel_tbl_2006_2031_forecasted_", scenario, "_res.rds"))
  x <- data.table::as.data.table(readRDS(f))[tax_yr == yr]
  data.frame(scenario = scenario, tax_yr = yr, n = nrow(x),
             land_actual = sum(x[["land_method"]] == "actual", na.rm = TRUE),
             land_delta  = sum(x[["land_method"]] == "delta",  na.rm = TRUE),
             impr_actual = sum(x[["impr_method"]] == "actual", na.rm = TRUE),
             impr_delta  = sum(x[["impr_method"]] == "delta",  na.rm = TRUE),
             total_na    = sum(is.na(x[["total_assessed_filled"]])))
}
'

STEPS <- list(
  list(id = "s0", title = "NWMLS forecast caches from the newest vintage", deps = character(0), code = '
source(file.path("scripts", "ml", "nwmls_features.R"))
vt <- nwmls_check_vintages(here::here("data", "nwmls"))
print(vt)
cmp <- list()
for (sc in NWMLS_SCENARIOS) {
  p <- nwmls_fcst_cache_path(CFG[["cache_dir"]], sc)
  old <- NULL
  if (file.exists(p)) {
    file.copy(p, file.path(LOG_DIR, paste0("prev_", basename(p))), overwrite = TRUE,
              copy.date = TRUE)
    old <- readRDS(p)
  }
  nwmls_write_fcst_cache(sc, cache_dir = CFG[["cache_dir"]],
                         nwmls_dir = here::here("data", "nwmls"),
                         forecast_start = CFG[["forecast_start"]],
                         forecast_end = CFG[["forecast_end"]])
  new <- readRDS(p)
  ty <- new[["tax_yr"]]
  cmp[[sc]] <- data.frame(scenario = sc, tax_yr = ty,
    prev_price_lag12 = if (is.null(old)) NA_real_ else
      old[["sea_pmedesfh_lag12"]][match(ty, old[["tax_yr"]])],
    new_price_lag12 = new[["sea_pmedesfh_lag12"]])
}
cmp <- do.call(rbind, cmp)
cmp[["changed"]] <- abs(cmp[["new_price_lag12"]] / cmp[["prev_price_lag12"]] - 1) > 1e-9
print(cmp)
saveRDS(list(vintages = vt, compare = cmp), file.path(LOG_DIR, "s0_result.rds"))
'),
  list(id = "a", title = "level baseline residential (TY2027 anchoring)", deps = "s0", code = '
af <- file.path(CFG[["cache_dir"]], paste0("area_report_actuals_", CFG[["forecast_start"]] - 1L, ".rds"))
message("area-report cache: ", af, if (file.exists(af)) paste0(" (modified ", format(file.mtime(af)), ")") else " (MISSING - Step 0 will re-import)")
do.call(run_main_ml, res_args("baseline", "level"))
ac <- anchor_counts(CFG[["cache_dir"]], "baseline")
print(ac)
saveRDS(ac, file.path(LOG_DIR, "a_result.rds"))
if (ac[["land_actual"]] == 0)
  stop("TY2027 land_actual count is 0 - the area-report anchoring did not apply")
'),
  list(id = "b", title = "res_driver_audit on the level models", deps = "a", code = '
RES_AUDIT_DEFINE_ONLY <- TRUE
source(file.path("ad_hoc", "res_driver_audit.R"))
res_driver_audit(model_dir = CFG[["model_dir"]], cache_dir = CFG[["cache_dir"]], tag = "level")
'),
  list(id = "c", title = "stage growth inputs", deps = "a", code = '
dst <- here::here("data", "cache_nwmls_growth")
if (dir.exists(dst) && length(list.files(dst))) {
  aside <- paste0(dst, "_old_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  message("existing staging dir is not empty - renaming it to ", aside)
  if (!file.rename(dst, aside)) stop("could not rename ", dst)
}
source(file.path("ad_hoc", "stage_nwmls_growth.R"))
'),
  list(id = "d", title = "growth baseline residential retrain", deps = "c", code = '
do.call(run_main_ml, c(res_args("baseline", "growth", train = TRUE), G))
ac <- anchor_counts(G[["cache_dir"]], "baseline"); print(ac)
'),
  list(id = "e1", title = "sensitivity, level models", deps = "a", code = '
NWMLS_SENS_DEFINE_ONLY <- TRUE
source(file.path("ad_hoc", "nwmls_sensitivity.R"))
sd <- here::here("data", "scratch_nwmls_sens_level")
r <- run_nwmls_sensitivity(model_dir = CFG[["model_dir"]], cache_dir = CFG[["cache_dir"]],
                           scratch_dir = sd)
saveRDS(r, file.path(LOG_DIR, "e1_result.rds"))
unlink(sd, recursive = TRUE)
'),
  list(id = "e2", title = "sensitivity, growth models", deps = "d", code = '
NWMLS_SENS_DEFINE_ONLY <- TRUE
source(file.path("ad_hoc", "nwmls_sensitivity.R"))
sd <- here::here("data", "scratch_nwmls_sens_growth")
r <- run_nwmls_sensitivity(model_dir = G[["model_dir"]], cache_dir = G[["cache_dir"]],
                           scratch_dir = sd)
saveRDS(r, file.path(LOG_DIR, "e2_result.rds"))
unlink(sd, recursive = TRUE)
'),
  list(id = "f1", title = "growth optimistic residential", deps = "d", code = '
do.call(run_main_ml, c(res_args("optimistic", "growth"), G))
'),
  list(id = "f2", title = "growth pessimistic residential", deps = "d", code = '
do.call(run_main_ml, c(res_args("pessimistic", "growth"), G))
'),
  list(id = "g1", title = "level optimistic residential", deps = "s0", code = '
do.call(run_main_ml, res_args("optimistic", "level"))
'),
  list(id = "g2", title = "level pessimistic residential", deps = "s0", code = '
do.call(run_main_ml, res_args("pessimistic", "level"))
')
)

# Commercial and condo: existing models (model_replicate = FALSE), cached
# extended panels (forecast_only = TRUE skips Steps 1-5b; Step 3 loads the
# cached subgroup/condo models), Step 0 loads the current area-report import
# from data/cache.  prop_scope has no "com+condo", so one run each.
for (sc in c("baseline", "optimistic", "pessimistic")) for (ps in c("com", "condo")) {
  STEPS[[length(STEPS) + 1L]] <- list(
    id = paste0("h_", ps, "_", sc), title = paste(ps, sc, "(existing models)"),
    deps = character(0), code = sprintf('
run_main_ml(scenario = "%s", prop_scope = "%s", model_replicate = FALSE,
            forecast_only = TRUE, extend_replicate = FALSE,
            panel_replicate = FALSE, retrofit_replicate = FALSE,
            diagnostics_replicate = FALSE, use_area_actuals = TRUE)
', sc, ps))
}

STEPS[[length(STEPS) + 1L]] <- list(
  id = "i", title = "AB_SUMMARY.txt", deps = character(0), always = TRUE,
  code = 'source(file.path("ad_hoc", "overnight_summary.R")); write_ab_summary(LOG_DIR)')

# Test hook: a list of steps in the same shape replaces the real ones
# (scripts/ml/test_overnight_runner.R).  Never set in production.
if (exists("OVERNIGHT_STEPS_OVERRIDE")) STEPS <- OVERNIGHT_STEPS_OVERRIDE

# ---- runner ------------------------------------------------------------------------
ov_write_step <- function(s) {
  f   <- file.path(OV_LOG_DIR, "steps", paste0(s[["id"]], ".R"))
  log <- file.path(OV_LOG_DIR, paste0(s[["id"]], ".log"))
  body <- paste0(
    '.log <- file("', log, '", open = "wt")\n',
    'sink(.log); sink(.log, type = "message")\n',
    'cat("== step ', s[["id"]], ': ', s[["title"]], '\\n== started ", format(Sys.time()), "\\n")\n',
    '.ok <- tryCatch({\n',
    '  setwd("', OV_ROOT, '")\n',
    '  options(warn = 1)\n',
    '  LOG_DIR <- "', OV_LOG_DIR, '"\n',
    if (!isTRUE(s[["always"]])) PRE else "",
    s[["code"]], '\n',
    '  TRUE\n',
    '}, error = function(e) {\n',
    '  message("STEP FAILED: ", conditionMessage(e))\n',
    '  if (!is.null(conditionCall(e))) message("  in: ", deparse(conditionCall(e))[1])\n',
    '  FALSE\n',
    '})\n',
    'cat("== finished ", format(Sys.time()), " status=", if (isTRUE(.ok)) "ok" else "FAILED", "\\n")\n',
    'sink(type = "message"); sink(); close(.log)\n',
    'quit(save = "no", status = if (isTRUE(.ok)) 0L else 1L)\n')
  writeLines(body, f)
  list(script = f, log = log)
}

# Dry run: OVERNIGHT_DRY_RUN <- TRUE before sourcing writes and parse-checks
# every step script, prints the plan, and runs nothing.
if (isTRUE(get0("OVERNIGHT_DRY_RUN", ifnotfound = FALSE))) {
  for (s in STEPS) {
    w <- ov_write_step(s)
    invisible(parse(w[["script"]]))
    cat(sprintf("  %-14s deps: %-8s %s\n", s[["id"]],
                if (length(s[["deps"]])) paste(s[["deps"]], collapse = ",") else "-",
                s[["title"]]))
  }
  cat("Dry run: ", length(STEPS), " step scripts written to ",
      file.path(OV_LOG_DIR, "steps"), " and parsed OK. Nothing was run.\n", sep = "")
} else {

status_file <- file.path(OV_LOG_DIR, "status.csv")
status <- data.frame(id = vapply(STEPS, `[[`, "", "id"),
                     title = vapply(STEPS, `[[`, "", "title"),
                     deps = vapply(STEPS, function(s) paste(s[["deps"]], collapse = " "), ""),
                     status = "pending", started = NA_character_,
                     elapsed_min = NA_real_, stringsAsFactors = FALSE)
write.csv(status, status_file, row.names = FALSE)

cat("overnight_monday.R: ", nrow(status), " steps, logs in ", OV_LOG_DIR, "\n", sep = "")
t_all <- Sys.time()
for (k in seq_along(STEPS)) {
  s <- STEPS[[k]]
  bad <- s[["deps"]][status[["status"]][match(s[["deps"]], status[["id"]])] != "ok"]
  if (length(bad)) {
    status[k, "status"] <- paste0("skipped (needs ", paste(bad, collapse = ", "), ")")
    cat(format(Sys.time(), "%H:%M"), " ", s[["id"]], " SKIPPED - failed/skipped dependency: ",
        paste(bad, collapse = ", "), "\n", sep = "")
    write.csv(status, status_file, row.names = FALSE)
    next
  }
  w <- ov_write_step(s)
  t0 <- Sys.time()
  status[k, "started"] <- format(t0, "%Y-%m-%d %H:%M:%S")
  cat(format(t0, "%H:%M"), " ", s[["id"]], " start: ", s[["title"]], "\n", sep = "")
  crash <- file.path(OV_LOG_DIR, paste0(s[["id"]], "_stderr.txt"))
  # The step script sinks its own output into its log; anything that escapes
  # (a crash before the sink opens) lands in <id>_stderr.txt.
  status[k, "status"] <- "running"
  write.csv(status, status_file, row.names = FALSE)
  rc <- tryCatch(system2(OV_RSCRIPT, shQuote(w[["script"]]), stdout = FALSE, stderr = crash),
                 error = function(e) -1L)
  if (file.exists(crash) && file.size(crash) == 0) unlink(crash)
  status[k, "elapsed_min"] <- round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1)
  status[k, "status"] <- if (identical(as.integer(rc), 0L)) "ok" else "FAILED"
  write.csv(status, status_file, row.names = FALSE)
  cat(format(Sys.time(), "%H:%M"), " ", s[["id"]], " ", status[k, "status"], " (",
      status[k, "elapsed_min"], " min)\n", sep = "")
}
cat("\nAll steps done in ", round(as.numeric(difftime(Sys.time(), t_all, units = "hours")), 2),
    " h.  Summary: ", file.path(OV_ROOT, "data", "outputs_nwmls_growth", "AB_SUMMARY.txt"),
    "\n", sep = "")
print(status[, c("id", "status", "elapsed_min")])

} # end !OVERNIGHT_DRY_RUN
