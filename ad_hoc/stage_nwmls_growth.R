# =============================================================================
# stage_nwmls_growth.R — stage the inputs for the nwmls_features = "growth"
# A/B run in its own directories, so the "level" caches and models in
# data/cache, data/model and data/outputs are never touched.
# -----------------------------------------------------------------------------
# run_main_ml() takes cache_dir / model_dir / output_dir, so the growth run
# reads and writes only:
#   data/cache_nwmls_growth/    (the six inputs below + everything it builds)
#   data/model_nwmls_growth/    (growth-trained residential models)
#   data/outputs_nwmls_growth/  (parquet / csv outputs)
#
# Inputs copied (read-only on data/cache).  Everything else the residential
# run needs it rebuilds itself:
#   panel_tbl_res.rds                    Step 2 panel; Step 3 trains on it.
#                                        The growth columns are joined in
#                                        memory (the copy is not rewritten).
#   panel_tbl_retro_res.rds              Step 4 is NOT re-run: history and
#                                        the TY2027 anchor match "level".
#   econ_fcst_2026_2031_<scenario>.rds   Step 5b extend, all three scenarios.
#   area_report_actuals_2026.rds         Step 0 anchors (else re-scraped).
# NOT copied: nwmls_fcst_2026_2031_<scenario>.rds.  The growth run rebuilds
# it from data/nwmls/ with the growth columns.
#
# Same copy rules as snapshot_cache.R: explicit file-by-file copy (never
# file.copy(dir, dir, recursive = TRUE)), refuse a non-empty destination,
# verify by size.
# =============================================================================

suppressPackageStartupMessages(library(here))

SRC  <- here::here("data", "cache")
DST  <- here::here("data", "cache_nwmls_growth")
FCST_START <- 2027L

FILES <- c(
  "panel_tbl_res.rds",
  "panel_tbl_retro_res.rds",
  paste0("econ_fcst_2026_2031_", c("baseline", "optimistic", "pessimistic"), ".rds"),
  paste0("area_report_actuals_", FCST_START - 1L, ".rds")
)

stopifnot(dir.exists(SRC))
if (dir.exists(DST) && length(list.files(DST)))
  stop("Staging dir already exists and is not empty: ", DST,
       "\n  Refusing to overwrite it. Move or rename it if you want a fresh copy.")
if (file.exists(DST) && !dir.exists(DST))
  stop(DST, " exists as a FILE, not a directory. Delete it first.")

src_files <- file.path(SRC, FILES)
miss <- FILES[!file.exists(src_files)]
optional <- paste0("area_report_actuals_", FCST_START - 1L, ".rds")
if (length(setdiff(miss, optional)))
  stop("Missing in ", SRC, ": ", paste(setdiff(miss, optional), collapse = ", "))
if (optional %in% miss)
  message("Note: ", optional, " not in ", SRC,
          " - the growth run will re-import the area reports in Step 0.")
FILES     <- setdiff(FILES, miss)
src_files <- file.path(SRC, FILES)

dir.create(DST, showWarnings = FALSE, recursive = TRUE)
for (d in c("model_nwmls_growth", "outputs_nwmls_growth"))
  dir.create(here::here("data", d), showWarnings = FALSE, recursive = TRUE)

ok <- file.copy(src_files, file.path(DST, FILES),
                overwrite = FALSE, copy.date = TRUE)

src_sz <- file.size(src_files)
dst_sz <- file.size(file.path(DST, FILES))
good   <- ok & !is.na(dst_sz) & dst_sz == src_sz
for (i in seq_along(FILES))
  cat(sprintf("  %-45s %9.1f MB  %s\n", FILES[i], src_sz[i] / 1024^2,
              if (good[i]) "OK" else "FAILED"))
if (!all(good))
  stop("Staging incomplete - do NOT run the growth A/B yet. Failed: ",
       paste(FILES[!good], collapse = ", "))
cat(sprintf("\n%d file(s) staged to %s (%.1f MB). Safe to run the growth A/B.\n",
            sum(good), DST, sum(dst_sz) / 1024^2))
