###############################################################################
# promote_growth_res.R -- switch the residential forecast from the "level"
# run to the "growth" A/B run with one command, and back.
#
#   source("ad_hoc/promote_growth_res.R")          # promotes (all 3 scenarios)
#
#   PROMOTE_DEFINE_ONLY <- TRUE
#   source("ad_hoc/promote_growth_res.R")
#   restore_level_res("data/cache/_level_res_backup_<timestamp>")
#
# !! WARNING !!  Any later LEVEL-mode residential run (run_main_ml() with
# prop_scope "res", "both" or "all" and nwmls_features = "level", which is
# the default) rewrites data/cache/panel_tbl_2006_2031_forecasted_<scenario>_res.rds
# and the matching parquet, silently replacing the promoted growth files.
# Re-run this script after such a run, or set nwmls_features = "growth".
#
# What it swaps (traced 2026-10-11):
#   data/cache/panel_tbl_2006_2031_forecasted_<scenario>_res.rds
#       read by scripts/av_reconcile_certified.R (load_matched_gyy, the
#       residential growth rates behind every certified-basis number and
#       chart), av_fcst_summary() in main_ml.R and scripts/ml/qa_trajectory.R
#   data/outputs/parcel_year_panel_2006_2031_forecasted_<scenario>_res.parquet
#       no R code reads it; swapped so files handed on match the numbers
#       (only when the level parquet exists)
# from data/cache_nwmls_growth/ and data/outputs_nwmls_growth/.
# Not swapped: the extended input panels and models (nothing downstream of
# the forecast reads them; a later run would rebuild them anyway).
#
# Safety:
#   - refuses if any growth file is missing (or a level parquet has no growth
#     twin);
#   - refuses unless each growth forecast is marked nwmls_features = "growth":
#     the attribute 06_forecast_av_2026_2031_sequential.R stamps (forecasts
#     from before 2026-10-11 carry none), or, for an unstamped file, its
#     provenance: it sits in data/cache_nwmls_growth, the newest residential
#     delta models in data/model_nwmls_growth are stamped "growth", and the
#     file is newer than those models.  A file stamped "level" always stops.
#   - copies the level files to data/cache/_level_res_backup_<timestamp>/
#     (with a manifest and md5 sums) before overwriting anything, and checks
#     every copy by md5;
#   - prints residential total AV by scenario and tax year before and after.
#
# STYLE: no $ operator. Use x[["name"]].
###############################################################################

suppressPackageStartupMessages({ library(data.table); library(here) })

PROMOTE_SCENARIOS <- c("baseline", "optimistic", "pessimistic")

promote_paths <- function(root, scenarios = PROMOTE_SCENARIOS) {
  rbindlist(lapply(scenarios, function(sc) {
    rds <- paste0("panel_tbl_2006_2031_forecasted_", sc, "_res.rds")
    pq  <- paste0("parcel_year_panel_2006_2031_forecasted_", sc, "_res.parquet")
    data.table(
      scenario = sc, kind = c("rds", "parquet"),
      level  = c(file.path(root, "data", "cache", rds),
                 file.path(root, "data", "outputs", pq)),
      growth = c(file.path(root, "data", "cache_nwmls_growth", rds),
                 file.path(root, "data", "outputs_nwmls_growth", pq)))
  }))
}

# Residential total AV (av_fcst_summary() definition) by tax year.
promote_res_totals <- function(x) {
  d <- as.data.table(x)
  pick <- function(a, b) {
    va <- if (a %in% names(d)) as.numeric(d[[a]]) else NULL
    vb <- if (b %in% names(d)) as.numeric(d[[b]]) else NULL
    if (is.null(va)) return(vb)
    if (is.null(vb)) return(va)
    fifelse(!is.na(va), va, vb)
  }
  lf <- pick("appr_land_val_filled", "appr_land_val")
  im <- pick("appr_imps_val_filled", "appr_imps_val")
  av <- fifelse(is.na(lf), 0, as.numeric(lf)) + fifelse(is.na(im), 0, as.numeric(im))
  data.table(tax_yr = as.integer(d[["tax_yr"]]), av = av)[
    tax_yr %in% 2026:2031, .(total_av = sum(av)), by = tax_yr][order(tax_yr)]
}

promote_newest <- function(dir, prefix) {
  f <- list.files(dir, pattern = paste0("^", prefix, "_.*[.]rds$"), full.names = TRUE)
  if (!length(f)) return(NULL)
  f[which.max(file.info(f)[["mtime"]])]
}

# "growth" evidence for one growth forecast object; stop() otherwise.
promote_check_mode <- function(obj, path, root) {
  m <- attr(obj, "nwmls_features", exact = TRUE)
  if (!is.null(m)) {
    if (!identical(m, "growth"))
      stop(basename(path), " is marked nwmls_features = \"", m,
           "\", not \"growth\" - refusing to promote it.", call. = FALSE)
    return("stamped nwmls_features = growth")
  }
  md <- file.path(root, "data", "model_nwmls_growth")
  mods <- vapply(c("lgb_land_delta_cv", "lgb_impr_delta_cv"), function(p) {
    f <- promote_newest(md, p); if (is.null(f)) NA_character_ else f }, "")
  if (anyNA(mods))
    stop(basename(path), " carries no nwmls_features stamp and ", md,
         " has no residential delta models to vouch for it - refusing.", call. = FALSE)
  modes <- vapply(mods, function(f) {
    a <- attr(readRDS(f), "nwmls_features", exact = TRUE); if (is.null(a)) "level" else a }, "")
  if (any(modes != "growth"))
    stop(basename(path), " carries no stamp and the newest models in ", md,
         " are not all stamped growth (", paste(basename(mods), modes, collapse = ", "),
         ") - refusing.", call. = FALSE)
  t_file <- file.info(path)[["mtime"]]
  t_mod  <- max(file.info(mods)[["mtime"]])
  if (!(t_file > t_mod))
    stop(basename(path), " carries no stamp and is not newer than the growth models (",
         format(t_file), " vs ", format(t_mod), ") - refusing.", call. = FALSE)
  paste0("unstamped; provenance: in cache_nwmls_growth, newer than growth-stamped ",
         paste(basename(mods), collapse = " + "))
}

promote_print_totals <- function(before, after) {
  b <- rbindlist(before, idcol = "scenario")
  a <- rbindlist(after,  idcol = "scenario")
  m <- merge(b, a, by = c("scenario", "tax_yr"), suffixes = c("_level", "_growth"), all = TRUE)
  m[, pct := sprintf("%+.3f%%", 100 * (total_av_growth / total_av_level - 1))]
  m[, `:=`(level_B = round(total_av_level / 1e9, 3), growth_B = round(total_av_growth / 1e9, 3))]
  setorder(m, scenario, tax_yr)
  print(m[, .(scenario, tax_yr, before_level_B = level_B, after_growth_B = growth_B,
              growth_vs_level = pct)], row.names = FALSE, nrows = 100)
  invisible(m)
}

promote_growth_res <- function(root = here::here(), scenarios = PROMOTE_SCENARIOS) {
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  pt <- promote_paths(root, scenarios)

  # ---- 1. everything present ------------------------------------------------
  pt[, `:=`(has_level = file.exists(level), has_growth = file.exists(growth))]
  miss <- pt[kind == "rds" & !has_growth, growth]
  miss <- c(miss, pt[kind == "parquet" & has_level & !has_growth, growth])
  if (length(miss))
    stop("growth file(s) missing - nothing changed:\n  ",
         paste(miss, collapse = "\n  "), call. = FALSE)
  if (any(pt[kind == "rds", !has_level]))
    message("note: no level file yet for ",
            paste(pt[kind == "rds" & !has_level, scenario], collapse = ", "),
            " - the growth file is copied in without a backup.")
  pt <- pt[kind == "rds" | has_level]          # parquet only where a level one exists

  # ---- 2. growth marker + totals (read each forecast once) -------------------
  before <- list(); after <- list()
  for (sc in scenarios) {
    g <- pt[scenario == sc & kind == "rds", growth]
    obj <- readRDS(g)
    ev <- promote_check_mode(obj, g, root)
    message("  ", sc, ": ", basename(g), " - ", ev)
    after[[sc]] <- promote_res_totals(obj); rm(obj); gc(verbose = FALSE)
    l <- pt[scenario == sc & kind == "rds", level]
    if (file.exists(l)) { obj <- readRDS(l); before[[sc]] <- promote_res_totals(obj); rm(obj); gc(verbose = FALSE) }
  }

  # ---- 3. back up the level files -----------------------------------------------
  stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  bdir <- file.path(root, "data", "cache", paste0("_level_res_backup_", stamp))
  if (dir.exists(bdir)) stop("backup dir already exists: ", bdir, call. = FALSE)
  dir.create(bdir, recursive = TRUE)
  bk <- pt[has_level == TRUE]
  bk[, backup := file.path(bdir, ifelse(kind == "parquet", "outputs", "cache"), basename(level))]
  for (d in unique(dirname(bk[["backup"]]))) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  ok <- file.copy(bk[["level"]], bk[["backup"]], overwrite = FALSE, copy.date = TRUE)
  bk[, md5 := unname(tools::md5sum(level))]
  if (!all(ok) || any(unname(tools::md5sum(bk[["backup"]])) != bk[["md5"]]))
    stop("backup to ", bdir, " failed or does not match - nothing was overwritten.", call. = FALSE)
  manifest <- bk[, .(scenario, kind, original = level, backup, md5)]
  fwrite(manifest, file.path(bdir, "manifest.csv"))
  message("Backup: ", nrow(bk), " level file(s) -> ", bdir)

  # ---- 4. swap in the growth files ------------------------------------------------
  pt[, md5_growth := unname(tools::md5sum(growth))]
  ok <- file.copy(pt[["growth"]], pt[["level"]], overwrite = TRUE, copy.date = TRUE)
  bad <- !ok | unname(tools::md5sum(pt[["level"]])) != pt[["md5_growth"]]
  if (any(bad))
    stop("copy failed for: ", paste(pt[["level"]][bad], collapse = ", "),
         "\n  Restore with restore_level_res(\"", bdir, "\").", call. = FALSE)
  writeLines(c(paste0("residential forecasts promoted from the growth A/B run at ", format(Sys.time())),
               paste0("backup: ", bdir),
               "a later level-mode residential run overwrites them"),
             file.path(root, "data", "cache", "_PROMOTED_GROWTH_RES.txt"))
  message("Promoted ", nrow(pt), " file(s); md5 verified.")

  cat("\n=== Residential total AV ($B): before (level) -> after (growth) ===\n")
  promote_print_totals(before, after)
  cat("\nCompare the after column with the growth arm in AB_SUMMARY.txt.\n",
      "Restore:  restore_level_res(\"", bdir, "\")\n", sep = "")
  invisible(bdir)
}

restore_level_res <- function(backup_dir, root = here::here()) {
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  mf <- file.path(backup_dir, "manifest.csv")
  if (!file.exists(mf)) stop("no manifest.csv in ", backup_dir, call. = FALSE)
  m <- fread(mf, colClasses = "character")
  if (any(!file.exists(m[["backup"]])) ||
      any(unname(tools::md5sum(m[["backup"]])) != m[["md5"]]))
    stop("backup files missing or changed since the backup - nothing restored.", call. = FALSE)
  before <- list(); after <- list()
  for (sc in unique(m[kind == "rds", scenario])) {
    o <- m[scenario == sc & kind == "rds", original]
    if (file.exists(o)) { x <- readRDS(o); before[[sc]] <- promote_res_totals(x); rm(x) }
  }
  ok <- file.copy(m[["backup"]], m[["original"]], overwrite = TRUE, copy.date = TRUE)
  if (!all(ok) || any(unname(tools::md5sum(m[["original"]])) != m[["md5"]]))
    stop("restore incomplete - check ", paste(m[["original"]], collapse = ", "), call. = FALSE)
  for (sc in unique(m[kind == "rds", scenario])) {
    x <- readRDS(m[scenario == sc & kind == "rds", backup]); after[[sc]] <- promote_res_totals(x); rm(x)
  }
  unlink(file.path(root, "data", "cache", "_PROMOTED_GROWTH_RES.txt"))
  message("Restored ", nrow(m), " level file(s) from ", backup_dir, "; md5 verified.")
  cat("\n=== Residential total AV ($B): before restore (growth) -> after (level) ===\n")
  # promote_print_totals labels its columns level/growth; here the roles swap
  b <- rbindlist(before, idcol = "scenario"); a <- rbindlist(after, idcol = "scenario")
  r <- merge(b, a, by = c("scenario", "tax_yr"), suffixes = c("_growth", "_level"), all = TRUE)
  setorder(r, scenario, tax_yr)
  print(r[, .(scenario, tax_yr, before_growth_B = round(total_av_growth / 1e9, 3),
              after_level_B = round(total_av_level / 1e9, 3))], row.names = FALSE, nrows = 100)
  invisible(TRUE)
}

if (!isTRUE(get0("PROMOTE_DEFINE_ONLY", envir = .GlobalEnv, ifnotfound = FALSE)))
  promote_growth_res()
