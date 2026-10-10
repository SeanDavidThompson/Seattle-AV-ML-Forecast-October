# scripts/ml/nwmls_features.R ----------------------------------------------
# NWMLS Seattle SFH features: one place for how they are built, which ones a
# residential model frame may contain, and the guards that keep a run from
# mixing modes.  Sourced by xx_nwmls_to_panel.R, 03_model_land.R,
# 03_model_impr.R, 04_retrofitting_values.R, 05_eval_holdout_2025.R,
# 06_forecast_av_2026_2031_sequential.R, main_ml.R and the ad_hoc scripts.
# Defines functions and constants only; running it changes nothing.
#
# nwmls_features (run_main_ml() / CFG, default "level"):
#   "level"  - today's behaviour.  The six 6/12-month trailing means of the
#              median price, closed sales and active listings go into both
#              the delta and the level frames.
#   "growth" - delta frames get year-over-year log growth of those means
#              (sea_*_lag6_yoy, sea_*_lag12_yoy) plus months of supply
#              (sea_mos_sfh_lag12) and NO levels.  Level frames keep the six
#              levels: the level models only drive the in-range historical
#              retrofit.
#
# Why: in "level" mode a tree reads the 12-month price level as a lookup key
# for "which past year looked like this", not as a market signal (2026-10
# audit, BUGS / NWMLS_GROWTH_DECISION.md).
#
# Units (checked against the 2026-06 EViews export, SEA_* headers):
#   sea_pmedesfh = monthly median closed SFH price ($)
#   sea_sesfh    = monthly closed SFH sales (count, not seasonally adjusted)
#   sea_alesfh   = active SFH listings at month end (count)
# so sea_alesfh_lag12 / sea_sesfh_lag12 is months of supply.
# -------------------------------------------------------------------------

# Today's six level columns, in the order 03_model_land.R has always selected
# them.  Order matters: it fixes the column order of the training frame, and
# "level" mode must reproduce today's frames exactly.
NWMLS_LEVEL_COLS <- c(
  "sea_pmedesfh_lag12", "sea_pmedesfh_lag6",
  "sea_sesfh_lag6",     "sea_sesfh_lag12",
  "sea_alesfh_lag6",    "sea_alesfh_lag12"
)

NWMLS_GROWTH_BASES   <- c("sea_pmedesfh", "sea_sesfh", "sea_alesfh")
NWMLS_GROWTH_WINDOWS <- c(6L, 12L)
NWMLS_MOS_COL        <- "sea_mos_sfh_lag12"

NWMLS_YOY_COLS <- as.vector(t(outer(NWMLS_GROWTH_BASES,
                                    paste0("_lag", NWMLS_GROWTH_WINDOWS, "_yoy"),
                                    paste0)))
NWMLS_GROWTH_COLS <- c(NWMLS_YOY_COLS, NWMLS_MOS_COL)

NWMLS_FEATURE_MODES <- c("level", "growth")

# Attribute name stamped on saved residential model artifacts.
NWMLS_MODE_ATTR <- "nwmls_features"

# ---- Mode -----------------------------------------------------------------
nwmls_check_mode <- function(mode) {
  if (!is.character(mode) || length(mode) != 1L || is.na(mode) ||
      !mode %in% NWMLS_FEATURE_MODES)
    stop("nwmls_features must be one of: ",
         paste0("\"", NWMLS_FEATURE_MODES, "\"", collapse = ", "),
         " (got ", paste(deparse(mode), collapse = ""), ")", call. = FALSE)
  mode
}

# Mode of the current run, read the same way every sourced script reads it.
nwmls_run_mode <- function() {
  nwmls_check_mode(get0("nwmls_features", envir = .GlobalEnv,
                        ifnotfound = "level"))
}

# NWMLS columns a residential model frame may contain.
#   level  mode, either frame : the six levels (exactly today's columns)
#   growth mode, delta frame  : the _yoy columns + months of supply, no levels
#   growth mode, level frame  : the six levels (level models are unchanged)
nwmls_model_cols <- function(mode, frame = c("delta", "level")) {
  mode  <- nwmls_check_mode(mode)
  frame <- match.arg(frame)
  if (mode == "growth" && frame == "delta") return(NWMLS_GROWTH_COLS)
  NWMLS_LEVEL_COLS
}

# ---- Build ----------------------------------------------------------------
nwmls_fcst_cache_path <- function(cache_dir, scenario) {
  file.path(cache_dir, paste0("nwmls_fcst_2026_2031_", scenario, ".rds"))
}

NWMLS_SCENARIOS <- c("baseline", "optimistic", "pessimistic")

# ---- Which export file -----------------------------------------------------
# data/nwmls/ holds several vintages of the same export, named
#   YYYY-MM_nwmls_housing_forecast_<scenario>.xlsx
# The vintage is the YYYY-MM prefix.  Files used to be chosen by newest
# modified time, which depends on how the files were copied: in a fresh
# checkout baseline resolved to 2026-03 while optimistic/pessimistic resolved
# to 2026-06.  The choice is now the newest PREFIX, and all scenarios must
# resolve to the same vintage.

# "YYYY-MM" from a file name, NA when the name has no such prefix.
nwmls_file_vintage <- function(path) {
  b <- basename(path)
  ifelse(grepl("^[0-9]{4}-[0-9]{2}_", b), substr(b, 1L, 7L), NA_character_)
}

# One file out of `files`: the newest vintage prefix.  Undated names (no
# YYYY-MM_ prefix, e.g. an Excel "~$" lock file) are ignored when any dated
# file exists; a single undated file is accepted on its own; anything else
# that cannot be ordered stops.
nwmls_pick_by_vintage <- function(files, label = "") {
  if (!length(files)) return(NULL)
  v <- nwmls_file_vintage(files)
  if (all(is.na(v))) {
    if (length(files) == 1L) return(files)
    stop("NWMLS ", label, ": ", length(files), " candidate files and none has ",
         "a YYYY-MM_ vintage prefix, so there is no way to choose: ",
         paste(basename(files), collapse = ", "), call. = FALSE)
  }
  if (any(is.na(v)))
    message("  NWMLS ", label, ": ignoring undated file(s) ",
            paste(basename(files[is.na(v)]), collapse = ", "))
  files <- files[!is.na(v)]
  v     <- v[!is.na(v)]
  hit   <- files[v == max(v)]
  if (length(hit) > 1L)
    stop("NWMLS ", label, ": more than one file for vintage ", max(v), ": ",
         paste(basename(hit), collapse = ", "), call. = FALSE)
  hit
}

# The file a scenario reads: Excel export first, legacy CSV second (same
# precedence as before), newest vintage within the format.  NULL if none.
nwmls_resolve_file <- function(scenario, nwmls_dir) {
  xlsx <- list.files(nwmls_dir,
                     pattern = paste0("nwmls_housing_forecast_", scenario, "\\.xlsx$"),
                     full.names = TRUE)
  csv  <- list.files(nwmls_dir,
                     pattern = paste0("combined_nwmls_m_", scenario, ".*\\.csv$"),
                     full.names = TRUE)
  if (length(xlsx))
    return(list(path = nwmls_pick_by_vintage(xlsx, scenario), kind = "xlsx"))
  if (length(csv))
    return(list(path = nwmls_pick_by_vintage(csv, scenario), kind = "csv"))
  NULL
}

# Resolve every scenario present in nwmls_dir and stop if they do not share
# one vintage.  Returns a data.frame (scenario, file, vintage), invisibly.
nwmls_check_vintages <- function(nwmls_dir, scenarios = NWMLS_SCENARIOS) {
  rows <- lapply(scenarios, function(sc) {
    r <- nwmls_resolve_file(sc, nwmls_dir)
    if (is.null(r)) return(NULL)
    data.frame(scenario = sc, file = basename(r[["path"]]),
               vintage = nwmls_file_vintage(r[["path"]]),
               stringsAsFactors = FALSE)
  })
  tab <- do.call(rbind, rows)
  if (is.null(tab)) return(invisible(NULL))
  vk <- ifelse(is.na(tab[["vintage"]]), "(undated)", tab[["vintage"]])
  if (length(unique(vk)) > 1L)
    stop("NWMLS scenarios resolve to different vintages in ", nwmls_dir, ":\n",
         paste0("    ", tab[["scenario"]], ": ", tab[["file"]], collapse = "\n"),
         "\n  Put the same YYYY-MM vintage of all three scenario exports in ",
         "the folder (older vintages may stay; the newest common one is not ",
         "guessed).", call. = FALSE)
  invisible(tab)
}

# Locate and read the scenario's NWMLS export.  The read itself is moved
# verbatim from xx_nwmls_to_panel.R; the file choice is nwmls_resolve_file().
# Returns list(data = monthly tibble with `date` and the raw lower-cased
# series, file = path read, vintage = "YYYY-MM").
nwmls_read_raw <- function(scenario, nwmls_dir) {
  nwmls_check_vintages(nwmls_dir)
  pick <- nwmls_resolve_file(scenario, nwmls_dir)
  nwmls_xlsx <- if (!is.null(pick) && pick[["kind"]] == "xlsx") pick[["path"]] else character(0)
  nwmls_csv  <- if (!is.null(pick) && pick[["kind"]] == "csv")  pick[["path"]] else character(0)

  if (length(nwmls_xlsx) > 0) {
    nwmls_file <- nwmls_xlsx
    message("  Reading NWMLS from Excel: ", basename(nwmls_file),
            " (vintage ", nwmls_file_vintage(nwmls_file), ")")

    # Row 1 = headers (col A blank, B+ = series names), Row 2 = blank, Row 3+ = data
    raw <- readxl::read_xlsx(nwmls_file, skip = 0, col_names = TRUE)
    names(raw)[1] <- "date_str"
    names(raw) <- tolower(names(raw))

    # Coerce all non-date columns to numeric (some may read as character)
    num_cols <- setdiff(names(raw), "date_str")
    raw[num_cols] <- lapply(raw[num_cols], function(x) as.numeric(as.character(x)))

    nwmls_raw <- raw |>
      dplyr::filter(!is.na(date_str), date_str != "") |>
      dplyr::mutate(date = as.Date(paste0(
        substr(date_str, 1, 4), "-",
        substr(date_str, 6, 7), "-01"
      ))) |>
      dplyr::select(-date_str)

  } else if (length(nwmls_csv) > 0) {
    nwmls_file <- nwmls_csv
    message("  Reading NWMLS from CSV (legacy): ", basename(nwmls_file),
            " (vintage ", nwmls_file_vintage(nwmls_file), ")")
    nwmls_raw <- readr::read_csv(nwmls_file, show_col_types = FALSE) |>
      janitor::clean_names()
  } else {
    stop("No NWMLS file found for scenario '", scenario, "' in ", nwmls_dir)
  }
  list(data = nwmls_raw, file = nwmls_file,
       vintage = nwmls_file_vintage(nwmls_file))
}

# Which raw columns feed sea_pmedesfh / sea_sesfh / sea_alesfh (and the KC
# twins).  First candidate set present wins: _fav (forecast average), then
# the individual spec names, then raw.
nwmls_detect_cols <- function(nwmls_raw) {
  sea_sfh_candidates <- list(
    c(sea_pmedesfh = "sea_pmedesfh_fav", sea_sesfh = "sea_sesfh_fav", sea_alesfh = "sea_alesfh_fav"),
    c(sea_pmedesfh = "sea_pmedesfh_f",   sea_sesfh = "sea_sesfhf",    sea_alesfh = "sea_alesfhf"),
    c(sea_pmedesfh = "sea_pmedesfh",     sea_sesfh = "sea_sesfh",     sea_alesfh = "sea_alesfh")
  )
  sea_sfh_found <- character(0)
  for (cand in sea_sfh_candidates) {
    if (all(cand %in% names(nwmls_raw))) {
      sea_sfh_found <- cand
      break
    }
  }
  if (length(sea_sfh_found) != 3) {
    stop("Seattle SFH columns not found in NWMLS file.\n",
         "  Available columns: ", paste(names(nwmls_raw), collapse = ", "))
  }

  kc_sfh_candidates <- list(
    c(k_pmedesfh = "k_pmedesfh_fav", k_sesfh = "k_sesfh_fav", k_alesfh = "k_alesfh_fav"),
    c(k_pmedesfh = "k_pmedesfh_f90", k_sesfh = "k_sesfh_f50", k_alesfh = "k_alesfh_f50"),
    c(k_pmedesfh = "k_pmedesfh",     k_sesfh = "k_sesfh",     k_alesfh = "k_alesfh")
  )
  kc_sfh_found <- character(0)
  for (cand in kc_sfh_candidates) {
    if (all(cand %in% names(nwmls_raw))) {
      kc_sfh_found <- cand
      break
    }
  }
  list(sea = sea_sfh_found, kc = kc_sfh_found)
}

# Monthly series -> annual features keyed by tax_yr (December snapshot of the
# trailing means, mapped to tax_yr = year + 1).  The level computations are
# moved verbatim from xx_nwmls_to_panel.R; growth columns are appended last.
# Returns list(annual = tibble, has_kc = logical).
nwmls_build_annual <- function(nwmls_raw) {
  found <- nwmls_detect_cols(nwmls_raw)
  sea_sfh_found <- found[["sea"]]
  kc_sfh_found  <- found[["kc"]]
  message("  Seattle SFH columns: ", paste(sea_sfh_found, collapse = ", "))

  has_kc_sfh <- length(kc_sfh_found) == 3
  if (has_kc_sfh) {
    message("  King County SFH columns: ", paste(kc_sfh_found, collapse = ", "))
  } else {
    message("  King County SFH columns not found in CSV — skipping KC variables")
  }

  # ---- Select and rename --------------------------------------------------
  rename_vec <- sea_sfh_found
  if (has_kc_sfh) rename_vec <- c(rename_vec, kc_sfh_found)

  cols_to_keep <- c("date", unname(rename_vec))
  nwmls <- nwmls_raw |>
    dplyr::select(dplyr::all_of(cols_to_keep)) |>
    dplyr::rename(dplyr::all_of(rename_vec)) |>
    dplyr::mutate(
      ym    = zoo::as.yearmon(date),
      year  = lubridate::year(date),
      month = lubridate::month(date)
    )

  # ---- Rolling lag windows: Seattle SFH -----------------------------------
  nwmls_lagged <- nwmls |>
    dplyr::select(ym, year, month,
                  sea_pmedesfh, sea_sesfh, sea_alesfh,
                  dplyr::any_of(c("k_pmedesfh", "k_sesfh", "k_alesfh"))) |>
    dplyr::arrange(ym) |>
    dplyr::mutate(
      sea_pmedesfh_lag6  = slider::slide_dbl(sea_pmedesfh, mean, .before = 5,  .complete = TRUE, na.rm = TRUE),
      sea_pmedesfh_lag12 = slider::slide_dbl(sea_pmedesfh, mean, .before = 11, .complete = TRUE, na.rm = TRUE),
      sea_sesfh_lag6     = slider::slide_dbl(sea_sesfh,    mean, .before = 5,  .complete = TRUE, na.rm = TRUE),
      sea_sesfh_lag12    = slider::slide_dbl(sea_sesfh,    mean, .before = 11, .complete = TRUE, na.rm = TRUE),
      sea_alesfh_lag6    = slider::slide_dbl(sea_alesfh,   mean, .before = 5,  .complete = TRUE, na.rm = TRUE),
      sea_alesfh_lag12   = slider::slide_dbl(sea_alesfh,   mean, .before = 11, .complete = TRUE, na.rm = TRUE)
    )

  # ---- Rolling lag windows: King County SFH (conditional) -----------------
  if (has_kc_sfh) {
    nwmls_lagged <- nwmls_lagged |>
      dplyr::mutate(
        k_pmedesfh_lag6  = slider::slide_dbl(k_pmedesfh, mean, .before = 5,  .complete = TRUE, na.rm = TRUE),
        k_pmedesfh_lag12 = slider::slide_dbl(k_pmedesfh, mean, .before = 11, .complete = TRUE, na.rm = TRUE),
        k_sesfh_lag6     = slider::slide_dbl(k_sesfh,    mean, .before = 5,  .complete = TRUE, na.rm = TRUE),
        k_sesfh_lag12    = slider::slide_dbl(k_sesfh,    mean, .before = 11, .complete = TRUE, na.rm = TRUE),
        k_alesfh_lag6    = slider::slide_dbl(k_alesfh,   mean, .before = 5,  .complete = TRUE, na.rm = TRUE),
        k_alesfh_lag12   = slider::slide_dbl(k_alesfh,   mean, .before = 11, .complete = TRUE, na.rm = TRUE)
      )
  }

  # ---- Annual: December snapshot → next tax_yr ----------------------------
  nwmls_av_year <- nwmls_lagged |>
    dplyr::filter(month == 12) |>
    dplyr::arrange(year) |>
    dplyr::transmute(
      tax_yr = year + 1,
      # Seattle SFH
      sea_pmedesfh_lag12,
      sea_pmedesfh_lag6,
      sea_sesfh_lag6,
      sea_sesfh_lag12,
      sea_alesfh_lag6,
      sea_alesfh_lag12
    )

  # Add King County SFH lags if available
  if (has_kc_sfh) {
    kc_annual <- nwmls_lagged |>
      dplyr::filter(month == 12) |>
      dplyr::arrange(year) |>
      dplyr::transmute(
        tax_yr = year + 1,
        k_pmedesfh_lag12,
        k_pmedesfh_lag6,
        k_sesfh_lag6,
        k_sesfh_lag12,
        k_alesfh_lag6,
        k_alesfh_lag12
      )
    nwmls_av_year <- nwmls_av_year |>
      dplyr::left_join(kc_annual, by = "tax_yr")
  }

  # ---- Growth features (additive in both modes) ---------------------------
  # Built after the snapshot so the level columns above are untouched.
  nwmls_av_year <- nwmls_add_growth(nwmls_av_year)

  list(annual = nwmls_av_year, has_kc = has_kc_sfh)
}

# Append the growth columns to an annual table that carries tax_yr and the six
# level columns.  The prior year is found by value (tax_yr - 1), not by row
# position, so a gap in tax_yr gives NA rather than a two-year change.
#   <col>_yoy         = log(x[tax_yr] / x[tax_yr - 1])
#   sea_mos_sfh_lag12 = sea_alesfh_lag12 / sea_sesfh_lag12   (months of supply)
nwmls_add_growth <- function(annual) {
  miss <- setdiff(c("tax_yr", NWMLS_LEVEL_COLS), names(annual))
  if (length(miss))
    stop("nwmls_add_growth: annual table lacks ", paste(miss, collapse = ", "))
  ty   <- annual[["tax_yr"]]
  prev <- match(ty - 1, ty)
  for (b in NWMLS_GROWTH_BASES) for (w in NWMLS_GROWTH_WINDOWS) {
    lvl <- paste0(b, "_lag", w)
    x   <- annual[[lvl]]
    annual[[paste0(lvl, "_yoy")]] <- log(x / x[prev])
  }
  annual[[NWMLS_MOS_COL]] <- annual[["sea_alesfh_lag12"]] / annual[["sea_sesfh_lag12"]]
  annual
}

# Rows written to nwmls_fcst_2026_2031_<scenario>.rds (forecast_start - 1
# through forecast_end).  Moved verbatim from xx_nwmls_to_panel.R.
nwmls_fcst_slice <- function(annual, forecast_start, forecast_end) {
  annual |>
    dplyr::filter(tax_yr >= forecast_start - 1L,
                  tax_yr <= forecast_end) |>
    dplyr::arrange(tax_yr) |>
    dplyr::distinct(tax_yr, .keep_all = TRUE)
}

# Rebuild and write the scenario's NWMLS forecast cache from the export.
nwmls_write_fcst_cache <- function(scenario, cache_dir, nwmls_dir,
                                   forecast_start, forecast_end) {
  raw   <- nwmls_read_raw(scenario, nwmls_dir)
  built <- nwmls_build_annual(raw[["data"]])
  out   <- nwmls_fcst_slice(built[["annual"]], forecast_start, forecast_end)
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  path <- nwmls_fcst_cache_path(cache_dir, scenario)
  saveRDS(out, path)
  message("\U0001f4be NWMLS forecast cached to: ", basename(path))
  invisible(path)
}

# ---- Guards ---------------------------------------------------------------

# Columns a mode needs in the forecast cache / panel.
nwmls_required_cols <- function(mode) {
  mode <- nwmls_check_mode(mode)
  union(nwmls_model_cols(mode, "delta"), nwmls_model_cols(mode, "level"))
}

# In growth mode the forecast cache must carry the growth columns.  A cache
# written before this branch has only the levels; running on it would leave
# every forecast-year growth feature NA (then median-imputed in Step 6).
#   rebuild = NULL     -> stop() with the exact instruction
#   rebuild = function -> called once to rewrite the cache, then re-checked
# Level mode never stops here.
nwmls_ensure_fcst_cache <- function(path, mode, rebuild = NULL) {
  mode <- nwmls_check_mode(mode)
  need <- nwmls_required_cols(mode)
  lacking <- function() {
    if (!file.exists(path)) return(need)
    setdiff(need, names(readRDS(path)))
  }
  miss <- lacking()
  if (!length(miss)) return(invisible("ok"))
  if (mode == "level") {
    # The level path has always tolerated what is on disk; 05_extend stops on
    # a missing file itself.
    return(invisible("level"))
  }
  why <- if (!file.exists(path)) "is missing" else
    paste0("lacks ", length(miss), " growth column(s): ",
           paste(miss, collapse = ", "))
  if (is.function(rebuild)) {
    message("  NWMLS cache ", basename(path), " ", why,
            " - rebuilding for nwmls_features = \"growth\"")
    rebuild()
    miss <- lacking()
    if (!length(miss)) return(invisible("rebuilt"))
    why <- paste0("still lacks ", paste(miss, collapse = ", "),
                  " after a rebuild")
  }
  stop("nwmls_features = \"growth\" but the NWMLS forecast cache ", path, " ",
       why, ".\n  Rebuild it (writes only that file):\n",
       "    source(here::here(\"scripts\", \"ml\", \"nwmls_features.R\"))\n",
       "    nwmls_write_fcst_cache(\"<scenario>\", cache_dir = \"", dirname(path),
       "\",\n      nwmls_dir = here::here(\"data\", \"nwmls\"), ",
       "forecast_start = 2027L, forecast_end = 2031L)",
       call. = FALSE)
}

# Join any growth/level columns the residential panel lacks, by tax_yr, from
# an annual table.  Used in growth mode on panels cached before this branch
# (panel_tbl_res.rds carries only the levels).  Existing columns are never
# overwritten.  Returns the panel with the same class it came in with.
nwmls_ensure_panel_cols <- function(panel, mode, annual) {
  need <- nwmls_required_cols(mode)
  miss <- setdiff(need, names(panel))
  if (!length(miss)) return(panel)
  bad <- setdiff(miss, names(annual))
  if (length(bad))
    stop("nwmls_ensure_panel_cols: annual table lacks ", paste(bad, collapse = ", "))
  is_dt <- data.table::is.data.table(panel)
  ann <- data.table::as.data.table(annual)[, c("tax_yr", miss), with = FALSE]
  ann[, tax_yr := as.numeric(tax_yr)]
  key <- data.table::data.table(tax_yr = as.numeric(panel[["tax_yr"]]))
  vals <- ann[key, on = "tax_yr"]
  for (cn in miss) {
    if (is_dt) panel[, (cn) := vals[[cn]]]
    else panel[[cn]] <- vals[[cn]]
  }
  message("  joined ", length(miss), " NWMLS column(s) onto the residential ",
          "panel by tax_yr: ", paste(miss, collapse = ", "))
  panel
}

# Mode recorded on a saved model artifact.  Artifacts trained before this
# branch carry no stamp and were all trained on levels.
nwmls_model_mode <- function(obj) {
  m <- attr(obj, NWMLS_MODE_ATTR, exact = TRUE)
  if (is.null(m)) "level" else m
}

nwmls_stamp_mode <- function(obj, mode) {
  attr(obj, NWMLS_MODE_ATTR) <- nwmls_check_mode(mode)
  obj
}

# stop() if any loaded model was trained in a different mode than the run.
#   models  named list of model artifacts (the *_cv lists)
nwmls_assert_model_mode <- function(models, mode, where = "") {
  mode <- nwmls_check_mode(mode)
  got  <- vapply(models, nwmls_model_mode, character(1))
  bad  <- got[got != mode]
  if (length(bad))
    stop(where, if (nzchar(where)) ": " else "",
         "nwmls_features = \"", mode, "\" but the loaded residential model(s) ",
         "were trained with ",
         paste0(names(bad), " = \"", bad, "\"", collapse = ", "),
         ".\n  Point model_dir at models trained in this mode, or retrain with ",
         "model_replicate = TRUE.  If the models came from an earlier call in ",
         "this R session, rm() them or restart R first.", call. = FALSE)
  invisible(TRUE)
}

# Every feature a booster uses must be present and not all-NA in each forecast
# year; otherwise Step 6 median-imputes the whole year without saying so.
#   panel     data.frame / data.table with tax_yr
#   features  raw column names to check
#   years     forecast years
#   mode      growth -> stop(); level -> warning() (see NWMLS_GROWTH_DECISION.md)
nwmls_assert_forecast_features <- function(panel, features, years, mode,
                                           where = "") {
  mode <- nwmls_check_mode(mode)
  features <- unique(features)
  missing_cols <- setdiff(features, names(panel))
  present <- intersect(features, names(panel))
  ty <- panel[["tax_yr"]]
  all_na <- character(0)
  for (f in present) {
    x <- panel[[f]]
    for (yr in years) {
      sel <- ty == yr
      if (any(sel) && all(is.na(x[sel])))
        all_na <- c(all_na, paste0(f, " (", yr, ")"))
    }
  }
  if (!length(missing_cols) && !length(all_na)) return(invisible(TRUE))
  msg <- paste0(
    where, if (nzchar(where)) ": " else "",
    "forecast-year features the residential delta boosters use are ",
    if (length(missing_cols))
      paste0("MISSING from the panel: ", paste(missing_cols, collapse = ", "),
             if (length(all_na)) "; and " else "") else "",
    if (length(all_na))
      paste0("ALL-NA in: ", paste(all_na, collapse = ", ")) else "",
    ".\n  Step 6 would median-impute them.  If these are NWMLS columns, the ",
    "extended panel predates this nwmls_features mode: re-run with ",
    "forecast_only = FALSE, extend_replicate = TRUE.")
  if (mode == "growth") stop(msg, call. = FALSE)
  warning(msg, call. = FALSE)
  invisible(FALSE)
}

# ---- Sensitivity helpers --------------------------------------------------

# Last month with an OBSERVED value.  The EViews export carries both the raw
# series (e.g. sea_pmedesfh, #N/A after the last actual) and the forecast
# average (sea_pmedesfh_fav, filled through the horizon), so the last
# non-NA raw value marks the boundary.  Never hard-coded.
nwmls_last_observed_month <- function(nwmls_raw, col = "sea_pmedesfh") {
  if (!col %in% names(nwmls_raw))
    stop("nwmls_last_observed_month: raw column '", col, "' not in the export; ",
         "cannot tell observed months from forecast months")
  d <- nwmls_raw[["date"]][!is.na(nwmls_raw[[col]])]
  if (!length(d)) stop("nwmls_last_observed_month: '", col, "' is all NA")
  max(d)
}

# Multiply the median SFH price by `factor` in months after `after` only.
# Touches every candidate price column present (the _fav / spec / raw names),
# so whichever one nwmls_detect_cols() picks is shocked.
nwmls_shock_price <- function(nwmls_raw, factor, after) {
  cols <- intersect(c("sea_pmedesfh_fav", "sea_pmedesfh_f", "sea_pmedesfh"),
                    names(nwmls_raw))
  if (!length(cols)) stop("nwmls_shock_price: no sea_pmedesfh column found")
  sel <- !is.na(nwmls_raw[["date"]]) & nwmls_raw[["date"]] > after
  for (cn in cols) {
    x <- nwmls_raw[[cn]]
    x[sel] <- x[sel] * factor
    nwmls_raw[[cn]] <- x
  }
  nwmls_raw
}
