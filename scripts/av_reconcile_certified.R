# =============================================================================
# av_reconcile_certified.R
#
# Strategy:
#   1. Use ad_hoc historical shares (appraised AV by prop type) to split
#      certified totals into property-type components (2022-2026)
#   2. For forecast (2027-2031), apply growth rates from:
#      - ML res panel for Residential
#      - ML condo panel for Condo
#      - Aggregate commercial script for Commercial
#   3. Compound forward from the 2026 certified-based levels
#
# This produces a single continuous certified-anchored series per prop type.
# =============================================================================

library(tidyverse)
library(data.table)
library(scales)
library(here)

cache_dir <- here("data", "cache")

# ============================================================================
# 1. CERTIFIED AV TOTALS  +  PERSONAL PROPERTY LAYER
# ============================================================================
# BASIS: REGULAR LEVY (the levy base OERF forecasts — per A. Zhang
# 2026-07-31).  grand_total = citypersprop statistics-report series (incl.
# senior-exempt AV); cert_total = regular-levy basis, by source:
#   2022: ESTIMATED grand x (1 - 2023 senior-exempt share) — no letter in
#         hand; exemption was pre-expansion, so 2023's share is the proxy
#   2023: grand - $1,172,001,173 senior exempt (Nov 21 2022 certification;
#         letter's certified AV $307,774,003,816 is the same basis pre-BoE)
#   2024: EXACT $299,443,737,414 (Feb 1 2024 certified letter)
#   2025: $297,696,494,175 (line R, PRELIMINARY worksheet 09.26.2024 —
#         refine with the Feb 2025 certified letter when located)
#   2026: EXACT $306,466,960,900 (Feb 17 2026 certified letter; grand
#         $308,777,816,989, excess $304,130,513,509)
exempt_2023 <- 1172001173
share_2023  <- exempt_2023 / (307774003816 + exempt_2023)

certified_av <- tribble(
  ~tax_yr, ~grand_total,  ~cert_total,
  2022L,   276293453116,  276293453116 * (1 - share_2023),
  2023L,   308874491598,  308874491598 - exempt_2023,
  2024L,   301214631275,  299443737414,
  2025L,   299963009621,  297696494175,
  2026L,   308777816989,  306466960900
)

# PP layer (locally assessed + state public service + omitted), exact:
pp_layer <- tribble(
  ~tax_yr, ~pp_av,
  2022L,   7490457363,
  2023L,   8542526282,
  2024L,   8947018132,
  2025L,   9073883825,
  2026L,   9330922938
)

certified_av <- certified_av %>%
  left_join(pp_layer, by = "tax_yr") %>%
  mutate(cert_real = cert_total - pp_av)

message("=== Certificate decomposition ($B) ===")
print(certified_av %>%
        mutate(grand = round(grand_total / 1e9, 2),
               total = round(cert_total / 1e9, 2),
               pp    = round(pp_av / 1e9, 2),
               real  = round(cert_real / 1e9, 2)) %>%
        select(tax_yr, grand, total, pp, real))

# ============================================================================
# 2. HISTORICAL APPRAISED AV BY PROP TYPE (from ad_hoc.R)
#    Commercial = all prop_type C (Commercial + Apartment + Industrial)
# ============================================================================
hist_appraised <- tribble(
  ~tax_yr, ~track,          ~appraised_b,
  2022L,   "res",           136.44,
  2022L,   "com",            99.12,
  2022L,   "condo",          28.88,
  2023L,   "res",           159.58,
  2023L,   "com",           104.96,
  2023L,   "condo",          30.76,
  2024L,   "res",           147.84,
  2024L,   "com",           105.44,
  2024L,   "condo",          32.85,
  2025L,   "res",           158.89,
  2025L,   "com",            94.53,
  2025L,   "condo",          30.94,
  2026L,   "res",           165.53,
  2026L,   "com",            94.79,
  2026L,   "condo",          32.25
) %>%
  mutate(appraised_av = appraised_b * 1e9)

# Compute shares
hist_totals <- hist_appraised %>%
  group_by(tax_yr) %>%
  summarise(total_appraised = sum(appraised_av), .groups = "drop")

hist_shares <- hist_appraised %>%
  left_join(hist_totals, by = "tax_yr") %>%
  mutate(share = appraised_av / total_appraised)

message("=== Historical Appraised Shares ===")
hist_shares %>%
  select(tax_yr, track, share) %>%
  pivot_wider(names_from = track, values_from = share) %>%
  mutate(across(c(res, com, condo), ~ round(.x, 4))) %>%
  print()

# Apply shares to the REAL PROPERTY portion of the certificate
cert_by_type <- hist_shares %>%
  left_join(certified_av, by = "tax_yr") %>%
  mutate(cert_av = cert_real * share) %>%
  select(tax_yr, track, cert_av)

# Personal property rides along as its own track (no share split)
cert_by_type <- bind_rows(
  cert_by_type,
  certified_av %>% transmute(tax_yr, track = "pp", cert_av = pp_av))

message("\n=== Certified AV by Property Type ($B) ===")
cert_by_type %>%
  mutate(cert_b = round(cert_av / 1e9, 2)) %>%
  select(tax_yr, track, cert_b) %>%
  pivot_wider(names_from = track, values_from = cert_b) %>%
  print()

# ============================================================================
# 3. FORECAST GROWTH RATES BY TRACK
# ============================================================================
message("\nLoading forecast growth rates...")

# --- All tracks: MATCHED-PARCEL YoY growth from the ML forecast caches ---
# Growth for year Y is computed ONLY on parcels with positive AV in both
# Y-1 and Y.  This is the settled rebase methodology: unmatched total-sum
# growth is contaminated by coverage composition (condo 2026->2027 sums
# jump +205% purely because 22.6K observed units become 80K forecast
# units; com jumps ~+12% from retail/industrial/condo coverage entrants).
# Matched chains measure value growth, which is what gets compounded off
# the certified base.
load_matched_gyy <- function(track, scenario) {
  fpath <- file.path(cache_dir,
                     paste0("panel_tbl_2006_2031_forecasted_", scenario,
                            "_", track, ".rds"))
  if (!file.exists(fpath)) return(NULL)

  dt <- as.data.table(readRDS(fpath))

  # Same AV definition as the scen_tbl QA helper: observed value first,
  # filled value second, model prediction last (forecast years carry the
  # pred_* columns; observed years the appr_* columns).
  vc <- intersect(c("appr_land_val", "appr_land_val_filled",
                    "pred_appr_land_val"), names(dt))
  ic <- intersect(c("appr_imps_val", "appr_imps_val_filled",
                    "pred_appr_imps_val"), names(dt))
  dt[, av := rowSums(cbind(
      do.call(fcoalesce, lapply(vc, function(cn) as.numeric(dt[[cn]]))),
      do.call(fcoalesce, lapply(ic, function(cn) as.numeric(dt[[cn]])))),
      na.rm = TRUE)]
  if ("pred_total_assessed" %in% names(dt))
    dt[av == 0 & !is.na(pred_total_assessed),
       av := as.numeric(pred_total_assessed)]   # com_other carry-forward

  dt <- unique(dt[av > 0 & tax_yr >= 2026,
                  .(parcel_id, tax_yr, av)],
               by = c("parcel_id", "tax_yr"))

  out <- lapply(2027:2031, function(yr) {
    m <- merge(dt[tax_yr == yr - 1L, .(parcel_id, av0 = av)],
               dt[tax_yr == yr,      .(parcel_id, av1 = av)],
               by = "parcel_id")
    if (nrow(m) == 0) return(NULL)
    data.table(tax_yr = yr, track = track,
               gyy = sum(m$av1) / sum(m$av0) - 1,
               n_matched = nrow(m), scenario = scenario)
  })
  rbindlist(Filter(Negate(is.null), out))
}

res_gyy_bas <- load_matched_gyy("res", "baseline")
res_gyy_opt <- load_matched_gyy("res", "optimistic")
res_gyy_pes <- load_matched_gyy("res", "pessimistic")

message("  Res baseline growth rates:")
if (!is.null(res_gyy_bas)) print(res_gyy_bas)

# --- Condo: matched-parcel (2027 chains off the 22.6K units with an
#     observed 2026 value; the 57K uncovered units inherit the same
#     growth via the certified-base compounding, which is the point) ---
condo_gyy_bas <- load_matched_gyy("condo", "baseline")
condo_gyy_opt <- load_matched_gyy("condo", "optimistic")
condo_gyy_pes <- load_matched_gyy("condo", "pessimistic")

message("  Condo baseline growth rates:")
if (!is.null(condo_gyy_bas)) print(condo_gyy_bas)

# --- Commercial: matched-parcel from the COMBINED per-subgroup ML
#     forecast (panel_tbl_2006_2031_forecasted_<sc>_com.rds — the
#     rbind of apt/office/industrial/retail/hospitality/medical +
#     com_other written by run_main_ml).  The previous version of this
#     script read the legacy aggregate CoStar CSV, or fell back to
#     hardcoded rates; both predate the subgroup pipeline. ---
com_gyy_bas <- load_matched_gyy("com", "baseline")
com_gyy_opt <- load_matched_gyy("com", "optimistic")
com_gyy_pes <- load_matched_gyy("com", "pessimistic")

if (is.null(com_gyy_bas))
  stop("Combined commercial forecast cache not found — run the three ",
       "run_main_ml() scenario passes for prop_scope='com' first.")

# --- Personal property: econ-linked growth ------------------------------
# PP (business equipment & fixtures) tracks business activity.  Growth for
# 2027-2031 = PP_ECON_ELASTICITY x YoY growth of PP_ECON_DRIVER, read from
# the scenario econ caches (econ_fcst_2026_2031_<scenario>.rds, built by
# xx_econ_to_panel.R from the OERF workbook) — this is what makes PP
# scenario-differentiated.  On first run leave PP_ECON_DRIVER = NULL: the
# script lists the cache's numeric columns so you can pick one, and falls
# back to the exact historical CAGR (2022-2026: +5.65%/yr, from the
# citypersprop reports) meanwhile.  Elasticity default 1.0; tune if the
# chosen driver historically over/under-shoots PP growth.
PP_ECON_DRIVER     <- NULL   # e.g. an employment or taxable-sales column
PP_ECON_ELASTICITY <- 1.0

pp_hist_cagr <- (9330922938 / 7490457363)^(1 / 4) - 1  # 2022->2026

pp_gyy <- function(scenario_label) {
  fallback <- function(msg) {
    message("  \u26a0\ufe0f  PP growth fallback (", msg, ") — using 2022-2026 ",
            "CAGR ", sprintf("%.2f%%", 100 * pp_hist_cagr), " for ",
            scenario_label, " (scenario-invariant)")
    data.table(tax_yr = 2027:2031, track = "pp", gyy = pp_hist_cagr,
               n_matched = NA_integer_, scenario = scenario_label)
  }
  ep <- file.path(cache_dir,
                  paste0("econ_fcst_2026_2031_", scenario_label, ".rds"))
  if (!file.exists(ep)) return(fallback("econ cache missing"))
  ec <- as.data.table(readRDS(ep))
  yr_col <- intersect(c("tax_yr", "year", "yr"), names(ec))[1]
  if (is.na(yr_col)) return(fallback("no year column in econ cache"))
  if (is.null(PP_ECON_DRIVER)) {
    num_cols <- setdiff(names(ec)[sapply(ec, is.numeric)], yr_col)
    message("  PP_ECON_DRIVER not set — columns available in ",
            basename(ep), ":\n    ",
            paste(num_cols, collapse = "\n    "))
    return(fallback("driver not configured"))
  }
  if (!PP_ECON_DRIVER %in% names(ec))
    return(fallback(paste0("'", PP_ECON_DRIVER, "' not in cache")))
  drv <- ec[, .(yr = as.integer(get(yr_col)),
                v  = as.numeric(get(PP_ECON_DRIVER)))]
  drv <- drv[!is.na(v), .(v = mean(v)), by = yr][order(yr)]
  drv[, g := v / shift(v) - 1]
  out <- drv[yr %in% 2027:2031 & !is.na(g),
             .(tax_yr = yr, track = "pp",
               gyy = PP_ECON_ELASTICITY * g,
               n_matched = NA_integer_, scenario = scenario_label)]
  if (nrow(out) < 5)
    return(fallback("driver lacks full 2027-2031 coverage"))
  message("  PP growth (", scenario_label, "): ",
          paste(sprintf("%d %+.2f%%", out$tax_yr, 100 * out$gyy),
                collapse = ", "))
  out
}
pp_gyy_bas <- pp_gyy("baseline")
pp_gyy_opt <- pp_gyy("optimistic")
pp_gyy_pes <- pp_gyy("pessimistic")

message("  Com baseline growth rates:")
print(com_gyy_bas)

# --- Sanity table: matched YoY rates, all tracks x scenarios ------------
# 2027 rates should look like value growth (single-digit %), NOT the
# coverage jumps (+205% condo / +12% com) the old sum-based rates showed.
gyy_check <- rbindlist(Filter(Negate(is.null), list(
  res_gyy_bas, res_gyy_opt, res_gyy_pes,
  condo_gyy_bas, condo_gyy_opt, condo_gyy_pes,
  com_gyy_bas, com_gyy_opt, com_gyy_pes)), use.names = TRUE)
message("\n=== Matched-parcel YoY growth (%) ===")
print(dcast(gyy_check[, .(tax_yr, scenario, track,
                          pct = round(100 * gyy, 2))],
            track + tax_yr ~ scenario, value.var = "pct"))
message("Matched parcel counts (baseline):")
print(dcast(gyy_check[scenario == "baseline"],
            track ~ tax_yr, value.var = "n_matched"))

# ============================================================================
# 3b. CALIBRATION TO THE ASSESSOR'S PRELIMINARY TY2027 ROLL (optional)
# ============================================================================
# Default NULL: off, output unchanged.  To calibrate, set
#   PRELIM_TOTAL <- 294452027000   # preliminary TY2027 roll, after the senior
#                                  # exemption = regular-levy basis, like the
#                                  # certified totals above; includes personal
#                                  # property and state-assessed value
#   PRELIM_NC    <- 1631730000     # new construction inside PRELIM_TOTAL
#   PRELIM_PP    <- NULL           # optional preliminary PP figure
# When set:
#   existing 2027 (real + PP) = PRELIM_TOTAL - PRELIM_NC
#   PP 2027                   = PRELIM_PP if given, else this script's PP
#                               forecast; state-assessed value rides inside
#                               PP / the total with no separate adjustment
#   real 2027 by type         = this script's certified-basis 2027 res / com /
#                               condo, scaled by one factor so real + PP hits
#                               the existing total
#   NC                        = stock 2027 = PRELIM_NC (replaces the CSV's
#                               2027 flow), then stock x (1 + real growth) +
#                               the CSV flows for 2028-2031
#   2028-2031                 = the pipeline's growth rates compounded from
#                               the calibrated 2027
# and NC is layered into every chart and table labelled Total ("Total incl.
# new construction"), so all of them show PRELIM_TOTAL in 2027.
PRELIM_TOTAL <- NULL
PRELIM_NC    <- NULL
PRELIM_PP    <- NULL

prelim_on <- !is.null(PRELIM_TOTAL) || !is.null(PRELIM_NC) || !is.null(PRELIM_PP)
if (prelim_on) {
  is_num1 <- function(x) is.numeric(x) && length(x) == 1L && is.finite(x)
  if (!is_num1(PRELIM_TOTAL) || !is_num1(PRELIM_NC))
    stop("Preliminary-roll calibration needs BOTH PRELIM_TOTAL and PRELIM_NC as ",
         "single finite numbers in dollars (or all PRELIM_* NULL to switch it off).")
  if (!is.null(PRELIM_PP) && !is_num1(PRELIM_PP))
    stop("PRELIM_PP must be NULL or a single finite number in dollars.")
  if (PRELIM_TOTAL < 1e9)
    stop("PRELIM_TOTAL = ", PRELIM_TOTAL, " - give dollars, not billions ",
         "(e.g. 294452027000).")
  if (PRELIM_NC < 0 || PRELIM_NC >= PRELIM_TOTAL)
    stop("PRELIM_NC must be >= 0 and below PRELIM_TOTAL.")
  if (!is.null(PRELIM_PP) && (PRELIM_PP < 0 || PRELIM_PP >= PRELIM_TOTAL - PRELIM_NC))
    stop("PRELIM_PP must be >= 0 and below PRELIM_TOTAL - PRELIM_NC.")
  message("\nPreliminary-roll calibration ON: total ",
          format(PRELIM_TOTAL, big.mark = ",", scientific = FALSE),
          " | NC ", format(PRELIM_NC, big.mark = ",", scientific = FALSE),
          " | PP ", if (is.null(PRELIM_PP)) "script forecast" else
            format(PRELIM_PP, big.mark = ",", scientific = FALSE))
}

# NC flow table (tax_yr + one column per scenario) with 2027 set to PRELIM_NC
# when the calibration is on.  NULL in, calibration off: NULL out.
# report = TRUE prints the CSV's own 2027 flow next to PRELIM_NC.
prelim_nc_override <- function(w, report = FALSE) {
  if (!prelim_on) return(w)
  sc <- c("baseline", "optimistic", "pessimistic")
  if (is.null(w)) {
    message("  No NC forecast CSV: NC = PRELIM_NC in 2027, no NC flows after ",
            "2027 (the 2027 stock still compounds).")
    w <- tibble(tax_yr = 2027L)
  }
  w <- as_tibble(w) %>% mutate(tax_yr = as.integer(tax_yr))
  if (!2027L %in% w$tax_yr) w <- bind_rows(w, tibble(tax_yr = 2027L))
  for (cn in sc) {
    if (!cn %in% names(w)) w[[cn]] <- NA_real_
    if (report)
      message(sprintf("  NC 2027 %-11s CSV flow %s -> PRELIM_NC %s", cn,
                      format(round(w[[cn]][w$tax_yr == 2027L]), big.mark = ",",
                             scientific = FALSE),
                      format(PRELIM_NC, big.mark = ",", scientific = FALSE)))
    w[[cn]][w$tax_yr == 2027L] <- PRELIM_NC
  }
  arrange(w, tax_yr)
}

# ============================================================================
# 4. BUILD CERTIFIED-BASED FORECAST (2027-2031)
# ============================================================================
message("\nBuilding certified-based forecast...")

build_forecast <- function(gyy_list, scenario_label) {
  gyy_all <- rbindlist(Filter(function(x) !is.null(x), gyy_list), use.names = TRUE)
  gyy_all <- gyy_all[scenario == scenario_label]

  tracks <- unique(gyy_all$track)
  results <- list()

  for (tr in tracks) {
    base_2026 <- cert_by_type %>%
      filter(tax_yr == 2026, track == tr) %>%
      pull(cert_av)
    if (length(base_2026) == 0) next

    tr_gyy <- gyy_all[track == tr][order(tax_yr)]

    fcst <- data.table(tax_yr = 2026:2031, track = tr,
                       scenario = scenario_label, cert_av = NA_real_)
    fcst$cert_av[1] <- base_2026

    for (i in 2:nrow(fcst)) {
      yr <- fcst$tax_yr[i]
      g <- tr_gyy[tax_yr == yr, gyy]
      if (length(g) == 0 || is.na(g)) g <- 0
      fcst$cert_av[i] <- fcst$cert_av[i-1] * (1 + g)
    }
    results[[tr]] <- fcst
  }
  rbindlist(results, use.names = TRUE)
}

fcst_bas <- build_forecast(list(res_gyy_bas, condo_gyy_bas, com_gyy_bas, pp_gyy_bas), "baseline")
fcst_opt <- build_forecast(list(res_gyy_opt, condo_gyy_opt, com_gyy_opt, pp_gyy_opt), "optimistic")
fcst_pes <- build_forecast(list(res_gyy_pes, condo_gyy_pes, com_gyy_pes, pp_gyy_pes), "pessimistic")

# ---- 4b. Preliminary-roll calibration (section 3b) -------------------------
# Scaling 2027-2031 of a track by one factor is the same as compounding the
# pipeline's own growth rates from its calibrated 2027 value.
calibrate_prelim <- function(fcst) {
  real_tr <- c("res", "com", "condo")
  sc <- fcst$scenario[1]
  miss <- setdiff(c(real_tr, "pp"), unique(fcst$track))
  if (length(miss))
    stop("Preliminary-roll calibration (", sc, "): no forecast for track(s) ",
         paste(miss, collapse = ", "), " - every track must be present to ",
         "split the existing total.")
  existing <- PRELIM_TOTAL - PRELIM_NC
  pp_pipe  <- fcst[track == "pp" & tax_yr == 2027, cert_av]
  pp27     <- if (is.null(PRELIM_PP)) pp_pipe else PRELIM_PP
  pipe27   <- fcst[track %in% real_tr & tax_yr == 2027, sum(cert_av)]
  k    <- (existing - pp27) / pipe27
  k_pp <- pp27 / pp_pipe
  out <- copy(fcst)
  out[track %in% real_tr & tax_yr >= 2027, cert_av := cert_av * k]
  out[track == "pp" & tax_yr >= 2027, cert_av := cert_av * k_pp]
  tab <- merge(
    as.data.table(cert_by_type)[tax_yr == 2026, .(track, certified_2026 = cert_av)],
    merge(fcst[tax_yr == 2027, .(track, pipeline_2027 = cert_av)],
          out[tax_yr == 2027, .(track, calibrated_2027 = cert_av)], by = "track"),
    by = "track", all.y = TRUE)
  tab <- tab[order(match(track, c(real_tr, "pp")))]
  tab[, scale := calibrated_2027 / pipeline_2027]
  tab <- rbind(tab, tab[, .(track = "Existing (real + PP)",
                            certified_2026 = sum(certified_2026),
                            pipeline_2027 = sum(pipeline_2027),
                            calibrated_2027 = sum(calibrated_2027),
                            scale = NA_real_)])
  message("\n=== TY2027 by type, ", sc, " ($B): certified 2026 | pipeline 2027 | ",
          "calibrated 2027 | scale factor ===")
  message("    real-property scale factor k = ", sprintf("%.6f", k),
          if (!is.null(PRELIM_PP)) sprintf(" | PP scale (PRELIM_PP) = %.6f", k_pp) else
            " | PP = script forecast (scale 1)")
  print(tab[, .(track,
                certified_2026  = round(certified_2026 / 1e9, 3),
                pipeline_2027   = round(pipeline_2027 / 1e9, 3),
                calibrated_2027 = round(calibrated_2027 / 1e9, 3),
                scale           = round(scale, 6),
                pipeline_vs_cert   = sprintf("%+.2f%%", 100 * (pipeline_2027 / certified_2026 - 1)),
                calibrated_vs_cert = sprintf("%+.2f%%", 100 * (calibrated_2027 / certified_2026 - 1)))],
        row.names = FALSE)
  out
}
if (prelim_on) {
  fcst_bas <- calibrate_prelim(fcst_bas)
  fcst_opt <- calibrate_prelim(fcst_opt)
  fcst_pes <- calibrate_prelim(fcst_pes)
}

# ============================================================================
# 5. COMBINE HISTORY + FORECAST
# ============================================================================

# History: same across scenarios
hist_dt <- as.data.table(cert_by_type)[, scenario := "baseline"]
hist_opt <- copy(hist_dt)[, scenario := "optimistic"]
hist_pes <- copy(hist_dt)[, scenario := "pessimistic"]

# Forecast: 2027+ only
fcst_combined <- rbindlist(list(fcst_bas, fcst_opt, fcst_pes), use.names = TRUE)
fcst_combined <- fcst_combined[tax_yr > 2026]

full_series <- rbindlist(list(hist_dt, hist_opt, hist_pes, fcst_combined), use.names = TRUE)

# ============================================================================
# 5b. NEW CONSTRUCTION LAYER  (additive + compounding)
# ============================================================================
# Consumes the output of 05_new_construction_forecast.R (TRS construction
# sales tax, 6-quarter lag, 47% realization, releveled off the certified
# 2026 NC of $4,511,865,685 — i.e. the layer is ALREADY certified-basis).
#
# History needs nothing: each certified total embeds that year's NC.
# Forecast years add NC explicitly because matched-parcel chains exclude
# entrants by construction.  NC added in year k joins the stock and
# revalues at blended real-property growth thereafter:
#   NC_stock(y) = NC_stock(y-1) * (1 + g_real(y)) + NC_flow(y)
INCLUDE_NC <- FALSE  # ← 2026-07-30 decision: NC dropped from this lineup
                     #   (handled separately downstream).  Flip to TRUE to
                     #   restore the additive-compounding layer; the TRS
                     #   extraction fix + both plausibility guards remain
                     #   in place below.

# The preliminary-roll calibration always layers NC in, so every Total
# (charts, display, summary CSV) includes it and hits PRELIM_TOTAL in 2027.
nc_in_total <- INCLUDE_NC || prelim_on

if (!nc_in_total) {
  message("\nNew construction layer EXCLUDED (INCLUDE_NC = FALSE).")
} else {

nc_files <- c(
  list.files(here(), pattern = "^OERF_New_Construction_Forecast_.*\\.csv$",
             full.names = TRUE),
  list.files(here("data", "wrangled"),
             pattern = "^OERF_New_Construction_Forecast_.*\\.csv$",
             full.names = TRUE))

nc_layer_wide <- NULL
if (length(nc_files) > 0) {
  nc_path <- sort(nc_files)[length(nc_files)]  # date-suffixed: last = newest
  message("\nNew construction layer from: ", basename(nc_path))
  nc_layer_wide <- as.data.table(read_csv(nc_path, show_col_types = FALSE))[
    , .(tax_yr = as.integer(tax_year), baseline, optimistic, pessimistic)]
}
nc_layer_wide <- prelim_nc_override(nc_layer_wide, report = TRUE)  # 2027 = PRELIM_NC if set

if (is.null(nc_layer_wide)) {
  warning("No OERF_New_Construction_Forecast_*.csv found — total EXCLUDES ",
          "new construction. Run 05_new_construction_forecast.R first.",
          call. = FALSE)
} else {
  nc_flow <- melt(as.data.table(nc_layer_wide), id.vars = "tax_yr",
                  variable.name = "scenario",
                  value.name = "flow", variable.factor = FALSE)
  nc_flow <- nc_flow[tax_yr >= 2027 & tax_yr <= 2031]

  # --- Plausibility guard -----------------------------------------------
  # Annual NC flow should be in the neighborhood of the certified 2026 NC
  # ($4.51B).  A flow beyond 3x that (or negative) almost certainly means
  # a units/column error upstream in 05_new_construction_forecast.R (e.g.
  # TRS sheet column I/J misalignment) — refuse to compound garbage.
  cert_nc_2026 <- 4511865685
  bad <- nc_flow[flow > 3 * cert_nc_2026 | flow < 0]
  if (nrow(bad) > 0) {
    message("  \u274c implausible NC flows:")
    print(bad[, .(tax_yr, scenario, flow_B = round(flow / 1e9, 2))])
    stop("NC flows outside plausible range (0 to 3x certified 2026 NC of ",
         "$4.51B). Check the TRS scenario column extraction in ",
         "05_new_construction_forecast.R before layering.")
  }

  # Blended real-property growth per scenario (res+com+condo, excl. pp)
  real_g <- full_series[track %in% c("res", "com", "condo"),
                        .(real_av = sum(cert_av)), by = .(scenario, tax_yr)]
  setorder(real_g, scenario, tax_yr)
  real_g[, g := real_av / shift(real_av) - 1, by = scenario]

  nc_rows <- list()
  for (sc in unique(nc_flow$scenario)) {
    stock <- 0
    for (yr in 2027:2031) {
      g  <- real_g[scenario == sc & tax_yr == yr, g]
      fl <- nc_flow[scenario == sc & tax_yr == yr, flow]
      if (length(g)  == 0 || is.na(g))  g  <- 0
      if (length(fl) == 0 || is.na(fl)) fl <- 0
      stock <- stock * (1 + g) + fl
      nc_rows[[paste(sc, yr)]] <-
        data.table(tax_yr = yr, track = "nc", cert_av = stock, scenario = sc)
    }
  }
  nc_dt <- rbindlist(nc_rows)
  message("  NC cumulative stock by 2031 ($B): ",
          paste(nc_dt[tax_yr == 2031,
                      paste0(scenario, "=", round(cert_av / 1e9, 2))],
                collapse = ", "))
  full_series <- rbind(full_series, nc_dt, use.names = TRUE)
}

} # end INCLUDE_NC

# Pivot to wide
full_wide <- dcast(full_series, tax_yr + track ~ scenario,
                   value.var = "cert_av", fun.aggregate = sum)

# nc exists only in forecast years; drop the zero-filled history rows the
# dcast creates (history embeds NC implicitly — no line to draw there)
full_wide <- full_wide[!(track == "nc" & baseline == 0 &
                           optimistic == 0 & pessimistic == 0)]

# Totals
total_wide <- full_wide[, .(baseline = sum(baseline),
                             optimistic = sum(optimistic),
                             pessimistic = sum(pessimistic)), by = tax_yr]
total_wide[, track := "Total"]
full_wide <- rbind(full_wide, total_wide, use.names = TRUE)

full_wide[, `:=`(av_b = baseline / 1e9,
                  av_opt = optimistic / 1e9,
                  av_pes = pessimistic / 1e9)]

# ============================================================================
# 6b. SUMMARY TABLES  (+ CSV export)
# ============================================================================
# Certified-basis analog of av_fcst_summary() in main_ml.R: one row per tax
# year starting at the 2026 certificate, columns baseline / pessimistic /
# optimistic.  The TOTAL table INCLUDES cumulative new construction (stock
# recursion at blended real-property growth) so the receiver gets one
# all-in number; the second table shows the annual NC additions so the NC
# contribution stays visible.  Charts remain governed by INCLUDE_NC.
wrangled_dir <- here("data", "wrangled")
dir.create(wrangled_dir, showWarnings = FALSE, recursive = TRUE)
stamp <- format(Sys.Date(), "%Y%m%d")
fmt_b <- function(x) paste0("$", format(round(x / 1e9, 2), big.mark = ","), "B")
scen_cols <- c("baseline", "pessimistic", "optimistic")

# ---- New construction flows (read once; used for stock AND the table) ------
nc_sum_files <- c(
  list.files(here(), pattern = "^OERF_New_Construction_Forecast_.*\\.csv$",
             full.names = TRUE),
  list.files(wrangled_dir,
             pattern = "^OERF_New_Construction_Forecast_.*\\.csv$",
             full.names = TRUE))

nc_stock_tbl <- NULL
nc_summary   <- NULL
if (length(nc_sum_files) > 0)
  nc_summary <- read_csv(sort(nc_sum_files)[length(nc_sum_files)],
                         show_col_types = FALSE) %>%
    mutate(tax_yr = as.integer(tax_year)) %>%
    filter(tax_yr >= 2026) %>%
    select(tax_yr, all_of(scen_cols))
nc_summary <- prelim_nc_override(nc_summary)   # 2027 = PRELIM_NC if set

if (is.null(nc_summary)) {
  message("\u26a0\ufe0f  No OERF_New_Construction_Forecast_*.csv found — ",
          "summary total will EXCLUDE new construction. ",
          "Run 05_new_construction_forecast.R.")
} else {

  # Plausibility guard (same rule as the layer): flows near certified 2026 NC
  cert_nc_2026 <- 4511865685
  bad <- as.data.table(nc_summary)[tax_yr >= 2027] |>
    melt(id.vars = "tax_yr", variable.name = "scenario",
         value.name = "flow", variable.factor = FALSE)
  bad <- bad[flow > 3 * cert_nc_2026 | flow < 0]
  if (nrow(bad) > 0) {
    print(bad[, .(tax_yr, scenario, flow_B = round(flow / 1e9, 2))])
    stop("NC flows outside plausible range (0 to 3x certified 2026 NC). ",
         "Check 05_new_construction_forecast.R before summarising.")
  }

  # Cumulative NC stock: stock(y) = stock(y-1) * (1 + g_real(y)) + flow(y)
  nc_dt <- as.data.table(nc_summary)
  real_tracks <- full_wide[track %in% c("res", "com", "condo")]
  nc_stock_tbl <- data.table(tax_yr = 2027:2031)
  for (sc in scen_cols) {
    rg <- real_tracks[, .(v = sum(get(sc))), by = tax_yr][order(tax_yr)]
    rg[, g := v / shift(v) - 1]
    stock <- 0
    stocks <- numeric(0)
    for (yr in 2027:2031) {
      g  <- rg[tax_yr == yr, g]
      fl <- nc_dt[tax_yr == yr][[sc]]
      if (length(g)  == 0 || is.na(g))  g  <- 0
      if (length(fl) == 0 || is.na(fl)) fl <- 0
      stock <- stock * (1 + g) + fl
      stocks <- c(stocks, stock)
    }
    nc_stock_tbl[, (sc) := stocks]
  }
}

# ---- Total certified AV (incl. cumulative new construction) ----------------
total_summary <- full_wide[track == "Total" & tax_yr >= 2026,
                           .(tax_yr, baseline, pessimistic, optimistic)]
if (nc_in_total) {
  # NC is already a track inside full_wide's Total (section 5b); adding the
  # stock again would count it twice.
  message("\n=== Certified-Basis Total AV Summary ",
          "(Total incl. new construction) ===")
} else if (!is.null(nc_stock_tbl)) {
  total_summary[nc_stock_tbl, on = "tax_yr",
                `:=`(baseline    = baseline    + i.baseline,
                     pessimistic = pessimistic + i.pessimistic,
                     optimistic  = optimistic  + i.optimistic)]
  message("\n=== Certified-Basis Total AV Summary ",
          "(incl. cumulative new construction) ===")
} else {
  message("\n=== Certified-Basis Total AV Summary ",
          "(EXCLUDES new construction — NC CSV not found) ===")
}
total_summary <- tibble::as_tibble(total_summary)
print(total_summary %>%
        mutate(across(all_of(scen_cols), fmt_b)))

if (prelim_on) {
  cert26 <- certified_av$cert_total[certified_av$tax_yr == 2026]
  f27 <- full_wide[tax_yr == 2027]
  fmt_d <- function(x) format(round(x), big.mark = ",", scientific = FALSE)
  message("\n=== Implied TY2027 total from the preliminary-roll calibration ===")
  message("  PRELIM_TOTAL ", fmt_d(PRELIM_TOTAL), " = existing ",
          fmt_d(PRELIM_TOTAL - PRELIM_NC), " + NC ", fmt_d(PRELIM_NC),
          "  (existing vs certified 2026 ", fmt_d(cert26), ": ",
          sprintf("%+.4f%%", 100 * ((PRELIM_TOTAL - PRELIM_NC) / cert26 - 1)), ")")
  for (sc in scen_cols) {
    ex <- f27[track %in% c("res", "com", "condo", "pp"), sum(get(sc))]
    nc <- f27[track == "nc", sum(get(sc))]
    tt <- f27[track == "Total", get(sc)]
    message(sprintf("  %-11s existing (real + PP) %s + NC %s = Total incl. new construction %s%s",
                    sc, fmt_d(ex), fmt_d(nc), fmt_d(tt),
                    if (abs(tt - PRELIM_TOTAL) < 1) "  (= PRELIM_TOTAL)" else "  ** differs from PRELIM_TOTAL **"))
  }

  message("\n=== Calibrated path by type and total, 2026-2031 ($B) ===")
  for (sc in scen_cols) {
    pth <- dcast(full_wide[tax_yr >= 2026], tax_yr ~ track, value.var = sc)
    setcolorder(pth, c("tax_yr", intersect(c("res", "com", "condo", "pp", "nc", "Total"), names(pth))))
    for (cn in setdiff(names(pth), "tax_yr")) set(pth, j = cn, value = round(pth[[cn]] / 1e9, 3))
    setnames(pth, "Total", "Total incl. NC", skip_absent = TRUE)
    message("--- ", sc, " ---")
    print(pth, row.names = FALSE)
  }
}
total_csv <- file.path(wrangled_dir,
                       paste0("av_certified_total_summary_", stamp, ".csv"))
write_csv(total_summary, total_csv)
message("\U1f4be exported: ", total_csv)

# ---- New construction (annual additions, shown for transparency) -----------
if (!is.null(nc_summary)) {
  message("\n=== New Construction Summary (annual additions) ===")
  message("    (2026 = certified; these additions are INCLUDED, cumulatively,",
          " in the total above)")
  print(nc_summary %>%
          mutate(across(all_of(scen_cols), fmt_b)))
  nc_csv <- file.path(wrangled_dir,
                      paste0("new_construction_summary_", stamp, ".csv"))
  write_csv(nc_summary, nc_csv)
  message("\U1f4be exported: ", nc_csv)
}

# ============================================================================
# 6. DISPLAY
# ============================================================================
message("\n=== Certified-Based AV Forecast ($B) ===\n")
for (tr in intersect(c("res", "condo", "com", "pp", "nc", "Total"),
               unique(full_wide$track))) {
  cat(paste0("--- ", tr, " ---\n"))
  print(full_wide[track == tr, .(tax_yr,
    baseline = round(baseline / 1e9, 2),
    optimistic = round(optimistic / 1e9, 2),
    pessimistic = round(pessimistic / 1e9, 2))], row.names = FALSE)
  cat("\n")
}

# ============================================================================
# 7. CHARTS
# ============================================================================
track_labels <- c("res" = "Residential", "condo" = "Condo",
                   "com" = "Commercial", "pp" = "Personal Property",
                   "nc" = "New Construction (cum.)",
                   "Total" = if (nc_in_total) "Total incl. new construction"
                             else "Total (Certified)")
full_wide[, series := track_labels[track]]
full_wide[, segment := fifelse(tax_yr <= 2026, "Historical", "Forecast")]

bridge <- copy(full_wide[tax_yr == 2026])
bridge[, segment := "Forecast"]
plot_data <- rbind(full_wide, bridge, use.names = TRUE)

pal <- c("Residential" = "#2E75B6", "Commercial" = "#C00000",
         "Condo" = "#7030A0", "Personal Property" = "#548235",
         "New Construction (cum.)" = "#ED7D31",
         "Total (Certified)" = "#404040",
         "Total incl. new construction" = "#404040")

nc_caption <- if (prelim_on)
  "Calibrated to the Assessor's preliminary TY2027 roll; Total includes new construction." else
  if (nc_in_total) "Total includes new construction." else "Excludes new construction."

theme_av <- theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom", legend.title = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "gray40"),
    plot.caption = element_text(size = 8, color = "gray50", hjust = 0),
    axis.title.x = element_blank()
  )

p1 <- ggplot(plot_data, aes(x = tax_yr)) +
  geom_ribbon(data = plot_data[segment == "Forecast"],
              aes(ymin = av_pes, ymax = av_opt, fill = series), alpha = 0.15) +
  geom_line(aes(y = av_b, color = series, linetype = segment), linewidth = 1.1) +
  geom_point(data = plot_data[segment == "Historical"],
             aes(y = av_b, color = series), size = 2) +
  geom_vline(xintercept = 2026.5, linetype = "dotted", color = "gray60") +
  annotate("text", x = 2026.7, y = max(plot_data$av_opt, na.rm = TRUE) * 0.97,
           label = "Forecast", hjust = 0, size = 3, color = "gray50", fontface = "italic") +
  scale_x_continuous(breaks = 2022:2031) +
  scale_y_continuous(labels = dollar_format(suffix = "B")) +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal, guide = "none") +
  scale_linetype_manual(values = c("Historical" = "solid", "Forecast" = "dashed"), guide = "none") +
  labs(title = "Seattle Assessed Value by Property Type (Certified Basis)",
       subtitle = "Certified real property split by type shares + PP layer; ML matched-parcel growth forward",
       caption = paste0("Real property split by appraised shares; PP carried separately (econ-linked). Growth: ML parcel models, matched-parcel chains. ", nc_caption, "\nSource: KC Assessor, CoStar, S&P Global, OERF"),
       y = "Assessed Value ($B)") +
  theme_av

p2 <- ggplot(plot_data, aes(x = tax_yr)) +
  geom_ribbon(data = plot_data[segment == "Forecast"],
              aes(ymin = av_pes, ymax = av_opt, fill = series), alpha = 0.15) +
  geom_line(aes(y = av_b, color = series, linetype = segment), linewidth = 1.0) +
  geom_point(data = plot_data[segment == "Historical"],
             aes(y = av_b, color = series), size = 1.5) +
  geom_vline(xintercept = 2026.5, linetype = "dotted", color = "gray60", linewidth = 0.4) +
  facet_wrap(~series, scales = "free_y", ncol = 2) +
  scale_x_continuous(breaks = seq(2022, 2031, 2)) +
  scale_y_continuous(labels = dollar_format(suffix = "B")) +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal, guide = "none") +
  scale_linetype_manual(values = c("Historical" = "solid", "Forecast" = "dashed"), guide = "none") +
  labs(title = "Seattle AV Forecast by Property Type (Certified Basis)",
       subtitle = "Faceted with free y-scales",
       caption = paste0("Real property split by appraised shares; PP carried separately (econ-linked). Growth: ML parcel models, matched-parcel chains. ", nc_caption, "\nSource: KC Assessor, CoStar, S&P Global, OERF"),
       y = "Assessed Value ($B)") +
  theme_av +
  theme(legend.position = "none", strip.text = element_text(face = "bold"))

total_plot <- plot_data[track == "Total"]
end_row <- as.data.frame(total_plot[tax_yr == max(tax_yr) & segment == "Forecast"][1])

p3 <- ggplot(total_plot, aes(x = tax_yr)) +
  geom_ribbon(data = as.data.frame(total_plot[segment == "Forecast"]),
              aes(ymin = av_pes, ymax = av_opt), fill = "#404040", alpha = 0.15) +
  geom_line(aes(y = av_b, linetype = segment), color = "#404040", linewidth = 1.2) +
  geom_point(data = as.data.frame(total_plot[segment == "Historical"]),
             aes(y = av_b), color = "#404040", size = 2.5) +
  geom_vline(xintercept = 2026.5, linetype = "dotted", color = "gray60") +
  geom_text(data = end_row, aes(x = tax_yr, y = av_opt,
            label = paste0("Optimistic: $", round(av_opt, 0), "B")),
            hjust = -0.05, vjust = -0.5, size = 3.2, color = "gray40") +
  geom_text(data = end_row, aes(x = tax_yr, y = av_b,
            label = paste0("Baseline: $", round(av_b, 0), "B")),
            hjust = -0.05, vjust = 0.5, size = 3.2, color = "#404040", fontface = "bold") +
  geom_text(data = end_row, aes(x = tax_yr, y = av_pes,
            label = paste0("Pessimistic: $", round(av_pes, 0), "B")),
            hjust = -0.05, vjust = 1.5, size = 3.2, color = "gray40") +
  scale_x_continuous(breaks = 2022:2031, limits = c(2022, 2033)) +
  scale_y_continuous(labels = dollar_format(suffix = "B")) +
  scale_linetype_manual(values = c("Historical" = "solid", "Forecast" = "dashed"), guide = "none") +
  labs(title = "Seattle Total Assessed Value Forecast (Certified Basis)",
       subtitle = "Certified AV with ML matched-parcel growth rates (2027-2031)",
       caption = paste0("All years anchored to certified AV. ",
                        if (nc_in_total) "Total includes new construction. " else "",
                        if (prelim_on) "TY2027 calibrated to the Assessor's preliminary roll." else "",
                        "\nSource: KC Assessor, CoStar, S&P Global, OERF"),
       y = "Assessed Value ($B)") +
  theme_av

# ============================================================================
# YoY GROWTH RATE CHARTS
# ============================================================================

# Compute YoY growth rates from the certified-based levels
gyy_data <- copy(full_wide)
setorder(gyy_data, track, tax_yr)
gyy_data[, `:=`(
  gyy_bas = baseline / shift(baseline, 1) - 1,
  gyy_opt = optimistic / shift(optimistic, 1) - 1,
  gyy_pes = pessimistic / shift(pessimistic, 1) - 1
), by = track]

gyy_data[, series := track_labels[track]]
gyy_data[, segment := fifelse(tax_yr <= 2026, "Historical", "Forecast")]

# Drop first year (no lag) and filter to 2023+
gyy_plot <- gyy_data[!is.na(gyy_bas) & tax_yr >= 2023]

# Bridge at 2026 for line continuity
gyy_bridge <- copy(gyy_plot[tax_yr == 2026])
gyy_bridge[, segment := "Forecast"]
gyy_plot <- rbind(gyy_plot, gyy_bridge, use.names = TRUE)

# Chart 4: YoY growth, all types combined
p4 <- ggplot(gyy_plot, aes(x = tax_yr)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_ribbon(data = gyy_plot[segment == "Forecast"],
              aes(ymin = gyy_pes, ymax = gyy_opt, fill = series), alpha = 0.15) +
  geom_line(aes(y = gyy_bas, color = series, linetype = segment), linewidth = 1.1) +
  geom_point(data = gyy_plot[segment == "Historical"],
             aes(y = gyy_bas, color = series), size = 2) +
  geom_vline(xintercept = 2026.5, linetype = "dotted", color = "gray60") +
  scale_x_continuous(breaks = 2023:2031) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal, guide = "none") +
  scale_linetype_manual(values = c("Historical" = "solid", "Forecast" = "dashed"), guide = "none") +
  labs(title = "Year-over-Year AV Growth by Property Type (Certified Basis)",
       subtitle = "Historical (2023-2026) and Forecast (2027-2031)",
       caption = "Growth rates on certified-basis AV. Shaded = optimistic/pessimistic range.\nSource: KC Assessor, CoStar, S&P Global, OERF",
       y = "YoY Growth") +
  theme_av

# Chart 5: YoY growth, faceted
p5 <- ggplot(gyy_plot, aes(x = tax_yr)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_ribbon(data = gyy_plot[segment == "Forecast"],
              aes(ymin = gyy_pes, ymax = gyy_opt, fill = series), alpha = 0.15) +
  geom_line(aes(y = gyy_bas, color = series, linetype = segment), linewidth = 1.0) +
  geom_point(data = gyy_plot[segment == "Historical"],
             aes(y = gyy_bas, color = series), size = 1.5) +
  geom_vline(xintercept = 2026.5, linetype = "dotted", color = "gray60", linewidth = 0.4) +
  facet_wrap(~series, scales = "free_y", ncol = 2) +
  scale_x_continuous(breaks = seq(2023, 2031, 2)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal, guide = "none") +
  scale_linetype_manual(values = c("Historical" = "solid", "Forecast" = "dashed"), guide = "none") +
  labs(title = "YoY AV Growth by Property Type (Certified Basis)",
       subtitle = "Faceted with free y-scales",
       caption = "Growth rates on certified-basis AV. Shaded = optimistic/pessimistic range.\nSource: KC Assessor, CoStar, S&P Global, OERF",
       y = "YoY Growth") +
  theme_av +
  theme(legend.position = "none", strip.text = element_text(face = "bold"))

# ============================================================================
# 8. SAVE
# ============================================================================
chart_dir <- here("charts")
dir.create(chart_dir, showWarnings = FALSE)

ggsave(file.path(chart_dir, "av_certified_by_type.png"), p1, width = 10, height = 6, dpi = 200, bg = "white")
ggsave(file.path(chart_dir, "av_certified_faceted.png"), p2, width = 10, height = 7, dpi = 200, bg = "white")
ggsave(file.path(chart_dir, "av_certified_total.png"),   p3, width = 10, height = 6, dpi = 200, bg = "white")
ggsave(file.path(chart_dir, "av_certified_yoy.png"),     p4, width = 10, height = 6, dpi = 200, bg = "white")
ggsave(file.path(chart_dir, "av_certified_yoy_faceted.png"), p5, width = 10, height = 7, dpi = 200, bg = "white")

message("\nCharts saved to: ", chart_dir)

export <- full_wide[, .(tax_yr, track, series, baseline, optimistic, pessimistic)]
write_csv(as_tibble(export),
          here("data", "wrangled", paste0("av_certified_by_type_", Sys.Date(), ".csv")))
message("Data exported to data/wrangled/")

print(p1)
print(p2)
print(p3)
print(p4)
print(p5)
