# =============================================================================
# main_ml.R  —  Master orchestration script
# =============================================================================
# Tracks: res, condo, apt, office, industrial, retail, hospitality, medical
#
# The original monolithic "com" track is replaced by six commercial subgroup
# tracks that each receive their own LightGBM land + impr models.  Subgroup
# assignment follows the two-pass logic in av_detailed_groups.R:
#   Pass 1: spec_area crosswalk (Apartment, MajorOffice, Hotel, Biotech, etc.)
#   Pass 2: present_use codes for parcels without a spec_area match
#
# CoStar commercial market data is attached to all non-residential,
# non-condo tracks (apt, office, industrial, retail, hospitality, medical).
# =============================================================================

# ---- Top-level config -------------------------------------------------------
CFG <- list(
  kca_date_data_extracted = "2026-07-28",
  seed                    = 123,

  replicate             = FALSE,
  panel_replicate       = FALSE,  # TRUE = rebuild panels; FALSE = load from cache
  model_replicate       = FALSE,
  retrofit_replicate    = FALSE,
  diagnostics_replicate = FALSE,
  extend_replicate      = FALSE,
  forecast_only         = TRUE,  # TRUE = load cached extended panels and jump to Step 6

  import_raw_changes    = FALSE,  # TRUE = re-read 65M-row change CSVs

  prop_scope = "all",
  # "res" | "com" | "condo" | "both" (res+com) | "all"
  # "com" now means all six commercial subgroups
  scenario   = "baseline",

  # Forecast horizon — update these each year instead of touching sourced scripts
  forecast_start = 2027,  # first year the models predict forward
                          # (tax_yr 2026 = payable-2026 roll = 1/1/2025
                          #  assessment, observed in the extract)
  forecast_end   = 2031,  # last year of the forecast horizon

  # Actual AV growth rates scraped from KCA area revalue reports (PDF)
  # placed in here("data","kca","area_reports",<area_reports_year>).
  # Produces `area_report_actuals` in .GlobalEnv + cache for downstream use.
  use_area_actuals  = TRUE,
  area_reports_year = NULL,   # NULL = defaults to forecast_start - 1

  # ---- Backtest controls (see BACKTEST_DESIGN.md) ----------------------------
  # train_through_year: NULL = current behaviour.  An integer T filters every
  #   training frame (res, six commercial subgroups, condo) to tax_yr <= T.
  #   forecast_start must then equal T + 1.  The observed history handed to
  #   the extend/forecast steps is truncated by the harness, not here.
  # locf_backfill: TRUE = current Step 4b/4c fill, na.locf forward then
  #   fromLast.  FALSE = forward pass only, so a later year can never fill an
  #   earlier one.  Both modes report how many parcel-years the backward pass
  #   fills / would have filled.
  train_through_year = NULL,
  locf_backfill      = TRUE,

  # Which parcels the GEOGRAPHIC district reports are allowed to anchor.
  #   "nonspecialty" (default) - parcels with spec_area == 0 only.  The
  #        published Geo Area totals are computed on the non-specialty
  #        population (Area 30's 2026 total reconciles to spec_area == 0
  #        within ~1%), so applying them to specialty parcels double-counts a
  #        decline that was measured with those parcels removed.
  #   "all" - legacy behaviour; reproduces the pre-fix results.
  geo_actuals_scope = "nonspecialty",

  # What a specialty parcel does when its specialty report has not been
  # published for this cycle (e.g. Specialty 280 Major Office in Aug 2026).
  #   "model" (default) - fall through to the LightGBM / CoStar path
  #   "hold"            - hold AV flat for the anchor year
  #   "prior"           - carry that specialty's most recent published rate
  specialty_actuals_policy = "model",

  # One-time revalue shock down-weighting.
  # KCA's 2024 Assessment Year Specialty 280 revalue repriced major office
  # improvements from $33.98B to $19.39B (-42.9%) in a single roll.  That
  # lands on tax_yr 2025 here (tax_yr N = assessment year N-1), and the
  # impr_delta model reads a one-time level repricing as a recurring annual
  # rate, then extrapolates it across the whole forecast horizon.
  # Down-weighting keeps those rows in the frame (the model still sees that
  # the repricing happened) while stopping them from dominating the fit.
  #   list() or NULL = off; reproduces the un-weighted results.
  #   Otherwise: named by subgroup key, each element a named numeric vector
  #   of tax_yr = weight.  1 = unchanged, 0 = effectively excluded.
  # If 0.25 on 2025 alone is not enough, 2024 (mean dlog_imps -0.1454) is
  # the next candidate: c(`2025` = 0.25, `2024` = 0.5).
  revalue_shock_weights = list(),

  # Which target types the weights apply to.  Land moved +0.04% in that
  # revalue and the level models are not extrapolating a rate, so the
  # improvement delta is the only place the shock does damage.
  revalue_shock_targets = c("impr_delta"),
  # Monotone constraint on vacancy features in train_subgroup_lgbm().
  #   "all"           - current: -1 on both the legacy citywide costar_vr_*
  #                     block and the subgroup cs_*_vacancy series
  #   "subgroup_only" - -1 on cs_*_vacancy only; frees costar_vr_nro, which
  #                     sits above its 2026 level in every forecast year and
  #                     so can only push AV down under a -1 constraint
  #   "none"          - no monotone constraints
  vacancy_monotone = "all",

  # ---- Commercial "other" residual treatment ---------------------------------
  # Replaces the flat zoo::na.locf carry-forward for panel_tbl_com_other.
  #   "com_weighted" (default) - value-weighted mean dlog across the six
  #        modelled subgroups, weighted by prior-year forecast AV.  The
  #        residual bucket grows like commercial as a whole and inherits the
  #        scenario variation the flat path could not.
  #   "blend"  - fixed weights, e.g.
  #              list(office = .3, retail = .3, hospitality = .3, industrial = .1)
  #              NOTE: restaurants live in `hospitality`; there is no separate
  #              restaurant subgroup.
  #   "proxy"  - a single subgroup's rate (com_other_proxy)
  #   "flat"   - reproduces the pre-patch behaviour exactly (use to A/B)
  com_other_growth_method   = "com_weighted",
  com_other_blend_weights   = NULL,
  com_other_proxy           = NULL,
  com_other_rate_cap        = 0.15,   # +/- annual dlog guardrail
  com_other_use_kca_actuals = TRUE,   # KCA geo area-report actuals anchor 2027

  # ---- com_other reclassification --------------------------------------------
  # Tiered waterfall (present_use -> section-use mix -> predominant use ->
  # keyword -> zoning) that pulls parcels out of com_other before the residual
  # growth is applied.  See scripts/ml/xx_com_classify.R.
  reclassify_com_other = TRUE,
  com_class_max_tier   = 3L,   # 1..5. Set to 3 on 2026-08-02: on the real
                               # com_other bucket, tiers 1-3 move $21.7B of
                               # $23.2B; tier 4 (free text) moves $0.08B and
                               # tier 5 ~$0. Not worth the unvalidated risk.

  # ---- Additional panel data sources -----------------------------------------
  use_kca_permits        = TRUE,   # EXTR_PermitHistory_V + detail    -> kcap_*
  use_construction_sales = TRUE,   # TRS construction sales tax       -> con_sales_*
  use_home_improvement   = TRUE,   # HI applications + exemptions     -> hi_*


  # Public Health food inspection ratings as hospitality/retail predictors.
  # Downloads from data.kingcounty.gov (cached 30 days; manual CSVs can be
  # dropped in data/health/), matches establishments to parcels by situs
  # address, joins hlth_* features to the commercial panel.
  use_health_ratings = TRUE,

  # ---- NWMLS residential features (see NWMLS_GROWTH_DECISION.md) ------------
  #   "level"  (default) - the six 6/12-month trailing means of median SFH
  #            price, closed sales and active listings go into the residential
  #            delta AND level models.  Reproduces every run before 2026-10.
  #   "growth" - residential DELTA models get year-over-year log growth of
  #            those means (sea_*_lag{6,12}_yoy) plus months of supply
  #            (sea_mos_sfh_lag12) instead of the levels; level models keep
  #            the levels.  Needs models trained in "growth" mode: Steps 4 and
  #            6 stop on a mode mismatch.  Condo / commercial are unaffected.
  nwmls_features = "level",

  cache_dir  = here::here("data", "cache"),
  model_dir  = here::here("data", "model"),
  output_dir = here::here("data", "outputs")
)

# =============================================================================
# COMMERCIAL SUBGROUP DEFINITIONS
# =============================================================================
# Each subgroup is defined by its filter on the commercial panel.
# The filter uses two fields: `spec_area_name` (from the crosswalk join)
# and `present_use` (integer code from EXTR_Parcel).
#
# These mirror the assign_detail_group() logic in av_detailed_groups.R.
# =============================================================================

COM_SUBGROUPS <- list(
  apt = list(
    label = "Apartment",
    spec_area_names = c("Apartment", "Nursinghome"),
    # LookUp type 102 (EXTR_Parcel.PresentUse) - verified against EXTR_LookUp.csv
    present_use_codes = c(
      10L,   # Congregate Housing
      11L,   # Apartment
      16L,   # Apartment(Mixed Use)
      17L,   # Apartment(Co-op)
      18L,   # Apartment(Subsidized)
      49L,   # Retirement Facility
      56L,   # Residence Hall/Dorm
      57L,   # Group Home
      59L    # Nursing Home (matches the "Nursinghome" spec_area already here)
    )
  ),
  office = list(
    label = "Major Office",
    spec_area_names = c("MajorOffice"),
    present_use_codes = c(
      106L,  # Office Building
      118L,  # Office Park
      126L,  # Condominium(Office)
      273L   # Historic Prop(Office)
    )
  ),
  industrial = list(
    label = "Industrial",
    spec_area_names = c("Industrial", "Warehouse"),
    present_use_codes = c(
      138L,  # Mining/Quarry/Ore Processing
      195L,  # Warehouse
      202L,  # High Tech/High Flex
      210L,  # Industrial Park
      216L,  # Service Building
      223L,  # Industrial(Gen Purpose)
      245L,  # Industrial(Heavy)
      246L,  # Industrial(Light)
      252L,  # Mini Warehouse
      168L,  # Conv Store with Gas   } gas stations: KCA's spec_area convention
      186L,  # Service Station       } files these industrial, not retail.
             #   Validation 2026-08-02: 420 of 444 industrial->retail
             #   misclassifications were these two codes; only 2 retail-labelled
             #   parcels carry either, so the move is close to free.
      261L,  # Terminal(Rail)
      262L,  # Terminal(Marine/Comm Fish)
      263L,  # Terminal(Grain)
      264L,  # Terminal(Auto/Bus/Other)
      271L,  # Terminal(Marine)
      276L,  # Historic Prop(Loft/Warehse)
      344L,  # Terminal (Freight Auto/Rail/Other)
      345L,  # Bus Base/Fleet Maint Fac
      346L   # Rail Freight Terminal
    )
  ),
  retail = list(
    label = "Retail",
    spec_area_names = c("MajorRetail"),
    present_use_codes = c(
      60L, 61L, 62L, 63L, 64L,  # Shopping Ctr: Nghbrhood/Community/Regional/
                                #               Maj Retail/Specialty
      96L,   # Retail(Line/Strip)
      101L,  # Retail Store
      104L,  # Retail(Big Box)
      105L,  # Retail(Discount)
      161L,  # Auto Showroom and Lot
      162L,  # Bank
      163L,  # Car Wash
      167L,  # Conv Store without Gas
      191L,  # Grocery Store
      194L,  # Mini Lube
      274L   # Historic Prop(Retail)
    )
  ),
  hospitality = list(
    label = "Hospitality",
    spec_area_names = c("Hotel", "Restaurants"),
    present_use_codes = c(
      51L,   # Hotel/Motel
      58L,   # Resort/Lodge/Retreat
      171L,  # Restaurant(Fast Food)
      183L,  # Restaurant/Lounge
      188L,  # Tavern/Lounge
      275L,  # Historic Prop(Eat/Drink)
      278L,  # Historic Prop(Transient Fac)
      340L,  # Bed & Breakfast
      341L   # Rooming House
    )
  ),
  medical = list(
    label = "Medical",
    spec_area_names = c("Biotech"),
    present_use_codes = c(
      55L,   # Rehabilitation Center
      122L,  # Medical/Dental Office
      173L   # Hospital
    )
  )
)

# Present-use codes that are NEVER eligible for a built-space subgroup.
# 300 Vacant(Single-family) and 309 Vacant(Commercial) were previously routed
# into `office` by the pre-2026-08-02 code list; parking parcels are held out
# because no subgroup models them.  See CLASSIFICATION_FINDINGS.md.
COM_PU_EXCLUDE <- c(
  299L, 300L, 301L, 309L, 316L,                    # vacant / historic vacant
  323L, 324L, 325L, 326L, 327L, 328L,              # forest / open space
  330L, 331L, 332L, 333L, 334L, 335L, 336L, 337L,  # easement / ROW / water
  339L,                                            # shell structure
  159L, 180L, 182L, 277L                           # parking
)

COM_SUBGROUP_KEYS <- names(COM_SUBGROUPS)

# NWMLS feature helpers: nwmls_model_cols(), mode guards, cache rebuild.
source(here::here("scripts", "ml", "nwmls_features.R"))

# =============================================================================
# retro_fill_av()
# =============================================================================
# The Step 4b/4c "retrofit" for the commercial subgroups, com_other and condo:
# (1) if log_appr_land_val is absent or all-NA, re-join observed AV from
#     av_history_cln (dash-format aware) and recompute the log / total columns;
# (2) fill the three log columns per parcel with na.locf forward and, when
#     backfill = TRUE, na.locf fromLast as well.
#
# Hoisted out of run_main_ml() so the backtest harness can build an origin's
# retro panels with the same code before Step 3 runs.  Behaviour with
# backfill = TRUE is the inline code it replaced; the one unification is that
# the re-join also fires when the column is absent (previously only the
# com_other copy did that), which cannot occur for panels the panel builder
# produced.
#
# Returns dt (modified by reference) with attribute "locf_counts": one row per
# filled column with n_rows, n_forward_filled, n_backfilled (rows still NA
# after the forward pass that the backward pass fills / would fill).
#
#   dt         data.table with parcel_id, tax_yr, appr_* / log_appr_* columns
#   cache_dir  where av_history_cln.rds lives (used only if av_hist is NULL
#              and av_history_cln is not in .GlobalEnv)
#   backfill   TRUE = forward + fromLast; FALSE = forward only
#   label      name used in messages
#   av_hist    optional av_history_cln object to avoid a disk read
# =============================================================================
retro_fill_av <- function(dt, cache_dir, backfill = TRUE, label = "",
                          av_hist = NULL) {
  data.table::setDT(dt)
  data.table::setkeyv(dt, c("parcel_id", "tax_yr"))

  if (!"log_appr_land_val" %in% names(dt) ||
      all(is.na(dt$log_appr_land_val))) {
    message("  log_appr_land_val all-NA in ", label,
            " retro panel — re-joining AV ...")
    if (is.null(av_hist)) {
      av_cache_path <- file.path(cache_dir, "av_history_cln.rds")
      if (exists("av_history_cln", envir = .GlobalEnv))
        av_hist <- get("av_history_cln", envir = .GlobalEnv)
      else if (file.exists(av_cache_path))
        av_hist <- readRDS(av_cache_path)
    }
    if (!is.null(av_hist)) {
      av_fix <- data.table::as.data.table(av_hist)
      # av_history_cln is no-dash; the subgroup / condo panels are
      # dash-format.  Strip then restore so the join key matches.
      av_fix[, parcel_id := gsub("-", "", parcel_id)]
      if (any(grepl("-", utils::head(dt$parcel_id, 10))))
        av_fix[, parcel_id := paste0(substr(parcel_id, 1, 6), "-",
                                     substr(parcel_id, 7, 10))]
      av_fix <- av_fix[parcel_id %in% unique(dt$parcel_id),
                       .(parcel_id, tax_yr, appr_land_val, appr_imps_val)]
      dt[av_fix, on = .(parcel_id, tax_yr),
         `:=`(appr_land_val = i.appr_land_val,
              appr_imps_val = i.appr_imps_val)]
      for (cc in c("log_appr_land_val", "log_appr_imps_val",
                   "log_total_assessed"))
        if (!cc %in% names(dt)) dt[, (cc) := NA_real_]
      dt[appr_land_val > 0, log_appr_land_val := log(appr_land_val)]
      dt[appr_imps_val > 0, log_appr_imps_val := log(appr_imps_val)]
      dt[, total_assessed :=
        data.table::fifelse(is.na(appr_land_val), 0, appr_land_val) +
        data.table::fifelse(is.na(appr_imps_val), 0, appr_imps_val)]
      dt[total_assessed > 0, log_total_assessed := log(total_assessed)]
      rm(av_fix)
      message("    AV re-join complete for ", label, ": land non-NA = ",
              scales::comma(sum(!is.na(dt$appr_land_val))),
              " | impr non-NA = ",
              scales::comma(sum(!is.na(dt$appr_imps_val))))
    } else {
      warning("av_history_cln unavailable — cannot repair all-NA AV for ",
              label, call. = FALSE)
    }
  }

  counts <- list()
  for (col in c("log_appr_land_val", "log_appr_imps_val",
                "log_total_assessed")) {
    if (!col %in% names(dt)) next
    n_na0 <- sum(is.na(dt[[col]]))
    dt[, (col) := zoo::na.locf(get(col), na.rm = FALSE), by = parcel_id]
    n_na1 <- sum(is.na(dt[[col]]))
    # Rows the backward pass fills / would fill: NA after the forward pass,
    # non-NA after a fromLast pass.
    back <- dt[, zoo::na.locf(get(col), fromLast = TRUE, na.rm = FALSE),
               by = parcel_id][[2L]]
    n_back <- sum(is.na(dt[[col]]) & !is.na(back))
    if (isTRUE(backfill))
      dt[, (col) := zoo::na.locf(get(col), fromLast = TRUE, na.rm = FALSE),
         by = parcel_id]
    rm(back)
    counts[[col]] <- data.table::data.table(
      column = col, n_rows = nrow(dt),
      n_forward_filled = n_na0 - n_na1,
      n_backfilled     = n_back,
      backfill_applied = isTRUE(backfill))
  }
  counts <- data.table::rbindlist(counts)
  if (nrow(counts)) {
    message("    locf fill (", label, ", backfill = ", isTRUE(backfill), "): ",
            paste0(counts$column, " fwd=", scales::comma(counts$n_forward_filled),
                   " back", if (isTRUE(backfill)) "=" else "(would be)=",
                   scales::comma(counts$n_backfilled),
                   collapse = " | "))
  }
  data.table::setattr(dt, "locf_counts", counts)
  dt
}

# =============================================================================
# run_main_ml()
# =============================================================================
run_main_ml <- function(replicate             = CFG$replicate,
                        panel_replicate       = CFG$panel_replicate,
                        model_replicate       = CFG$model_replicate,
                        retrofit_replicate    = CFG$retrofit_replicate,
                        diagnostics_replicate = CFG$diagnostics_replicate,
                        extend_replicate      = CFG$extend_replicate,
                        forecast_only         = CFG$forecast_only,
                        import_raw_changes    = CFG$import_raw_changes,
                        kca_date_data_extracted = CFG$kca_date_data_extracted,
                        scenario              = CFG$scenario,
                        prop_scope            = CFG$prop_scope,
                        seed                  = CFG$seed,
                        forecast_start        = CFG$forecast_start,
                        forecast_end          = CFG$forecast_end,
                        use_area_actuals      = CFG$use_area_actuals,
                        area_reports_year     = CFG$area_reports_year,
                        # ---- backtest controls (BACKTEST_DESIGN.md) ----
                        train_through_year    = CFG$train_through_year,
                        locf_backfill         = CFG$locf_backfill,
                        # stop_after = "extend": return after Step 5b, before
                        # Step 6.  NULL = run everything.
                        stop_after            = NULL,
                        # require_area_actuals = TRUE: with use_area_actuals,
                        # stop() instead of warn if Step 0 imported nothing
                        # or the wrong assessment year.
                        require_area_actuals  = FALSE,
                        geo_actuals_scope     = CFG$geo_actuals_scope,
                        specialty_actuals_policy = CFG$specialty_actuals_policy,
                        revalue_shock_weights = CFG$revalue_shock_weights,
                        revalue_shock_targets = CFG$revalue_shock_targets,
                        vacancy_monotone      = CFG$vacancy_monotone,
                        com_other_growth_method   = CFG$com_other_growth_method,
                        com_other_blend_weights   = CFG$com_other_blend_weights,
                        com_other_proxy           = CFG$com_other_proxy,
                        com_other_rate_cap        = CFG$com_other_rate_cap,
                        com_other_use_kca_actuals = CFG$com_other_use_kca_actuals,
                        reclassify_com_other      = CFG$reclassify_com_other,
                        com_class_max_tier        = CFG$com_class_max_tier,
                        use_kca_permits           = CFG$use_kca_permits,
                        use_construction_sales    = CFG$use_construction_sales,
                        use_home_improvement      = CFG$use_home_improvement,
                        use_health_ratings    = CFG$use_health_ratings,
                        nwmls_features        = CFG$nwmls_features,
                        cache_dir             = CFG$cache_dir,
                        model_dir             = CFG$model_dir,
                        output_dir            = CFG$output_dir) {
  # ---- Validate ---------------------------------------------------------------
  valid_scenarios <- c("baseline", "optimistic", "pessimistic")
  if (!scenario %in% valid_scenarios)
    stop("scenario must be one of: ",
         paste(valid_scenarios, collapse = ", "))

  valid_scopes <- c("res", "com", "condo", "both", "all")
  if (!prop_scope %in% valid_scopes)
    stop("prop_scope must be one of: ",
         paste(valid_scopes, collapse = ", "))

  # forecast_start == forecast_end is a one-year horizon (e.g. backtest
  # origin 2025 forecasting 2026 only).
  if (!is.numeric(forecast_start) || !is.numeric(forecast_end) ||
      length(forecast_start) != 1 || length(forecast_end) != 1 ||
      is.na(forecast_start) || is.na(forecast_end) ||
      forecast_start > forecast_end)
    stop("forecast_start must be a year <= forecast_end")
  forecast_start <- as.integer(forecast_start)
  forecast_end   <- as.integer(forecast_end)

  if (!is.logical(use_area_actuals) || length(use_area_actuals) != 1)
    stop("use_area_actuals must be TRUE or FALSE")
  # Default reports year = forecast_start - 1: the 1/1/A revalue reports
  # post to the A+1 tax roll, and A+1 is the first forecast year.
  if (is.null(area_reports_year)) area_reports_year <- forecast_start - 1L
  area_reports_year <- as.integer(area_reports_year)

  # ---- Backtest controls ----------------------------------------------------
  if (!is.null(train_through_year)) {
    if (!is.numeric(train_through_year) || length(train_through_year) != 1 ||
        is.na(train_through_year))
      stop("train_through_year must be NULL or a single year")
    train_through_year <- as.integer(train_through_year)
    # The comm/condo forecast scripts seed their lag tracker from
    # tax_yr == forecast_start - 1; any other pairing seeds from a year the
    # truncated history does not contain.
    if (forecast_start != train_through_year + 1L)
      stop("train_through_year = ", train_through_year,
           " requires forecast_start = ", train_through_year + 1L,
           " (got ", forecast_start, ")")
  }
  if (!is.logical(locf_backfill) || length(locf_backfill) != 1 ||
      is.na(locf_backfill))
    stop("locf_backfill must be TRUE or FALSE")
  if (!is.null(stop_after) && !identical(stop_after, "extend"))
    stop("stop_after must be NULL or \"extend\"")
  if (!is.logical(require_area_actuals) || length(require_area_actuals) != 1)
    stop("require_area_actuals must be TRUE or FALSE")

  valid_geo_scope <- c("nonspecialty", "all")
  if (!geo_actuals_scope %in% valid_geo_scope)
    stop("geo_actuals_scope must be one of: ",
         paste(valid_geo_scope, collapse = ", "))

  valid_spec_pol <- c("model", "hold", "prior")
  if (!specialty_actuals_policy %in% valid_spec_pol)
    stop("specialty_actuals_policy must be one of: ",
         paste(valid_spec_pol, collapse = ", "))

  valid_co_method <- c("com_weighted", "blend", "proxy", "flat")
  if (!com_other_growth_method %in% valid_co_method)
    stop("com_other_growth_method must be one of: ",
         paste(valid_co_method, collapse = ", "))
  if (com_other_growth_method == "blend" &&
      (is.null(com_other_blend_weights) || !length(com_other_blend_weights)))
    stop("com_other_growth_method = 'blend' requires com_other_blend_weights, ",
         "e.g. list(office = .3, retail = .3, hospitality = .3, industrial = .1)")
  if (!is.null(com_other_blend_weights)) {
    .unknown_b <- setdiff(names(com_other_blend_weights), COM_SUBGROUP_KEYS)
    if (length(.unknown_b))
      warning("com_other_blend_weights names not in COM_SUBGROUP_KEYS and will ",
              "be ignored: ", paste(.unknown_b, collapse = ", "),
              ". Restaurants are part of 'hospitality'.", call. = FALSE)
  }
  if (com_other_growth_method == "proxy" &&
      (is.null(com_other_proxy) || !com_other_proxy %in% COM_SUBGROUP_KEYS))
    stop("com_other_growth_method = 'proxy' requires com_other_proxy to be one ",
         "of: ", paste(COM_SUBGROUP_KEYS, collapse = ", "))
  if (!is.numeric(com_other_rate_cap) || com_other_rate_cap <= 0)
    stop("com_other_rate_cap must be a positive number (annual dlog bound)")
  com_class_max_tier <- as.integer(com_class_max_tier)
  if (is.na(com_class_max_tier) || com_class_max_tier < 1L ||
      com_class_max_tier > 5L)
    stop("com_class_max_tier must be an integer 1-5")

  if (is.null(revalue_shock_weights)) revalue_shock_weights <- list()
  if (!is.list(revalue_shock_weights))
    stop("revalue_shock_weights must be a named list, e.g. ",
         "list(office = c(`2025` = 0.25))")
  if (length(revalue_shock_weights)) {
    .bad_w <- vapply(revalue_shock_weights, function(w)
      !is.numeric(w) || is.null(names(w)) || any(is.na(w)) || any(w < 0),
      logical(1))
    if (any(.bad_w))
      stop("each revalue_shock_weights element must be a named numeric ",
           "vector of tax_yr = weight with non-negative weights ",
           "(offending: ",
           paste(names(revalue_shock_weights)[.bad_w], collapse = ", "), ")")
    .unknown_w <- setdiff(names(revalue_shock_weights), COM_SUBGROUP_KEYS)
    if (length(.unknown_w))
      warning("revalue_shock_weights names not in COM_SUBGROUP_KEYS and will ",
              "never fire: ", paste(.unknown_w, collapse = ", "), call. = FALSE)
  }

  valid_targets <- c("land_delta", "land_level", "impr_delta", "impr_level")
  if (!all(revalue_shock_targets %in% valid_targets))
    stop("revalue_shock_targets must be a subset of: ",
         paste(valid_targets, collapse = ", "))

  if (!is.logical(use_health_ratings) || length(use_health_ratings) != 1)
    stop("use_health_ratings must be TRUE or FALSE")

  if (is.null(nwmls_features)) nwmls_features <- "level"
  nwmls_features <- nwmls_check_mode(nwmls_features)

  run_res   <- prop_scope %in% c("res", "both", "all")
  run_com   <- prop_scope %in% c("com", "both", "all")
  run_condo <- prop_scope %in% c("condo", "all")

  # ---- Header -----------------------------------------------------------------
  start_time <- Sys.time()
  message("==============================================")
  message("run_main_ml() started at: ",
          format(start_time, "%Y-%m-%d %H:%M:%S"))
  message(
    "replicate=", replicate,
    " | panel_replicate=", panel_replicate,
    " | model_replicate=", model_replicate,
    " | retrofit=", retrofit_replicate,
    " | diagnostics=", diagnostics_replicate,
    " | extend=", extend_replicate,
    " | forecast_only=", forecast_only
  )
  message("scenario = ", scenario, " | prop_scope = ", prop_scope)
  if (run_com)
    message("  commercial subgroups: ",
            paste(COM_SUBGROUP_KEYS, collapse = ", "))
  message("forecast = ", forecast_start, "-", forecast_end)
  message("use_area_actuals = ", use_area_actuals,
          if (use_area_actuals) paste0(" (reports year ", area_reports_year, ")") else "")
  message("use_health_ratings = ", use_health_ratings)
  message("nwmls_features = ", nwmls_features)
  if (!is.null(train_through_year) || !isTRUE(locf_backfill) ||
      !is.null(stop_after))
    message("BACKTEST MODE: train_through_year = ",
            if (is.null(train_through_year)) "NULL" else train_through_year,
            " | locf_backfill = ", locf_backfill,
            " | stop_after = ", if (is.null(stop_after)) "NULL" else stop_after)
  if (length(revalue_shock_weights))
    message("revalue_shock_weights = ",
            paste(vapply(names(revalue_shock_weights), function(k)
              paste0(k, ": ", paste0(names(revalue_shock_weights[[k]]), "=",
                                     revalue_shock_weights[[k]],
                                     collapse = ", ")),
              character(1)), collapse = " | "),
            " on ", paste(revalue_shock_targets, collapse = ", "))
  message("kca_date = ", kca_date_data_extracted, " | seed = ", seed)
  message("import_raw_changes = ", import_raw_changes)
  message("==============================================")

  # Must be defined before first use (sources 00_init.R immediately below)
  source_global <- function(path) source(path, local = .GlobalEnv)

  # ---- Globals for sourced scripts --------------------------------------------
  assign("kca_date_data_extracted", kca_date_data_extracted, envir = .GlobalEnv)
  assign("retrofit_replicate",      retrofit_replicate,      envir = .GlobalEnv)
  assign("forecast_start",          forecast_start,          envir = .GlobalEnv)
  assign("forecast_end",            forecast_end,            envir = .GlobalEnv)
  # Read by the residential and condo model scripts via get0(); NULL means
  # "no filter" there, so the assignment is made in both cases to overwrite
  # anything a previous call left behind.
  assign("train_through_year",      train_through_year,      envir = .GlobalEnv)
  assign("diagnostics_replicate",   diagnostics_replicate,   envir = .GlobalEnv)
  assign("scenario",                scenario,                envir = .GlobalEnv)
  assign("cache_dir",               cache_dir,               envir = .GlobalEnv)
  assign("model_dir",               model_dir,               envir = .GlobalEnv)
  assign("output_dir",              output_dir,              envir = .GlobalEnv)
  # Read by 03_model_*.R, 04, 05_eval and 06 via nwmls_run_mode().  Assigned
  # on every call so a previous call's mode cannot leak into this one.
  assign("nwmls_features",          nwmls_features,          envir = .GlobalEnv)
  set.seed(seed)

  source_global(here::here("scripts", "ml", "00_init.R"))

  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(model_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  # ============================================================================
  # STEP 0: Actual AV growth from KCA area revalue reports
  # ============================================================================
  # Scrapes per-area actual AV changes (prev yr -> area_reports_year) from the
  # assessor's PDF area reports and exposes `area_report_actuals` to the rest
  # of the pipeline (also cached as area_report_actuals_<year>.rds).
  assign("geo_actuals_scope",        geo_actuals_scope,        envir = .GlobalEnv)
  assign("specialty_actuals_policy", specialty_actuals_policy, envir = .GlobalEnv)
  assign("com_other_growth_method",   com_other_growth_method,   envir = .GlobalEnv)
  assign("com_other_blend_weights",   com_other_blend_weights,   envir = .GlobalEnv)
  assign("com_other_proxy",           com_other_proxy,           envir = .GlobalEnv)
  assign("com_other_rate_cap",        com_other_rate_cap,        envir = .GlobalEnv)
  assign("com_other_use_kca_actuals", com_other_use_kca_actuals, envir = .GlobalEnv)
  assign("reclassify_com_other",      reclassify_com_other,      envir = .GlobalEnv)
  assign("com_class_max_tier",        com_class_max_tier,        envir = .GlobalEnv)
  if (run_com) {
    message("  com_other_growth_method = ", com_other_growth_method,
            " | reclassify_com_other = ", reclassify_com_other,
            " (max tier ", com_class_max_tier, ")")
    message("  extra sources -> kca_permits: ", use_kca_permits,
            " | construction_sales: ", use_construction_sales,
            " | home_improvement: ", use_home_improvement)
  }
  if (exists("actuals_rate_coverage", envir = .GlobalEnv))
    rm("actuals_rate_coverage", envir = .GlobalEnv)

  if (use_area_actuals) {
    message("\n--- Step 0: Import actual AV growth from area reports ---")
    message("  geo_actuals_scope = ", geo_actuals_scope,
            " | specialty_actuals_policy = ", specialty_actuals_policy)
    assign("area_reports_year", area_reports_year, envir = .GlobalEnv)

    actuals_cache <- file.path(cache_dir,
                               paste0("area_report_actuals_", area_reports_year, ".rds"))
    # A cache written before the specialty/geographic split has no report_kind
    # column; reusing it would silently route every rate as geographic.
    .cache_ok <- FALSE
    if (!replicate && file.exists(actuals_cache)) {
      .cached <- readRDS(actuals_cache)
      if (all(c("report_kind", "spec_area") %in% names(.cached))) {
        assign("area_report_actuals", .cached, envir = .GlobalEnv)
        message("  \u2705 loaded from cache: ", basename(actuals_cache))
        .cache_ok <- TRUE
      } else {
        message("  cache predates the specialty/geographic split - re-importing")
      }
      rm(.cached)
    }
    if (!.cache_ok)
      source_global(here::here("scripts", "ml", "area_report_import.R"))

    if (exists("area_report_actuals", envir = .GlobalEnv)) {
      .ara <- get("area_report_actuals", envir = .GlobalEnv)
      n_area <- dplyr::n_distinct(stats::na.omit(.ara$area))
      n_spec <- if ("spec_area" %in% names(.ara))
        dplyr::n_distinct(stats::na.omit(.ara$spec_area)) else 0L
      message("  area_report_actuals available: ", n_area,
              " geographic areas | ", n_spec, " specialty areas")
      if (n_spec == 0L)
        message("  note: no specialty rates this cycle - specialty parcels ",
                "follow specialty_actuals_policy = '", specialty_actuals_policy,
                "' and will NOT inherit geographic rates")
    } else if (isTRUE(require_area_actuals)) {
      stop("use_area_actuals=TRUE and require_area_actuals=TRUE but no ",
           "actuals were imported for assessment year ", area_reports_year,
           " (missing/empty ", file.path("data", "kca", "area_reports",
                                          area_reports_year), "?)",
           call. = FALSE)
    } else {
      warning("use_area_actuals=TRUE but no actuals were imported ",
              "(missing/empty area_reports directory?) — continuing without.",
              call. = FALSE)
    }
    # A backtest must never silently anchor to a later vintage.  The cache
    # file name carries the year, but check the rows as well.
    if (isTRUE(require_area_actuals) &&
        exists("area_report_actuals", envir = .GlobalEnv)) {
      .ara <- get("area_report_actuals", envir = .GlobalEnv)
      .yrs <- unique(stats::na.omit(as.integer(.ara$assessment_yr)))
      if (nrow(.ara) == 0L)
        stop("require_area_actuals=TRUE: area_report_actuals is empty for ",
             area_reports_year, call. = FALSE)
      if (!identical(.yrs, area_reports_year))
        stop("require_area_actuals=TRUE: area_report_actuals carries ",
             "assessment_yr ", paste(.yrs, collapse = ", "),
             " but area_reports_year = ", area_reports_year, call. = FALSE)
      rm(.ara, .yrs)
    }
  } else if (exists("area_report_actuals", envir = .GlobalEnv)) {
    # Don't let a stale actuals object leak into a run that disabled them
    rm("area_report_actuals", envir = .GlobalEnv)
  }

  # ============================================================================
  # HELPERS
  # ============================================================================

  cache_path <- function(name)
    file.path(cache_dir, paste0(name, ".rds"))

  cache_save <- function(obj_name) {
    if (!exists(obj_name, envir = .GlobalEnv))
      return(invisible(NULL))
    obj   <- get(obj_name, envir = .GlobalEnv)
    sz_mb <- round(as.numeric(object.size(obj)) / 1024^2, 1)
    saveRDS(obj, cache_path(obj_name))
    message("  \U1f4be cached: ", obj_name, " (", sz_mb, " MB)")
  }

  cache_load <- function(obj_name) {
    p <- cache_path(obj_name)
    if (!file.exists(p))
      stop("Missing cache file: ",
           p,
           "\nRun with replicate=TRUE once to rebuild caches.")
    assign(obj_name, readRDS(p), envir = .GlobalEnv)
    message("  \u2705 loaded: ", obj_name)
  }

  # Remove named objects from GlobalEnv after caching/consumption.
  # Logs total MB freed and calls gc() once.
  drop_if_exists <- function(...) {
    nms      <- c(...)
    freed_mb <- 0
    dropped  <- character(0)
    for (nm in nms) {
      if (exists(nm, envir = .GlobalEnv)) {
        freed_mb <- freed_mb +
          as.numeric(object.size(get(nm, envir = .GlobalEnv))) / 1024^2
        rm(list = nm, envir = .GlobalEnv)
        dropped <- c(dropped, nm)
      }
    }
    if (length(dropped) > 0) {
      gc(verbose = FALSE)
      message("  \U1f9f9 freed: ",
              paste(dropped, collapse = ", "),
              "  (",
              round(freed_mb, 1),
              " MB)")
    }
    invisible(NULL)
  }

  latest_model_file <- function(prefix) {
    pat   <- paste0("^", prefix, ".*\\.rds$")
    files <- list.files(model_dir, pattern = pat, full.names = TRUE)
    if (length(files) == 0)
      return(NULL)
    files[which.max(file.info(files)$mtime)]
  }

  load_model <- function(obj_name, prefix, required = TRUE) {
    if (exists(obj_name, envir = .GlobalEnv))
      return(invisible(NULL))
    f <- latest_model_file(prefix)
    if (is.null(f)) {
      if (required)
        stop(
          "No model file found for: ",
          obj_name,
          "\nRun with model_replicate=TRUE once to train models."
        )
      message("  \u2139\ufe0f  optional model not found (skipping): ",
              obj_name)
      return(invisible(NULL))
    }
    assign(obj_name, readRDS(f), envir = .GlobalEnv)
    message("  \u2705 loaded: ", obj_name, " (", basename(f), ")")
  }

  load_train_frame <- function(obj_name, file_name) {
    if (exists(obj_name, envir = .GlobalEnv))
      return(invisible(NULL))
    p <- file.path(cache_dir, file_name)
    if (!file.exists(p)) {
      message(
        "  \u2139\ufe0f  training frame not found: ",
        file_name,
        "  (run model_replicate=TRUE to generate)"
      )
      return(invisible(NULL))
    }
    assign(obj_name, readRDS(p), envir = .GlobalEnv)
    message("  \u2705 loaded training frame: ", obj_name)
  }

  # ------------------------------------------------------------------
  # ensure_res_panel_nwmls()
  # ------------------------------------------------------------------
  # nwmls_features = "growth" only.  panel_tbl_res.rds caches built before
  # 2026-10 carry the six NWMLS levels but not the growth columns.  Rather
  # than force panel_replicate = TRUE (hours, and it rebuilds everything
  # else too), join the missing columns by tax_yr from the scenario's NWMLS
  # export.  In memory only: the cached panel file is not rewritten.  The
  # history rows (tax_yr <= forecast_start - 1) are observed months, the same
  # in every scenario file.  Returns TRUE if it had to load panel_tbl_res.
  ensure_res_panel_nwmls <- function() {
    if (nwmls_features != "growth") return(invisible(FALSE))
    loaded <- FALSE
    if (!exists("panel_tbl_res", envir = .GlobalEnv)) {
      cache_load("panel_tbl_res")
      loaded <- TRUE
    }
    pr <- get("panel_tbl_res", envir = .GlobalEnv)
    if (!length(setdiff(nwmls_required_cols(nwmls_features), names(pr))))
      return(invisible(loaded))
    message("  nwmls_features = \"growth\": residential panel lacks the growth ",
            "columns - joining them from the NWMLS export ...")
    ann <- nwmls_build_annual(
      nwmls_read_raw(scenario, here::here("data", "nwmls"))[["data"]])[["annual"]]
    # The joined growth columns must come from the same NWMLS history as the
    # levels already in the panel.  A different export vintage (revised
    # history) would make them inconsistent.
    if ("sea_pmedesfh_lag12" %in% names(pr)) {
      yv <- unique(data.table::data.table(
        tax_yr = as.numeric(pr[["tax_yr"]]), v = pr[["sea_pmedesfh_lag12"]]))
      yv <- yv[!is.na(v)]
      ref <- ann[["sea_pmedesfh_lag12"]][match(yv[["tax_yr"]], ann[["tax_yr"]])]
      dmax <- suppressWarnings(max(abs(yv[["v"]] / ref - 1), na.rm = TRUE))
      if (is.finite(dmax) && dmax > 1e-6)
        stop("nwmls_features = \"growth\": sea_pmedesfh_lag12 in the cached ",
             "panel differs from the current NWMLS export by up to ",
             signif(100 * dmax, 3), "% - the panel was built from a different ",
             "export vintage.  Rebuild the panel (panel_replicate = TRUE) or ",
             "put the matching export in data/nwmls/.", call. = FALSE)
    }
    assign("panel_tbl_res",
           nwmls_ensure_panel_cols(pr, nwmls_features, ann),
           envir = .GlobalEnv)
    invisible(loaded)
  }

  expose_cv <- function(cv_name, model_name, feat_name) {
    if (!exists(cv_name, envir = .GlobalEnv))
      return(invisible(NULL))
    cv <- get(cv_name, envir = .GlobalEnv)
    assign(model_name, cv$model, envir = .GlobalEnv)
    assign(feat_name, cv$x_cols, envir = .GlobalEnv)
  }

  # ------------------------------------------------------------------
  # repair_suffixed()
  # ------------------------------------------------------------------
  # Repeated joins in the panel/extend chain leave three copies of the econ
  # block: the unsuffixed base name plus .x / .y twins.  The models are
  # trained on the BASE names, but the extend step populates forecast-year
  # values only in the twins -- so from 2027 on the base column is all-NA and
  # LightGBM routes every econ split to its default branch.  Coalescing base
  # <- .y <- .x restores the series under the name the model reads.  .y is
  # preferred over .x because it carries the scenario-varying values.
  #
  # Called in two places: on the training frame in
  # build_subgroup_model_data(), and on the extended panel in Step 6 (the
  # latter is the one that actually fixes the forecast years).
  # ------------------------------------------------------------------
  repair_suffixed <- function(dt) {
    data.table::setDT(dt)
    base <- unique(sub("\\.(x|y)$", "", grep("\\.(x|y)$", names(dt), value = TRUE)))
    n_fixed <- 0L
    for (b in base) {
      src <- intersect(c(b, paste0(b, ".y"), paste0(b, ".x")), names(dt))
      src <- src[vapply(src, function(cn) is.numeric(dt[[cn]]), logical(1))]
      if (length(src) < 2) next
      dt[, (b) := do.call(data.table::fcoalesce,
                          lapply(src, function(cn) as.numeric(dt[[cn]])))]
      n_fixed <- n_fixed + 1L
    }
    if (n_fixed > 0)
      message("    coalesced ", n_fixed, " suffixed feature columns")
    dt
  }

  # ============================================================================
  # COMMERCIAL SUBGROUP HELPERS
  # ============================================================================

  # ------------------------------------------------------------------
  # split_com_panel_to_subgroups()
  # ------------------------------------------------------------------
  # Takes the full commercial panel (panel_tbl_com) and splits it into

  # six subgroup panels: panel_tbl_apt, panel_tbl_office, etc.
  #
  # Requires:
  #   - panel_tbl_com in GlobalEnv with columns parcel_id, present_use,
  #     and optionally spec_area / spec_area_name already joined.
  #   - commercial2_crosswalk (from crosswalk.xlsx) for spec_area → name
  #     mapping (loaded once and cached).
  # ------------------------------------------------------------------
  split_com_panel_to_subgroups <- function() {
    stopifnot(exists("panel_tbl_com", envir = .GlobalEnv))
    com <- data.table::copy(get("panel_tbl_com", envir = .GlobalEnv))
    data.table::setDT(com)

    # --- Ensure present_use and spec_area are present -------------------------
    # If the panel was built without these, join from parcel lookup.
    if (!"present_use" %in% names(com) || !"spec_area" %in% names(com)) {
      message("  Joining present_use / spec_area from EXTR_Parcel ...")
      parcel_path <- here::here("data", "kca", kca_date_data_extracted,
                                "EXTR_Parcel.csv")
      p_lkp <- data.table::fread(
        parcel_path,
        select = c("Major", "Minor", "PresentUse", "SpecArea")
      )
      p_lkp[, major     := stringr::str_pad(trimws(as.character(Major)), 6, "left", "0")]
      p_lkp[, minor     := stringr::str_pad(trimws(as.character(Minor)), 4, "left", "0")]
      p_lkp[, present_use := as.integer(PresentUse)]
      p_lkp[, spec_area   := as.integer(SpecArea)]

      # Match the panel's parcel_id format (dash vs no-dash) BEFORE dropping
      # major/minor — the previous ordering subsetted them away and then
      # referenced them, erroring on dash-format panels.
      use_dash <- any(grepl("-", head(com$parcel_id, 10)))
      p_lkp[, parcel_id := if (use_dash) paste0(major, "-", minor)
                           else          paste0(major, minor)]

      p_lkp <- unique(p_lkp[, .(parcel_id, present_use, spec_area)],
                       by = "parcel_id")

      # Drop existing columns if they are all NA (partial join artifacts)
      for (col in c("present_use", "spec_area")) {
        if (col %in% names(com) && all(is.na(com[[col]])))
          com[, (col) := NULL]
      }

      com <- merge(com, p_lkp, by = "parcel_id", all.x = TRUE)
      rm(p_lkp)
    }

    # --- Join crosswalk for spec_area → name ----------------------------------
    if (!"spec_area_name" %in% names(com)) {
      xwalk_path <- here::here("data", "crosswalk.xlsx")
      if (file.exists(xwalk_path)) {
        xwalk <- data.table::as.data.table(
          readxl::read_excel(xwalk_path, sheet = "commercial2")
        )
        # Expect columns: spec_area, name  (at minimum)
        if ("name" %in% names(xwalk) && "spec_area" %in% names(xwalk)) {
          xwalk[, spec_area := as.integer(spec_area)]
          com <- merge(com, xwalk[, .(spec_area, spec_area_name = name)],
                       by = "spec_area", all.x = TRUE)
        }
      } else {
        message("  \u26a0\ufe0f  crosswalk.xlsx not found — subgroup split uses present_use only")
        com[, spec_area_name := NA_character_]
      }
    }

    # --- Assign subgroup membership -------------------------------------------
    com[, com_subgroup := NA_character_]

    for (key in COM_SUBGROUP_KEYS) {
      sg <- COM_SUBGROUPS[[key]]

      # Pass 1: spec_area crosswalk.  Unchanged, and unaffected by the
      # present_use code-system defect fixed on 2026-08-02.  Split out from
      # pass 2 so a parcel matching spec_area for subgroup A and present_use
      # for subgroup B lands in A, which is the documented intent (previously
      # the single `|` condition let COM_SUBGROUP_KEYS order decide).
      com[is.na(com_subgroup) &
          spec_area_name %chin% sg$spec_area_names,
          com_subgroup := key]

      # Pass 2: present_use (LookUp type 102), with vacant land and parking
      # explicitly held out of every built-space subgroup.
      com[is.na(com_subgroup) &
          present_use %in% sg$present_use_codes &
          !(present_use %in% COM_PU_EXCLUDE),
          com_subgroup := key]
    }

    # --- Pass 3: tiered reclassification of the residual ----------------------
    # present_use -> section-use sqft mix -> predominant use -> keyword ->
    # zoning.  Only fills parcels still unassigned; never overwrites a pass-1
    # or pass-2 match.  com_class_max_tier caps how far down the waterfall the
    # run is allowed to go (3 = deterministic sources only, no free text).
    if (isTRUE(get0("reclassify_com_other", envir = .GlobalEnv,
                    ifnotfound = FALSE))) {
      .n_before <- sum(is.na(com$com_subgroup))
      assign("panel_tbl_com_other_ids",
             unique(com[is.na(com_subgroup), as.character(parcel_id)]),
             envir = .GlobalEnv)
      .ok <- tryCatch({
        source_global(here::here("scripts", "ml", "xx_com_classify.R"))
        TRUE
      }, error = function(e) {
        message("  \u26a0\ufe0f  xx_com_classify.R failed (", conditionMessage(e),
                ") - com_other left as-is")
        FALSE
      })
      if (isTRUE(.ok) && exists("com_class_map", envir = .GlobalEnv)) {
        ccm <- data.table::as.data.table(
          get("com_class_map", envir = .GlobalEnv))
        max_tier <- get0("com_class_max_tier", envir = .GlobalEnv,
                         ifnotfound = 4L)
        ccm[, .tier_n := suppressWarnings(
          as.integer(substr(class_tier, 1L, 1L)))]
        ccm <- ccm[!is.na(com_subgroup_assigned) & !is.na(.tier_n) &
                     .tier_n <= max_tier &
                     com_subgroup_assigned %chin% COM_SUBGROUP_KEYS,
                   .(parcel_id = as.character(parcel_id),
                     sg_new    = com_subgroup_assigned)]
        ccm <- unique(ccm, by = "parcel_id")
        if (nrow(ccm)) {
          com[ccm, on = "parcel_id",
              com_subgroup := data.table::fifelse(is.na(com_subgroup),
                                                  i.sg_new, com_subgroup)]
          message("  Pass 3 reclassified ",
                  scales::comma(.n_before - sum(is.na(com$com_subgroup))),
                  " parcels out of com_other (max tier ", max_tier, ")")
        } else {
          message("  Pass 3: no parcels met the tier cutoff (", max_tier, ")")
        }
      }
      drop_if_exists("panel_tbl_com_other_ids")
    }

    # Parcels that matched no subgroup go to "Other" — they stay in the
    # monolithic com panel and use the legacy aggregate forecast approach.
    n_other <- sum(is.na(com$com_subgroup))
    message("  Subgroup split: ",
            paste(sapply(COM_SUBGROUP_KEYS, function(k)
              paste0(k, "=", sum(com$com_subgroup == k, na.rm = TRUE))),
              collapse = ", "),
            ", other=", n_other)

    # --- Create per-subgroup panels -------------------------------------------
    for (key in COM_SUBGROUP_KEYS) {
      obj_name <- paste0("panel_tbl_", key)
      sub_dt   <- com[com_subgroup == key]
      sub_dt[, com_subgroup := NULL]
      assign(obj_name, sub_dt, envir = .GlobalEnv)
      message("  \u2705 created: ", obj_name,
              " (", scales::comma(data.table::uniqueN(sub_dt$parcel_id)),
              " parcels, ",
              scales::comma(nrow(sub_dt)), " rows)")
    }

    # --- Keep "Other" parcels in panel_tbl_com_other (legacy aggregate path) --
    other_dt <- com[is.na(com_subgroup)]
    other_dt[, com_subgroup := NULL]
    assign("panel_tbl_com_other", other_dt, envir = .GlobalEnv)
    message("  \u2705 created: panel_tbl_com_other (",
            scales::comma(data.table::uniqueN(other_dt$parcel_id)),
            " parcels)")

    rm(com, other_dt)
    gc(verbose = FALSE)
    invisible(NULL)
  }

  # ------------------------------------------------------------------
  # build_subgroup_model_data()
  # ------------------------------------------------------------------
  # Prepares training data for a single commercial subgroup.
  # Returns a list with land_delta, land_level, impr_delta, impr_level
  # data.table frames filtered to the subgroup's parcels.
  # ------------------------------------------------------------------
  build_subgroup_model_data <- function(key, panel_obj_name = NULL) {
    if (is.null(panel_obj_name))
      panel_obj_name <- paste0("panel_tbl_retro_", key)
    if (!exists(panel_obj_name, envir = .GlobalEnv)) {
      message("  \u26a0\ufe0f  ", panel_obj_name, " not found — skipping model build for ", key)
      return(invisible(NULL))
    }
    dt <- data.table::copy(get(panel_obj_name, envir = .GlobalEnv))
    data.table::setDT(dt)

    # --- Backtest: truncate BEFORE anything else -------------------------
    # Sits ahead of the defensive AV re-join (so it can only populate <= T
    # rows), the dlog / lag shift, and the predictor scans (near-zero
    # variance, usable_cols, econ-level twins), so all of those see the
    # truncated frame.  NULL = no filter = current behaviour.
    if (!is.null(train_through_year)) {
      .n0 <- nrow(dt)
      dt <- dt[tax_yr <= train_through_year]
      message("    train_through_year = ", train_through_year, ": ",
              scales::comma(nrow(dt)), " of ", scales::comma(.n0),
              " rows kept for ", key)
      rm(.n0)
    }

    # --- Target columns -------------------------------------------------------
    # Ensure log AV columns exist
    if (!"log_appr_land_val" %in% names(dt) && "appr_land_val" %in% names(dt))
      dt[appr_land_val > 0, log_appr_land_val := log(appr_land_val)]
    if (!"log_appr_imps_val" %in% names(dt) && "appr_imps_val" %in% names(dt))
      dt[appr_imps_val > 0, log_appr_imps_val := log(appr_imps_val)]

    # --- Defensive AV re-join --------------------------------------------
    # Cached retro panels written before the dash-format fix can carry
    # all-NA AV.  Without this re-join, every dlog target is NA, all four
    # model frames are silently skipped, and stale cached models get used.
    # (Step 4's retrofit re-join can't help here: Step 3 runs before it.)
    land_all_na <- !"log_appr_land_val" %in% names(dt) ||
      all(is.na(dt$log_appr_land_val))
    impr_all_na <- !"log_appr_imps_val" %in% names(dt) ||
      all(is.na(dt$log_appr_imps_val))
    if (land_all_na || impr_all_na) {
      message("    \u26a0\ufe0f  AV all-NA in ", panel_obj_name,
              " — re-joining from av_history_cln ...")
      av_cache_path <- file.path(cache_dir, "av_history_cln.rds")
      if (!exists("av_history_cln", envir = .GlobalEnv) &&
          file.exists(av_cache_path))
        assign("av_history_cln", readRDS(av_cache_path), envir = .GlobalEnv)
      if (exists("av_history_cln", envir = .GlobalEnv)) {
        av_fix <- data.table::as.data.table(
          get("av_history_cln", envir = .GlobalEnv))
        av_fix[, parcel_id := gsub("-", "", parcel_id)]
        if (any(grepl("-", head(dt$parcel_id, 10))))
          av_fix[, parcel_id := paste0(substr(parcel_id, 1, 6), "-",
                                       substr(parcel_id, 7, 10))]
        av_fix <- av_fix[parcel_id %in% unique(dt$parcel_id),
                         .(parcel_id, tax_yr, appr_land_val, appr_imps_val)]
        dt[av_fix, on = .(parcel_id, tax_yr),
           `:=`(appr_land_val = i.appr_land_val,
                appr_imps_val = i.appr_imps_val)]
        dt[, log_appr_land_val := NA_real_]
        dt[, log_appr_imps_val := NA_real_]
        dt[appr_land_val > 0, log_appr_land_val := log(appr_land_val)]
        dt[appr_imps_val > 0, log_appr_imps_val := log(appr_imps_val)]
        rm(av_fix)
        message("    AV re-join complete: land non-NA = ",
                scales::comma(sum(!is.na(dt$log_appr_land_val))),
                " | impr non-NA = ",
                scales::comma(sum(!is.na(dt$log_appr_imps_val))))
      } else {
        warning("av_history_cln unavailable — cannot repair all-NA AV for ",
                key, "; model frames will be skipped.", call. = FALSE)
      }
    }

    # --- Join per-subgroup CoStar market features ------------------------
    dt <- join_costar_subgroup_features(dt, key)

    # Repair the .x/.y join twins so the training frame carries the same
    # unsuffixed econ column names the forecast panel will supply.
    dt <- repair_suffixed(dt)

    # Delta targets (YoY change in log AV)
    data.table::setkeyv(dt, c("parcel_id", "tax_yr"))
    dt[, dlog_land := log_appr_land_val - shift(log_appr_land_val, 1L),
       by = parcel_id]
    dt[, dlog_imps := log_appr_imps_val - shift(log_appr_imps_val, 1L),
       by = parcel_id]
    # in build_subgroup_model_data(), right after the dlog targets:
    dt[, log_appr_imps_val_lag1 := shift(log_appr_imps_val, 1L), by = parcel_id]
    dt[, log_appr_land_val_lag1 := shift(log_appr_land_val, 1L), by = parcel_id]

    # --- Identify available predictors ----------------------------------------
    # Exclude ID, target, and admin columns
    exclude_cols <- c(
      "parcel_id", "tax_yr", "parcel_id_dash",
      "appr_land_val", "appr_imps_val", "total_assessed",
      "log_appr_land_val", "log_appr_imps_val", "log_total_assessed",
      "appr_land_val_filled", "appr_imps_val_filled",
      "pred_appr_land_val", "pred_appr_imps_val", "pred_total_assessed",
      "dlog_land", "dlog_imps",
      "com_subgroup", "spec_area_name", "detail_group", "name",
      "prop_type", "PropType", "Major", "Minor",
      "major", "minor", "present_use", "spec_area",
      "econ_seattle_msa_cpi_u_1982_1984_100_yoy_lag1"
    )

    all_cols <- names(dt)
    candidate_cols <- setdiff(all_cols, exclude_cols)
    candidate_cols <- candidate_cols[!grepl("cpi", candidate_cols, ignore.case = TRUE)]

    # Keep only numeric and integer columns (LightGBM requirement)
    # plus factor/character that could be one-hot or label-encoded
    numeric_cols <- candidate_cols[
      sapply(candidate_cols, function(col) {
        is.numeric(dt[[col]]) || is.integer(dt[[col]])
      })
    ]

    # Remove columns that are constant or near-constant (>99% one value)
    usable_cols <- numeric_cols[
      sapply(numeric_cols, function(col) {
        vals <- dt[[col]][!is.na(dt[[col]])]
        if (length(vals) < 50) return(FALSE)
        length(unique(vals)) > 1
      })
    ]
    FCST_DEAD_CS <- c(
      "availability_rate", "avail_rate_direct", "avail_rate_sublet",
      "available_sf", "leasing_sf", "base_rent", "base_rent_direct",
      "gross_rent", "gross_rent_direct", "cap_rate", "median_cap_rate",
      "median_price_sf", "under_constr_sf", "constr_starts_sf",
      # cs_off_cap_value_index is NaN across every forecast year: it is
      # derived from cap_rate, which itself has no CoStar forecast coverage
      # (see the entry above).  It cannot be repaired downstream — a value
      # would have to be invented — so it is dropped rather than left to
      # contribute gain to the historical fit only.
      "cap_value_index"
    )
    dead_pat <- paste0("^cs_[a-z]+_(", paste(FCST_DEAD_CS, collapse = "|"), ")$")
    n_drop <- sum(grepl(dead_pat, usable_cols))
    usable_cols <- usable_cols[!grepl(dead_pat, usable_cols)]
    message("    dropped ", n_drop, " features with no CoStar forecast coverage")
    # --- Drop non-stationary econ LEVELS where a growth twin exists ------
    # econ_*_lvl_lag1 series are levels that rise almost every year, so a
    # tree reads them as a time index rather than a market signal. The
    # 2026-09 audit found econ_population_thous_lvl_lag1 fully broken in
    # all six commercial models: perfectly monotone, forecast range
    # 3,310-3,393 entirely above the training max of 3,294, and the highest
    # split at 3,278 - so every forecast year lands in one leaf and the
    # response is a constant. Case-Shiller index level had no splits in the
    # forecast region in five of six.
    #
    # Same defect as cs_apt_demand_units, but the fix is simpler: the
    # matching _yoy_lag1 growth series is ALREADY a predictor, so the
    # stationary information is retained and only the trend proxy goes.
    # A level is dropped ONLY when its growth twin is present.
    if (isTRUE(get0("drop_econ_levels", envir = .GlobalEnv,
                    ifnotfound = TRUE))) {
      lvl_cand <- grep("^econ_.*_lvl_lag[0-9]+$", usable_cols, value = TRUE)
      if (length(lvl_cand)) {
        twin <- sub("_lvl_lag", "_yoy_lag", lvl_cand)
        has_twin <- twin %in% usable_cols
        to_drop <- lvl_cand[has_twin]
        kept    <- lvl_cand[!has_twin]
        if (length(to_drop)) {
          usable_cols <- setdiff(usable_cols, to_drop)
          message("    dropped ", length(to_drop),
                  " non-stationary econ level(s) with a growth twin:")
          for (z in to_drop) message("       - ", z)
        }
        if (length(kept))
          message("    kept ", length(kept),
                  " econ level(s) with NO growth twin: ",
                  paste(kept, collapse = ", "))
      }
    }

    # --- Forecast-coverage guard ----------------------------------------
    # FCST_DEAD_CS is hand-maintained and has already been wrong once
    # (cap_value_index was NaN across the horizon and missing from it).
    # Check the matching extend panel directly: any cs_* column that is
    # entirely NA/NaN across the forecast years can only shape the
    # historical fit, never a forecast-year prediction.  Advisory by
    # default — set auto_drop_dead_cs = TRUE below to act on it.
    auto_drop_dead_cs <- FALSE
    ext_probe_nm <- paste0("panel_tbl_", forecast_start, "_", forecast_end,
                           "_inputs_", scenario, "_", key)
    ext_probe_path <- file.path(cache_dir, paste0(ext_probe_nm, ".rds"))
    ext_probe <- NULL
    if (exists(ext_probe_nm, envir = .GlobalEnv)) {
      ext_probe <- get(ext_probe_nm, envir = .GlobalEnv)
    } else if (file.exists(ext_probe_path)) {
      ext_probe <- tryCatch(readRDS(ext_probe_path), error = function(e) NULL)
    }
    if (!is.null(ext_probe) && "tax_yr" %in% names(ext_probe)) {
      ext_probe <- data.table::as.data.table(ext_probe)
      fy <- ext_probe[tax_yr >= forecast_start & tax_yr <= forecast_end]
      cs_cand <- usable_cols[grepl("^cs_|^costar_", usable_cols)]
      cs_cand <- intersect(cs_cand, names(fy))
      if (nrow(fy) > 0 && length(cs_cand)) {
        no_cov <- cs_cand[vapply(cs_cand, function(col)
          !any(is.finite(fy[[col]])), logical(1))]
        if (length(no_cov)) {
          message("    \u26a0\ufe0f  no forecast coverage in ", ext_probe_nm,
                  " for ", length(no_cov), " feature(s): ",
                  paste(no_cov, collapse = ", "))
          if (isTRUE(auto_drop_dead_cs)) {
            usable_cols <- setdiff(usable_cols, no_cov)
            message("       dropped (auto_drop_dead_cs = TRUE)")
          } else {
            message("       NOT dropped — add to FCST_DEAD_CS or set ",
                    "auto_drop_dead_cs = TRUE")
          }
        }
      }
      rm(ext_probe, fy)
    }

    message("    ", key, ": ", length(usable_cols), " numeric predictors available")

    # --- Build four model frames -----------------------------------------------
    result <- list()

    # Delta land
    # Delta land — trim |dlog| > 1 (redevelopment jumps like $1K -> $50M
    # pollute the delta target and teach the model explosive growth)
    n_wild <- sum(abs(dt$dlog_land) > 1, na.rm = TRUE) +
      sum(abs(dt$dlog_imps) > 1, na.rm = TRUE)
    if (n_wild > 0)
      message("    trimming ", n_wild,
              " training rows with |dlog| > 1 (land+impr)")
    mask_dl <- !is.na(dt$dlog_land) & is.finite(dt$dlog_land) &
      abs(dt$dlog_land) <= 1
    if (sum(mask_dl) > 100) {
      dl <- dt[mask_dl, c("parcel_id", "tax_yr", "dlog_land", usable_cols), with = FALSE]
      obj_nm <- paste0("model_data_", key, "_land_delta")
      assign(obj_nm, dl, envir = .GlobalEnv)
      saveRDS(dl, file.path(cache_dir, paste0(obj_nm, ".rds")))
      result$land_delta_cols <- usable_cols
      message("    \u2705 ", obj_nm, ": ", scales::comma(nrow(dl)), " rows")
    }

    # Level land
    mask_ll <- !is.na(dt$log_appr_land_val) & is.finite(dt$log_appr_land_val)
    if (sum(mask_ll) > 100) {
      ll <- dt[mask_ll, c("parcel_id", "tax_yr", "log_appr_land_val", usable_cols), with = FALSE]
      obj_nm <- paste0("model_data_", key, "_land_level")
      assign(obj_nm, ll, envir = .GlobalEnv)
      saveRDS(ll, file.path(cache_dir, paste0(obj_nm, ".rds")))
      result$land_level_cols <- usable_cols
      message("    \u2705 ", obj_nm, ": ", scales::comma(nrow(ll)), " rows")
    }

    # Delta impr
    # Delta impr (same trim as land)
    mask_di <- !is.na(dt$dlog_imps) & is.finite(dt$dlog_imps) &
      abs(dt$dlog_imps) <= 1
    if (sum(mask_di) > 100) {
      di <- dt[mask_di, c("parcel_id", "tax_yr", "dlog_imps", usable_cols), with = FALSE]
      obj_nm <- paste0("model_data_", key, "_impr_delta")
      assign(obj_nm, di, envir = .GlobalEnv)
      saveRDS(di, file.path(cache_dir, paste0(obj_nm, ".rds")))
      result$impr_delta_cols <- usable_cols
      message("    \u2705 ", obj_nm, ": ", scales::comma(nrow(di)), " rows")
    }

    # Level impr
    mask_li <- !is.na(dt$log_appr_imps_val) & is.finite(dt$log_appr_imps_val)
    if (sum(mask_li) > 100) {
      li <- dt[mask_li, c("parcel_id", "tax_yr", "log_appr_imps_val", usable_cols), with = FALSE]
      obj_nm <- paste0("model_data_", key, "_impr_level")
      assign(obj_nm, li, envir = .GlobalEnv)
      saveRDS(li, file.path(cache_dir, paste0(obj_nm, ".rds")))
      result$impr_level_cols <- usable_cols
      message("    \u2705 ", obj_nm, ": ", scales::comma(nrow(li)), " rows")
    }

    rm(dt)
    invisible(result)
  }

  # ------------------------------------------------------------------
  # train_subgroup_lgbm()
  # ------------------------------------------------------------------
  # Trains a LightGBM model with rolling-year CV for one subgroup/target
  # combination.  Mirrors the approach in 03_model_comm_land.R.
  # ------------------------------------------------------------------
  train_subgroup_lgbm <- function(key, target_type, target_col) {
    # target_type: "land_delta", "land_level", "impr_delta", "impr_level"
    data_obj <- paste0("model_data_", key, "_", target_type)
    if (!exists(data_obj, envir = .GlobalEnv)) {
      message("    \u26a0\ufe0f  ", data_obj, " not in memory — skipping")
      return(invisible(NULL))
    }
    train_dt <- data.table::copy(get(data_obj, envir = .GlobalEnv))

    x_cols <- setdiff(names(train_dt), c("parcel_id", "tax_yr", target_col))
    # Drop columns with >50% NA in training set
    na_pct <- sapply(x_cols, function(col) mean(is.na(train_dt[[col]])))
    x_cols <- x_cols[na_pct < 0.50]
    
    # --- Monotone constraint: vacancy ------------------------------------
    # Vacancy up must not push AV up.  Rent/absorption stay UNCONSTRAINED:
    # they trend up through the horizon and a +1 constraint ratchets into
    # runaway levels (com 2027 jumped $103B -> $152B with them constrained).
    mono <- integer(length(x_cols))
    vm <- get0("vacancy_monotone", envir = environment(), ifnotfound = "all")
    vac_pat <- switch(vm,
                      all           = "^costar_vr|^cs_.*_(vacancy|vac_rate)",
                      subgroup_only = "^cs_.*_(vacancy|vac_rate)",
                      none          = NA_character_,
                      stop("vacancy_monotone must be one of: all, subgroup_only, none"))
    if (!is.na(vac_pat)) mono[grepl(vac_pat, x_cols)] <- -1L
    message("    vacancy_monotone = ", vm, " (", sum(mono != 0L),
            " of ", length(x_cols), " features constrained)")

    if (length(x_cols) < 3) {
      message("    \u26a0\ufe0f  too few predictors for ", key, "/", target_type,
              " (", length(x_cols), ") — skipping")
      return(invisible(NULL))
    }

    message("    Training ", key, "/", target_type,
            " (", length(x_cols), " features, ",
            scales::comma(nrow(train_dt)), " rows) ...")

    # --- Rolling-year folds ---------------------------------------------------
    years <- sort(unique(train_dt$tax_yr))
    # Use the last 3 years as validation folds (or fewer if not enough history)
    n_folds <- min(3L, length(years) - 2L)
    if (n_folds < 1L) {
      message("    \u26a0\ufe0f  insufficient year coverage for CV — skipping ", key, "/", target_type)
      return(invisible(NULL))
    }
    fold_years <- tail(years, n_folds)

    folds <- lapply(fold_years, function(yr) {
      list(
        train = which(train_dt$tax_yr < yr),
        test  = which(train_dt$tax_yr == yr)
      )
    })

    # --- LightGBM training ----------------------------------------------------
    # Convert to matrix
    x_mat <- as.matrix(train_dt[, ..x_cols])
    y_vec <- train_dt[[target_col]]

    # --- One-time revalue shock down-weighting ---------------------------
    # See revalue_shock_weights in CFG.  Rows stay in the frame; they just
    # stop counting as full observations of a recurring annual rate.
    w_vec <- rep(1, nrow(train_dt))
    shock_w <- revalue_shock_weights[[key]]
    shock_applied <- NULL
    if (!is.null(shock_w) && target_type %in% revalue_shock_targets) {
      for (yr_nm in names(shock_w)) {
        idx <- which(train_dt$tax_yr == as.integer(yr_nm))
        if (length(idx)) {
          w_vec[idx] <- as.numeric(shock_w[[yr_nm]])
          shock_applied <- c(shock_applied,
                             stats::setNames(as.numeric(shock_w[[yr_nm]]), yr_nm))
          message("    revalue shock: tax_yr ", yr_nm, " weight ",
                  as.numeric(shock_w[[yr_nm]]), " on ",
                  scales::comma(length(idx)), " of ",
                  scales::comma(nrow(train_dt)), " rows (",
                  round(100 * length(idx) / nrow(train_dt), 1), "%)")
        } else {
          message("    \u26a0\ufe0f  revalue shock: no ", key, "/", target_type,
                  " rows in tax_yr ", yr_nm, " — weight not applied")
        }
      }
    }

    dtrain <- lightgbm::lgb.Dataset(data = x_mat, label = y_vec,
                                    weight = w_vec)

    params <- list(
      objective        = "regression",
      metric           = "rmse",
      learning_rate    = 0.05,
      num_leaves       = 31,
      min_data_in_leaf = 20,
      feature_fraction = 0.8,
      bagging_fraction = 0.8,
      bagging_freq     = 5,
      verbose          = -1,
      monotone_constraints = mono
    )

    # Simple train (no early stopping on folds — we do manual CV below)
    model <- lightgbm::lgb.train(
      params  = params,
      data    = dtrain,
      nrounds = 500,
      verbose = -1
    )

    # --- CV evaluation --------------------------------------------------------
    # NOTE: `model` above was trained on ALL rows, including these test
    # folds, so these are in-sample numbers.  Use them to compare runs of
    # the same shape, not to judge forecast accuracy.
    cv_metrics <- lapply(seq_along(folds), function(i) {
      f <- folds[[i]]
      if (length(f$test) == 0) return(NULL)
      preds <- predict(model, x_mat[f$test, , drop = FALSE])
      actuals <- y_vec[f$test]
      valid <- !is.na(actuals) & !is.na(preds)
      if (sum(valid) < 10) return(NULL)
      rmse <- sqrt(mean((preds[valid] - actuals[valid])^2))
      mae  <- mean(abs(preds[valid] - actuals[valid]))
      data.frame(tax_yr = fold_years[i], rmse = rmse, mae = mae, n = sum(valid))
    })
    cv_metrics <- do.call(rbind, Filter(Negate(is.null), cv_metrics))
    if (!is.null(cv_metrics) && nrow(cv_metrics) > 0) {
      message("    CV RMSE = ", round(mean(cv_metrics$rmse), 4),
              ", MAE = ", round(mean(cv_metrics$mae), 4))
      message("      by fold year: ",
              paste0(cv_metrics$tax_yr, "=", round(cv_metrics$rmse, 4),
                     collapse = ", "))
    }

    # --- Save model artifacts -------------------------------------------------
    cv_obj <- list(model = model, x_cols = x_cols, cv_metrics = cv_metrics,
                   shock_weights = shock_applied)
    cv_name    <- paste0("lgb_", key, "_", target_type, "_cv")
    model_name <- paste0("lgb_", key, "_", target_type, "_model")
    feat_name  <- paste0("lgb_", key, "_", target_type, "_features")

    assign(cv_name,    cv_obj,  envir = .GlobalEnv)
    assign(model_name, model,   envir = .GlobalEnv)
    assign(feat_name,  x_cols,  envir = .GlobalEnv)

    saveRDS(cv_obj, file.path(model_dir, paste0(cv_name, ".rds")))
    message("    \U1f4be saved: ", cv_name)

    # Also save a dummy "dv" object for compatibility with downstream scripts
    dv_name <- paste0("dv_", key, "_", target_type)
    dv_obj  <- list(x_cols = x_cols, target = target_col)
    assign(dv_name, dv_obj, envir = .GlobalEnv)
    saveRDS(dv_obj, file.path(model_dir, paste0(dv_name, ".rds")))

    rm(x_mat, y_vec, w_vec, dtrain, train_dt)
    gc(verbose = FALSE)
    invisible(cv_obj)
  }

  # ------------------------------------------------------------------
  # join_costar_subgroup_features()
  # ------------------------------------------------------------------
  # Joins annualized per-subgroup CoStar market features (cs_<type>_*) by
  # tax_yr using the CITY_WIDE aggregate.  Reads the cached
  # costar_<type>_fcst_*.rds written by xx_costar_<type>_to_panel.R,
  # sourcing that import script once (against the xlsx exports in
  # data/costar/) if the cache is absent.  The 2026-07 CoStar exports
  # include forecast quarters through 2033-34, so joined features cover
  # observed AND forecast years alike.  Idempotent: returns dt unchanged
  # when cs_* columns are already present.  medical / com_other have no
  # CoStar product type and pass through unchanged.
  join_costar_subgroup_features <- function(dt, key) {
    costar_map <- list(
      apt         = list(prefix = "cs_apt_",
                         cache_pattern = "^costar_apt_fcst_",
                         script = "xx_costar_apt_to_panel.R"),
      office      = list(prefix = "cs_off_",
                         cache_pattern = "^costar_office_fcst_",
                         script = "xx_costar_office_to_panel.R"),
      industrial  = list(prefix = "cs_ind_",
                         cache_pattern = "^costar_industrial_fcst_",
                         script = "xx_costar_industrial_to_panel.R"),
      retail      = list(prefix = "cs_ret_",
                         cache_pattern = "^costar_retail_fcst_",
                         script = "xx_costar_retail_to_panel.R"),
      hospitality = list(prefix = "cs_hosp_",
                         cache_pattern = "^costar_hospitality_fcst_",
                         script = "xx_costar_hospitality_to_panel.R")
    )
    if (!key %in% names(costar_map)) return(dt)
    cs_info <- costar_map[[key]]
    if (any(grepl(paste0("^", cs_info$prefix), names(dt)))) return(dt)

    load_cs_cache <- function() {
      cs_files <- list.files(cache_dir,
                             pattern = paste0(cs_info$cache_pattern,
                                              ".*\\.rds$"),
                             full.names = TRUE)
      if (length(cs_files) == 0) return(NULL)
      # Prefer a scenario-matched cache; otherwise most recent (the CoStar
      # exports are Base Case only, so contents are scenario-invariant).
      scen_hit <- grep(paste0("_", scenario, "\\.rds$"), cs_files,
                       value = TRUE)
      f <- if (length(scen_hit) > 0)
             scen_hit[which.max(file.info(scen_hit)$mtime)]
           else cs_files[which.max(file.info(cs_files)$mtime)]
      data.table::as.data.table(readRDS(f))
    }

    cs_wide <- load_cs_cache()
    if (is.null(cs_wide)) {
      cs_script <- here::here("scripts", "ml", cs_info$script)
      if (file.exists(cs_script)) {
        message("    CoStar cache not found for ", key, " — sourcing ",
                cs_info$script, " ...")
        ok <- try(source(cs_script, local = .GlobalEnv), silent = TRUE)
        if (inherits(ok, "try-error"))
          message("    \u26a0\ufe0f  ", cs_info$script,
                  " failed (missing xlsx in data/costar/?) — ",
                  "continuing without cs_* features for ", key)
        cs_wide <- load_cs_cache()
      }
    }
    if (is.null(cs_wide)) return(dt)

    cs_join <- cs_wide[submarket == "CITY_WIDE"]
    cs_join[, submarket := NULL]
        # ---- Detrend monotone level series --------------------------------
    # A CoStar level that only ever rises (demand_units: 275,809 in 2002 to
    # 557,239 in 2025) acts as a time index, not a market signal. The tree
    # splits it where early years end, and because the highest values in
    # training are 2024-2025 (targets -0.135, -0.198) it learns "high level
    # implies decline". Forecast values (579,354-619,897) sit ABOVE every
    # training value and above the highest split (531,289), so the whole
    # horizon lands in one leaf: a constant, learned from the worst years.
    # Rising demand can never improve the forecast.
    #
    # Replacing the level with its YoY growth makes it stationary and puts
    # the forecast back inside the training range.
    # Set costar_detrend = FALSE in the global env to keep the old behaviour.
    if (isTRUE(get0("costar_detrend", envir = .GlobalEnv, ifnotfound = TRUE))) {
      trend_pat <- get0("costar_detrend_pattern", envir = .GlobalEnv,
                        ifnotfound = "_(demand_units|inventory_units)$")
      trend_cols <- grep(trend_pat, names(cs_join), value = TRUE)
      if (length(trend_cols)) {
        data.table::setorder(cs_join, tax_yr)
        for (tc in trend_cols) {
          v <- as.numeric(cs_join[[tc]])
          # A zero in a series that is otherwise 6-figure is a bad row, not
          # a real observation (2026 demand_units is 0 against 557,239 in
          # 2025). Blank it and interpolate rather than differencing to it.
          v[!is.na(v) & v <= 0] <- NA_real_
          ok <- which(!is.na(v))
          if (length(ok) >= 2) {
            v <- stats::approx(cs_join[["tax_yr"]][ok], v[ok],
                               xout = cs_join[["tax_yr"]],
                               rule = 2)[["y"]]
          }
          g <- c(NA_real_, v[-1] / v[-length(v)] - 1)
          cs_join[, (paste0(tc, "_yoy")) := g]
          cs_join[, (tc) := NULL]
          message("    detrended ", tc, " -> ", tc, "_yoy")
        }
      }
    }
    cs_val_cols <- setdiff(names(cs_join), "tax_yr")
    dt <- merge(dt, cs_join, by = "tax_yr", all.x = TRUE)
    message("    CoStar: joined ", length(cs_val_cols), " ",
            cs_info$prefix, "* features (",
            scales::comma(sum(!is.na(dt[[cs_val_cols[1]]]))), "/",
            scales::comma(nrow(dt)), " rows matched)")
    dt
  }

  # ============================================================================
  # STEP 1  —  ETL + Transforms
  # ============================================================================
  # (skipped entirely when panel_replicate=FALSE — panels loaded from cache in Step 2)
  # (skipped entirely when forecast_only=TRUE — extended panels loaded from cache in Step 5b)
  # ============================================================================
  if (forecast_only) {
    message("\n--- Steps 1-5b: Skipped (forecast_only=TRUE — loading cached extended panels) ---")
  } else if (panel_replicate) {

    # --------------------------------------------------------------------------
    # 1a. Residential
    # --------------------------------------------------------------------------
    if (run_res) {
      message("\n--- Step 1a: ETL + transforms (residential) ---")
      if (replicate) {
        source_global(here::here("scripts", "ml", "01_import_res.R"))
        source_global(here::here("scripts", "ml", "02_transfrm.R"))
        cache_save("parcel_res_full")

        if (import_raw_changes) {
          message("  Importing raw changes history (import_raw_changes = TRUE) ...")
          source_global(here::here("scripts", "ml", "xx_tracking_changes.R"))
          cache_save("changes_long_tbl")
        } else {
          message("  Skipping raw changes import (import_raw_changes = FALSE) ...")
          cache_load("changes_long_tbl")
        }

        source_global(here::here("scripts", "ml", "xx_av_history.R"))
        cache_save("av_history_cln")

      } else {
        for (nm in c("parcel_res_full", "changes_long_tbl", "av_history_cln"))
          cache_load(nm)
      }
    }

    # --------------------------------------------------------------------------
    # 1b. Commercial (full — split into subgroups happens in Step 2)
    # --------------------------------------------------------------------------
    # parcel_comm_full is also required by xx_combine_res_comm_condo_panel.R
    # (for the small-MF cross-track dedup), so load it whenever run_condo=TRUE
    # even if run_com=FALSE.
    if (run_com || run_condo) {
      message("\n--- Step 1b: ETL + transforms (commercial) ---")
      if (replicate && run_com) {
        source_global(here::here("scripts", "ml", "01_import_comm.R"))
        source_global(here::here("scripts", "ml", "02_transfrm_comm.R"))
        cache_save("parcel_comm_full")
      } else {
        cache_load("parcel_comm_full")
      }
    }

    # --------------------------------------------------------------------------
    # 1c. Condo
    # --------------------------------------------------------------------------
    # Also ensure parcel_res_full and av_history_cln are available when
    # run_condo=TRUE but run_res=FALSE — both are required by
    # xx_combine_res_comm_condo_panel.R for the residential backbone build.
    if (run_condo && !run_res) {
      if (!exists("parcel_res_full", envir = .GlobalEnv))
        cache_load("parcel_res_full")
      if (!exists("changes_long_tbl", envir = .GlobalEnv))
        cache_load("changes_long_tbl")
      if (!exists("av_history_cln", envir = .GlobalEnv))
        cache_load("av_history_cln")
    }

    if (run_condo) {
      message("\n--- Step 1c: ETL + transforms (condo) ---")
      if (replicate) {
        source_global(here::here("scripts", "ml", "01_import_condo.R"))
        source_global(here::here("scripts", "ml", "02_transfrm_condo.R"))
        cache_save("parcel_condo_full")
        cache_save("condo_complex_cln")
      } else {
        cache_load("parcel_condo_full")
        cache_load("condo_complex_cln")
      }
    }

  } else {
    message("\n--- Step 1: Skipped (panel_replicate=FALSE — panels load from cache in Step 2) ---")
  }

  if (!forecast_only) {

  # ============================================================================
  # STEP 2  —  Panel Assembly
  # ============================================================================
  message("\n--- Step 2: Panel assembly (scenario = ", scenario, ") ---")

  if (panel_replicate) {
    # --------------------------------------------------------------------------
    # panel_replicate = TRUE: rebuild panels from scratch
    # --------------------------------------------------------------------------

    # 2a. Residential backbone — always built; chg_dt / av_dt consumed here
    source_global(here::here("scripts", "ml", "xx_combine_parcel_history_changes.R"))
    gc(verbose = FALSE)
    source_global(here::here("scripts", "ml", "xx_permits_to_panel.R"))
    # KCA permit history (EXTR_PermitHistory_V + EXTR_PermitDetailHistory_V)
    # -> kcap_* features.  Additive to the SDCI permit features above, not a
    # replacement: the two sources differ on coverage and valuation basis.
    # Nothing is merged into the SDCI columns -- `any_newconst` is left alone
    # so the two sources stay separable in the gain report.
    if (isTRUE(use_kca_permits))
      source_global(here::here("scripts", "ml", "xx_kca_permits_to_panel.R"))
    # Home improvement exemptions -> hi_*.  hi_rolloff_next_val is a scheduled,
    # forward-known AV step-up (LastBillYr is set at grant time), so it stays
    # live past the forecast boundary.
    if (isTRUE(use_home_improvement))
      source_global(here::here("scripts", "ml", "xx_home_improvement_to_panel.R"))
    source_global(here::here("scripts", "ml", "xx_nwmls_to_panel.R"))
    source_global(here::here("scripts", "ml", "xx_econ_to_panel.R"))
    # TRS construction sales tax -> con_sales_*.  Citywide, tax_yr-keyed, and
    # one of the few inputs that genuinely varies by scenario (CoStar exports
    # are Base Case only and housing permits are frozen in 05_extend_panel).
    if (isTRUE(use_construction_sales))
      source_global(here::here("scripts", "ml", "xx_construction_sales_to_panel.R"))

    # 2b. Stack property types and attach market signals
    if (run_condo) {
      source_global(here::here("scripts", "ml", "xx_combine_res_comm_condo_panel.R"))
      # → creates panel_tbl_res, panel_tbl_com, panel_tbl_condo, panel_tbl_all
      if (run_com) {
        source_global(here::here("scripts", "ml", "xx_costar_to_panel.R"))
      if (use_health_ratings) {
        source_global(here::here("scripts", "ml", "xx_health_inspections_import.R"))
        source_global(here::here("scripts", "ml", "xx_health_to_panel.R"))
      }
      }
      source_global(here::here("scripts", "ml", "xx_nwmls_condo_to_panel.R"))
      cache_save("panel_tbl_res")
      cache_save("panel_tbl_com")
      cache_save("panel_tbl_condo")
      cache_save("panel_tbl_all")
    } else if (run_com) {
      source_global(here::here("scripts", "ml", "xx_combine_res_comm_panel.R"))
      source_global(here::here("scripts", "ml", "xx_costar_to_panel.R"))
      if (use_health_ratings) {
        source_global(here::here("scripts", "ml", "xx_health_inspections_import.R"))
        source_global(here::here("scripts", "ml", "xx_health_to_panel.R"))
      }
      cache_save("panel_tbl_res")
      cache_save("panel_tbl_com")
      cache_save("panel_tbl_all")
    } else {
      if (exists("panel_tbl", envir = .GlobalEnv)) {
        assign("panel_tbl_res", get("panel_tbl", envir=.GlobalEnv), envir = .GlobalEnv)
        cache_save("panel_tbl_res")
      } else if (exists("panel_tbl_res", envir = .GlobalEnv)) {
        cache_save("panel_tbl_res")
      } else {
        stop("Panel assembly did not create panel_tbl or panel_tbl_res.")
      }
    }

    # 2c. Split commercial panel into subgroups
    if (run_com) {
      message("\n--- Step 2c: Splitting commercial panel into subgroups ---")
      split_com_panel_to_subgroups()
      for (key in COM_SUBGROUP_KEYS)
        cache_save(paste0("panel_tbl_", key))
      cache_save("panel_tbl_com_other")
    }

    # Raw parcel tables and panel-assembly intermediates fully consumed above.
    drop_if_exists(
      "parcel_res_full", "parcel_comm_full", "parcel_condo_full",
      "condo_complex_cln", "changes_long_tbl", "av_history_cln",
      "chg_dt", "av_dt", "panel_tbl", "panel_tbl_all"
    )
    # panel_tbl_com/condo and subgroups already cached to disk — drop so they
    # do not sit idle during the residential retrofit.
    drop_if_exists("panel_tbl_com", "panel_tbl_condo", "panel_tbl_com_other")
    for (key in COM_SUBGROUP_KEYS)
      drop_if_exists(paste0("panel_tbl_", key))
    gc(verbose = FALSE)

  } else {
    # --------------------------------------------------------------------------
    # panel_replicate = FALSE: load pre-built panels from cache
    # --------------------------------------------------------------------------
    message("  panel_replicate=FALSE — loading panels from cache ...")

    # Always load residential panel
    if (run_res) {
      if (!exists("panel_tbl_res", envir = .GlobalEnv)) cache_load("panel_tbl_res")
    }
    if (run_com) {
      # Load full com panel then split, or load pre-split subgroup panels
      for (key in COM_SUBGROUP_KEYS) {
        sg_cache <- cache_path(paste0("panel_tbl_", key))
        if (file.exists(sg_cache)) {
          cache_load(paste0("panel_tbl_", key))
        }
      }
      # If subgroup caches don't exist, load full com and split
      any_missing <- any(!sapply(COM_SUBGROUP_KEYS, function(k)
        exists(paste0("panel_tbl_", k), envir = .GlobalEnv)))
      if (any_missing) {
        message("  Subgroup caches missing — loading panel_tbl_com and splitting ...")
        if (!exists("panel_tbl_com", envir = .GlobalEnv)) cache_load("panel_tbl_com")
        split_com_panel_to_subgroups()
        for (key in COM_SUBGROUP_KEYS)
          cache_save(paste0("panel_tbl_", key))
        cache_save("panel_tbl_com_other")
      }
    }
    if (run_condo) {
      if (!exists("panel_tbl_condo", envir = .GlobalEnv)) cache_load("panel_tbl_condo")
    }

    # Drop any ETL intermediates that may have been loaded in Step 1
    drop_if_exists(
      "parcel_res_full", "parcel_comm_full", "parcel_condo_full",
      "condo_complex_cln", "changes_long_tbl", "av_history_cln",
      "chg_dt", "av_dt", "panel_tbl", "panel_tbl_all"
    )
    # Drop subgroup panels and com/condo — already cached; not needed until Steps 4+
    drop_if_exists("panel_tbl_com", "panel_tbl_condo", "panel_tbl_com_other")
    for (key in COM_SUBGROUP_KEYS)
      drop_if_exists(paste0("panel_tbl_", key))
    gc(verbose = FALSE)
  }

  } # end !forecast_only (Step 2)

  # ============================================================================
  # STEP 3  —  Model Training or Loading
  # (runs regardless of forecast_only — models always needed for Step 6)
  # ============================================================================
  message("\n--- Step 3: Models ---")

  # --------------------------------------------------------------------------
  # 3a. RESIDENTIAL  —  03_model_land.R  +  03_model_impr.R
  # --------------------------------------------------------------------------
  if (run_res) {
    message("\n  [Residential models]")
    if (model_replicate) {
      message("  Training residential LightGBM models ...")
      .res_panel_loaded_for_nwmls <- ensure_res_panel_nwmls()
      cl <- init_parallel()
      on.exit({
        try(stopCluster(cl), silent = TRUE)
        try(registerDoSEQ(), silent = TRUE)
      }, add = TRUE)
      source_global(here::here("scripts", "ml", "03_model_land.R"))
      source_global(here::here("scripts", "ml", "03_model_impr.R"))
      message("  \u2705 Residential models trained and saved.")

      # Training frames written to cache_dir by the model scripts; drop copies
      drop_if_exists(
        "model_data_land_delta_model",
        "model_data_land_model",
        "model_data_impr_delta_model",
        "model_data_impr_level_model"
      )
      # forecast_only skips Step 4, so a panel loaded only for the growth
      # join is not needed again.
      if (isTRUE(.res_panel_loaded_for_nwmls) && forecast_only)
        drop_if_exists("panel_tbl_res")

    } else {
      message("  Loading cached residential models ...")
      load_model("lgb_land_delta_cv", "lgb_land_delta_cv")
      load_model("dv_land_delta", "dv_land_delta")
      load_model("lgb_impr_delta_cv", "lgb_impr_delta_cv")
      load_model("dv_impr_delta", "dv_impr_delta")
      load_model("lgb_impr_level_cv", "lgb_impr_level_cv")
      load_model("dv_impr_level", "dv_impr_level")
      load_model("lgb_land_level_cv", "lgb_land_level_cv", required = FALSE)
      load_model("dv_land_level", "dv_land_level", required = FALSE)
    }

    expose_cv("lgb_land_delta_cv",
              "lgb_land_delta_model",
              "lgb_land_delta_features")
    expose_cv("lgb_impr_delta_cv",
              "lgb_impr_delta_model",
              "lgb_impr_delta_features")
    expose_cv("lgb_impr_level_cv",
              "lgb_impr_level_model",
              "lgb_impr_level_features")
    expose_cv("lgb_land_level_cv",
              "lgb_land_level_model",
              "lgb_land_level_features")

    load_train_frame("model_data_land_delta_model",
                     "model_data_land_delta_model.rds")
    load_train_frame("model_data_land_model", "model_data_land_model.rds")
    load_train_frame("model_data_impr_delta_model",
                     "model_data_impr_delta_model.rds")
    load_train_frame("model_data_impr_level_model",
                     "model_data_impr_level_model.rds")
  }

  # --------------------------------------------------------------------------
  # 3b. COMMERCIAL SUBGROUPS  —  per-subgroup LightGBM land + impr models
  #
  # Each subgroup (apt, office, industrial, retail, hospitality, medical)
  # gets four models: land_delta, land_level, impr_delta, impr_level.
  # --------------------------------------------------------------------------
  if (run_com) {
    message("\n  [Commercial subgroup models]")

    for (key in COM_SUBGROUP_KEYS) {
      sg_label <- COM_SUBGROUPS[[key]]$label
      message("\n  --- Subgroup: ", sg_label, " (", key, ") ---")

      if (model_replicate) {
        # Load the retro panel for this subgroup (built in Step 4, but
        # if we're doing model_replicate we need the panel now)
        retro_name <- paste0("panel_tbl_retro_", key)
        if (!exists(retro_name, envir = .GlobalEnv)) {
          retro_path <- file.path(cache_dir, paste0(retro_name, ".rds"))
          if (file.exists(retro_path)) {
            assign(retro_name, readRDS(retro_path), envir = .GlobalEnv)
            message("  \u2705 loaded retro panel for model build: ", retro_name)
          } else {
            # Fall back to the raw subgroup panel
            raw_name <- paste0("panel_tbl_", key)
            if (!exists(raw_name, envir = .GlobalEnv)) {
              raw_path <- file.path(cache_dir, paste0(raw_name, ".rds"))
              if (file.exists(raw_path)) {
                assign(raw_name, readRDS(raw_path), envir = .GlobalEnv)
                # Use it as retro (locf fill will have happened or will happen)
                assign(retro_name, get(raw_name, envir = .GlobalEnv), envir = .GlobalEnv)
                drop_if_exists(raw_name)
              } else {
                message("  \u26a0\ufe0f  No panel found for ", key, " — skipping model training")
                next
              }
            }
          }
        }

        # Build model data frames
        build_subgroup_model_data(key)

        # Train four models (capture results so success is reported honestly)
        trained <- list(
          train_subgroup_lgbm(key, "land_delta", "dlog_land"),
          train_subgroup_lgbm(key, "land_level", "log_appr_land_val"),
          train_subgroup_lgbm(key, "impr_delta", "dlog_imps"),
          train_subgroup_lgbm(key, "impr_level", "log_appr_imps_val")
        )
        n_trained <- sum(!vapply(trained, is.null, logical(1)))
        if (n_trained == 0) {
          warning(sg_label, ": 0/4 models trained (empty training frames — ",
                  "check AV columns in the retro panel). Downstream forecast ",
                  "will fall back to STALE cached models from a prior run.",
                  call. = FALSE)
        } else {
          message("  \u2705 ", sg_label, ": ", n_trained,
                  "/4 models trained and saved.")
        }

        # Drop training frames
        drop_if_exists(
          paste0("model_data_", key, "_land_delta"),
          paste0("model_data_", key, "_land_level"),
          paste0("model_data_", key, "_impr_delta"),
          paste0("model_data_", key, "_impr_level")
        )
        drop_if_exists(retro_name)

      } else {
        message("  Loading cached ", sg_label, " models ...")
        for (tt in c("land_delta", "land_level", "impr_delta", "impr_level")) {
          cv_nm <- paste0("lgb_", key, "_", tt, "_cv")
          dv_nm <- paste0("dv_", key, "_", tt)
          load_model(cv_nm, cv_nm, required = (tt %in% c("land_delta", "impr_delta")))
          load_model(dv_nm, dv_nm, required = FALSE)
        }
      }

      # Expose CV objects → model + features
      for (tt in c("land_delta", "land_level", "impr_delta", "impr_level")) {
        expose_cv(
          paste0("lgb_", key, "_", tt, "_cv"),
          paste0("lgb_", key, "_", tt, "_model"),
          paste0("lgb_", key, "_", tt, "_features")
        )
      }

      # Load training frames (for diagnostics / reference)
      for (tt in c("land_delta", "land_level", "impr_delta", "impr_level")) {
        load_train_frame(
          paste0("model_data_", key, "_", tt),
          paste0("model_data_", key, "_", tt, ".rds")
        )
      }
    }
  }

  # --------------------------------------------------------------------------
  # 3c. CONDO  —  03_model_condo_land.R  +  03_model_condo_impr.R
  #
  # Both scripts use make_rolling_year_folds() for chronological CV.
  #   _land  →  log(appr_land_val) per unit   delta (primary) + level (fallback)
  #   _impr  →  log(appr_imps_val) per unit   delta (primary) + level (fallback)
  # --------------------------------------------------------------------------
  if (run_condo) {
    message("\n  [Condo models]")
    if (model_replicate) {
      message("  Training condo LightGBM models (land + impr) ...")
      source_global(here::here("scripts", "ml", "03_model_condo_land.R"))
      source_global(here::here("scripts", "ml", "03_model_condo_impr.R"))
      message("  \u2705 Condo models trained and saved.")
      # Expose aliases used by 06_forecast_av_2026_2031_sequential_condo.R
      if (exists("lgb_condo_land_delta_cv", envir = .GlobalEnv))
        assign("lgb_condo_delta_cv", get("lgb_condo_land_delta_cv", envir = .GlobalEnv), envir = .GlobalEnv)
      if (exists("lgb_condo_land_level_cv", envir = .GlobalEnv))
        assign("lgb_condo_level_cv", get("lgb_condo_land_level_cv", envir = .GlobalEnv), envir = .GlobalEnv)

      drop_if_exists(
        "model_data_condo_land_delta",
        "model_data_condo_land_level",
        "model_data_condo_impr_delta",
        "model_data_condo_impr_level"
      )

    } else {
      message("  Loading cached condo models ...")
      # Land
      load_model("lgb_condo_land_delta_cv", "lgb_condo_land_delta_cv")
      load_model("dv_condo_land_delta", "dv_condo_land_delta")
      load_model("lgb_condo_land_level_cv",
                 "lgb_condo_land_level_cv",
                 required = FALSE)
      load_model("dv_condo_land_level",
                 "dv_condo_land_level",
                 required = FALSE)
      # Improvement
      load_model("lgb_condo_impr_delta_cv", "lgb_condo_impr_delta_cv")
      load_model("dv_condo_impr_delta", "dv_condo_impr_delta")
      load_model("lgb_condo_impr_level_cv",
                 "lgb_condo_impr_level_cv",
                 required = FALSE)
      load_model("dv_condo_impr_level",
                 "dv_condo_impr_level",
                 required = FALSE)
    }

    expose_cv(
      "lgb_condo_land_delta_cv",
      "lgb_condo_land_delta_model",
      "lgb_condo_land_delta_features"
    )
    expose_cv(
      "lgb_condo_land_level_cv",
      "lgb_condo_land_level_model",
      "lgb_condo_land_level_features"
    )
    expose_cv(
      "lgb_condo_impr_delta_cv",
      "lgb_condo_impr_delta_model",
      "lgb_condo_impr_delta_features"
    )
    expose_cv(
      "lgb_condo_impr_level_cv",
      "lgb_condo_impr_level_model",
      "lgb_condo_impr_level_features"
    )

    load_train_frame("model_data_condo_land_delta",
                     "model_data_condo_land_delta.rds")
    load_train_frame("model_data_condo_land_level",
                     "model_data_condo_land_level.rds")
    load_train_frame("model_data_condo_impr_delta",
                     "model_data_condo_impr_delta.rds")
    load_train_frame("model_data_condo_impr_level",
                     "model_data_condo_impr_level.rds")

    # Short-name aliases for 06_forecast_av_2026_2031_sequential_condo.R
    if (exists("lgb_condo_land_delta_cv", envir = .GlobalEnv))
      assign("lgb_condo_delta_cv", get("lgb_condo_land_delta_cv", envir = .GlobalEnv), envir = .GlobalEnv)
    if (exists("lgb_condo_land_level_cv", envir = .GlobalEnv))
      assign("lgb_condo_level_cv", get("lgb_condo_land_level_cv", envir = .GlobalEnv), envir = .GlobalEnv)
  }

  # panel_tbl (res backbone created by xx_combine_parcel*) no longer needed
  drop_if_exists("panel_tbl")

  # Training frames are not needed past Step 3.  Every later consumer
  # (04_retrofitting_values.R, 05_eval_holdout_2025.R, 06_..._sequential.R)
  # reloads what it needs from cache_dir when the object is absent, so this
  # changes peak memory only.  This block used to sit after Step 5a and named
  # model_data_comm_* (which no longer exist), so the 24 subgroup frames
  # reloaded "for reference" in Step 3b stayed resident through Step 4 — the
  # 2026-09-19 OOM.
  model_data_frames <- c(
    "model_data_land_delta_model", "model_data_land_model",
    "model_data_impr_delta_model", "model_data_impr_level_model",
    "model_data_condo_land_delta", "model_data_condo_land_level",
    "model_data_condo_impr_delta", "model_data_condo_impr_level",
    as.vector(outer(paste0("model_data_", COM_SUBGROUP_KEYS, "_"),
                    c("land_delta", "land_level", "impr_delta", "impr_level"),
                    paste0))
  )
  drop_if_exists(model_data_frames)
  gc(verbose = FALSE)

  if (!forecast_only) {

  # ============================================================================
  # STEP 4  —  Retrofit Historical AV
  # ============================================================================

  # --------------------------------------------------------------------------
  # 4a. Residential  (full residual-correction retrofit via 04_retrofitting_values.R)
  # --------------------------------------------------------------------------
  if (run_res) {
    message("\n--- Step 4a: Retrofit historical AV (residential) ---")
    retro_cache_res <- file.path(cache_dir, "panel_tbl_retro_res.rds")

    if (retrofit_replicate || !file.exists(retro_cache_res)) {
      # growth mode: the retrofit's delta models read the growth columns
      ensure_res_panel_nwmls()
      assign("panel_tbl", get("panel_tbl_res", envir = .GlobalEnv),
             envir = .GlobalEnv)
      # Drop panel_tbl_res immediately — retrofit only needs panel_tbl.
      # Keeping both alive simultaneously adds ~2.5 GB at peak.
      drop_if_exists("panel_tbl_res")
      source_global(here::here("scripts", "ml", "04_retrofitting_values.R"))
      if (exists("panel_tbl_retro", envir = .GlobalEnv)) {
        assign("panel_tbl_retro_res", panel_tbl_retro, envir = .GlobalEnv)
        saveRDS(panel_tbl_retro_res, retro_cache_res)
        message("  \U1f4be cached: panel_tbl_retro_res")
      }
    } else {
      message("  retrofit_replicate=FALSE and cache exists — loading.")
      assign("panel_tbl_retro_res",
             readRDS(retro_cache_res),
             envir = .GlobalEnv)
      message("  \u2705 loaded: panel_tbl_retro_res")
    }

    # panel_tbl_res and the intermediate panel_tbl_retro are now superseded
    # by panel_tbl_retro_res.  04_retrofitting_values.R reloads three
    # training frames into .GlobalEnv on its way through; drop them again.
    drop_if_exists("panel_tbl_res", "panel_tbl_retro", "panel_tbl",
                   "model_data_land_model", "model_data_impr_delta_model",
                   "model_data_impr_level_model")
  }

  # --------------------------------------------------------------------------
  # 4b. Commercial subgroups  (forward/backward na.locf fill, per subgroup)
  # --------------------------------------------------------------------------
  if (run_com) {
    for (key in COM_SUBGROUP_KEYS) {
      sg_label <- COM_SUBGROUPS[[key]]$label
      message("\n--- Step 4b: Retrofit historical AV (", sg_label, " / ", key, ") ---")
      retro_cache_sg <- file.path(cache_dir, paste0("panel_tbl_retro_", key, ".rds"))

      # Reload subgroup panel from cache (was dropped after Step 2)
      sg_panel_name <- paste0("panel_tbl_", key)
      if (!exists(sg_panel_name, envir = .GlobalEnv)) {
        sg_path <- file.path(cache_dir, paste0(sg_panel_name, ".rds"))
        if (file.exists(sg_path)) {
          assign(sg_panel_name, readRDS(sg_path), envir = .GlobalEnv)
          message("  \u2705 loaded: ", sg_panel_name)
        } else {
          message("  \u26a0\ufe0f  ", sg_panel_name, " cache not found — skipping retrofit")
          next
        }
      }

      if (retrofit_replicate || !file.exists(retro_cache_sg)) {
        panel_retro_sg <- data.table::copy(get(sg_panel_name, envir = .GlobalEnv))
        data.table::setDT(panel_retro_sg)
        data.table::setkeyv(panel_retro_sg, c("parcel_id", "tax_yr"))

        # Re-join AV if all-NA (dash-format mismatch in panel build), then
        # locf fill.  retro_fill_av() is the former inline code, hoisted to
        # top level so the backtest harness can call it; av_history_cln is
        # read from .GlobalEnv or cache_dir inside and released on return.
        # locf_backfill = FALSE (backtest) drops the fromLast pass.
        panel_retro_sg <- retro_fill_av(panel_retro_sg, cache_dir = cache_dir,
                                        backfill = locf_backfill, label = key)

        retro_name <- paste0("panel_tbl_retro_", key)
        assign(retro_name, panel_retro_sg, envir = .GlobalEnv)
        saveRDS(panel_retro_sg, retro_cache_sg)
        message("  \U1f4be cached: ", retro_name)
        rm(panel_retro_sg)

      } else {
        retro_name <- paste0("panel_tbl_retro_", key)
        message("  retrofit_replicate=FALSE and cache exists — loading.")
        assign(retro_name, readRDS(retro_cache_sg), envir = .GlobalEnv)
        message("  \u2705 loaded: ", retro_name)
      }

      drop_if_exists(sg_panel_name)
    }

    # Also handle "Other" commercial parcels with legacy retrofit
    message("\n--- Step 4b: Retrofit historical AV (com_other) ---")
    retro_cache_com_other <- file.path(cache_dir, "panel_tbl_retro_com_other.rds")
    if (!exists("panel_tbl_com_other", envir = .GlobalEnv)) {
      co_path <- file.path(cache_dir, "panel_tbl_com_other.rds")
      if (file.exists(co_path))
        assign("panel_tbl_com_other", readRDS(co_path), envir = .GlobalEnv)
    }
    if (exists("panel_tbl_com_other", envir = .GlobalEnv)) {
      if (retrofit_replicate || !file.exists(retro_cache_com_other)) {
        panel_retro_other <- data.table::copy(get("panel_tbl_com_other", envir = .GlobalEnv))
        data.table::setDT(panel_retro_other)
        data.table::setkeyv(panel_retro_other, c("parcel_id", "tax_yr"))

        # com_other never had the AV re-join the six subgroups get, so its
        # retro panel carried no observed AV at all and the residual bucket
        # forecast to $0.  Same repair, same dash handling, same locf fill \u2014
        # via retro_fill_av() (see the subgroup loop above).
        panel_retro_other <- retro_fill_av(panel_retro_other,
                                           cache_dir = cache_dir,
                                           backfill  = locf_backfill,
                                           label     = "com_other")
        assign("panel_tbl_retro_com_other", panel_retro_other, envir = .GlobalEnv)
        saveRDS(panel_retro_other, retro_cache_com_other)
        message("  \U1f4be cached: panel_tbl_retro_com_other")
        rm(panel_retro_other)
      } else {
        assign("panel_tbl_retro_com_other", readRDS(retro_cache_com_other), envir = .GlobalEnv)
        message("  \u2705 loaded: panel_tbl_retro_com_other")
      }
      drop_if_exists("panel_tbl_com_other")
    }
  }

  # --------------------------------------------------------------------------
  # 4c. Condo  (forward/backward na.locf fill)
  # --------------------------------------------------------------------------
  if (run_condo) {
    message("\n--- Step 4c: Retrofit historical AV (condo) ---")
    retro_cache_condo <- file.path(cache_dir, "panel_tbl_retro_condo.rds")
    # Reload panel_tbl_condo from cache (was dropped after Step 2 to save memory)
    if (!exists("panel_tbl_condo", envir = .GlobalEnv))
      cache_load("panel_tbl_condo")

    if (retrofit_replicate || !file.exists(retro_cache_condo)) {
      panel_retro_condo <- copy(panel_tbl_condo)
      setDT(panel_retro_condo)
      setkeyv(panel_retro_condo, c("parcel_id", "tax_yr"))

      # Re-join AV if all-NA (dash-format mismatch in panel build), then locf
      # fill — same helper as Step 4b.
      panel_retro_condo <- retro_fill_av(panel_retro_condo,
                                         cache_dir = cache_dir,
                                         backfill  = locf_backfill,
                                         label     = "condo")

      assign("panel_tbl_retro_condo", panel_retro_condo, envir = .GlobalEnv)
      saveRDS(panel_retro_condo, retro_cache_condo)
      message("  \U1f4be cached: panel_tbl_retro_condo")
      rm(panel_retro_condo)

    } else {
      assign("panel_tbl_retro_condo",
             readRDS(retro_cache_condo),
             envir = .GlobalEnv)
      message("  \u2705 loaded: panel_tbl_retro_condo")
    }

    drop_if_exists("panel_tbl_condo")
  }

  # Drop all retro panels from memory — already cached to disk.
  # They are reloaded just before their respective extend scripts.
  for (key in COM_SUBGROUP_KEYS)
    drop_if_exists(paste0("panel_tbl_retro_", key))
  drop_if_exists("panel_tbl_retro_com_other",
                 "panel_tbl_retro_condo")
  gc(verbose = FALSE)

  # ============================================================================
  # STEP 5a  —  Holdout Diagnostics
  # ============================================================================
  message("\n--- Step 5a: Holdout diagnostics ---")
  eval_script <- here::here("scripts", "ml", "05_eval_holdout_2025.R")

  if (!file.exists(eval_script)) {
    message("  05_eval_holdout_2025.R not found — skipping.")
  } else if (!diagnostics_replicate) {
    # Cache-first: load pre-computed metrics without running the eval script
    out_dir_eval <- file.path(output_dir, "eval_2025")
    metrics_land_path <- file.path(out_dir_eval,
      paste0("metrics_land_2025_", kca_date_data_extracted, ".csv"))
    if (file.exists(metrics_land_path)) {
      message("  diagnostics_replicate=FALSE — loading cached eval metrics ...")
      # Build list of possible metric files: res + condo + all subgroups
      metrics_files <- c(
        file.path(out_dir_eval, paste0("metrics_land_2025_",       kca_date_data_extracted, ".csv")),
        file.path(out_dir_eval, paste0("metrics_impr_2025_",       kca_date_data_extracted, ".csv")),
        file.path(out_dir_eval, paste0("metrics_condo_land_2025_", kca_date_data_extracted, ".csv")),
        file.path(out_dir_eval, paste0("metrics_condo_impr_2025_", kca_date_data_extracted, ".csv"))
      )
      # Add subgroup metric files
      for (key in COM_SUBGROUP_KEYS) {
        metrics_files <- c(metrics_files,
          file.path(out_dir_eval, paste0("metrics_", key, "_land_2025_", kca_date_data_extracted, ".csv")),
          file.path(out_dir_eval, paste0("metrics_", key, "_impr_2025_", kca_date_data_extracted, ".csv"))
        )
      }
      # Legacy com-wide metrics (backward compat)
      metrics_files <- c(metrics_files,
        file.path(out_dir_eval, paste0("metrics_com_land_2025_",   kca_date_data_extracted, ".csv")),
        file.path(out_dir_eval, paste0("metrics_com_impr_2025_",   kca_date_data_extracted, ".csv"))
      )

      metrics_all <- dplyr::bind_rows(
        lapply(metrics_files[sapply(metrics_files, file.exists)],
               function(p) readr::read_csv(p, show_col_types = FALSE))
      )
      assign("metrics_all", metrics_all, envir = .GlobalEnv)
      message("  === 2025 Holdout Eval — All Tracks (cached) ===")
      print(metrics_all %>%
              dplyr::select(Track, Target, Model, RMSE_log, MAE_log, MAPE, WAPE) %>%
              dplyr::arrange(Track, Target))
    } else {
      message("  No cached eval metrics found — run with diagnostics_replicate=TRUE to generate.")
    }
  } else if (run_res) {
    assign("panel_tbl", panel_tbl_retro_res, envir = .GlobalEnv)
    source_global(eval_script)
  }

  } # end !forecast_only (Steps 4 + 5a)

  # Eval (Step 5a) can leave multi-GB objects behind: the panel_tbl alias of
  # the retro panel, the condo retro panel, and training frames.  None are
  # needed by Steps 5b/6 — drop them before the extend/forecast peak.
  drop_if_exists(
    "panel_tbl", "panel_tbl_retro_condo",
    "model_data_land_delta_model", "model_data_land_model",
    "model_data_impr_delta_model", "model_data_impr_level_model",
    "model_data_comm_land_delta",  "model_data_comm_impr_delta",
    "model_data_condo_land_delta", "model_data_condo_land_level",
    "model_data_condo_impr_delta", "model_data_condo_impr_level"
  )
  gc(verbose = FALSE)

  # ============================================================================
  # STEP 5b  —  Extend Panel 2026-2031  (or load cached extended panels)
  # ============================================================================
  # When forecast_only=TRUE: skip extend scripts, load cached extended panels.
  # When forecast_only=FALSE: run extend scripts (respecting extend_replicate).
  # ============================================================================
  message(paste0("\n--- Step 5b: Extend panel ", forecast_start, "-", forecast_end, " (scenario = ", scenario, ") ---"))

  if (forecast_only) {
    # --------------------------------------------------------------------------
    # forecast_only = TRUE: load cached extended panels directly
    # --------------------------------------------------------------------------
    message("  forecast_only=TRUE — loading cached extended panels ...")

    load_ext <- function(track_label, obj_name, cache_name) {
      if (exists(obj_name, envir = .GlobalEnv)) {
        message("  \u2705 already in memory: ", obj_name)
        return(invisible(NULL))
      }
      p <- file.path(cache_dir, cache_name)
      if (!file.exists(p))
        stop("forecast_only=TRUE but extended panel cache not found: ", p,
             "\nRun with extend_replicate=TRUE once to build it.")
      assign(obj_name, readRDS(p), envir = .GlobalEnv)
      message("  \u2705 loaded extended panel (", track_label, "): ", cache_name)
    }

    if (run_res) {
      load_ext("res",
               paste0("panel_tbl_", forecast_start, "_", forecast_end, "_inputs_", scenario, "_res"),
               paste0("panel_tbl_", forecast_start, "_", forecast_end, "_inputs_", scenario, "_res.rds"))
      # 06_forecast_..._sequential.R reads the legacy fixed name
      assign(paste0("panel_tbl_2006_2031_inputs_", scenario, "_res"),
             get(paste0("panel_tbl_", forecast_start, "_", forecast_end,
                        "_inputs_", scenario, "_res"), envir = .GlobalEnv),
             envir = .GlobalEnv)
    }

    if (run_com) {
      for (key in COM_SUBGROUP_KEYS) {
        load_ext(key,
                 paste0("panel_tbl_", forecast_start, "_", forecast_end, "_inputs_", scenario, "_", key),
                 paste0("panel_tbl_", forecast_start, "_", forecast_end, "_inputs_", scenario, "_", key, ".rds"))
      }
      # Also load com_other extended panel if it exists
      co_ext_name <- paste0("panel_tbl_", forecast_start, "_", forecast_end, "_inputs_", scenario, "_com_other")
      co_ext_path <- file.path(cache_dir, paste0(co_ext_name, ".rds"))
      if (file.exists(co_ext_path))
        load_ext("com_other", co_ext_name, paste0(co_ext_name, ".rds"))
    }

    if (run_condo)
      load_ext("condo",
               paste0("panel_tbl_", forecast_start, "_", forecast_end, "_inputs_", scenario, "_condo"),
               paste0("panel_tbl_", forecast_start, "_", forecast_end, "_inputs_", scenario, "_condo.rds"))

  } else {
    # --------------------------------------------------------------------------
    # forecast_only = FALSE: run extend scripts (normal path)
    # --------------------------------------------------------------------------

    # Residential extend
    if (run_res) {
      # growth mode: the extend joins every column of the NWMLS forecast
      # cache onto the forecast years, so the cache must carry the growth
      # columns.  A pre-2026-10 cache is rebuilt from data/nwmls/ (only that
      # file is rewritten; its level columns are byte-identical).
      if (nwmls_features == "growth")
        nwmls_ensure_fcst_cache(
          nwmls_fcst_cache_path(cache_dir, scenario), nwmls_features,
          rebuild = function()
            nwmls_write_fcst_cache(scenario, cache_dir = cache_dir,
                                   nwmls_dir = here::here("data", "nwmls"),
                                   forecast_start = forecast_start,
                                   forecast_end = forecast_end))
      ext_cache_res <- file.path(cache_dir,
                                 paste0("panel_tbl_", forecast_start, "_", forecast_end, "_inputs_", scenario, "_res.rds"))
      if (extend_replicate || !file.exists(ext_cache_res)) {
        assign("panel_tbl_retro", panel_tbl_retro_res, envir = .GlobalEnv)
        source_global(here::here("scripts", "ml", "05_extend_panel_2026_2031.R"))
        # The extend script publishes under a legacy fixed name
        # (panel_tbl_2006_2031_inputs_<scenario>); accept either that or the
        # horizon-parameterized name, then expose the _res alias that
        # 06_forecast_..._sequential.R reads from GlobalEnv (avoiding a
        # redundant multi-GB disk re-read inside the forecast script).
        ext_obj        <- paste0("panel_tbl_", forecast_start, "_", forecast_end,
                                 "_inputs_", scenario)
        ext_obj_legacy <- paste0("panel_tbl_2006_2031_inputs_", scenario)
        src_obj <- if (exists(ext_obj, envir = .GlobalEnv)) ext_obj
                   else if (exists(ext_obj_legacy, envir = .GlobalEnv)) ext_obj_legacy
                   else NULL
        if (!is.null(src_obj)) {
          panel_ext_res <- get(src_obj, envir = .GlobalEnv)
          saveRDS(panel_ext_res, ext_cache_res)
          assign(paste0(ext_obj_legacy, "_res"), panel_ext_res, envir = .GlobalEnv)
          assign(paste0(ext_obj, "_res"),        panel_ext_res, envir = .GlobalEnv)
          message("  \U1f4be cached: ", basename(ext_cache_res))
          rm(panel_ext_res)
          drop_if_exists(src_obj)
        }
      } else {
        message("  Res extend cache exists — skipping.")
        message("  \u2705 using: ", basename(ext_cache_res))
      }
      drop_if_exists("panel_tbl_retro_res", "panel_tbl_retro")
    }

    # Commercial subgroup extends
    if (run_com) {
      for (key in COM_SUBGROUP_KEYS) {
        sg_label <- COM_SUBGROUPS[[key]]$label
        ext_cache_sg <- file.path(cache_dir,
                                  paste0("panel_tbl_", forecast_start, "_", forecast_end,
                                         "_inputs_", scenario, "_", key, ".rds"))
        if (extend_replicate || !file.exists(ext_cache_sg)) {
          # Reload retro panel from cache
          retro_name <- paste0("panel_tbl_retro_", key)
          if (!exists(retro_name, envir = .GlobalEnv)) {
            retro_path <- file.path(cache_dir, paste0(retro_name, ".rds"))
            if (file.exists(retro_path)) {
              assign(retro_name, readRDS(retro_path), envir = .GlobalEnv)
            } else {
              message("  \u26a0\ufe0f  ", retro_name, " cache not found — skipping extend for ", key)
              next
            }
          }
          assign("panel_tbl_retro", get(retro_name, envir = .GlobalEnv), envir = .GlobalEnv)

          # Use the commercial extend script (applies CoStar signals)
          source_global(here::here("scripts", "ml", "05_extend_panel_2026_2031_comm.R"))
          ext_obj_sg <- paste0("panel_tbl_", forecast_start, "_", forecast_end,
                               "_inputs_", scenario, "_", key)
          # The extend script creates the generic com object; rename to subgroup-specific
          ext_obj_generic <- paste0("panel_tbl_", forecast_start, "_", forecast_end,
                                    "_inputs_", scenario, "_com")
          if (exists(ext_obj_generic, envir = .GlobalEnv)) {
            assign(ext_obj_sg, get(ext_obj_generic, envir = .GlobalEnv), envir = .GlobalEnv)
            saveRDS(get(ext_obj_sg, envir = .GlobalEnv), ext_cache_sg)
            message("  \U1f4be cached: ", basename(ext_cache_sg))
            drop_if_exists(ext_obj_generic)
          } else if (exists(ext_obj_sg, envir = .GlobalEnv)) {
            saveRDS(get(ext_obj_sg, envir = .GlobalEnv), ext_cache_sg)
            message("  \U1f4be cached: ", basename(ext_cache_sg))
          }
          drop_if_exists(retro_name, "panel_tbl_retro")
        } else {
          message("  ", sg_label, " extend cache exists — skipping.")
        }

        # Free memory between subgroups
        gc(verbose = FALSE)
      }

      # Also extend com_other with legacy commercial approach
      ext_cache_co <- file.path(cache_dir,
                                paste0("panel_tbl_", forecast_start, "_", forecast_end,
                                       "_inputs_", scenario, "_com_other.rds"))
      if (extend_replicate || !file.exists(ext_cache_co)) {
        if (!exists("panel_tbl_retro_com_other", envir = .GlobalEnv)) {
          co_retro_path <- file.path(cache_dir, "panel_tbl_retro_com_other.rds")
          if (file.exists(co_retro_path))
            assign("panel_tbl_retro_com_other", readRDS(co_retro_path), envir = .GlobalEnv)
        }
        if (exists("panel_tbl_retro_com_other", envir = .GlobalEnv)) {
          assign("panel_tbl_retro", get("panel_tbl_retro_com_other", envir = .GlobalEnv), envir = .GlobalEnv)
          source_global(here::here("scripts", "ml", "05_extend_panel_2026_2031_comm.R"))
          ext_obj_generic <- paste0("panel_tbl_", forecast_start, "_", forecast_end,
                                    "_inputs_", scenario, "_com")
          ext_obj_co <- paste0("panel_tbl_", forecast_start, "_", forecast_end,
                               "_inputs_", scenario, "_com_other")
          if (exists(ext_obj_generic, envir = .GlobalEnv)) {
            assign(ext_obj_co, get(ext_obj_generic, envir = .GlobalEnv), envir = .GlobalEnv)
            saveRDS(get(ext_obj_co, envir = .GlobalEnv), ext_cache_co)
            message("  \U1f4be cached: ", basename(ext_cache_co))
            drop_if_exists(ext_obj_generic)
          }
          drop_if_exists("panel_tbl_retro_com_other", "panel_tbl_retro")
        }
      }
    }

    # Free subgroup extended panels before condo extend
    if (run_com && run_condo) {
      for (key in c(COM_SUBGROUP_KEYS, "com_other"))
        drop_if_exists(paste0("panel_tbl_", forecast_start, "_", forecast_end,
                              "_inputs_", scenario, "_", key))
      gc(verbose = FALSE)
    }

    # Condo extend
    if (run_condo) {
      ext_cache_condo <- file.path(cache_dir,
                                   paste0("panel_tbl_", forecast_start, "_", forecast_end, "_inputs_", scenario, "_condo.rds"))
      if (extend_replicate || !file.exists(ext_cache_condo)) {
        # Reload retro_condo from cache (was dropped after Step 4 to save memory)
        if (!exists("panel_tbl_retro_condo", envir = .GlobalEnv))
          assign("panel_tbl_retro_condo", readRDS(file.path(cache_dir, "panel_tbl_retro_condo.rds")), envir = .GlobalEnv)
        assign("panel_tbl_retro", panel_tbl_retro_condo, envir = .GlobalEnv)
        source_global(here::here("scripts", "ml", "05_extend_panel_2026_2031_condo.R"))
        ext_obj_condo <- paste0("panel_tbl_", forecast_start, "_", forecast_end, "_inputs_", scenario, "_condo")
        if (exists(ext_obj_condo, envir = .GlobalEnv)) {
          saveRDS(get(ext_obj_condo, envir = .GlobalEnv), ext_cache_condo)
          message("  \U1f4be cached: ", basename(ext_cache_condo))
        }
      } else {
        message("  Condo extend cache exists — skipping.")
      }
      drop_if_exists("panel_tbl_retro_condo", "panel_tbl_retro")
    }

  } # end forecast_only / normal extend branch

  gc(verbose = FALSE)

  # Backtest harness: the extended panels are on disk in cache_dir; the
  # harness swaps their forecast-year drivers for realized values and comes
  # back with forecast_only = TRUE for Step 6.
  if (identical(stop_after, "extend")) {
    message("\nstop_after = \"extend\" — returning before Step 6 (elapsed ",
            round(as.numeric(difftime(Sys.time(), start_time,
                                      units = "mins")), 2), " min)")
    return(invisible(NULL))
  }

  # ============================================================================
  # STEP 6  —  Sequential Forecast 2026-2031
  # ============================================================================
  message(paste0("\n--- Step 6: Sequential forecast ", forecast_start, "-", forecast_end, " ---"))

  if (run_res) {
    message("  [Residential forecast]")
    source_global(here::here("scripts", "ml", "06_forecast_av_2026_2031_sequential.R"))
  }

  if (run_com) {
    # --- Forecast each commercial subgroup with its own models ----------------
    for (key in COM_SUBGROUP_KEYS) {
      sg_label <- COM_SUBGROUPS[[key]]$label
      message("\n  [", sg_label, " forecast (", key, ")]")

      # Reload subgroup extended panel if needed
      ext_obj_name <- paste0("panel_tbl_", forecast_start, "_", forecast_end,
                             "_inputs_", scenario, "_", key)
      if (!exists(ext_obj_name, envir = .GlobalEnv)) {
        ext_path <- file.path(cache_dir, paste0(ext_obj_name, ".rds"))
        if (file.exists(ext_path)) {
          assign(ext_obj_name, readRDS(ext_path), envir = .GlobalEnv)
          message("  \u2705 reloaded: ", ext_obj_name)
        } else {
          message("  \u26a0\ufe0f  ", ext_obj_name, " not found — skipping forecast for ", key)
          next
        }
      }

      # Ensure per-subgroup CoStar features are present (extend caches built
      # before the cs_* integration lack them; the join is idempotent, and
      # CoStar's own forecast quarters cover the full horizon natively).
      assign(ext_obj_name,
             join_costar_subgroup_features(
               get(ext_obj_name, envir = .GlobalEnv), key),
             envir = .GlobalEnv)

      # Coalesce the .x/.y econ twins into the base names the models read.
      # Without this the econ block is all-NA from forecast_start onward.
      assign(ext_obj_name,
             repair_suffixed(get(ext_obj_name, envir = .GlobalEnv)),
             envir = .GlobalEnv)

      # Set up aliases expected by the commercial forecast script
      # The forecast script reads panel_tbl_<start>_<end>_inputs_<scenario>_com
      # and uses lgb_com_land_delta_model, etc.
      ext_obj_com_generic <- paste0("panel_tbl_", forecast_start, "_", forecast_end,
                                    "_inputs_", scenario, "_com")
      assign(ext_obj_com_generic, get(ext_obj_name, envir = .GlobalEnv), envir = .GlobalEnv)
      # 06_forecast_..._comm.R reads the LEGACY fixed name from GlobalEnv
      # (falling back to a disk cache that holds whichever subgroup extended
      # last) — publish the legacy alias so each subgroup forecasts on ITS
      # OWN panel.
      ext_obj_com_legacy <- paste0("panel_tbl_2006_2031_inputs_", scenario, "_com")
      assign(ext_obj_com_legacy, get(ext_obj_name, envir = .GlobalEnv), envir = .GlobalEnv)

      # Map subgroup model objects to the generic com model names
      for (tt in c("land_delta", "land_level", "impr_delta", "impr_level")) {
        sg_cv   <- paste0("lgb_", key, "_", tt, "_cv")
        sg_mod  <- paste0("lgb_", key, "_", tt, "_model")
        sg_feat <- paste0("lgb_", key, "_", tt, "_features")
        gen_cv  <- paste0("lgb_com_", tt, "_cv")
        gen_mod <- paste0("lgb_com_", tt, "_model")
        gen_feat <- paste0("lgb_com_", tt, "_features")

        if (exists(sg_cv, envir = .GlobalEnv))
          assign(gen_cv, get(sg_cv, envir = .GlobalEnv), envir = .GlobalEnv)
        if (exists(sg_mod, envir = .GlobalEnv))
          assign(gen_mod, get(sg_mod, envir = .GlobalEnv), envir = .GlobalEnv)
        if (exists(sg_feat, envir = .GlobalEnv))
          assign(gen_feat, get(sg_feat, envir = .GlobalEnv), envir = .GlobalEnv)
      }
      # Legacy aliases used by 06_forecast_av_2026_2031_sequential_comm.R
      if (exists("lgb_com_land_delta_cv", envir = .GlobalEnv))
        assign("lgb_com_delta_cv", get("lgb_com_land_delta_cv", envir = .GlobalEnv), envir = .GlobalEnv)
      if (exists("lgb_com_land_level_cv", envir = .GlobalEnv))
        assign("lgb_com_level_cv", get("lgb_com_land_level_cv", envir = .GlobalEnv), envir = .GlobalEnv)

      source_global(here::here(
        "scripts", "ml",
        "06_forecast_av_2026_2031_sequential_comm.R"
      ))

      # Rename the forecasted output from com → subgroup-specific
      fcst_com_name <- "panel_tbl_forecasted_com"
      fcst_sg_name  <- paste0("panel_tbl_forecasted_", key)
      if (exists(fcst_com_name, envir = .GlobalEnv)) {
        assign(fcst_sg_name, get(fcst_com_name, envir = .GlobalEnv), envir = .GlobalEnv)

        # Also save scenario-specific cache with subgroup suffix
        cache_fcst_name <- paste0("panel_tbl_2006_2031_forecasted_", scenario, "_", key, ".rds")
        saveRDS(get(fcst_sg_name, envir = .GlobalEnv),
                file.path(cache_dir, cache_fcst_name))
        message("  \U1f4be cached: ", cache_fcst_name)

        drop_if_exists(fcst_com_name)
      }

      # Clean up generic aliases
      drop_if_exists(ext_obj_com_generic, ext_obj_com_legacy, ext_obj_name)
      for (tt in c("land_delta", "land_level", "impr_delta", "impr_level")) {
        drop_if_exists(
          paste0("lgb_com_", tt, "_cv"),
          paste0("lgb_com_", tt, "_model"),
          paste0("lgb_com_", tt, "_features")
        )
      }
      drop_if_exists("lgb_com_delta_cv", "lgb_com_level_cv")

      gc(verbose = FALSE)
    }

    # Forecast com_other with legacy commercial approach
    message("\n  [Commercial Other forecast]")
    ext_co_name <- paste0("panel_tbl_", forecast_start, "_", forecast_end,
                          "_inputs_", scenario, "_com_other")
    if (!exists(ext_co_name, envir = .GlobalEnv)) {
      ext_co_path <- file.path(cache_dir, paste0(ext_co_name, ".rds"))
      if (file.exists(ext_co_path))
        assign(ext_co_name, readRDS(ext_co_path), envir = .GlobalEnv)
    }
    if (exists(ext_co_name, envir = .GlobalEnv)) {
      # Use the last-available model set or a simple naive approach for Other
      ext_obj_com_generic <- paste0("panel_tbl_", forecast_start, "_", forecast_end,
                                    "_inputs_", scenario, "_com")
      assign(ext_obj_com_generic, get(ext_co_name, envir = .GlobalEnv), envir = .GlobalEnv)
      ext_obj_com_legacy <- paste0("panel_tbl_2006_2031_inputs_", scenario, "_com")
      assign(ext_obj_com_legacy, get(ext_co_name, envir = .GlobalEnv), envir = .GlobalEnv)

      # For com_other, attempt to use any available commercial models
      # (these may not exist if we only trained subgroup models)
      has_com_models <- any(sapply(
        paste0("lgb_com_land_delta_cv"),
        function(n) exists(n, envir = .GlobalEnv)
      ))
      if (has_com_models) {
        source_global(here::here("scripts", "ml",
                                 "06_forecast_av_2026_2031_sequential_comm.R"))
      } else {
        message("  \u2139\ufe0f  No aggregate commercial models available — com_other uses residual growth")
        # Residual growth (replaces the pre-2026-08-02 flat carry-forward).
        # Precedence per parcel-year:
        #   1. KCA geographic area-report actual, anchor year, non-specialty
        #   2. com_other_growth_method rate (com_weighted / blend / proxy)
        #   3. flat, as fallback
        # Land and improvement are grown separately and compound off the prior
        # FORECAST year.  pred_appr_land_val / pred_appr_imps_val are populated
        # as well as pred_total_assessed - av_reconcile_certified.R reads all
        # three, and com_other previously supplied only the total.
        #
        # Ordering: this reads the six subgroup forecast panels to build its
        # weights, so it must stay AFTER the subgroup forecast loop above.
        source_global(here::here("scripts", "ml", "xx_com_other_growth.R"))
        co_dt <- run_com_other_forecast(
          co_dt          = get(ext_co_name, envir = .GlobalEnv),
          scenario       = scenario,
          cache_dir      = cache_dir,
          forecast_start = forecast_start,
          forecast_end   = forecast_end
        )
        message("  com_other residual: ",
                scales::comma(co_dt[tax_yr >= forecast_start &
                                      !is.na(pred_total_assessed),
                                    data.table::uniqueN(parcel_id)]),
                " parcels forecast into the horizon")
        assign("panel_tbl_forecasted_com", co_dt, envir = .GlobalEnv)
        rm(co_dt)
      }

      fcst_co_name <- "panel_tbl_forecasted_com_other"
      if (exists("panel_tbl_forecasted_com", envir = .GlobalEnv)) {
        assign(fcst_co_name, get("panel_tbl_forecasted_com", envir = .GlobalEnv), envir = .GlobalEnv)
        cache_fcst_name <- paste0("panel_tbl_2006_2031_forecasted_", scenario, "_com_other.rds")
        saveRDS(get(fcst_co_name, envir = .GlobalEnv),
                file.path(cache_dir, cache_fcst_name))
        message("  \U1f4be cached: ", cache_fcst_name)
        drop_if_exists("panel_tbl_forecasted_com")
      }
      drop_if_exists(ext_obj_com_generic, ext_obj_com_legacy, ext_co_name)
    }

    # --- Combine all commercial subgroups into panel_tbl_forecasted_com --------
    # This creates a combined commercial forecasted panel so that downstream
    # scripts (av_fcst_summary, av_detailed_groups) can still read the "com" track.
    message("\n  Combining commercial subgroup forecasts into panel_tbl_forecasted_com ...")
    com_parts <- list()
    for (key in c(COM_SUBGROUP_KEYS, "com_other")) {
      fcst_nm <- paste0("panel_tbl_forecasted_", key)
      if (exists(fcst_nm, envir = .GlobalEnv)) {
        part <- data.table::copy(get(fcst_nm, envir = .GlobalEnv))
        part[, com_subgroup := key]
        com_parts[[key]] <- part
      } else {
        # Try loading from disk
        fcst_path <- file.path(cache_dir,
                               paste0("panel_tbl_2006_2031_forecasted_", scenario, "_", key, ".rds"))
        if (file.exists(fcst_path)) {
          part <- data.table::as.data.table(readRDS(fcst_path))
          part[, com_subgroup := key]
          com_parts[[key]] <- part
        }
      }
    }
    if (length(com_parts) > 0) {
      combined_com <- data.table::rbindlist(com_parts, use.names = TRUE, fill = TRUE)
      assign("panel_tbl_forecasted_com", combined_com, envir = .GlobalEnv)
      saveRDS(combined_com,
              file.path(cache_dir,
                        paste0("panel_tbl_2006_2031_forecasted_", scenario, "_com.rds")))
      message("  \U1f4be cached combined commercial forecast: ",
              scales::comma(nrow(combined_com)), " rows, ",
              length(com_parts), " subgroups")
      rm(com_parts, combined_com)
    }
  }

  if (run_condo) {
    message("  [Condo forecast]")
    source_global(here::here(
      "scripts",
      "ml",
      "06_forecast_av_2026_2031_sequential_condo.R"
    ))
  }

  # Extended input panels consumed by forecast scripts above
  # (legacy fixed names included — the extend/forecast scripts publish/read
  #  panel_tbl_2006_2031_inputs_* regardless of the configured horizon)
  drop_if_exists(
    paste0("panel_tbl_", forecast_start, "_", forecast_end, "_inputs_", scenario, "_res"),
    paste0("panel_tbl_", forecast_start, "_", forecast_end, "_inputs_", scenario, "_condo"),
    paste0("panel_tbl_2006_2031_inputs_", scenario),
    paste0("panel_tbl_2006_2031_inputs_", scenario, "_res"),
    paste0("panel_tbl_2006_2031_inputs_", scenario, "_condo")
  )
  for (key in c(COM_SUBGROUP_KEYS, "com_other"))
    drop_if_exists(paste0("panel_tbl_", forecast_start, "_", forecast_end,
                          "_inputs_", scenario, "_", key))

  # ---- Footer -----------------------------------------------------------------
  elapsed <- round(as.numeric(difftime(Sys.time(), start_time, units = "mins")), 2)
  message("\n==============================================")
  message("run_main_ml() finished — elapsed: ", elapsed, " min")
  message("  prop_scope = ", prop_scope, " | scenario = ", scenario)
  if (run_com)
    message("  commercial subgroups: ",
            paste(COM_SUBGROUP_KEYS, collapse = ", "), " + other")
  message("==============================================")

  invisible(
    list(
      panel_tbl_forecasted_res   = if (exists("panel_tbl_forecasted_res", envir = .GlobalEnv))
        get("panel_tbl_forecasted_res", envir = .GlobalEnv)
      else
        NULL,
      panel_tbl_forecasted_com   = if (exists("panel_tbl_forecasted_com", envir = .GlobalEnv))
        get("panel_tbl_forecasted_com", envir = .GlobalEnv)
      else
        NULL,
      panel_tbl_forecasted_condo = if (exists("panel_tbl_forecasted_condo", envir = .GlobalEnv))
        get("panel_tbl_forecasted_condo", envir = .GlobalEnv)
      else
        NULL,
      # Individual subgroup forecasts
      panel_tbl_forecasted_apt         = if (exists("panel_tbl_forecasted_apt", envir = .GlobalEnv))
        get("panel_tbl_forecasted_apt", envir = .GlobalEnv) else NULL,
      panel_tbl_forecasted_office      = if (exists("panel_tbl_forecasted_office", envir = .GlobalEnv))
        get("panel_tbl_forecasted_office", envir = .GlobalEnv) else NULL,
      panel_tbl_forecasted_industrial  = if (exists("panel_tbl_forecasted_industrial", envir = .GlobalEnv))
        get("panel_tbl_forecasted_industrial", envir = .GlobalEnv) else NULL,
      panel_tbl_forecasted_retail      = if (exists("panel_tbl_forecasted_retail", envir = .GlobalEnv))
        get("panel_tbl_forecasted_retail", envir = .GlobalEnv) else NULL,
      panel_tbl_forecasted_hospitality = if (exists("panel_tbl_forecasted_hospitality", envir = .GlobalEnv))
        get("panel_tbl_forecasted_hospitality", envir = .GlobalEnv) else NULL,
      panel_tbl_forecasted_medical     = if (exists("panel_tbl_forecasted_medical", envir = .GlobalEnv))
        get("panel_tbl_forecasted_medical", envir = .GlobalEnv) else NULL,
      panel_tbl_forecasted_com_other   = if (exists("panel_tbl_forecasted_com_other", envir = .GlobalEnv))
        get("panel_tbl_forecasted_com_other", envir = .GlobalEnv) else NULL,
      area_report_actuals        = if (exists("area_report_actuals", envir = .GlobalEnv))
        get("area_report_actuals", envir = .GlobalEnv)
      else
        NULL,
      metrics_all                = if (exists("metrics_all", envir = .GlobalEnv))
        get("metrics_all", envir = .GlobalEnv)
      else
        NULL,
      rmse_land                  = if (exists("rmse_comparison_land", envir = .GlobalEnv))
        get("rmse_comparison_land", envir = .GlobalEnv)
      else
        NULL,
      rmse_impr                  = if (exists("rmse_comparison_impr", envir = .GlobalEnv))
        get("rmse_comparison_impr", envir = .GlobalEnv)
      else
        NULL
    )
  )
}
# Sourcing this file runs the pipeline.  Set MAIN_ML_DEFINE_ONLY <- TRUE in
# .GlobalEnv first to load the definitions without running anything (the
# backtest harness does; same pattern as BT_DEFINE_ONLY in
# xx_area_report_backtest.R).
if (!isTRUE(get0("MAIN_ML_DEFINE_ONLY", envir = .GlobalEnv,
                 ifnotfound = FALSE)))
run_main_ml(
  scenario = "baseline",
  prop_scope = "com",
  forecast_only = TRUE,
  model_replicate = TRUE)

#run_main_ml(scenario="baseline",  model_replicate=FALSE, forecast_only=TRUE, prop_scope="res")
#run_main_ml(scenario="optimistic",  model_replicate=FALSE, forecast_only=TRUE, prop_scope="res")
#run_main_ml(scenario="pessimistic", model_replicate=FALSE, forecast_only=TRUE, prop_scope="res")

# =============================================================================
# prep_scenario_caches()
# =============================================================================
# Generates the scenario-specific econ and NWMLS forecast cache files needed
# by the extend scripts (05_extend_panel_2026_2031*.R) without rebuilding the
# full panel.  Run this once per new scenario before calling run_main_ml() with
# that scenario and extend_replicate=TRUE.
#
# Requirements:
#   - panel_tbl_res.rds must exist in cache_dir  (panel_replicate=TRUE once)
#   - The NWMLS Excel file for the scenario must be in data/nwmls/
#   - The OERF econ forecast CSV for the scenario must be in data/econ/
#
# Usage:
#   prep_scenario_caches("optimistic")
#   prep_scenario_caches("pessimistic")
#   # Then run the forecast:
#   run_main_ml(scenario="optimistic", extend_replicate=TRUE, forecast_only=FALSE, ...)
# =============================================================================
prep_scenario_caches <- function(
    scenario,
    force = FALSE,
    kca_date_data_extracted = if (exists("CFG", envir = .GlobalEnv))
                                CFG$kca_date_data_extracted
                              else
                                get0("kca_date_data_extracted", envir = .GlobalEnv,
                                     ifnotfound = stop(
                                       "kca_date_data_extracted not found. ",
                                       "Either source main_ml.R first or pass it explicitly:\n",
                                       "  prep_scenario_caches('optimistic', kca_date_data_extracted='2026-03-27')"
                                     )),
    cache_dir  = if (exists("CFG", envir = .GlobalEnv)) CFG$cache_dir
                 else here::here("data", "cache"),
    model_dir  = if (exists("CFG", envir = .GlobalEnv)) CFG$model_dir
                 else here::here("data", "model"),
    output_dir = if (exists("CFG", envir = .GlobalEnv)) CFG$output_dir
                 else here::here("data", "outputs")
) {
  valid_scenarios <- c("baseline", "optimistic", "pessimistic")
  if (!scenario %in% valid_scenarios)
    stop("scenario must be one of: ", paste(valid_scenarios, collapse = ", "))

  message("==============================================")
  message("prep_scenario_caches() — scenario: ", scenario)
  message("==============================================")

  source_global <- function(path) source(path, local = .GlobalEnv)

  # Push all required globals for sourced scripts before calling 00_init.R
  assign("scenario",               scenario,               envir = .GlobalEnv)
  assign("kca_date_data_extracted", kca_date_data_extracted, envir = .GlobalEnv)
  assign("cache_dir",              cache_dir,              envir = .GlobalEnv)
  assign("model_dir",              model_dir,              envir = .GlobalEnv)
  assign("output_dir",             output_dir,             envir = .GlobalEnv)

  source_global(here::here("scripts", "ml", "00_init.R"))

  # ---- Check whether caches already exist ----------------------------------
  econ_path  <- file.path(cache_dir,
                          paste0("econ_fcst_2026_2031_",  scenario, ".rds"))
  nwmls_path <- file.path(cache_dir,
                          paste0("nwmls_fcst_2026_2031_", scenario, ".rds"))
  nwmls_condo_path <- file.path(cache_dir,
                          paste0("nwmls_condo_fcst_2026_2031_", scenario, ".rds"))

  costar_path <- file.path(cache_dir,
                           paste0("costar_fcst_1990_2033_", scenario, ".rds"))

  # ---- Staleness protection -------------------------------------------------
  # A cache is rebuilt when (a) force=TRUE, (b) it does not exist, or (c) any
  # of its source input files carries a newer mtime than the cache.  Source
  # files: OERF workbook(s) in data/oerf/, scenario NWMLS xlsx/csv in
  # data/nwmls/, scenario costar_q CSV in data/costar/.
  newest_mtime <- function(paths) {
    paths <- paths[file.exists(paths)]
    if (length(paths) == 0) return(as.POSIXct(NA))
    max(file.info(paths)$mtime)
  }
  cache_status <- function(cache_path, src_paths) {
    if (force) return("force")
    if (!file.exists(cache_path)) return("missing")
    sm <- newest_mtime(src_paths)
    if (is.na(sm)) return("ok")  # no sources found — cannot judge; keep cache
    if (file.info(cache_path)$mtime < sm) return("stale")
    "ok"
  }

  econ_srcs  <- list.files(here::here("data", "oerf"),
                           pattern = "\\.xlsx$", full.names = TRUE)
  nwmls_srcs <- list.files(here::here("data", "nwmls"),
                           pattern = paste0("(nwmls_housing_forecast_", scenario,
                                            "\\.xlsx|combined_nwmls_m_", scenario,
                                            ".*\\.csv)$"),
                           full.names = TRUE)
  costar_srcs <- list.files(here::here("data", "costar"),
                            pattern = paste0("costar_q_.*_", scenario, "\\.csv$"),
                            full.names = TRUE)

  st <- c(econ   = cache_status(econ_path,        econ_srcs),
          nwmls  = cache_status(nwmls_path,       nwmls_srcs),
          condo  = cache_status(nwmls_condo_path, nwmls_srcs),
          costar = cache_status(costar_path,      costar_srcs))
  for (nm in names(st))
    message("  ", format(nm, width = 6), ": ",
            switch(st[[nm]],
                   ok      = "\u2705 cache current",
                   missing = "\u2699\ufe0f  cache missing — will build",
                   stale   = "\u267b\ufe0f  STALE (source input newer) — will rebuild",
                   force   = "\u267b\ufe0f  force=TRUE — will rebuild"))

  needs_econ   <- st[["econ"]]   != "ok"
  needs_nwmls  <- st[["nwmls"]]  != "ok"
  needs_condo  <- st[["condo"]]  != "ok"
  needs_costar <- st[["costar"]] != "ok"

  if (!any(needs_econ, needs_nwmls, needs_condo, needs_costar)) {
    message("  All scenario caches current — nothing to do.",
            "  (use force=TRUE to rebuild anyway)")
    return(invisible(NULL))
  }

  # ---- Load residential panel from cache (needed by join scripts) ----------
  panel_cache <- file.path(cache_dir, "panel_tbl_res.rds")
  if (!file.exists(panel_cache))
    stop("panel_tbl_res.rds not found in cache_dir.\n",
         "Run run_main_ml(panel_replicate=TRUE) once first.")

  if (!exists("panel_tbl", envir = .GlobalEnv)) {
    message("  Loading panel_tbl_res from cache ...")
    assign("panel_tbl", readRDS(panel_cache), envir = .GlobalEnv)
    message("  \u2705 loaded panel_tbl_res")
    loaded_panel <- TRUE
  } else {
    loaded_panel <- FALSE
  }

  # ---- Run the forecast-input scripts --------------------------------------
  if (needs_econ) {
    message("  Running xx_econ_to_panel.R (", scenario, ") ...")
    source_global(here::here("scripts", "ml", "xx_econ_to_panel.R"))
  } else {
    message("  \u2705 econ cache exists: ", basename(econ_path))
  }

  if (needs_nwmls) {
    message("  Running xx_nwmls_to_panel.R (", scenario, ") ...")
    source_global(here::here("scripts", "ml", "xx_nwmls_to_panel.R"))
  } else {
    message("  \u2705 nwmls cache exists: ", basename(nwmls_path))
  }

  if (needs_condo) {
    message("  Running xx_nwmls_condo_to_panel.R (", scenario, ") ...")
    nwmls_condo_script <- here::here("scripts", "ml", "xx_nwmls_condo_to_panel.R")
    if (file.exists(nwmls_condo_script)) {
      # xx_nwmls_condo_to_panel.R requires panel_tbl_condo — load if needed
      condo_cache <- file.path(cache_dir, "panel_tbl_condo.rds")
      loaded_condo <- FALSE
      if (!exists("panel_tbl_condo", envir = .GlobalEnv) && file.exists(condo_cache)) {
        message("  Loading panel_tbl_condo from cache ...")
        assign("panel_tbl_condo", readRDS(condo_cache), envir = .GlobalEnv)
        loaded_condo <- TRUE
      } else if (!exists("panel_tbl_condo", envir = .GlobalEnv)) {
        message("  \u26a0\ufe0f  panel_tbl_condo not found — skipping condo NWMLS cache.")
        loaded_condo <- FALSE
        nwmls_condo_script <- NULL
      }
      if (!is.null(nwmls_condo_script)) {
        source_global(nwmls_condo_script)
        if (loaded_condo) {
          rm("panel_tbl_condo", envir = .GlobalEnv)
          gc(verbose = FALSE)
        }
      }
    } else {
      message("  \u2139\ufe0f  xx_nwmls_condo_to_panel.R not found — skipping condo NWMLS cache.")
    }
  } else {
    message("  \u2705 nwmls condo cache exists: ", basename(nwmls_condo_path))
  }

  # ---- Also cache CoStar for this scenario (commercial extend needs it) ----
  if (needs_costar) {
    message("  Running xx_costar_to_panel.R (", scenario, ") ...")
    costar_script <- here::here("scripts", "ml", "xx_costar_to_panel.R")
    if (file.exists(costar_script)) {
      # xx_costar_to_panel.R requires panel_tbl_com — load if needed
      com_cache <- file.path(cache_dir, "panel_tbl_com.rds")
      loaded_com <- FALSE
      if (!exists("panel_tbl_com", envir = .GlobalEnv) && file.exists(com_cache)) {
        assign("panel_tbl_com", readRDS(com_cache), envir = .GlobalEnv)
        loaded_com <- TRUE
      }
      source_global(costar_script)
      if (loaded_com) {
        rm("panel_tbl_com", envir = .GlobalEnv)
        gc(verbose = FALSE)
      }
    } else {
      message("  \u2139\ufe0f  xx_costar_to_panel.R not found — skipping CoStar cache.")
    }
  } else {
    message("  \u2705 costar cache exists: ", basename(costar_path))
  }

  # ---- Clean up panel (only drop if we loaded it here) --------------------
  if (loaded_panel) {
    rm("panel_tbl", envir = .GlobalEnv)
    gc(verbose = FALSE)
    message("  \U1f9f9 dropped panel_tbl from memory")
  }

  message("\n\u2705 prep_scenario_caches() complete for: ", scenario)
  message("  You can now run:")
  message("    run_main_ml(scenario=\'", scenario,
          "\', extend_replicate=TRUE, forecast_only=FALSE, ...)")
  invisible(NULL)
}

#

#prep_scenario_caches("optimistic")
#prep_scenario_caches("baseline")
#prep_scenario_caches("pessimistic")

#run_main_ml(scenario="optimistic", model_replicate=FALSE, extend_replicate=TRUE, prop_scope="all")
#run_main_ml(scenario="pessimistic", model_replicate=FALSE, extend_replicate=TRUE, prop_scope="condo")
#run_main_ml(scenario = "pessimistic", extend_replicate = TRUE)
# =============================================================================
# av_fcst_summary()
# =============================================================================
# Summarises total assessed value (AV) by year (2025–2031) across all three
# forecast scenarios (baseline, optimistic, pessimistic), reading from the
# cached forecast RDS files produced by the Step 6 forecast scripts.
#
# The "com" track is now a combined RDS that includes all subgroups.
# Individual subgroup summaries can also be produced by passing the subgroup
# key directly (e.g., prop_scope handled externally).
#
# Arguments:
#   prop_scope  — which property types to include: "res", "com", "condo",
#                 "both" (res+com), or "all" (res+com+condo). Default: "all"
#   scenarios   — character vector of scenarios to include.
#                 Default: c("baseline", "optimistic", "pessimistic")
#   years       — integer vector of tax years to include. Default: 2025:2031
#   export_csv  — if TRUE, write the summary table to a CSV file. Default: FALSE
#   csv_path    — path for the CSV export. If NULL, auto-generates a name in
#                 output_dir.
#   cache_dir   — where the forecast RDS caches live. Default: CFG$cache_dir
#   output_dir  — where CSV exports are written. Default: CFG$output_dir
#   digits      — rounding digits for dollar amounts in display. Default: 0
#
# Returns: a tibble with columns:
#   tax_yr, <scenario_1>, <scenario_2>, ..., and optionally diff_opt_base,
#   diff_pes_base (absolute $ differences from baseline)
# =============================================================================
av_fcst_summary <- function(
    prop_scope  = "all",
    scenarios   = c("baseline", "optimistic", "pessimistic"),
    years       = if (exists("CFG", envir = .GlobalEnv))
                    (CFG$forecast_start - 1):CFG$forecast_end
                  else 2025:2031,
    export_csv  = TRUE,
    csv_path    = NULL,
    cache_dir   = if (exists("CFG", envir = .GlobalEnv)) CFG$cache_dir
                  else here::here("data", "cache"),
    output_dir  = if (exists("CFG", envir = .GlobalEnv)) CFG$output_dir
                  else here::here("data", "outputs"),
    digits      = 0
) {
  # ---- Validate inputs -------------------------------------------------------
  valid_scopes    <- c("res", "com", "condo", "both", "all")
  valid_scenarios <- c("baseline", "optimistic", "pessimistic")

  if (!prop_scope %in% valid_scopes)
    stop("prop_scope must be one of: ", paste(valid_scopes, collapse = ", "))
  if (!all(scenarios %in% valid_scenarios))
    stop("scenarios must be subset of: ", paste(valid_scenarios, collapse = ", "))

  run_res   <- prop_scope %in% c("res", "both", "all")
  run_com   <- prop_scope %in% c("com", "both", "all")
  run_condo <- prop_scope %in% c("condo", "all")

  tracks <- c(
    if (run_res)   "res",
    if (run_com)   "com",
    if (run_condo) "condo"
  )

  message("==============================================")
  message("av_fcst_summary()")
  message("  prop_scope : ", prop_scope,
          "  (", paste(tracks, collapse = "+"), ")")
  message("  scenarios  : ", paste(scenarios, collapse = ", "))
  message("  years      : ", min(years), "-", max(years))
  message("==============================================")

  # ---- Helper: read one RDS and extract total AV by year -------------------
  read_track_av <- function(scenario, track) {
    # Always read from the scenario-specific RDS cache — never use in-memory
    # objects, since the GlobalEnv forecasted panels reflect only the last
    # run_main_ml() scenario and would silently return wrong values for others.
    fname <- paste0("panel_tbl_2006_2031_forecasted_", scenario, "_", track, ".rds")
    fpath <- file.path(cache_dir, fname)

    if (!file.exists(fpath)) {
      return(NULL)  # caller handles missing gracefully
    }
    dt <- data.table::as.data.table(readRDS(fpath))

    # Filter to requested years
    dt <- dt[tax_yr %in% years]

    # ---- Identify the AV columns by track ----------------------------------
    if (track == "res") {
      # Residential: historical years have appr_land_val / appr_imps_val,
      # forecast years have appr_land_val_filled / appr_imps_val_filled.
      # Use filled where available, fall back to observed.
      if ("appr_land_val_filled" %in% names(dt)) {
        dt[, av_land := fifelse(!is.na(appr_land_val_filled),
                                appr_land_val_filled, appr_land_val)]
        dt[, av_imps := fifelse(!is.na(appr_imps_val_filled),
                                appr_imps_val_filled, appr_imps_val)]
      } else {
        dt[, av_land := appr_land_val]
        dt[, av_imps := appr_imps_val]
      }
      dt[, av_total := fifelse(!is.na(av_land), av_land, 0) +
                       fifelse(!is.na(av_imps), av_imps, 0)]

    } else {
      # Commercial / Condo: 2025 (seed year) uses appr_land_val/appr_imps_val;
      # 2026+ uses pred_appr_land_val / pred_appr_imps_val.
      if ("pred_total_assessed" %in% names(dt)) {
        # For forecast years use pred_total_assessed; for seed year use observed
        dt[, av_total := data.table::fcase(
          !is.na(pred_total_assessed) & pred_total_assessed > 0,
            as.numeric(pred_total_assessed),
          !is.na(appr_land_val) | !is.na(appr_imps_val),
            fifelse(!is.na(appr_land_val), appr_land_val, 0) +
            fifelse(!is.na(appr_imps_val), appr_imps_val, 0),
          default = NA_real_
        )]
      } else if (all(c("appr_land_val", "appr_imps_val") %in% names(dt))) {
        dt[, av_total := fifelse(!is.na(appr_land_val), appr_land_val, 0) +
                         fifelse(!is.na(appr_imps_val), appr_imps_val, 0)]
      } else {
        warning("Could not identify AV columns for track '", track,
                "' scenario '", scenario, "'")
        return(NULL)
      }
    }

    # Sum total AV by year
    dt[, .(av = sum(av_total, na.rm = TRUE)), by = tax_yr]
  }

  # ---- Loop over scenarios and tracks, build combined table ----------------
  results <- list()

  for (sc in scenarios) {
    message("  Reading scenario: ", sc)
    sc_av <- NULL

    for (tr in tracks) {
      tr_av <- read_track_av(sc, tr)
      if (is.null(tr_av)) {
        message("    \u26a0\ufe0f  ", sc, "/", tr,
                " — cache not found, skipping track.")
        next
      }
      message("    \u2705  ", sc, "/", tr,
              " — ", nrow(tr_av), " years loaded")

      sc_av <- if (is.null(sc_av)) {
        tr_av
      } else {
        merge(sc_av, tr_av, by = "tax_yr", all = TRUE)[
          , .(tax_yr, av = rowSums(cbind(av.x, av.y), na.rm = TRUE))
        ]
      }
    }

    if (!is.null(sc_av))
      results[[sc]] <- sc_av[order(tax_yr)][tax_yr %in% years]
  }

  if (length(results) == 0)
    stop("No forecast data found. Check cache_dir and run run_main_ml() first.")

  # ---- Pivot to wide: one column per scenario ------------------------------
  summary_tbl <- Reduce(
    function(a, b) merge(a, b, by = "tax_yr", all = TRUE),
    mapply(
      function(sc, dt) {
        setNames(dt, c("tax_yr", sc))
      },
      names(results), results,
      SIMPLIFY = FALSE
    )
  )
  summary_tbl <- data.table::as.data.table(summary_tbl)[order(tax_yr)]

  # ---- Compute differences from baseline if baseline present ---------------
  if ("baseline" %in% names(summary_tbl) && length(scenarios) > 1) {
    for (sc in setdiff(scenarios, "baseline")) {
      if (sc %in% names(summary_tbl)) {
        diff_col <- paste0("diff_", sc, "_vs_baseline")
        summary_tbl[, (diff_col) := get(sc) - baseline]
      }
    }
  }

  # ---- Convert to tibble for display ---------------------------------------
  out <- tibble::as_tibble(summary_tbl)

  # ---- Print formatted summary ---------------------------------------------
  message("\n=== AV Forecast Summary (", prop_scope, ") ===")
  av_cols <- intersect(scenarios, names(out))
  print_tbl <- out
  for (col in av_cols) {
    print_tbl[[col]] <- scales::dollar(round(out[[col]], digits),
                                        scale  = 1e-9,
                                        suffix = "B",
                                        prefix = "$")
  }
  diff_cols <- grep("^diff_", names(out), value = TRUE)
  for (col in diff_cols) {
    print_tbl[[col]] <- scales::dollar(round(out[[col]], digits),
                                        scale  = 1e-9,
                                        suffix = "B",
                                        prefix = "$")
  }
  print(print_tbl)
  message("NOTE: this is NOT the certified series.  av_fcst_summary() sums the ",
          "raw parcel-model forecasts (appraised basis, no personal property, ",
          "no new construction, not calibrated to the preliminary roll).  The ",
          "certified-basis series is scripts/av_reconcile_certified.R.")

  # ---- Export to CSV if requested ------------------------------------------
  if (export_csv) {
    if (is.null(csv_path)) {
      dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
      csv_path <- file.path(
        output_dir,
        paste0("av_fcst_summary_", prop_scope, "_",
               format(Sys.Date(), "%Y%m%d"), ".csv")
      )
    }
    readr::write_csv(out, csv_path)
    message("\n\U1f4be exported: ", csv_path)
  }

  invisible(out)
}
#source(here::here("scripts","ml","00_init.R"))
#av_fcst_summary(prop_scope = "res")
#av_fcst_summary(prop_scope = "condo")
#av_fcst_summary(prop_scope = "com")
