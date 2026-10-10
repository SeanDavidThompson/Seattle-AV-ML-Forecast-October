# Reads `scenario` from .GlobalEnv (set by main_ml.R or prep_scenario_caches).
scenario_local <- get0("scenario", envir = .GlobalEnv, ifnotfound = "baseline")
message("xx_econ_to_panel.R: scenario = ", scenario_local)

econ_sheet <- switch(scenario_local,
  baseline    = "Baseline ANN",
  optimistic  = "Optimistic ANN",
  pessimistic = "Pessimistic ANN",
  stop("xx_econ_to_panel.R: unknown scenario '", scenario_local,
       "'. Must be baseline, optimistic, or pessimistic.")
)
message("  Reading OERF sheet: ", econ_sheet)

# Fixed workbook, not chosen by modified time: every scenario reads a sheet
# of this one file, so scenarios cannot mix vintages.  Update the name when
# a new OERF forecast lands.
econ_file <- here("data", "oerf",
                  "OERF_EcoForecast_KS_2026Q3_202607 2026-07-26.xlsx")
message("  OERF workbook: ", basename(econ_file))

econ_lvl_raw   <- read_xlsx(econ_file, sheet = econ_sheet, range = "A4:AR33")
econ_delta_raw <- read_xlsx(econ_file, sheet = econ_sheet, range = "A37:AR64")

econ_lvl_cln <- econ_lvl_raw %>% 
  filter(!is.na(`2031`)) %>% 
  rename(var_code = `...1`, var = `...2`) %>% 
  pivot_longer(-1:-2, names_to = "year", values_to = "lvl")

econ_delta_cln <- econ_delta_raw  %>% 
  filter(!is.na(`2031`)) %>% 
  rename(var_code = `...1`, var = `...2`) %>% 
  select(-var_code) %>% 
  left_join(econ_lvl_cln %>% 
              select(var_code, var), by = "var") %>% 
  select(var_code, everything()) %>% 
  pivot_longer(-1:-2, names_to = "year", values_to = "delta") %>% 
  distinct()


econ_lvl <- econ_lvl_cln %>%
  mutate(
    tax_yr = as.integer(year)
  ) %>%
  select(-year)

econ_delta <- econ_delta_cln %>%
  mutate(
    tax_yr = as.integer(year)
  ) %>%
  select(-year)


clean_var_name <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_replace_all("_+$", "") %>%
    str_replace_all("^_+", "")
}

econ_lvl_wide <- econ_lvl %>%
  mutate(
    var_clean = paste0("econ_", clean_var_name(var), "_lvl")
  ) %>%
  select(tax_yr, var_clean, lvl) %>%
  pivot_wider(
    names_from  = var_clean,
    values_from = lvl
  )

econ_delta_wide <- econ_delta %>%
  mutate(
    var_clean = paste0("econ_", clean_var_name(var), "_yoy")
  ) %>%
  select(tax_yr, var_clean, delta) %>%
  pivot_wider(
    names_from  = var_clean,
    values_from = delta
  )
econ_panel <- econ_lvl_wide %>%
  left_join(econ_delta_wide, by = "tax_yr") %>%
  arrange(tax_yr)

lvl_vars <- econ_lvl_wide %>%
  select(matches("^econ_.*(_lvl|_level)$")) %>%  # adjust if your suffix differs
  names()

yoy_vars <- econ_delta_wide %>%
  select(matches("^econ_.*_yoy$")) %>%           # adjust if your suffix differs
  names()


# -------------------------------------------------------
# PRODUCTION ECON PREDICTOR SETS (LOCKED)
# -------------------------------------------------------

econ_prod_predictors_yoy <- c(
  "econ_employment_thous_yoy_lag1",
  "econ_services_providing_yoy_lag1",
  "econ_population_thous_yoy_lag1",
  "econ_wholesale_and_retail_trade_yoy_lag1",
  "econ_housing_permits_thous_yoy_lag1",
  "econ_seattle_msa_s_p_corelogic_case_shilller_home_price_index_yoy_lag1",
  "econ_seattle_msa_cpi_u_1982_1984_100_yoy_lag1"
)

econ_prod_predictors_lvl <- c(
  "econ_employment_thous_lvl_lag1",
  "econ_services_providing_lvl_lag1",
  "econ_population_thous_lvl_lag1",
  "econ_seattle_msa_s_p_corelogic_case_shilller_home_price_index_lvl_lag1",
  "econ_seattle_msa_cpi_u_1982_1984_100_lvl_lag1"
)

econ_prod_predictors <- c(
  econ_prod_predictors_yoy,
  econ_prod_predictors_lvl
)

# -------------------------------------------------------
# LAG ECON PANEL (UNCHANGED)
# -------------------------------------------------------

econ_panel_lagged <- econ_panel %>%
  arrange(tax_yr) %>%
  mutate(
    across(
      all_of(lvl_vars),
      list(
        lag1 = ~ lag(.x, 1),
        lag2 = ~ lag(.x, 2)
      ),
      .names = "{.col}_{.fn}"
    )
  ) %>%
  mutate(
    across(
      all_of(yoy_vars),
      ~ lag(.x, 1),
      .names = "{.col}_lag1"
    )
  )

# -------------------------------------------------------
# SELECT ONLY LOCKED PRODUCTION VARIABLES
# -------------------------------------------------------

econ_vars_to_join <- econ_panel_lagged %>%
  select(
    tax_yr,
    any_of(econ_prod_predictors)
  )

econ_vars_forecast_to_cache <- econ_vars_to_join %>% 
  filter(tax_yr > 2025)

cache_dir <- get0("cache_dir", envir = .GlobalEnv, ifnotfound = here("data", "cache"))
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

# --- basic schema checks ---
stopifnot("tax_yr" %in% names(econ_vars_forecast_to_cache))
stopifnot(nrow(econ_vars_forecast_to_cache) > 0)

econ_fcst_out <-
  econ_vars_forecast_to_cache %>%
  arrange(tax_yr) %>%
  distinct(tax_yr, .keep_all = TRUE)

# Forecast-only cache (2026+) — used by 05_extend_panel_* scripts
scenario_local <- get0("scenario", envir = .GlobalEnv, ifnotfound = "baseline")
econ_fcst_path <- file.path(cache_dir,
  paste0("econ_fcst_2026_2031_", scenario_local, ".rds"))
saveRDS(econ_fcst_out, econ_fcst_path)
message("💾 econ forecast cached to: ", basename(econ_fcst_path))

# Full all-years cache (historical + forecast) — used by xx_combine_* to join
# econ columns onto commercial and condo panels which cover years 2000+.
econ_all_out <- econ_vars_to_join %>%
  arrange(tax_yr) %>%
  distinct(tax_yr, .keep_all = TRUE)
econ_all_path <- file.path(cache_dir,
  paste0("econ_all_years_", scenario_local, ".rds"))
saveRDS(econ_all_out, econ_all_path)
message("💾 econ all-years cached to: ", basename(econ_all_path))

# safety check (optional but recommended)
missing_vars <- setdiff(econ_prod_predictors, names(econ_vars_to_join))
if (length(missing_vars) > 0) {
  warning(
    "Missing econ production predictors: ",
    paste(missing_vars, collapse = ", ")
  )
}

# -------------------------------------------------------
# JOIN TO PANEL
# -------------------------------------------------------

panel_tbl <- panel_tbl %>%
  select(-contains("econ")) %>%
  left_join(econ_vars_to_join, by = "tax_yr")
