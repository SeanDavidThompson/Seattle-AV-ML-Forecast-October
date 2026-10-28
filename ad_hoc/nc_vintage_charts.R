# NC forecast review - OERF vs OEFA vintages, 2022-2026
# reads nc_vintages_long.csv, writes pngs + tables to charts/nc_review/

library(tidyverse)
library(lubridate)
library(scales)
library(here)

out_dir <- here("charts", "nc_review")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

start_date  <- as.Date("2022-01-01")
plot_years  <- 2016:2032
focus_years <- 2026:2028
index_base  <- 2016
max_h       <- 4

vars <- c(nc_seattle                = "Seattle NC (OERF)",
          nc_king_county            = "King County NC (OEFA)",
          construction_sales_lagged = "Constr. sales, Jul-Jun (OERF)")

theme_set(theme_bw(base_size = 11) +
            theme(legend.position = "bottom", panel.grid.minor = element_blank()))
vint_col <- scale_colour_gradient(low = "grey80", high = "darkblue", trans = "date", name = "Release")

save_png <- function(p, name, w = 10, h = 6) {
  ggsave(file.path(out_dir, paste0(name, ".png")), p, width = w, height = h, dpi = 150)
  print(p)
}

# ---- data ----
nc <- read_csv(here("nc_vintages_long.csv"), show_col_types = FALSE, name_repair = "unique_quiet") |>
  select(-starts_with("...")) |>
  mutate(release_date = as_date(parse_date_time(release_date, c("mdy", "ymd"))),
         year = as.integer(year), qrtr = as.integer(qrtr),
         is_forecast = as.logical(is_forecast))

# lagged construction sales: TY T = Q3-Q4 of T-2 + Q1-Q2 of T-1
lagged <- nc |>
  filter(variable == "construction_sales_seattle", !is.na(qrtr)) |>
  mutate(tax_year = if_else(qrtr >= 3L, year + 2L, year + 1L)) |>
  group_by(source, vintage, release_date, season, scenario, tax_year) |>
  summarise(n_q = n(), is_forecast = any(is_forecast), value = sum(value), .groups = "drop") |>
  filter(n_q == 4) |>
  transmute(source, variable = "construction_sales_lagged", vintage, release_date, season,
            scenario, year = tax_year, is_forecast, value)

ty <- nc |>
  filter(is.na(qrtr), variable != "construction_sales_lagged") |>
  select(source, variable, vintage, release_date, season, scenario, year, is_forecast, value) |>
  bind_rows(lagged) |>
  filter(variable %in% names(vars)) |>
  mutate(label = factor(vars[variable], levels = vars))

# baseline only (OEFA has one forecast)
fc <- ty |>
  filter(source %in% c("OERF", "OEFA"), is.na(scenario) | scenario == "baseline",
         release_date >= start_date)

# actuals - source == "actual" if there, else latest release where the year isn't a forecast
act1 <- ty |> filter(source == "actual") |> distinct(variable, year, .keep_all = TRUE) |>
  select(variable, year, actual = value)
act2 <- ty |>
  filter(source != "actual", !is_forecast, is.na(scenario) | scenario == "baseline") |>
  slice_max(release_date, n = 1, with_ties = FALSE, by = c(variable, year)) |>
  select(variable, year, actual = value)
actuals <- bind_rows(act1, anti_join(act2, act1, by = c("variable", "year"))) |>
  mutate(label = factor(vars[variable], levels = vars))
actuals |> summarise(last_actual = max(year), .by = variable) |> print()

# ---- 1. forecast paths by vintage ----
paths <- fc |>
  group_by(variable, vintage) |>
  filter(is_forecast | year == max(c(year[!is_forecast], -Inf))) |>
  ungroup() |>
  filter(year %in% plot_years)

p1 <- ggplot() +
  geom_line(data = paths, aes(year, value / 1e9, group = vintage, colour = release_date)) +
  geom_line(data = filter(actuals, year %in% plot_years), aes(year, actual / 1e9), linewidth = 1) +
  geom_point(data = filter(actuals, year %in% plot_years), aes(year, actual / 1e9)) +
  facet_wrap(~label, ncol = 1, scales = "free_y") +
  vint_col +
  labs(title = "Baseline forecasts by release vs actual (black)", x = "Tax year", y = "$B") +
  theme(legend.key.width = unit(2, "cm"))
save_png(p1, "1_paths_by_vintage", h = 9)

# ---- 2. forecast for TY2026-28 by release ----
tgt <- fc |> filter(year %in% focus_years, is_forecast) |> mutate(target = factor(year))
tgt_act <- actuals |> filter(year %in% focus_years) |> mutate(target = factor(year))

p2 <- ggplot(tgt, aes(release_date, value / 1e9, colour = target)) +
  geom_hline(data = tgt_act, aes(yintercept = actual / 1e9, colour = target), linetype = 2) +
  geom_line() +
  geom_point() +
  facet_wrap(~label, ncol = 1, scales = "free_y") +
  labs(title = "Forecast for each tax year, by release date (dashed = actual)",
       x = "Release date", y = "$B", colour = "Tax year")
save_png(p2, "2_target_year_by_release", h = 9)

# ---- 3. errors by horizon + no-change benchmark ----
naive <- fc |>
  filter(!is_forecast) |>
  slice_max(year, n = 1, with_ties = FALSE, by = c(variable, vintage)) |>
  select(variable, vintage, naive = value)

errors <- fc |>
  filter(is_forecast) |>
  inner_join(select(actuals, variable, year, actual), by = c("variable", "year")) |>
  left_join(naive, by = c("variable", "vintage")) |>
  mutate(horizon = year - year(release_date),
         err = value / actual - 1,
         err_naive = naive / actual - 1) |>
  filter(between(horizon, 0, max_h))
write_csv(errors, file.path(out_dir, "errors.csv"))

p3a <- ggplot(errors, aes(factor(horizon), err)) +
  geom_hline(yintercept = 0) +
  geom_jitter(aes(colour = factor(season, levels = c("spring", "summer", "fall"))),
              width = 0.15, height = 0, size = 2) +
  stat_summary(fun = mean, geom = "crossbar", width = 0.5) +
  facet_wrap(~label) +
  scale_y_continuous(labels = percent) +
  labs(title = "Forecast error by horizon (+ = too high, bar = mean)",
       x = "Years ahead", y = "Error", colour = "Release")
save_png(p3a, "3a_errors_by_horizon", w = 11)

mae <- errors |>
  summarise(n = n(), forecast = mean(abs(err)), no_change = mean(abs(err_naive)),
            bias = mean(err), .by = c(label, horizon)) |>
  arrange(label, horizon)
write_csv(mae, file.path(out_dir, "mae.csv"))
print(mae)

p3b <- mae |>
  pivot_longer(c(forecast, no_change), names_to = "method", values_to = "mae") |>
  ggplot(aes(factor(horizon), mae, fill = method)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = percent(mae, 1)), position = position_dodge(0.9), vjust = -0.3, size = 3) +
  facet_wrap(~label) +
  scale_y_continuous(labels = percent) +
  labs(title = "MAE: forecast vs no-change (last actual carried forward)",
       x = "Years ahead", y = "MAE", fill = NULL)
save_png(p3b, "3b_mae_vs_no_change", w = 11)

# ---- 4. indexed trends ----
latest <- fc |>
  filter(release_date == max(release_date), .by = variable) |>
  group_by(variable) |>
  filter(is_forecast | year == max(c(year[!is_forecast], -Inf))) |>
  ungroup() |>
  transmute(variable, label, year, value, type = "latest forecast")

trend <- actuals |>
  transmute(variable, label, year, value = actual, type = "actual") |>
  bind_rows(latest) |>
  left_join(actuals |> filter(year == index_base) |> select(variable, base = actual), by = "variable") |>
  mutate(index = 100 * value / base) |>
  filter(year >= index_base - 4)

p4 <- ggplot(trend, aes(year, index, colour = label, linetype = type)) +
  geom_hline(yintercept = 100, colour = "grey50") +
  geom_line(linewidth = 0.8) +
  labs(title = paste0("Indexed, TY", index_base, " = 100"), x = "Tax year", y = NULL,
       colour = NULL, linetype = NULL) +
  guides(colour = guide_legend(nrow = 1))
save_png(p4, "4_indexed")

# ---- 5. NC / lagged construction sales ----
ratio_act <- actuals |>
  filter(variable %in% c("nc_seattle", "construction_sales_lagged")) |>
  select(variable, year, actual) |>
  pivot_wider(names_from = variable, values_from = actual) |>
  mutate(ratio = nc_seattle / construction_sales_lagged) |>
  filter(!is.na(ratio))
write_csv(ratio_act, file.path(out_dir, "ratio_actual.csv"))

# ratio for each release's NC forecast years (sales window can already be actual)
ratio_fc <- fc |>
  filter(variable == "nc_seattle", is_forecast) |>
  select(vintage, release_date, year, nc = value) |>
  inner_join(fc |> filter(variable == "construction_sales_lagged") |>
               select(vintage, year, sales = value), by = c("vintage", "year")) |>
  mutate(ratio = nc / sales)

p5 <- ggplot() +
  geom_hline(yintercept = mean(ratio_act$ratio), linetype = 2, colour = "grey50") +
  geom_line(data = ratio_fc, aes(year, ratio, group = vintage, colour = release_date)) +
  geom_line(data = ratio_act, aes(year, ratio), linewidth = 1) +
  geom_point(data = ratio_act, aes(year, ratio)) +
  vint_col +
  scale_y_continuous(labels = percent) +
  labs(title = "Seattle NC / lagged construction sales (black = actual, dashed = avg)",
       x = "Tax year", y = NULL) +
  theme(legend.key.width = unit(2, "cm"))
save_png(p5, "5_nc_sales_ratio")

# ---- 6. Seattle share of KC NC ----
# pair each OERF release with the latest OEFA release before it
pairs <- fc |> filter(variable == "nc_seattle") |> distinct(vintage, release_date) |>
  left_join(fc |> filter(variable == "nc_king_county") |>
              distinct(oefa_vintage = vintage, oefa_date = release_date),
            join_by(closest(release_date >= oefa_date))) |>
  arrange(release_date)
write_csv(pairs, file.path(out_dir, "release_pairs.csv"))
print(pairs, n = Inf)

share_fc <- fc |>
  filter(variable == "nc_seattle", is_forecast) |>
  select(vintage, release_date, year, sea = value) |>
  inner_join(select(pairs, vintage, oefa_vintage), by = "vintage") |>
  inner_join(fc |> filter(variable == "nc_king_county") |> select(oefa_vintage = vintage, year, kc = value),
             by = c("oefa_vintage", "year")) |>
  mutate(share = sea / kc)

share_act <- actuals |>
  filter(variable %in% c("nc_seattle", "nc_king_county")) |>
  select(variable, year, actual) |>
  pivot_wider(names_from = variable, values_from = actual) |>
  mutate(share = nc_seattle / nc_king_county) |>
  filter(!is.na(share))

p6 <- ggplot() +
  geom_line(data = share_fc, aes(year, share, group = vintage, colour = release_date)) +
  geom_line(data = share_act, aes(year, share), linewidth = 1) +
  geom_point(data = share_act, aes(year, share)) +
  vint_col +
  scale_y_continuous(labels = percent) +
  labs(title = "Seattle share of King County NC (black = actual)",
       subtitle = "forecast = OERF release / latest OEFA release before it",
       x = "Tax year", y = NULL) +
  theme(legend.key.width = unit(2, "cm"))
save_png(p6, "6_seattle_share_kc")
