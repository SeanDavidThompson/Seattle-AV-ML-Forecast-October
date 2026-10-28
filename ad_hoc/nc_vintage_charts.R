# =============================================================================
# nc_vintage_charts.R
# New construction forecast review: OERF (Seattle) vs OEFA (King County),
# forecast vintages from spring 2022 to fall 2026.
#
# Input : nc_vintages_long.csv (long format; see nc_vintages_dictionary.csv)
# Output: charts/nc_review/*.png plus the tables behind them (*.csv)
#
# Charts
#   1. Forecast paths by vintage (each release a line, actuals in black)
#   2. Path to the actual: how the forecast for TY2026-2028 moved release by release
#   3. Forecast errors by horizon, and mean absolute error vs a no-change forecast
#   4. Trends: lagged construction sales vs new construction, indexed
#   5. Seattle new construction as a share of lagged construction sales
#   6. Seattle as a share of King County new construction (each OERF release
#      paired with the latest OEFA release published before it)
# =============================================================================

library(tidyverse)
library(lubridate)
library(scales)
library(here)

# ---- 0. Settings -------------------------------------------------------------
in_file      <- here("nc_vintages_long.csv")
out_dir      <- here("charts", "nc_review")
first_date   <- as.Date("2022-01-01")   # releases on/after this date
plot_years   <- 2016:2032               # x-range for the level charts
focus_years  <- 2026:2028               # target years for chart 2
index_base   <- 2016                    # chart 4 index year (= 100)
max_horizon  <- 4                       # chart 3 horizons shown (years ahead)

# variable codes in the file -> chart labels (edit if your codes differ)
var_labels <- c(
  nc_seattle                = "Seattle new construction (OERF)",
  nc_king_county            = "King County new construction (OEFA)",
  construction_sales_lagged = "Seattle construction sales, Jul-Jun window (OERF)"
)
sales_quarterly <- "construction_sales_seattle"

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# palette: categorical slots 1-3, vintage ramp (older = lighter), ink
col_cat  <- c("#2a78d6", "#eb6834", "#1baf7a")
col_ramp <- c("#86b6ef", "#2a78d6", "#0d366b")
ink      <- "#0b0b0b"
muted    <- "#898781"

theme_nc <- theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#e6e5e0", linewidth = 0.3),
    axis.text          = element_text(color = muted),
    axis.title         = element_text(color = "#52514e", size = 10),
    strip.text         = element_text(face = "bold", hjust = 0, size = 10.5),
    plot.title         = element_text(face = "bold", size = 13),
    plot.subtitle      = element_text(color = "#52514e", size = 10),
    plot.caption       = element_text(color = muted, size = 8, hjust = 0),
    legend.position    = "bottom",
    plot.background    = element_rect(fill = "#fcfcfb", color = NA)
  )

save_chart <- function(p, name, w = 10, h = 7) {
  ggsave(file.path(out_dir, paste0(name, ".png")), p, width = w, height = h, dpi = 200)
  print(p)
  invisible(p)
}
lab_b <- label_dollar(scale = 1e-9, suffix = "B", accuracy = 0.1)

# ---- 1. Load and clean -------------------------------------------------------
nc <- read_csv(in_file, show_col_types = FALSE, name_repair = "unique_quiet") |>
  select(-starts_with("...")) |>                       # empty Excel columns
  mutate(
    release_date = as_date(parse_date_time(release_date, c("mdy", "ymd"))),
    year         = as.integer(year),
    qrtr         = as.integer(qrtr),
    is_forecast  = as.logical(is_forecast)
  )

# July-June window of quarterly construction sales -> tax year (the model's driver):
# tax year T = Q3-Q4 of T-2 + Q1-Q2 of T-1
lagged <- nc |>
  filter(variable == sales_quarterly, !is.na(qrtr)) |>
  mutate(tax_year = if_else(qrtr >= 3L, year + 2L, year + 1L)) |>
  group_by(source, vintage, release_date, season, scenario, tax_year) |>
  summarise(n_q = n(), is_forecast = any(is_forecast), value = sum(value), .groups = "drop") |>
  filter(n_q == 4) |>
  transmute(source, variable = "construction_sales_lagged", vintage, release_date, season,
            scenario, year = tax_year, is_forecast, value)

# tax-year panel: annual rows + the derived window (replacing any stored copy)
ty <- nc |>
  filter(is.na(qrtr), variable != "construction_sales_lagged") |>
  select(source, variable, vintage, release_date, season, scenario, year, is_forecast, value) |>
  bind_rows(lagged) |>
  filter(variable %in% names(var_labels)) |>
  mutate(label = factor(var_labels[variable], levels = var_labels))

missing_vars <- setdiff(names(var_labels), unique(ty$variable))
if (length(missing_vars)) stop("Not found in the file: ", paste(missing_vars, collapse = ", "),
                               ". Variables present: ", paste(unique(nc$variable), collapse = ", "))

# baseline forecasts from each release in the window (OEFA publishes one forecast)
fc <- ty |>
  filter(source %in% c("OERF", "OEFA"),
         is.na(scenario) | scenario == "baseline",
         release_date >= first_date)

# actuals: source == "actual" rows where given; otherwise each year's value from
# the latest release in which that year was no longer a forecast
act_given <- ty |>
  filter(source == "actual") |>
  distinct(variable, year, .keep_all = TRUE) |>
  select(variable, year, actual = value)
act_hist <- ty |>
  filter(source != "actual", !is_forecast, is.na(scenario) | scenario == "baseline") |>
  slice_max(release_date, n = 1, with_ties = FALSE, by = c(variable, year)) |>
  select(variable, year, actual = value)
actuals <- bind_rows(act_given, anti_join(act_hist, act_given, by = c("variable", "year"))) |>
  mutate(label = factor(var_labels[variable], levels = var_labels))
last_actual <- actuals |> summarise(last = max(year), .by = variable)
print(last_actual)

# ---- Chart 1. Forecast paths by vintage --------------------------------------
# each release's forecast years, starting from its last actual year
paths <- fc |>
  group_by(variable, vintage) |>
  filter(is_forecast | year == max(c(year[!is_forecast], -Inf))) |>
  ungroup() |>
  filter(year %in% plot_years)

p1 <- ggplot() +
  geom_line(data = paths, aes(year, value, group = vintage, color = release_date),
            linewidth = 0.6, alpha = 0.9) +
  geom_line(data = filter(actuals, year %in% plot_years), aes(year, actual),
            color = ink, linewidth = 1) +
  geom_point(data = filter(actuals, year %in% plot_years), aes(year, actual),
             color = ink, size = 1.4) +
  facet_wrap(~label, ncol = 1, scales = "free_y") +
  scale_color_gradientn(colors = col_ramp, trans = "date", labels = label_date("%b %Y"),
                        name = "Release") +
  scale_y_continuous(labels = lab_b) +
  scale_x_continuous(breaks = seq(min(plot_years), max(plot_years), 2)) +
  labs(title = "Every release's baseline path, against the actuals",
       subtitle = "Black = actual (latest year may be preliminary). Darker lines = more recent releases.",
       x = "Tax year", y = NULL,
       caption = "Sources: OERF forecast vintages; OEFA (King County) forecast releases; levy worksheets.") +
  theme_nc + theme(legend.key.width = unit(2.5, "cm"))
save_chart(p1, "1_forecast_paths_by_vintage", h = 9)

# ---- Chart 2. Path to the actual for selected target years -------------------
tgt <- fc |>
  filter(year %in% focus_years, is_forecast) |>
  mutate(target = factor(paste0("TY", year)))
tgt_act <- actuals |>
  filter(year %in% focus_years) |>
  mutate(target = factor(paste0("TY", year), levels = levels(tgt$target)))
tgt_lab <- tgt |> slice_max(release_date, n = 1, by = c(label, target))

p2 <- ggplot(tgt, aes(release_date, value, color = target)) +
  geom_hline(data = tgt_act, aes(yintercept = actual, color = target),
             linetype = "dashed", linewidth = 0.5) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 2) +
  geom_text(data = tgt_lab, aes(label = target), hjust = -0.25, size = 3.2,
            color = "#52514e", show.legend = FALSE) +
  facet_wrap(~label, ncol = 1, scales = "free_y") +
  scale_color_manual(values = col_cat, name = "Target year") +
  scale_y_continuous(labels = lab_b) +
  scale_x_date(date_labels = "%b %Y", expand = expansion(mult = c(0.02, 0.08))) +
  labs(title = "How the forecast for each tax year moved from release to release",
       subtitle = "Dots = baseline forecast in each release. Dashed line = the actual, where known.",
       x = "Release date", y = NULL) +
  theme_nc
save_chart(p2, "2_path_to_actual", h = 9)

# ---- Chart 3. Errors by horizon, and vs a no-change forecast ------------------
# no-change benchmark: each release's last actual value carried forward
naive <- fc |>
  filter(!is_forecast) |>
  slice_max(year, n = 1, with_ties = FALSE, by = c(variable, vintage)) |>
  select(variable, vintage, naive = value)

errors <- fc |>
  filter(is_forecast) |>
  inner_join(select(actuals, variable, year, actual), by = c("variable", "year")) |>
  left_join(naive, by = c("variable", "vintage")) |>
  mutate(horizon   = year - year(release_date),
         err       = value / actual - 1,
         err_naive = naive / actual - 1) |>
  filter(horizon >= 0, horizon <= max_horizon)
write_csv(errors, file.path(out_dir, "3_errors_by_vintage.csv"))

p3a <- ggplot(errors, aes(factor(horizon), err)) +
  geom_hline(yintercept = 0, color = muted, linewidth = 0.4) +
  geom_jitter(aes(color = season), width = 0.15, height = 0, size = 2, alpha = 0.85) +
  stat_summary(fun = mean, geom = "crossbar", width = 0.5, linewidth = 0.3, color = ink) +
  facet_wrap(~label, nrow = 1) +
  scale_color_manual(values = c(spring = col_cat[1], summer = col_cat[2], fall = col_cat[3]),
                     breaks = c("spring", "summer", "fall"), name = "Release season") +
  scale_y_continuous(labels = label_percent()) +
  labs(title = "Forecast error by years ahead (baseline vs actual)",
       subtitle = "Each dot is one release's forecast for one tax year. Bar = mean error (bias). Above 0 = forecast too high.",
       x = "Years ahead (tax year minus release year)", y = "Error") +
  theme_nc
save_chart(p3a, "3a_errors_by_horizon", w = 12, h = 6)

mae <- errors |>
  summarise(n = n(), Forecast = mean(abs(err)), `No-change` = mean(abs(err_naive)),
            bias = mean(err), .by = c(label, horizon)) |>
  arrange(label, horizon)
write_csv(mae, file.path(out_dir, "3_mean_abs_error.csv"))
print(mae)

p3b <- mae |>
  pivot_longer(c(Forecast, `No-change`), names_to = "method", values_to = "mae") |>
  ggplot(aes(factor(horizon), mae, fill = method)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = percent(mae, accuracy = 1)), position = position_dodge(width = 0.8),
            vjust = -0.4, size = 3, color = "#52514e") +
  geom_text(data = distinct(mae, label, horizon, n), aes(factor(horizon), 0, label = paste0("n=", n)),
            inherit.aes = FALSE, vjust = 1.6, size = 2.8, color = muted) +
  facet_wrap(~label, nrow = 1) +
  scale_fill_manual(values = c(Forecast = col_cat[1], `No-change` = col_cat[2]), name = NULL) +
  scale_y_continuous(labels = label_percent(), expand = expansion(mult = c(0.08, 0.12))) +
  labs(title = "Mean absolute error: the forecast vs simply carrying the last actual forward",
       subtitle = "Lower is better. Where orange is lower, a no-change forecast would have done better.",
       x = "Years ahead", y = "Mean absolute error") +
  theme_nc
save_chart(p3b, "3b_mae_vs_no_change", w = 12, h = 6)

# ---- Chart 4. Trends, indexed ------------------------------------------------
latest <- fc |>
  filter(release_date == max(release_date), .by = variable) |>
  group_by(variable) |>
  filter(is_forecast | year == max(c(year[!is_forecast], -Inf))) |>
  ungroup() |>
  select(variable, label, year, value) |>
  mutate(kind = "Latest forecast")
trend <- bind_rows(actuals |> transmute(variable, label, year, value = actual, kind = "Actual"), latest) |>
  left_join(actuals |> filter(year == index_base) |> select(variable, base = actual), by = "variable") |>
  mutate(index = 100 * value / base) |>
  filter(year >= index_base - 4, !is.na(index))
trend_lab <- trend |> slice_max(year, n = 1, with_ties = FALSE, by = label)   # right end of each line

p4 <- ggplot(trend, aes(year, index, color = label, linetype = kind)) +
  geom_hline(yintercept = 100, color = muted, linewidth = 0.3) +
  geom_line(linewidth = 0.8) +
  geom_text(data = trend_lab, aes(label = str_wrap(label, 22)), hjust = 0, nudge_x = 0.3,
            size = 3, lineheight = 0.9, show.legend = FALSE, color = "#52514e") +
  scale_color_manual(values = setNames(col_cat, var_labels), guide = "none") +
  scale_linetype_manual(values = c(Actual = "solid", `Latest forecast` = "22"), name = NULL) +
  scale_x_continuous(breaks = seq(2008, 2036, 2), expand = expansion(mult = c(0.02, 0.22))) +
  labs(title = paste0("Construction sales vs new construction, indexed (TY", index_base, " = 100)"),
       subtitle = "Actuals solid; each series' latest release dashed.",
       x = "Tax year", y = "Index") +
  theme_nc
save_chart(p4, "4_trends_indexed", h = 6)

# ---- Chart 5. Seattle NC / lagged construction sales --------------------------
ratio_act <- actuals |>
  filter(variable %in% c("nc_seattle", "construction_sales_lagged")) |>
  select(variable, year, actual) |>
  pivot_wider(names_from = variable, values_from = actual) |>
  mutate(ratio = nc_seattle / construction_sales_lagged) |>
  filter(!is.na(ratio))
ratio_fc <- fc |>
  filter(source == "OERF", variable %in% c("nc_seattle", "construction_sales_lagged")) |>
  select(vintage, release_date, year, is_forecast, variable, value) |>
  pivot_wider(names_from = variable, values_from = value) |>
  mutate(ratio = nc_seattle / construction_sales_lagged) |>
  filter(!is.na(ratio), is_forecast)
write_csv(ratio_act, file.path(out_dir, "5_ratio_actual.csv"))
print(tail(ratio_act, 8))

p5 <- ggplot() +
  geom_line(data = ratio_fc, aes(year, ratio, group = vintage, color = release_date), linewidth = 0.5) +
  geom_line(data = ratio_act, aes(year, ratio), color = ink, linewidth = 1) +
  geom_point(data = ratio_act, aes(year, ratio), color = ink, size = 1.6) +
  geom_text(data = slice_max(ratio_act, year, n = 1),
            aes(year, ratio, label = percent(ratio, accuracy = 1)),
            vjust = 1.8, size = 3.2, color = ink) +
  scale_color_gradientn(colors = col_ramp, trans = "date", labels = label_date("%b %Y"), name = "Release") +
  scale_y_continuous(labels = label_percent()) +
  scale_x_continuous(breaks = seq(2008, 2032, 2)) +
  labs(title = "Seattle new construction as a share of lagged construction sales",
       subtitle = "Black = actual. Colored = ratio implied by each OERF release's baseline forecast.",
       x = "Tax year", y = "New construction / construction sales (Jul-Jun window)") +
  theme_nc + theme(legend.key.width = unit(2.5, "cm"))
save_chart(p5, "5_nc_to_sales_ratio", h = 6)

# ---- Chart 6. Seattle share of King County NC ---------------------------------
oerf_rel <- fc |> filter(variable == "nc_seattle", source == "OERF") |> distinct(vintage, release_date)
oefa_rel <- fc |> filter(variable == "nc_king_county", source == "OEFA") |>
  distinct(oefa_vintage = vintage, oefa_date = release_date)
pairs <- oerf_rel |>
  left_join(oefa_rel, join_by(closest(release_date >= oefa_date))) |>
  arrange(release_date)
write_csv(pairs, file.path(out_dir, "6_release_pairs.csv"))
print(pairs, n = Inf)

share_fc <- fc |>
  filter(variable == "nc_seattle", source == "OERF", is_forecast) |>
  select(vintage, release_date, year, sea = value) |>
  inner_join(pairs |> select(vintage, oefa_vintage), by = "vintage") |>
  inner_join(fc |> filter(variable == "nc_king_county") |> select(oefa_vintage = vintage, year, kc = value),
             by = c("oefa_vintage", "year")) |>
  mutate(share = sea / kc)
share_act <- actuals |>
  filter(variable %in% c("nc_seattle", "nc_king_county")) |>
  select(variable, year, actual) |>
  pivot_wider(names_from = variable, values_from = actual) |>
  mutate(share = nc_seattle / nc_king_county) |>
  filter(!is.na(share))
write_csv(share_act, file.path(out_dir, "6_share_actual.csv"))

p6 <- ggplot() +
  geom_line(data = share_fc, aes(year, share, group = vintage, color = release_date), linewidth = 0.5) +
  geom_line(data = share_act, aes(year, share), color = ink, linewidth = 1) +
  geom_point(data = share_act, aes(year, share), color = ink, size = 1.6) +
  geom_text(data = slice_max(share_act, year, n = 1),
            aes(year, share, label = percent(share, accuracy = 1)), vjust = 1.8, size = 3.2, color = ink) +
  scale_color_gradientn(colors = col_ramp, trans = "date", labels = label_date("%b %Y"), name = "OERF release") +
  scale_y_continuous(labels = label_percent(), limits = c(0, NA)) +
  labs(title = "Seattle's share of King County new construction",
       subtitle = "Black = actual. Colored = OERF baseline / OEFA forecast from the latest OEFA release before it.",
       x = "Tax year", y = NULL,
       caption = "Pairing table: 6_release_pairs.csv. OEFA figures are local new construction only (no state-assessed utility change).") +
  theme_nc + theme(legend.key.width = unit(2.5, "cm"))
save_chart(p6, "6_seattle_share_of_kc", h = 6)

message("Charts and tables written to ", out_dir)
