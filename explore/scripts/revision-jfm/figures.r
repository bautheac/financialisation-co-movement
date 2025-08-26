library(dplyr)
library(ggplot2)
library(here)
library(MetBrewer)
library(readr)
library(slituR)
library(tidyr)



tables_directory_path <- here("explore", "tables", "revision-jfm")
results_directory_path <- here::here("explore", "results", "revision-jfm")
figures_directory_path <- here("explore", "results", "figures")

table_stats_individuals_path <- paste_forward_slash(tables_directory_path, "stats-individual-assets.rds")
stats_individuals <- read_rds(table_stats_individuals_path) |>
  mutate(across(past:present, ~ as.numeric(gsub("[%*]", "", .)) / 100L))



table_estimates <- filter(stats_individuals, !stringr::str_detect(asset, "commodities"), regime == "whole period") |> group_by(estimate) |>
  summarise(
    `past vs. financialisation <` = sum(past < financialization, na.rm = TRUE),
    `past vs. financialisation >` = sum(past > financialization, na.rm = TRUE),
    `financialisation vs. crisis <` = sum(financialization < crisis, na.rm = TRUE),
    `financialisation vs. crisis >` = sum(financialization > crisis, na.rm = TRUE),
    `crisis vs. post-crisis <` = sum(crisis < present, na.rm = TRUE),
    `crisis vs. post-crisis >` = sum(crisis > present, na.rm = TRUE)
  ) |>
  pivot_longer(
    cols = -estimate, names_to = c("period", "operator"), names_pattern = "(.*)\\s([<>])", values_to = "count"
  ) |>
  pivot_wider(names_from = period, values_from = count) |>
  select(estimate, operator, `past vs. financialisation`, `financialisation vs. crisis`, `crisis vs. post-crisis`)  |>
  pivot_longer(
    cols = c(`past vs. financialisation`, `financialisation vs. crisis`, `crisis vs. post-crisis`),
    names_to = "period", values_to = "value"
  ) |>
  # dplyr::mutate(period = factor(period, levels = c("past vs. financialisation", "financialisation vs. crisis", "crisis vs. post-crisis")))
  dplyr::mutate(
    period = factor(period, levels = unique(period)), operator = paste("previous", operator, "next", sep = " ")
  ) 


figures_estimates <- ggplot(table_estimates, aes(x = period, y = value, fill = operator)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = value), position = position_stack(vjust = 0.5), color = "white", size = 3, fontface = "bold") +
  facet_wrap(~estimate, ncol = 1) +
  labs(title = "Estimate comparison from one period to the next", x = "", y = "", fill = "") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 10), plot.title = element_text(size = 12, hjust = 0.5),
    legend.position = "bottom", legend.justification = "left", 
    legend.direction = "horizontal", legend.box = "horizontal",
    legend.margin = margin(t = -10, b = 20, unit = "pt")
    ) +
  scale_fill_manual(values = met.brewer("Isfahan2", 2))

figure_estimates_path <- paste_forward_slash(figures_directory_path, "estimates.rds")
write_rds(figures_estimates, figure_estimates_path)



table_regimes <- filter(stats_individuals, !stringr::str_detect(asset, "commodities"), regime != "whole period") |> 
  pivot_longer(cols = past:present, names_to = "period", values_to = "value") |>
    dplyr::mutate(period = stringr::str_replace_all(period, c("financialization" = "financialisation", "present" = "post-crisis"))) |>
  pivot_wider(names_from = regime, values_from = value) |>
  summarise(
    `<` = sum(backwardation < contango, na.rm = TRUE), `>` = sum(backwardation > contango, na.rm = TRUE),
    .by = c("estimate", "period")
  ) |>
  pivot_longer(cols = c("<", ">"), names_to = "operator", values_to = "value") |>
  dplyr::mutate(
    period = factor(period, levels = unique(period)), operator = paste("backwardation", operator, "contango", sep = " ")
  )

figure_regimes <- ggplot(table_regimes, aes(x = period, y = value, fill = operator)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = value), position = position_stack(vjust = 0.5), color = "white", size = 3, fontface = "bold") +
  facet_wrap(~estimate, ncol = 1) +
  labs(title = expression("Estimate comparison between " * bold("CHP") * " regime phases"), x = "", y = "", fill = "") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 10), plot.title = element_text(size = 12, hjust = 0.5),
    legend.position = "bottom", legend.justification = "left", 
    legend.direction = "horizontal", legend.box = "horizontal",
    legend.margin = margin(t = -10, b = 20, unit = "pt")
    ) +
  scale_fill_manual(values = met.brewer("Isfahan2", 2))


figure_regimes_path <- paste_forward_slash(figures_directory_path, "regimes.rds")
write_rds(figure_regimes, figure_regimes_path)





table_correlations_path <- paste_forward_slash(tables_directory_path, "correlations-inner-years.rds")
correlations <- read_rds(table_correlations_path) 


table_correlations <- readr::read_rds(
  slituR::paste_forward_slash(results_directory_path, "correlations-inner.rds")
) |> dplyr::filter(
    field == "close price", type == "return", frequency == "day", 
    timespan == "year", regime == "whole period", subsector == "all"
  ) |>
    dplyr::select(country, sector, year, average) |>
    dplyr::slice_tail(n = 1L, by = c("country", "sector", "year")) |>
    dplyr::mutate(
       `asset pool` = paste(country, sector, sep = "-"),
      `asset pool` = factor(
        `asset pool`, levels = c("all-all", "US-all", "US-agriculturals", "US-energy", "US-metals", "GB-all")
      )
    ) |>
    dplyr::select(`asset pool`, year, average)


# Create a line chart for correlations
figure_correlations <- ggplot(table_correlations, aes(x = year, y = average, color = `asset pool`, group = `asset pool`)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2, show.legend = FALSE) +
  # geom_text(aes(label = round(average, 2)), vjust = -1, size = 5, fontface = "bold", show.legend = FALSE) +
  labs(title = "Average returns pairwise correlations", x = "", y = "", color = "Asset Pool") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 10),
    plot.title = element_text(size = 12, hjust = 0.5),
    legend.position = "bottom",
    legend.justification = "left",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.margin = margin(t = -10, b = 20, unit = "pt")
  ) +
  scale_color_manual(values = met.brewer("Isfahan2", length(unique(table_correlations$`asset pool`)))) +
  guides(color = guide_legend(nrow = 1)) +
  scale_x_continuous(breaks = sort(unique(table_correlations$year)))

figure_correlations_path <- paste_forward_slash(figures_directory_path, "correlations.rds")
write_rds(figure_correlations, figure_correlations_path)