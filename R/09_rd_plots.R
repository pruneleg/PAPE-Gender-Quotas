# appendix figures A1-A3

source("R/00_config.R")

panel <- readRDS(file.path(PATH_PROCESSED, "analysis_panel.rds"))
year_data <- function(y) filter(panel, year == y)

rd_panel <- function(data, outcome, title, ylab) {
  dd <- filter(data, !is.na(.data[[outcome]]))
  p <- rdplot(y = dd[[outcome]], x = dd$population, c = CUTOFF, hide = TRUE)$rdplot
  p + labs(title = title, x = "Legal population", y = ylab) +
    theme_bw(base_size = 11) +
    theme(plot.title = element_text(size = 11))
}

# change between 2008 and the post year, stable municipalities only
first_difference <- function(outcome, post_year) {
  panel %>%
    filter(stable, year %in% c(YEAR_PLACEBO, post_year)) %>%
    select(code_commune, name_commune, year, population, all_of(outcome)) %>%
    pivot_wider(id_cols = c(code_commune, name_commune), names_from = year,
                values_from = c(population, all_of(outcome))) %>%
    transmute(population = .data[[paste0("population_", YEAR_PLACEBO)]],
              diff = .data[[paste0(outcome, "_", post_year)]] -
                     .data[[paste0(outcome, "_", YEAR_PLACEBO)]]) %>%
    filter(!is.na(diff), !is.na(population))
}

did_figure <- function(outcome, ylab, title) {
  top <- rd_panel(year_data(2014), outcome, "RD 2014 (post-reform)", ylab) |
         rd_panel(year_data(YEAR_PLACEBO), outcome, "RD 2008 (placebo)", ylab)
  bottom <- rd_panel(first_difference(outcome, 2014), "diff",
                     "Diff-in-disc (2014 − 2008)", paste("Change in", tolower(ylab)))
  (top / bottom) + plot_annotation(title = title,
                                   theme = theme(plot.title = element_text(face = "bold", size = 14)))
}

figA1 <- did_figure("share_women_councillors", "Share of female councillors", "A1 — Councillors")
figA2 <- did_figure("female_mayor", "Probability of a female mayor", "A2 — Mayors")

ylims <- range(filter(panel, year %in% YEARS_POST)$share_women_deputies, na.rm = TRUE)
figA3 <- (rd_panel(year_data(2014), "share_women_deputies", "RD 2014", "Share of female deputy mayors") +
            coord_cartesian(ylim = ylims) |
          rd_panel(year_data(2020), "share_women_deputies", "RD 2020", "Share of female deputy mayors") +
            coord_cartesian(ylim = ylims)) +
  plot_annotation(title = "A3 — Deputy mayors",
                  theme = theme(plot.title = element_text(face = "bold", size = 14)))

save_figure(figA1, "figureA1_councillors",   width = 10, height = 9)
save_figure(figA2, "figureA2_mayors",        width = 10, height = 9)
save_figure(figA3, "figureA3_deputy_mayors", width = 10, height = 4.5)
