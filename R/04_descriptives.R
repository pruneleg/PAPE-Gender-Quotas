# Table 1

source("R/00_config.R")

panel <- readRDS(file.path(PATH_PROCESSED, "analysis_panel.rds"))

descriptives <- panel %>%
  filter(year %in% c(YEAR_PLACEBO, YEARS_POST)) %>%
  mutate(side = if_else(treated == 1, "≥ 1,000 inhabitants", "< 1,000 inhabitants")) %>%
  group_by(year, side) %>%
  summarise(
    N                 = n(),
    mayors            = 100 * mean(female_mayor,            na.rm = TRUE),
    deputy_mayors     = 100 * mean(share_women_deputies,    na.rm = TRUE),
    councillors       = 100 * mean(share_women_councillors, na.rm = TRUE),
    municipal_council = 100 * mean(share_women_council,     na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(year, side)

table1 <- descriptives %>%
  mutate(year = as.character(year)) %>%
  gt(groupname_col = "year") %>%
  tab_header(title = "Table 1 — Descriptive statistics",
             subtitle = "Municipalities with at most 10,000 inhabitants") %>%
  cols_label(side = "Population", N = "N", mayors = "Mayors",
             deputy_mayors = "Deputy mayors", councillors = "Councillors",
             municipal_council = "Whole council") %>%
  fmt_integer(N) %>%
  fmt_number(c(mayors, deputy_mayors, councillors, municipal_council), decimals = 1) %>%
  tab_spanner("Share of women (%)",
              c(mayors, deputy_mayors, councillors, municipal_council)) %>%
  tab_source_note("Sources: BRÉF (2008, 2014), RNE (2020), INSEE legal populations.
                   Deputy mayors are poorly recorded in 2008 (see Section III).")

save_table(table1, "table1_descriptives")
print(descriptives)
