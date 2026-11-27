# validity checks: density test, covariate balance, placebo cutoffs

source("R/00_config.R")

panel <- readRDS(file.path(PATH_PROCESSED, "analysis_panel.rds"))

# density test at 1000, each election

density_tests <- panel %>%
  group_split(year) %>%
  map_dfr(function(dy) {
    t <- rddensity(X = dy$population, c = CUTOFF)
    tibble(Election = as.character(dy$year[1]),
           `N left` = t$N$left, `N right` = t$N$right,
           `h left` = t$h$left, `h right` = t$h$right,
           T = t$test$t_jk, p = t$test$p_jk)
  })

tableB1 <- density_tests %>%
  mutate(T = fmt_coef(T, p), p = formatC(p, format = "f", digits = 3)) %>%
  gt() %>%
  tab_header(title = "Density test at the 1,000-inhabitant threshold",
             subtitle = "Cattaneo, Jansson & Ma (2020) manipulation test, by election") %>%
  fmt_integer(c(`N left`, `N right`)) %>%
  fmt_number(c(`h left`, `h right`), decimals = 0) %>%
  tab_spanner("Observations", starts_with("N ")) %>%
  tab_spanner("Bandwidth", starts_with("h ")) %>%
  cols_label(T = "T-statistic", p = "p-value") %>%
  tab_source_note("H0: the density of legal population is continuous at 1,000. * p<0.1, ** p<0.05, *** p<0.01")

save_table(tableB1, "tableB1_density_test")

# covariate balance

balance_vars <- c("Share women (pop.)"   = "share_women_pop",
                  "Share executives"     = "share_executives",
                  "Share employees"      = "share_employees",
                  "Share manual workers" = "share_workers",
                  "Age 15–24"       = "share_age_15_24",
                  "Age 25–54"       = "share_age_25_54",
                  "Age 55+"              = "share_age_55plus",
                  "Unemployment rate"    = "unemployment_rate",
                  "Left vote share"      = "share_left")

balance <- expand_grid(year = c(YEAR_PLACEBO, 2014), variable = names(balance_vars)) %>%
  mutate(est = map2(year, variable, ~ tidy_rd(
    fit_rd(filter(panel, year == .x), balance_vars[[.y]])))) %>%
  unnest(est)

tableB2 <- balance %>%
  mutate(cell = paste0(fmt_coef(coef, p), "<br><span style='color:grey'>", fmt_se(se), "</span>")) %>%
  select(variable, year, cell) %>%
  pivot_wider(names_from = year, values_from = cell) %>%
  gt(rowname_col = "variable") %>%
  fmt_markdown(everything()) %>%
  tab_header(title = "Covariate balance at the 1,000-inhabitant threshold",
             subtitle = "RD estimate of each pre-determined characteristic") %>%
  tab_spanner("Election", everything()) %>%
  tab_source_note(md("Local linear RD (rdrobust), CCT-optimal bandwidth, robust standard errors in parentheses. The jump in left vote share is similar in 2008 and 2014 and is therefore differenced out by the diff-in-disc (paper, footnote 1). \\* p<0.1, \\*\\* p<0.05, \\*\\*\\* p<0.01"))

save_table(tableB2, "tableB2_covariate_balance")

# placebo cutoffs (below 1000: municipalities < 1000 only; above: between 1000 and 3500)

COVARIATES <- c("share_women_pop", "share_executives", "share_employees",
                "unemployment_rate", "share_left")
PLACEBO_CUTOFFS <- c(500, 750, 1500, 2000)

placebo_estimate <- function(cutoff, outcome, post_year) {
  window <- if (cutoff < CUTOFF) c(0, CUTOFF - 1) else c(CUTOFF, 3499)
  dd <- panel %>% filter(stable, between(population, window[1], window[2]))
  dy <- function(y) filter(dd, year == y)

  rd <- fit_rd(dy(post_year), outcome, covs = COVARIATES, cutoff = cutoff,
               h = optimal_bw(dy(post_year), outcome, cutoff))
  if (outcome == "share_women_deputies") return(tidy_rd(rd))
  rd0 <- fit_rd(dy(YEAR_PLACEBO), outcome, covs = COVARIATES, cutoff = cutoff,
                h = optimal_bw(dy(YEAR_PLACEBO), outcome, cutoff))
  diff_in_disc(rd, rd0)
}

placebo <- expand_grid(cutoff = PLACEBO_CUTOFFS,
                       outcome = c(Councillors = "share_women_councillors",
                                   `Deputy mayors` = "share_women_deputies",
                                   Mayors = "female_mayor")) %>%
  mutate(level = names(outcome), post_year = 2014,
         est = pmap(list(cutoff, outcome, post_year), placebo_estimate)) %>%
  unnest(est)

tableB3 <- placebo %>%
  mutate(cell = paste0(fmt_coef(coef, p), "<br><span style='color:grey'>", fmt_se(se), "</span>"),
         cutoff = formatC(cutoff, format = "d", big.mark = ",")) %>%
  select(cutoff, level, cell) %>%
  pivot_wider(names_from = level, values_from = cell) %>%
  gt(rowname_col = "cutoff") %>%
  fmt_markdown(everything()) %>%
  tab_stubhead("Placebo cutoff") %>%
  tab_header(title = "Placebo thresholds",
             subtitle = "Table 2 specification (2014 vs. 2008) estimated at cutoffs where no rule changes") %>%
  tab_source_note(md("Councillors and mayors: diff-in-disc 2014 vs. 2008; deputy mayors: simple RD in 2014. Cutoffs below 1,000 use municipalities under 1,000 only; cutoffs above use municipalities between 1,000 and 3,499. Same controls as Table 2. \\* p<0.1, \\*\\* p<0.05, \\*\\*\\* p<0.01"))

save_table(tableB3, "tableB3_placebo_cutoffs")

print(density_tests)
print(balance %>% mutate(across(c(coef, se, p), ~ round(.x, 4))), n = Inf)
print(placebo %>% select(cutoff, level, coef, se, p) %>% mutate(across(c(coef, se, p), ~ round(.x, 3))))
