# Table 2 + robustness checks

source("R/00_config.R")

panel <- readRDS(file.path(PATH_PROCESSED, "analysis_panel.rds")) %>%
  filter(stable)
by_year <- split(panel, panel$year)
d <- function(year) by_year[[as.character(year)]]

COVARIATES <- c("share_women_pop", "share_executives", "share_employees",
                "unemployment_rate", "share_left")

OUTCOMES <- c(councillors   = "share_women_councillors",
              deputy_mayors = "share_women_deputies",
              mayors        = "female_mayor")

# estimation
# bw_rule: "own" = each year's optimal bw, "average" = mean of the two years

estimate <- function(outcome, post_year, covs = COVARIATES,
                     bw_rule = "own", bw_factor = 1) {
  bw_post <- optimal_bw(d(post_year), outcome)

  if (outcome == OUTCOMES["deputy_mayors"]) {
    h   <- bw_post * bw_factor
    rd  <- fit_rd(d(post_year), outcome, h = h, covs = covs)
    res <- tidy_rd(rd)
    design <- "RDD"
  } else {
    bw_pre <- optimal_bw(d(YEAR_PLACEBO), outcome)
    if (bw_rule == "average") bw_post <- bw_pre <- mean(c(bw_post, bw_pre))
    h   <- bw_post * bw_factor
    rd  <- fit_rd(d(post_year), outcome, h = h, covs = covs)
    rd0 <- fit_rd(d(YEAR_PLACEBO), outcome, h = bw_pre * bw_factor, covs = covs)
    res <- diff_in_disc(rd, rd0)
    design <- "Diff-in-Disc"
  }
  res %>% mutate(design = design, bandwidth = h, n_eff = sum(rd$N_h))
}

# control mean below the cutoff in 2008 (2014 for deputies, with the 2014 bw)
control_mean <- function(outcome, bw) {
  base_year <- if (outcome == OUTCOMES["deputy_mayors"]) 2014 else YEAR_PLACEBO
  if (outcome == OUTCOMES["deputy_mayors"]) bw <- optimal_bw(d(2014), outcome)
  d(base_year) %>%
    filter(population < CUTOFF, population >= CUTOFF - bw, !is.na(.data[[outcome]])) %>%
    summarise(m = mean(.data[[outcome]])) %>% pull(m)
}

run_specification <- function(label, ...) {
  spec <- list(...)
  expand_grid(level = names(OUTCOMES), post_year = YEARS_POST) %>%
    mutate(est = map2(level, post_year, function(lvl, yr)
      do.call(estimate, c(list(OUTCOMES[[lvl]], yr), spec)))) %>%
    unnest(est) %>%
    mutate(specification = label)
}

baseline <- run_specification("Baseline: covariates, own CCT bandwidth") %>%
  mutate(control_mean = map2_dbl(level, bandwidth, ~ control_mean(OUTCOMES[[.x]], .y)))

robustness <- bind_rows(
  baseline,
  run_specification("No covariates",                       covs = NULL),
  run_specification("Covariates without left vote share",  covs = setdiff(COVARIATES, "share_left")),
  run_specification("Average of the two years' bandwidths", bw_rule = "average"),
  run_specification("Bandwidth × 2",                   bw_factor = 2),
  run_specification("Bandwidth ÷ 2",                   bw_factor = 0.5)
)

write_csv(robustness, "output/main_estimates.csv")

# Table 2

col_name <- function(level, year) {
  i <- match(level, names(OUTCOMES)) + 3 * (year == 2020)
  sprintf("(%d) %s", i, c("Councillors", "Deputy Mayors", "Mayors")[match(level, names(OUTCOMES))])
}

table2_data <- baseline %>%
  mutate(column = col_name(level, post_year),
         is_did = design == "Diff-in-Disc") %>%
  transmute(column,
            `Diff-in-Disc`   = if_else(is_did, fmt_coef(coef, p), "—"),
            did_se           = if_else(is_did, fmt_se(se), ""),
            RDD              = if_else(!is_did, fmt_coef(coef, p), "—"),
            rdd_se           = if_else(!is_did, fmt_se(se), ""),
            `Control mean`   = formatC(control_mean, format = "f", digits = 3),
            Bandwidth        = formatC(round(bandwidth), format = "d", big.mark = ","),
            Observations     = formatC(n_eff, format = "d", big.mark = ",")) %>%
  pivot_longer(-column, names_to = "row") %>%
  pivot_wider(names_from = column, values_from = value) %>%
  mutate(row = if_else(row %in% c("did_se", "rdd_se"), "", row))

table2 <- table2_data %>%
  gt(rowname_col = "row") %>%
  tab_header(title = "Table 2 — Effect of Gender Quotas along the Municipal Hierarchy",
             subtitle = "Difference-in-discontinuities at the 1,000-inhabitant threshold, with sociodemographic controls") %>%
  tab_spanner("2014 vs. 2008", starts_with(c("(1)", "(2)", "(3)"))) %>%
  tab_spanner("2020 vs. 2008", starts_with(c("(4)", "(5)", "(6)"))) %>%
  style_reg_table(se_rows = c(2, 4), rule_rows = c(5, 6)) %>%
  tab_footnote("Deputy mayors are only recorded from 2014: simple RD estimate, to be read as an upper bound.",
               cells_column_labels(contains("Deputy"))) %>%
  tab_source_note(md("Local linear RD (rdrobust), triangular kernel, CCT-optimal bandwidth estimated separately for each year; sample restricted to municipalities that never cross the threshold. Robust bias-corrected standard errors in parentheses. Control mean: mean outcome below the threshold, within the bandwidth, in the pre-reform year. Controls: share of women in the population, share of executives, share of employees, unemployment rate, left-wing vote share. Observations: effective observations in the post-reform year. \\* p<0.1, \\*\\* p<0.05, \\*\\*\\* p<0.01"))

save_table(table2, "table2_main_results")

# robustness table

tableA <- robustness %>%
  mutate(column = col_name(level, post_year),
         cell = paste0(fmt_coef(coef, p), "<br><span style='color:grey'>", fmt_se(se), "</span>")) %>%
  select(specification, column, cell) %>%
  pivot_wider(names_from = column, values_from = cell) %>%
  gt(rowname_col = "specification") %>%
  fmt_markdown(-specification) %>%
  tab_header(title = "Robustness — Effect of Gender Quotas along the Municipal Hierarchy",
             subtitle = "Each row re-estimates Table 2 under an alternative specification") %>%
  tab_spanner("2014 vs. 2008", starts_with(c("(1)", "(2)", "(3)"))) %>%
  tab_spanner("2020 vs. 2008", starts_with(c("(4)", "(5)", "(6)"))) %>%
  tab_source_note(md("Councillors and mayors: difference-in-discontinuities relative to 2008. Deputy mayors: simple RD (the *average bandwidth* row therefore equals the baseline). Robust bias-corrected standard errors in parentheses. \\* p<0.1, \\*\\* p<0.05, \\*\\*\\* p<0.01"))

save_table(tableA, "tableA_robustness")

baseline %>%
  select(level, post_year, design, coef, se, p, control_mean, bandwidth, n_eff) %>%
  mutate(across(c(coef, se, p, control_mean), ~ round(.x, 3)), bandwidth = round(bandwidth)) %>%
  print(width = Inf)
