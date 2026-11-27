# Tables 3 and 4 (role model)

source("R/00_config.R")

panel <- readRDS(file.path(PATH_PROCESSED, "analysis_panel.rds")) %>%
  filter(stable)

LEVELS <- c(Councillors     = "share_women_councillors",
            `Deputy Mayors` = "share_women_deputies",
            Mayors          = "female_mayor")

# est: 1 = conventional, 2 = bias-corrected / se_row: 1 = conventional, 3 = robust
# defaults give Table 3, Table 4 uses est = 1, se_row = 1, vce = "hc1"
estimate_subsample <- function(data, est = 2, se_row = 3, vce = "nn") {
  d14 <- filter(data, year == 2014)
  d08 <- filter(data, year == YEAR_PLACEBO)
  rd  <- function(dd, outcome) {
    s <- rd_sample(dd, outcome)
    fit <- rdrobust(y = s[[outcome]], x = s$population, c = CUTOFF)
    if (vce == "nn") return(fit)
    # Table 4: bw chosen with the default vce, then re-estimated with hc1
    rdrobust(y = s[[outcome]], x = s$population, c = CUTOFF,
             h = fit$bws["h", "left"], vce = vce)
  }
  imap_dfr(LEVELS, function(outcome, level) {
    r14 <- rd(d14, outcome)
    if (outcome == LEVELS[["Deputy Mayors"]]) {
      coef <- r14$coef[est]; se <- r14$se[se_row]
      tibble(level, design = "RDD", coef, se,
             bw_2008 = NA_real_, bw_2014 = r14$bws[1, 1],
             n_total = sum(r14$N), n_eff = sum(r14$N_h))
    } else {
      r08  <- rd(d08, outcome)
      coef <- r14$coef[est] - r08$coef[est]
      se   <- sqrt(r14$se[se_row]^2 + r08$se[se_row]^2)
      tibble(level, design = "Diff-in-Disc", coef, se,
             bw_2008 = r08$bws[1, 1], bw_2014 = r14$bws[1, 1],
             n_total = sum(r14$N), n_eff = sum(r14$N_h))
    }
  }) %>% mutate(p = 2 * pnorm(-abs(coef / se)))
}

role_model_table <- function(results, stats_rows, title, footnote) {
  cells <- results %>%
    mutate(is_did = design == "Diff-in-Disc",
           column = sprintf("(%d) %s", row_number(), level)) %>%
    transmute(group, column,
              `Diff-in-Disc` = if_else(is_did, fmt_coef(coef, p), "—"),
              did_se         = if_else(is_did, fmt_se(se), ""),
              RDD            = if_else(!is_did, fmt_coef(coef, p), "—"),
              rdd_se         = if_else(!is_did, fmt_se(se), ""),
              !!!stats_rows)
  wide <- cells %>%
    select(-group) %>%
    pivot_longer(-column, names_to = "row") %>%
    pivot_wider(names_from = column, values_from = value) %>%
    mutate(row = if_else(row %in% c("did_se", "rdd_se"), "", row))

  tbl <- gt(wide, rowname_col = "row") %>%
    tab_header(title = title, subtitle = "2014 vs. 2008 — RD at the 1,000-inhabitant threshold")
  for (g in unique(cells$group)) {
    tbl <- tab_spanner(tbl, g, columns = cells$column[cells$group == g])
  }
  tbl %>%
    style_reg_table(se_rows = c(2, 4), rule_rows = 5) %>%
    tab_style(cell_fill(color = "grey97"), cells_body(columns = contains("Deputy"))) %>%
    tab_footnote("Deputy mayors are only recorded from 2014: simple RD estimate.",
                 cells_column_labels(contains("Deputy"))) %>%
    tab_source_note(md(footnote))
}

fmt_int <- function(x) if_else(is.na(x), "—", formatC(round(x), format = "d", big.mark = ","))

# Table 3

groups_t3 <- c("Previous female mayor (t−1)" = 1, "No previous female mayor (t−1)" = 0)

table3_results <- imap_dfr(groups_t3, ~ estimate_subsample(filter(panel, prev_female_mayor == .x)) %>%
                             mutate(group = .y))

table3 <- role_model_table(
  table3_results,
  stats_rows = rlang::exprs(Bandwidth    = fmt_int(if_else(is_did, (bw_2008 + bw_2014) / 2, bw_2014)),
                     Observations = fmt_int(n_total)),
  title = "Table 3 — Role Model Effect: Heterogeneity by Previous Female Mayor",
  footnote = "Split by the gender of the mayor elected at the previous election. Each year uses its own CCT-optimal bandwidth (diff-in-disc columns report the average of the 2008 and 2014 bandwidths). Bias-corrected estimates, robust standard errors in parentheses. Observations: municipalities in the 2014 subsample. \\* p<0.1, \\*\\* p<0.05, \\*\\*\\* p<0.01"
)
save_table(table3, "table3_previous_female_mayor")

# Table 4

dept_context <- panel %>%
  group_by(department, year) %>%
  # share of female mayors in the département, excluding the municipality itself
  mutate(share_female_mayors_dept = (sum(female_mayor, na.rm = TRUE) - female_mayor) / (n() - 1)) %>%
  group_by(code_commune) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(share_female_mayors_dept_lag = lag(share_female_mayors_dept)) %>%
  ungroup() %>%
  filter(year %in% c(YEAR_PLACEBO, YEARS_POST), !is.na(share_female_mayors_dept_lag)) %>%
  mutate(tercile = cut(share_female_mayors_dept_lag,
                       breaks = c(-Inf, quantile(share_female_mayors_dept_lag, c(1, 2) / 3), Inf),
                       labels = c("Low female leadership (bottom tercile)",
                                  "Medium female leadership (middle tercile)",
                                  "High female leadership (top tercile)")))

table4_results <- map_dfr(levels(dept_context$tercile), function(t) {
  estimate_subsample(filter(dept_context, tercile == t), est = 1, se_row = 1, vce = "hc1") %>%
    mutate(group = t)
})

table4 <- role_model_table(
  table4_results,
  stats_rows = rlang::exprs(`Bandwidth (2008)` = fmt_int(bw_2008),
                     `Bandwidth (2014)` = fmt_int(bw_2014),
                     Observations       = fmt_int(n_eff)),
  title = "Table 4 — Département Role Model Effect: Heterogeneity by Female Leadership Context",
  footnote = "Female leadership context: share of female mayors in the département at the previous election, excluding the municipality itself; terciles computed on the pooled sample. Each year uses its own CCT-optimal bandwidth. Conventional estimates with HC1 standard errors in parentheses. Observations: effective observations in 2014. \\* p<0.1, \\*\\* p<0.05, \\*\\*\\* p<0.01"
)
save_table(table4, "table4_department_terciles")

bind_rows(table3_results, table4_results) %>%
  select(group, level, coef, se, p, bw_2008, bw_2014, n_total, n_eff) %>%
  mutate(across(c(coef, se, p), ~ round(.x, 3)), across(starts_with("bw"), round),
         group = substr(group, 1, 28)) %>%
  print(n = Inf, width = Inf)
