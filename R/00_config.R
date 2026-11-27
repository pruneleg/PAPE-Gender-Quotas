# packages, paths and helper functions used by all scripts

suppressPackageStartupMessages({
  library(tidyverse)
  library(janitor)
  library(stringi)
  library(rdrobust)    # RD estimates
  library(rddensity)   # density test
  library(gt)
  library(patchwork)
})

# paths (raw data not on github: set QUOTAS_DATA_RAW or put the files in data/raw/)
PATH_RAW       <- Sys.getenv("QUOTAS_DATA_RAW", unset = "data/raw")
PATH_PROCESSED <- "data/processed"
PATH_TABLES    <- "output/tables"
PATH_FIGURES   <- "output/figures"

raw <- function(file) file.path(PATH_RAW, file)

# constants
CUTOFF        <- 1000    # 2013 reform threshold
MAX_POP       <- 10000
YEAR_PLACEBO  <- 2008    # last election without quota at 1000
YEARS_POST    <- c(2014, 2020)
OVERSEAS_DEPT <- c("971", "972", "973", "974", "976")  # excluded: metropolitan France only

# election dates (both rounds), to drop by-elections
ELECTION_DATES <- as.Date(c(
  "2001-03-11", "2001-03-18",
  "2008-03-09", "2008-03-16",
  "2014-03-23", "2014-03-30"
))

# formatting
stars <- function(p) {
  case_when(p < 0.01 ~ "***", p < 0.05 ~ "**", p < 0.10 ~ "*", TRUE ~ "")
}

fmt_coef <- function(coef, p) paste0(formatC(coef, format = "f", digits = 3), stars(p))
fmt_se   <- function(se) paste0("(", formatC(se, format = "f", digits = 3), ")")

# RD helpers

rd_sample <- function(data, outcome, covs = NULL) {
  data %>% filter(!is.na(.data[[outcome]]), !is.na(population),
                  if_all(all_of(covs %||% character()), ~ !is.na(.)))
}

# optimal bandwidth (without covariates)
optimal_bw <- function(data, outcome, cutoff = CUTOFF) {
  d <- rd_sample(data, outcome)
  rdrobust(y = d[[outcome]], x = d$population, c = cutoff)$bws["h", "left"]
}

fit_rd <- function(data, outcome, h = NULL, covs = NULL, cutoff = CUTOFF) {
  d <- rd_sample(data, outcome, covs)
  args <- list(y = d[[outcome]], x = d$population, c = cutoff)
  if (!is.null(h))    args$h    <- h
  if (!is.null(covs)) args$covs <- as.matrix(d[, covs])
  do.call(rdrobust, args)
}

# diff-in-disc = beta_post - beta_2008, independent samples so se = sqrt(se1^2 + se2^2)
# est: 1 = conventional, 2 = bias-corrected (se always robust)
diff_in_disc <- function(rd_post, rd_placebo, est = 1) {
  coef <- rd_post$coef[est] - rd_placebo$coef[est]
  se   <- sqrt(rd_post$se[3]^2 + rd_placebo$se[3]^2)
  tibble(coef = coef, se = se, p = 2 * pnorm(-abs(coef / se)))
}

tidy_rd <- function(rd, est = 1) {
  coef <- rd$coef[est]
  se   <- rd$se[3]
  tibble(coef = coef, se = se, p = 2 * pnorm(-abs(coef / se)))
}

# saving
save_table <- function(gt_tbl, name) {
  gtsave(gt_tbl, file.path(PATH_TABLES, paste0(name, ".html")))
  tryCatch(gtsave(gt_tbl, file.path(PATH_TABLES, paste0(name, ".png")), expand = 10),
           error = function(e) message("PNG export skipped for ", name, ": ", conditionMessage(e)))
  invisible(gt_tbl)
}

save_figure <- function(plot, name, width = 9, height = 5.5) {
  ggsave(file.path(PATH_FIGURES, paste0(name, ".png")), plot,
         width = width, height = height, dpi = 300, bg = "white")
  invisible(plot)
}

style_reg_table <- function(gt_tbl, se_rows, rule_rows) {
  gt_tbl %>%
    tab_style(cell_text(color = "grey40", style = "italic"),
              cells_body(rows = se_rows)) %>%
    tab_style(cell_borders(sides = "top", color = "black", weight = px(1)),
              cells_body(rows = rule_rows))
}
