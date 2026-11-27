# runs everything from the repo root: source("run_all.R")
# steps 01-03 read the raw data (~10 min) and are skipped once data/processed/ exists

REBUILD_DATA <- !file.exists("data/processed/analysis_panel.rds")

build <- c("R/01_build_officials.R",
           "R/02_build_covariates.R",
           "R/03_build_panel.R")

analysis <- c("R/04_descriptives.R",     # Table 1
              "R/05_validity_checks.R",
              "R/06_main_results.R",     # Table 2
              "R/07_deputy_ranks.R",
              "R/08_role_model.R",       # Tables 3 and 4
              "R/09_rd_plots.R")         # A1-A3

for (script in c(if (REBUILD_DATA) build, analysis)) {
  message("\n==> ", script)
  t0 <- Sys.time()
  source(script, local = new.env())
  message(sprintf("    done in %.0f s", as.numeric(difftime(Sys.time(), t0, units = "secs"))))
}

writeLines(capture.output(sessionInfo()), "output/session_info.txt")
