# analysis panel: municipalities <= 10,000 inhab., stable treatment status, lags

source("R/00_config.R")

municipal  <- readRDS(file.path(PATH_PROCESSED, "officials_municipal.rds"))
covariates <- readRDS(file.path(PATH_PROCESSED, "covariates.rds"))

panel <- municipal %>%
  filter(population <= MAX_POP) %>%
  left_join(covariates, by = c("code_commune", "year")) %>%
  # stable = never crosses 1000 between elections
  group_by(code_commune) %>%
  mutate(stable = n_distinct(treated) == 1) %>%
  # mayor elected at the previous election (role model)
  arrange(year, .by_group = TRUE) %>%
  mutate(prev_female_mayor = lag(female_mayor)) %>%
  ungroup() %>%
  mutate(department = substr(code_commune, 1, 2)) %>%
  relocate(code_commune, name_commune, department, year, population, treated, stable)

saveRDS(panel, file.path(PATH_PROCESSED, "analysis_panel.rds"))
write_csv(panel, file.path(PATH_PROCESSED, "analysis_panel.csv"))

message("Analysis panel: ", nrow(panel), " municipality-years")
print(count(panel, year, treated))
print(count(panel, year, stable))
