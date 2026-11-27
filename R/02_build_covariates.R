# covariates: INSEE census + left vote share (presidential 2nd round)

source("R/00_config.R")

# census year used for each election (INSEE series start in 2011)
CENSUS_FOR_ELECTION <- tibble(time_period = c("2011", "2011", "2016"),
                              year        = c(2008, 2014, 2020))

# 1. census: sex, occupation, age

census <- read_csv2(raw("DS_RP_POPULATION_COMP_2022_data.csv"),
                    col_types = cols(.default = "c")) %>%
  clean_names() %>%
  filter(geo_object == "COM") %>%
  mutate(obs_value = as.numeric(obs_value))

census_shares <- function(data, var, drop_total) {
  data %>%
    filter(.data[[var]] != drop_total) %>%
    group_by(geo, time_period, category = .data[[var]]) %>%
    summarise(n = sum(obs_value, na.rm = TRUE), .groups = "drop") %>%
    group_by(geo, time_period) %>%
    mutate(share = n / sum(n)) %>%
    ungroup() %>%
    select(-n) %>%
    pivot_wider(names_from = category, values_from = share,
                names_prefix = paste0("share_", var, "_"))
}

shares_sex <- census_shares(census, "sex", "_T")
shares_pcs <- census_shares(census, "pcs", "_T")   # PCS = socio-professional category
shares_age <- census_shares(census, "age", "Y_GE15")

# 2. unemployment rate

unemployment <- read_csv2(raw("DS_RP_EMPLOI_LR_PRINC_2022_data.csv"),
                          col_types = cols(.default = "c")) %>%
  clean_names() %>%
  filter(geo_object == "COM", sex == "_T") %>%   # municipalities only
  mutate(obs_value = as.numeric(obs_value)) %>%
  group_by(geo, time_period, empsta_enq) %>%
  summarise(n = sum(obs_value, na.rm = TRUE), .groups = "drop") %>%
  group_by(geo, time_period) %>%
  summarise(share_unemployed = sum(n[empsta_enq == "2"]) / sum(n), .groups = "drop")

census_covs <- shares_sex %>%
  left_join(shares_pcs,   by = c("geo", "time_period")) %>%
  left_join(shares_age,   by = c("geo", "time_period")) %>%
  left_join(unemployment, by = c("geo", "time_period")) %>%
  transmute(
    code_commune      = geo,
    time_period,
    share_women_pop   = share_sex_F,
    share_executives  = share_pcs_3,   # cadres et professions intellectuelles supérieures
    share_employees   = share_pcs_5,   # employés
    share_workers     = share_pcs_6,   # ouvriers
    share_age_15_24   = share_age_Y15T24,
    share_age_25_54   = share_age_Y25T54,
    share_age_55plus  = share_age_Y_GE55,
    unemployment_rate = share_unemployed
  ) %>%
  inner_join(CENSUS_FOR_ELECTION, by = "time_period", relationship = "many-to-many") %>%
  select(-time_period)

# 3. left vote share: Royal 2007 for 2008, Hollande 2012 for 2014 and 2020

left_share <- function(file, candidate, encoding) {
  read_delim(raw(file), delim = ";", col_types = cols(.default = "c"),
             locale = locale(encoding = encoding)) %>%
    clean_names() %>%
    filter(code_departement %in% c(sprintf("%02d", 1:95), "2A", "2B"),
           nom_du_candidat == candidate) %>%
    mutate(across(c(votants, nombre_de_voix_du_candidat), as.numeric),
           code_commune = paste0(code_departement,
                                 str_pad(code_de_la_commune, 3, pad = "0"))) %>%
    group_by(code_commune) %>%
    summarise(share_left = sum(nombre_de_voix_du_candidat) / sum(votants),
              .groups = "drop")
}

left_2007 <- left_share("Résultat présidentielle T2 2007.txt", "ROYAL",    "UTF-8")
left_2012 <- left_share("Résultat présidentielle T2 2012.txt", "HOLLANDE", "Latin1")

left_vote <- bind_rows(
  left_2007 %>% mutate(year = 2008),
  left_2012 %>% mutate(year = 2014),
  left_2012 %>% mutate(year = 2020)
)

# 4. merge and save

covariates <- full_join(census_covs, left_vote, by = c("code_commune", "year")) %>%
  arrange(code_commune, year)

saveRDS(covariates, file.path(PATH_PROCESSED, "covariates.rds"))

message("Covariates: ", nrow(covariates), " municipality-years")
covariates %>% group_by(year) %>%
  summarise(across(-code_commune, ~ round(mean(.x, na.rm = TRUE), 3))) %>% print(width = Inf)
