# elected officials (BRÉF for 2001-2014, RNE for 2020) -> outcomes by municipality

source("R/00_config.R")

# 1. legal populations (running variable): 1999 for 2001/2008, 2011 for 2014, 2017 for 2020

read_pop_ref <- function(file, pop_col, years) {
  # numbers are written "1,408"
  read_csv(raw(file), col_types = cols(.default = "c"), name_repair = "minimal",
           locale = locale(encoding = "Latin1")) %>%
    select(1:3) %>%
    clean_names() %>%
    transmute(code_commune = com, population = parse_number(.data[[pop_col]])) %>%
    filter(!is.na(code_commune)) %>%
    crossing(year = years)
}

pop_ref <- bind_rows(
  read_pop_ref("pop_ref_1999.csv", "psdc99", c(2001, 2008)),
  read_pop_ref("pop_ref_2011.csv", "pmun11", 2014),
  read_pop_ref("pop_ref_2017.csv", "pmun17", 2020)
)

# 2. BRÉF: councillors elected in 2001, 2008, 2014 (election days only)

bref <- read_csv(raw("bref_20200708_v1.0.2.csv"),
                 col_types = cols(.default = "c")) %>%
  clean_names() %>%
  filter(libelle_mandat == "CONSEILLER MUNICIPAL") %>%
  mutate(start_date = as.Date(date_debut_mandat, format = "%Y-%m-%d")) %>%
  filter(start_date %in% ELECTION_DATES) %>%
  transmute(
    code_commune = paste0(code_departement, code_insee_commune),
    name_commune = libelle_commune,
    year         = as.integer(format(start_date, "%Y")),
    sex          = code_sexe,
    function_raw = libelle_fonction,
    is_mayor     = function_raw %in% c("MAIRE", "MAIRE DELEGUE")
  )

# 3. RNE: councillors elected in 2020 (no by-elections, no Paris/Lyon/Marseille sectors)

rne <- read_csv2(raw("elus-conseillers-municipaux-cm.csv"),
                 col_types = cols(.default = "c")) %>%
  clean_names() %>%
  filter(is.na(code_de_la_collectivite_a_statut_particulier)) %>%
  mutate(start_date = as.Date(date_de_debut_du_mandat, format = "%d/%m/%Y")) %>%
  filter(start_date <= as.Date("2020-07-31")) %>%
  transmute(
    code_commune = code_de_la_commune,
    name_commune = toupper(libelle_de_la_commune),
    year         = as.integer(format(start_date, "%Y")),
    sex          = code_sexe,
    function_raw = toupper(libelle_de_la_fonction),
    is_mayor     = function_raw %in% "MAIRE"
  )

# 4. individual file

officials <- bind_rows(bref, rne) %>%
  mutate(
    female        = as.integer(sex == "F"),
    is_deputy     = str_detect(coalesce(function_raw, ""), "ADJOINT"),
    is_councillor = is.na(function_raw)
  ) %>%
  filter(!substr(code_commune, 1, 3) %in% OVERSEAS_DEPT)

# deputy mayor rank from the label ("1ER ADJOINT", "PREMIER ADJOINT", "2EME ADJ."...)
deputy_rank <- function(label) {
  s <- label %>% str_to_upper() %>% str_trim() %>%
    stri_trans_general("Latin-ASCII")
  ordinals <- c("PREMIER|1\\s*E?R?E?", "DEUXIEME|2\\s*E?M?E?|SECOND",
                "TROISIEME|3\\s*E?M?E?", "QUATRIEME|4\\s*E?M?E?",
                "CINQUIEME|5\\s*E?M?E?", "SIXIEME|6\\s*E?M?E?",
                "SEPTIEME|7\\s*E?M?E?", "HUITIEME|8\\s*E?M?E?",
                "NEUVIEME|9\\s*E?M?E?", "DIXIEME|10\\s*E?M?E?")
  rank <- rep(NA_integer_, length(s))
  # from 10 down to 1 so that "10E" is not read as "1E"
  for (r in rev(seq_along(ordinals))) {
    pattern <- paste0("(", ordinals[r], ")")
    hit <- is.na(rank) & !is.na(s) &
      ((str_detect(s, pattern) & str_detect(s, "ADJ")) |
         (r <= 5 & str_detect(s, paste0("^", pattern, "$"))))
    rank[hit] <- r
  }
  # labels starting with a number
  leading <- str_extract(s, "^\\d+")
  rank[is.na(rank) & !is.na(leading)] <- as.integer(leading[is.na(rank) & !is.na(leading)])
  rank
}

officials <- officials %>%
  mutate(rank_deputy = if_else(is_mayor, NA_integer_, deputy_rank(function_raw)))

# 5. aggregate by municipality and year

safe_mean <- function(x) if (length(x) > 0) mean(x) else NA_real_

municipal <- officials %>%
  group_by(code_commune, name_commune, year) %>%
  summarise(
    female_mayor            = if (any(is_mayor)) max(female[is_mayor]) else NA_real_,
    share_women_council     = mean(female),
    share_women_deputies    = safe_mean(female[is_deputy]),
    share_women_councillors = safe_mean(female[is_councillor]),
    any_female_deputy       = if (any(is_deputy)) max(female[is_deputy]) else 0,
    n_council               = n(),
    n_deputies              = sum(is_deputy),
    n_councillors           = sum(is_councillor),
    .groups = "drop"
  ) %>%
  left_join(pop_ref, by = c("code_commune", "year")) %>%
  mutate(treated = as.integer(population >= CUTOFF)) %>%
  arrange(code_commune, year)

officials <- officials %>%
  left_join(pop_ref, by = c("code_commune", "year"))

# 6. save

dir.create(PATH_PROCESSED, showWarnings = FALSE, recursive = TRUE)
saveRDS(officials, file.path(PATH_PROCESSED, "officials_individual.rds"))
saveRDS(municipal, file.path(PATH_PROCESSED, "officials_municipal.rds"))

message("Officials: ", nrow(officials), " rows | municipality-years: ", nrow(municipal))
print(count(municipal, year))
