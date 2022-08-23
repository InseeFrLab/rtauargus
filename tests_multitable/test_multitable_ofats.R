library(dplyr)

devtools::load_all()

options(
  rtauargus.tauargus_exe =
    "Y:/Logiciels/TauArgus/TauArgus4.2.2b1/TauArgus.exe"
)

rep_X <- "X:/HAB-Traitement-Confidentialite/Ofats/OFATS 2020/tests"
rep_inputs <- file.path(rep_X, "inputs")
rep_traits <- file.path(rep_X, "traitements")
rep_hrc <- file.path(rep_traits, "hrc")
rep_res <- file.path(rep_X, "resultats")

p <- lapply(
  list(rep_X, rep_inputs, rep_traits, rep_hrc, rep_res),
  function(rep){
    if(!dir.exists(rep)) dir.create(rep)
  }
)

nace <- readxl::read_xls(
  file.path(rep_inputs, "Nomenclatures.xls"),
  sheet = "NACE"
)
str(nace)

table_passage = nace %>%
  select(6:2) %>%
  mutate(across(.fns = ~gsub("F$","",.x))) %>%
  unique()
racine = "ZZZ"

file_nace_hrc <- nace %>%
  select(6:2) %>%
  mutate(across(.fns = ~gsub("F$","",.x))) %>%
  unique() %>%
  arrange(across(CODE5:CODE1)) %>%
  write_hrc2(
    output_name = "nace",
    dir_name = rep_hrc,
    sort_table = FALSE,
    adjust_unique_roots = TRUE
  )

# Création de deux hiérarchies non emboîtées des pays ####

pays <- readxl::read_xls(
  file.path(rep_inputs, "Nomenclatures.xls"),
  sheet = "PAYS"
) %>%
  mutate(
    across(everything(), ~gsub(" ","_",.))
  )
str(pays)

file_zone_hrc <- pays %>%
  select(zone, PAYS) %>%
  unique() %>%
  arrange(across(zone:PAYS)) %>%
  write_hrc2(
    output_name = "zone",
    dir_name = rep_hrc,
    sort_table = FALSE,
    adjust_unique_roots = TRUE
  )

file_off_hrc <- pays %>%
  select(Offshore, PAYS) %>%
  unique() %>%
  write_hrc2(
    output_name = "offshore",
    dir_name = rep_hrc,
    sort_table = FALSE,
    adjust_unique_roots = TRUE
  )

# Constitution des différents tableaux ####

ofats <- haven::read_sas(file.path(rep_inputs, "ofats_2020_pr_app_secret_V4.sas7bdat")) %>%
  filter(stringr::str_ends(NACE, pattern = "F$", negate = TRUE)) %>%
  mutate(
    across(PAYS, ~gsub(" ","_",.))
  )
ofats <- ofats %>%
  bind_rows(
    ofats %>% group_by(PAYS, NOM_VAR) %>%
      summarise(
        VAL_ABS = sum(VAL_ABS),
        FREQ = sum(FREQ),
        MAX_VAL_ABS = max(MAX_VAL_ABS),
        .groups = "drop"
    ) %>%
      mutate(NACE = "Total", diffusion = 1)
  )


poser_secret_primaire <- function(df, var = "ENT"){
  df %>%
    {if(var == "ENT"){
      mutate(
        .,
        is_secret_freq = FREQ > 0 & FREQ < 3 & (diffusion == 1),
        is_secret_dom = FALSE
      )} else{
        mutate(
          .,
          is_secret_freq = FREQ > 0 & FREQ < 3 & (diffusion == 1),
          is_secret_dom = VAL_ABS > 0 & MAX_VAL_ABS/VAL_ABS > 0.85 & (diffusion == 1)
        )}} %>%
    mutate(is_secret_prim = is_secret_freq | is_secret_dom) %>%
    mutate(FREQ = ifelse(FREQ > 0 & FREQ < 1, 1, round(FREQ)))
}

constituer_tableaux_par_var <- function(data = ofats, type_var){
  ofats_sel <- ofats %>%
    filter(NOM_VAR == type_var) %>%
    poser_secret_primaire(var = type_var)

  var_liste <- list()
  var_liste[["zone"]] <- ofats_sel %>%
    filter(PAYS %in% c(pays$PAYS, pays$zone, "A1")) %>%
    select(PAYS, NACE, VAL_ABS, MAX_VAL_ABS, FREQ, diffusion, starts_with("is_secret"))
  var_liste[["off"]] <- ofats_sel %>%
    filter(PAYS %in% c(pays$PAYS, pays$Offshore, "A1")) %>%
    select(PAYS, NACE, VAL_ABS, MAX_VAL_ABS, FREQ, diffusion, starts_with("is_secret"))
  # lapply(var_liste, str)

  return(var_liste)
}

all_vars <- ofats$NOM_VAR %>% unique() ##################TODO#########
all_tableaux <- setNames(lapply(all_vars, constituer_tableaux_par_var, data = ofats), all_vars)

# Pose du Secret secondaire ####

all_masques <- purrr::map(
  all_vars,
  function(var){
    cat("GESTION TABLEAUX ", var, "\n")
    list_data_2_tabs <- list(
      t_zone = all_tableaux[[var]]$zone,
      t_off = all_tableaux[[var]]$off
    )
    res <- multi_linked_tables(
      liste_tbx = list_data_2_tabs,
      list_explanatory_vars = list(t_zone = c("PAYS", "NACE"), t_off = c("PAYS", "NACE")),
      list_hrc = list(
        t_zone = c(PAYS = file_zone_hrc, NACE = file_nace_hrc),
        t_off = c(PAYS = file_off_hrc, NACE = file_nace_hrc)
      ),
      dir_name = file.path(rep_traits, var, "tauargus_files"),
      totcode =  c(PAYS = "A1", NACE = "Total"),
      value = "VAL_ABS",
      freq = "FREQ",
      secret_var = "is_secret_prim",
      num_iter_max = 20,
      ip_start = 1,
      ip_end = 0
    )
  }
)

res$t_zone %>% mutate(status = case_when(is_secret_prim ~ "AB", is_secret_7 ~ "D", TRUE ~ "V")) %>%
  count(status)

controle <- list()

controle[["t_zone"]] <- read.csv(
  file.path("X:\\HAB-Traitement-Confidentialite\\Ofats\\OFATS 2020\\traitements\\fichiers_tauargus\\EMP\\zone.csv"),
  header = FALSE,
  col.names = c(c("PAYS", "NACE"), "VAL_ABS", "FREQ", "Status","Dom"),
  colClasses = c(rep("character", 2), rep("numeric",2), "character", "numeric"),
  na.strings = "",
  stringsAsFactors = FALSE
)

controle$t_zone %>% count(Status)

fus <- controle$t_zone %>% select(1:2,Status) %>% unique() %>%
  full_join(
    res$t_zone %>% select(1:3, last_col()))

nrow(fus)

fus %>%
  filter(is.na(Status)) %>%
  nrow()
fus %>%
  filter(is.na(is_secret_7)) %>%
  nrow()
fus %>%  filter(is_secret_7 & Status == "V")


all_masques <- setNames(all_masques, all_vars)
