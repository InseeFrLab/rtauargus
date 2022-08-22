library(dplyr)

devtools::load_all()
load("tests_multitable/data/red_vegetables_4_linked_tabs.RData")


options(
  rtauargus.tauargus_exe =
    "Y:/Logiciels/TauArgus/TauArgus4.2.2b1/TauArgus.exe"
)

traiter <- function(
  tabular,
  files_name = "link",
  dir_name = "tests_multitable/tauargus_files",
  explanatory_vars,
  hrc,
  totcode = "Ensemble",
  secret_var
){
  res <- tab_rtauargus(
    tabular = tabular,
    files_name = files_name,
    dir_name = dir_name,
    explanatory_vars = explanatory_vars,
    hrc = hrc,
    totcode = rep(totcode, length(explanatory_vars)),
    secret_var = secret_var,
    cost_var = NULL,
    value          = "tot",
    freq           = "n_obs",
    maxscore       = "max",
    safety_rules = "MAN(0)",
    output_type        = "4",
    output_options     = "",
    is_tabular = TRUE,
    show_batch_console = FALSE,
    import = FALSE,
    separator = ",",
    verbose = FALSE,
    unif_hrc = TRUE,
    unif_expl = TRUE
  ) %>%
    mutate(
      is_secret = Status != "V"
    )

  print(res %>% count(Status))

  return(res %>% select(-Status))
}


# res <- traiter(
#   list_data_2_tabs$act_treff,
#   explanatory_vars = c("ACTIVITY","treff"),
#   hrc = c(ACTIVITY = "tests_multitable/legumes.hrc"),
#   secret_var = "is_secret_prim"
# )

# Test 1 -----
# Liste de deux tables liées

list_data_2_tabs <- list(
  act_treff = red_vegetables_act_treff,
  act_cj = red_vegetables_act_cj
) %>%
  purrr::map(
    function(df){
      df %>%
        mutate(
          is_secret_freq = n_obs > 0 & n_obs < 3,
          is_secret_dom = ifelse(max == 0, FALSE, max/tot>0.85),
          is_secret_prim = is_secret_freq | is_secret_dom
        ) %>%
        mutate(n_obs = ifelse(n_obs > 0 & n_obs < 1, 1, round(n_obs)))
    }
  )


res_1 <- multi_linked_tables(
  liste_tbx = list_data_2_tabs,
  list_explanatory_vars = list(act_treff = c("ACTIVITY", "treff"), act_cj = c("ACTIVITY", "cj")),
  list_hrc = list(
    act_treff = c(ACTIVITY = "tests_multitable/legumes.hrc"),
    act_cj = c(ACTIVITY = "tests_multitable/legumes.hrc")
  ),
  value = "tot",
  freq = "n_obs",
  maxscore = "max",
  is_secret_primaire = "is_secret_prim",
  num_iter_max = 5,
  totcode =  "Ensemble",
  indice_depart_tableau = 1
)


# Test 2 ----

# Test avec 4 tables liées

list_data_4_tabs <- list(
  act_treff = red_vegetables_act_treff,
  act_cj = red_vegetables_act_cj,
  nuts_treff = red_vegetables_nuts_treff,
  nuts_cj = red_vegetables_nuts_cj
) %>%
  purrr::map(
    function(df){
      df %>%
        mutate(
          is_secret_freq = n_obs > 0 & n_obs < 3,
          is_secret_dom = ifelse(max == 0, FALSE, max/tot>0.85),
          is_secret_prim = is_secret_freq | is_secret_dom
        ) %>%
        mutate(n_obs = ifelse(n_obs > 0 & n_obs < 1, 1, round(n_obs)))
    }
  )

load(file = "tests_multitable/data/corr_table_nuts23_fr.RData")
file_nuts_hrc <- corr_table_nuts23_fr %>% write_hrc2("nuts23", dir_name = "tests_multitable")

res_2 <- multi_linked_tables(
  liste_tbx = list_data_4_tabs,
  list_explanatory_vars = list(
    act_treff = c("ACTIVITY", "treff"), act_cj = c("ACTIVITY", "cj"),
    nuts_treff = c("NUTS", "treff"), nuts_cj = c("NUTS", "cj")
  ),
  list_hrc = list(
    act_treff = c(ACTIVITY = "tests_multitable/legumes.hrc"),
    act_cj = c(ACTIVITY = "tests_multitable/legumes.hrc"),
    nuts_treff = c(NUTS = file_nuts_hrc),
    nuts_cj = c(NUTS = file_nuts_hrc)
  ),
  value = "tot",
  freq = "n_obs",
  maxscore = "max",
  is_secret_primaire = "is_secret_prim",
  num_iter_max = 20,
  totcode =  "Ensemble",
  indice_depart_tableau = 1
)


res_2$act_treff %>% filter(treff == "Ensemble") %>% select(ACTIVITY, is_secret_treff = last_col()) %>%
  full_join(
    res_2$act_cj %>% filter(cj == "Ensemble") %>% select(ACTIVITY, is_secret_cj = last_col()),
    by = "ACTIVITY"
  ) %>%
  filter(is_secret_treff != is_secret_cj)

res_2$nuts_treff %>% filter(treff == "Ensemble") %>% select(NUTS, is_secret_treff = last_col()) %>%
  full_join(
    res_2$nuts_cj %>% filter(cj == "Ensemble") %>% select(NUTS, is_secret_cj = last_col()),
    by = "NUTS"
  ) %>%
  filter(is_secret_treff != is_secret_cj)
