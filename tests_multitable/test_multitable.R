library(dplyr)

devtools::load_all()
load("tests_multitable/data/turnover_2var_4tabs.RData")


# Preparation of hrc files ----
load("tests_multitable/data/activity_corr_table.RData")
hrc_file_activity <- activity_corr_table %>%
  write_hrc2(file_name = "tests_multitable/hrc/activity")

load(file = "tests_multitable/data/corr_table_nuts23_fr.RData")
hrc_file_nuts <- corr_table_nuts23_fr %>%
  write_hrc2(file_name = "tests_multitable/hrc/nuts23")

options(
  rtauargus.tauargus_exe =
    "Y:/Logiciels/TauArgus/TauArgus4.2.2b1/TauArgus.exe"
)

# Ajout des tables au data/ du package
# nuts23_fr_corr_table <- corr_table_nuts23_fr %>%
#   rename_with(toupper)
#
# usethis::use_data(
#   turnover_act_size,
#   turnover_act_cj,
#   turnover_nuts_cj,
#   turnover_nuts_size,
#   nuts23_fr_corr_table,
#   activity_corr_table,
#   overwrite = TRUE
# )
#
# purrr::walk(
#   list(
#     turnover_act_size,
#     turnover_act_cj,
#     turnover_nuts_cj,
#     turnover_nuts_size,
#     nuts23_fr_corr_table,
#     activity_corr_table
#   ),
#   str
# )

# Test 1 -----
# Liste de deux tables liées

list_data_2_tabs <- list(
  act_size = turnover_act_size,
  act_cj = turnover_act_cj
) %>%
  purrr::map(
    function(df){
      df %>%
        mutate(
          is_secret_freq = N_OBS > 0 & N_OBS < 3,
          is_secret_dom = ifelse(MAX == 0, FALSE, MAX/TOT>0.85),
          is_secret_prim = is_secret_freq | is_secret_dom
        )
      # mutate(N_OBS = ifelse(N_OBS > 0 & N_OBS < 1, 1, round(N_OBS)))
    }
  )

res_1 <- tab_multi_manager(
  list_tables = list_data_2_tabs,
  list_explanatory_vars = list(
    act_size = c("ACTIVITY", "SIZE"),
    act_cj = c("ACTIVITY", "CJ")
  ),
  hrc = c(ACTIVITY = hrc_file_activity),
  dir_name = "tests_multitable/test_1/tauargus_files",
  value = "TOT",
  freq = "N_OBS",
  secret_var = "is_secret_prim",
  num_iter_max = 5,
  totcode =  "Total"
)

# controle de cohérence des masques sur les cases communes
res_1$act_size %>% filter(SIZE == "Total") %>%
  select(ACTIVITY, TOT, N_OBS, is_secret_prim, is_secret_treff = last_col()) %>%
  full_join(
    res_1$act_cj %>% filter(CJ == "Total") %>%
      select(ACTIVITY, TOT, N_OBS, is_secret_prim, is_secret_cj = last_col())
  ) %>%
  mutate(pb = is_secret_cj != is_secret_treff) %>%
  filter(pb)

# Test 2 ----

# Test avec 4 tables liées

list_data_4_tabs <- list(
  act_size = turnover_act_size,
  act_cj = turnover_act_cj,
  nuts_treff = turnover_nuts_size,
  nuts_cj = turnover_nuts_cj
) %>%
  purrr::map(
    function(df){
      df %>%
        mutate(
          is_secret_freq = N_OBS > 0 & N_OBS < 3,
          is_secret_dom = ifelse(MAX == 0, FALSE, MAX/TOT>0.85),
          is_secret_prim = is_secret_freq | is_secret_dom
        )
      # mutate(N_OBS = ifelse(N_OBS > 0 & N_OBS < 1, 1, round(N_OBS)))
    }
  )

res_2 <- tab_multi_manager(
  list_tables = list_data_4_tabs,
  list_explanatory_vars = list(
    act_size = c("ACTIVITY", "SIZE"), act_cj = c("ACTIVITY", "CJ"),
    nuts_treff = c("NUTS", "SIZE"), nuts_cj = c("NUTS", "CJ")
  ),
  hrc = c(
    ACTIVITY = hrc_file_activity,
    NUTS = hrc_file_nuts
  ),
  dir_name = "tests_multitable/test_2/tauargus_files",
  value = "TOT",
  freq = "N_OBS",
  secret_var = "is_secret_prim",
  num_iter_max = 5,
  totcode =  "Total"
)

res_2$act_size %>% filter(SIZE == "Total") %>% select(ACTIVITY, is_secret_treff = last_col()) %>%
  full_join(
    res_2$act_cj %>% filter(CJ == "Total") %>% select(ACTIVITY, is_secret_cj = last_col()),
    by = "ACTIVITY"
  ) %>%
  filter(is_secret_treff != is_secret_cj)

res_2$nuts_treff %>% filter(SIZE == "Total") %>% select(NUTS, is_secret_treff = last_col()) %>%
  full_join(
    res_2$nuts_cj %>% filter(CJ == "Total") %>% select(NUTS, is_secret_cj = last_col()),
    by = "NUTS"
  ) %>%
  filter(is_secret_treff != is_secret_cj)
