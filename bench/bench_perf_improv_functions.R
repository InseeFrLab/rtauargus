# =============================================================================
# Test comparatif – Grosses données – Mode SMART (cas d'usage réel)
# Version officielle (package) vs version locale optimisée
# =============================================================================

true_package = TRUE # Warning : restart R before editing this value
if (true_package){
  library(rtauargus)      # package originale
} else  {
  devtools::load_all("Z:/rtauargus") # path où est la version split_perf de github : https://github.com/InseeFrLab/rtauargus/tree/split_perf_improvement
}

library(dplyr)
library(sdcHierarchies)
library(microbenchmark)

# ----------------------------- Chargement des sources locales optimisées
local_R_dir <- "Z:/rtauargus/R"   # <-- adapter au besoin
source(file.path(local_R_dir, "sp_var_to_merge.R"))
source(file.path(local_R_dir, "sp_from_4_to_3.R"))
source(file.path(local_R_dir, "sp_from_4_to_3_case_0_hr.R"))
source(file.path(local_R_dir, "sp_from_4_to_3_case_1_hr.R"))
source(file.path(local_R_dir, "sp_from_4_to_3_case_2_hr.R"))
source(file.path(local_R_dir, "sp_from_5_to_3.R"))
source(file.path(local_R_dir, "sp_reduce_dims.R"))
source(file.path(local_R_dir, "sp_restore_format.R"))

# =============================================================================
# 1. Création de gros jeux de données avec hiérarchies profondes
# =============================================================================

# --- 4D volumineux (ACT hiérarchique, GEO hiérarchique, SEX & AGE non hiérarchiques)
create_big_4D <- function(n = 50000) {
  set.seed(123)
  data <- data.frame(
    ACT  = sample(c("Total", paste0("A", 1:20), paste0("A", 1:20, "_", 1:5)), n, replace = TRUE),
    GEO  = sample(c("Total", "G1", "G2", "G1a", "G1b", "G2a", "G2b"), n, replace = TRUE),
    SEX  = sample(c("Total", "F", "M"), n, replace = TRUE),
    AGE  = sample(c("Total", "AGE1", "AGE2", "AGE3", "AGE4"), n, replace = TRUE),
    stringsAsFactors = FALSE
  )
  data$VALUE <- 1

  # Hiérarchie ACT : 20 groupes, chacun avec 5 sous‑niveaux
  hrc_act <- tempfile(fileext = ".hrc")
  sdcHierarchies::hier_create(root = "Total", nodes = paste0("A", 1:20)) %>%
    {
      for (i in 1:20) {
        . <- sdcHierarchies::hier_add(., root = paste0("A", i), nodes = paste0("A", i, "_", 1:5))
      }
      .
    } %>%
    sdcHierarchies::hier_convert(as = "argus") %>%
    slice(-1) %>%
    mutate(levels = substring(paste0(level, name), 3)) %>%
    select(levels) %>%
    write.table(file = hrc_act, row.names = FALSE, col.names = FALSE, quote = FALSE)

  # Hiérarchie GEO : 2 régions, chacune avec 2 sous‑régions
  hrc_geo <- tempfile(fileext = ".hrc")
  sdcHierarchies::hier_create(root = "Total", nodes = c("G1", "G2")) %>%
    sdcHierarchies::hier_add(root = "G1", nodes = c("G1a", "G1b")) %>%
    sdcHierarchies::hier_add(root = "G2", nodes = c("G2a", "G2b")) %>%
    sdcHierarchies::hier_convert(as = "argus") %>%
    slice(-1) %>%
    mutate(levels = substring(paste0(level, name), 3)) %>%
    select(levels) %>%
    write.table(file = hrc_geo, row.names = FALSE, col.names = FALSE, quote = FALSE)

  list(data = data,
       hrcfiles = c(ACT = hrc_act, GEO = hrc_geo),
       totcode  = c(SEX = "Total", AGE = "Total", GEO = "Total", ACT = "Total"))
}

# --- 5D volumineux (ACT hiérarchique profond, GEO hiérarchique, autres non hiérarchiques)
create_big_5D <- function(n = 80000) {
  set.seed(234)
  data <- data.frame(
    ACT  = sample(c("Total_A", paste0("A", 1:20, "_"), paste0("A", 1:20, "_", 1:5)), n, replace = TRUE),
    GEO  = sample(c("Total_G", "GA","GB","GA1","GA2","GB1","GB2","GA3","GB3"), n, replace = TRUE),
    SEX  = sample(c("Total_S", "F", "M", "F1", "F2", "M1", "M2"), n, replace = TRUE),
    AGE  = sample(c("Ensemble", "AGE1", "AGE2", "AGE11", "AGE12", "AGE21", "AGE22"), n, replace = TRUE),
    ECO  = sample(c("PIB", "Ménages", "Entreprises"), n, replace = TRUE),
    stringsAsFactors = FALSE
  )
  data$VALUE <- 1

  # Hiérarchie ACT (5D) : 20 groupes, chaque avec 5 sous‑niveaux
  hrc_act <- tempfile(fileext = ".hrc")
  sdcHierarchies::hier_create(root = "Total_A", nodes = paste0("A", 1:20, "_")) %>%
    {
      for (i in 1:20) {
        . <- sdcHierarchies::hier_add(., root = paste0("A", i, "_"), nodes = paste0("A", i, "_", 1:5))
      }
      .
    } %>%
    sdcHierarchies::hier_convert(as = "argus") %>%
    slice(-1) %>%
    mutate(levels = substring(paste0(level, name), 3)) %>%
    select(levels) %>%
    write.table(file = hrc_act, row.names = FALSE, col.names = FALSE, quote = FALSE)

  # Hiérarchie GEO (5D) : 2 régions, 3‑4 sous‑régions chacune
  hrc_geo <- tempfile(fileext = ".hrc")
  sdcHierarchies::hier_create(root = "Total_G", nodes = c("GA", "GB")) %>%
    sdcHierarchies::hier_add(root = "GA", nodes = c("GA1", "GA2", "GA3")) %>%
    sdcHierarchies::hier_add(root = "GB", nodes = c("GB1", "GB2", "GB3")) %>%
    sdcHierarchies::hier_convert(as = "argus") %>%
    slice(-1) %>%
    mutate(levels = substring(paste0(level, name), 3)) %>%
    select(levels) %>%
    write.table(file = hrc_geo, row.names = FALSE, col.names = FALSE, quote = FALSE)

  list(data = data,
       hrcfiles = c(ACT = hrc_act, GEO = hrc_geo),
       totcode  = c(SEX = "Total_S", AGE = "Ensemble", GEO = "Total_G", ACT = "Total_A", ECO = "PIB"))
}

# =============================================================================
# 2. Fonctions de validation et de benchmark
# =============================================================================

check_equality <- function(res_orig, res_opt) {
  if (is.list(res_orig) && is.list(res_opt)) {
    if (length(res_orig) != length(res_opt)) return(FALSE)
    return(all(mapply(check_equality, res_orig, res_opt, SIMPLIFY = TRUE)))
  } else if (is.data.frame(res_orig)) {
    return(identical(res_orig, res_opt))
  } else {
    return(identical(res_orig, res_opt))
  }
}

validate <- function(desc, expr_orig, expr_opt) {
  cat("\n--- Validation :", desc, "---\n")

  # On capture les erreurs sous forme d'objets R "error" exploitables
  res_o <- tryCatch(expr_orig, error = function(e) e)
  res_n <- tryCatch(expr_opt, error = function(e) e)

  has_error_o <- inherits(res_o, "error")
  has_error_n <- inherits(res_n, "error")

  # Si l'une des versions a levé une erreur
  if (has_error_o || has_error_n) {
    cat("Une erreur est survenue lors de l'exécution :\n")
    if (has_error_o) {
      cat("  - Original : Erreur ->", res_o$message, "\n")
    } else {
      cat("  - Original : Succès (Type :", class(res_o)[1], ")\n")
    }
    if (has_error_n) {
      cat("  - Optimisé : Erreur ->", res_n$message, "\n")
    } else {
      cat("  - Optimisé : Succès (Type :", class(res_n)[1], ")\n")
    }
    return(FALSE)
  }

  # Si les deux versions ont réussi, on compare l'égalité logique
  eq <- check_equality(res_o, res_n)
  if (eq) {
    cat("Résultats identiques.\n")
  } else {
    cat("Résultats DIFFÉRENTS.\n")
  }
  return(eq)
}

all_benchmarks <- list()

clear_cache <- function() {
  if (exists(".hrc_cache", envir = .GlobalEnv)) {
    rm(list = ls(envir = .hrc_cache), envir = .hrc_cache)
  }
}

bench <- function(name_o, name_n, expr_o, expr_n, times = 20) {
  expr_o_sub <- substitute(expr_o)
  expr_n_sub <- substitute(expr_n)

  # Création d'expressions composées avec vidage du cache
  expr_o_clean <- bquote({ clear_cache(); .(expr_o_sub) })
  expr_n_clean <- bquote({ clear_cache(); .(expr_n_sub) })

  res <- eval(bquote(microbenchmark::microbenchmark(
    original = .(expr_o_clean),
    optimise = .(expr_n_clean),
    times = .(times)
  )))
  print(res, unit = "ms")
  all_benchmarks[[paste(name_o, "vs", name_n)]] <<- res
  invisible(res)
}

# =============================================================================
# 3. Exécution des tests
# =============================================================================

# ---- Initialisation des gros jeux de données ----
cat("Création du jeu 4D volumineux...\n")
big4 <- create_big_4D(50000)
dfs4 <- big4$data; tot4 <- big4$totcode; hrc4 <- big4$hrcfiles

cat("Création du jeu 5D volumineux...\n")
big5 <- create_big_5D(80000)
dfs5 <- big5$data; tot5 <- big5$totcode; hrc5 <- big5$hrcfiles

# ---- 3.1 length_tabs (tests de bas niveau avec variables imposées) ----
cat("\n========== length_tabs ==========\n")

validate("length_tabs 4D ACT x SEX",
         rtauargus:::length_tabs(dfs4, "ACT", "SEX", totcode = tot4, hrcfiles = hrc4),
         length_tabs(dfs4, "ACT", "SEX", totcode = tot4, hrcfiles = hrc4))
bench("length_tabs_4D_ACT_SEX", "length_tabs_4D_ACT_SEX_opt",
      rtauargus:::length_tabs(dfs4, "ACT", "SEX", totcode = tot4, hrcfiles = hrc4),
      length_tabs(dfs4, "ACT", "SEX", totcode = tot4, hrcfiles = hrc4), times = 5)

validate("length_tabs 5D ACT/GEO/AGE/ECO",
         rtauargus:::length_tabs(dfs5, "ACT", "GEO", "AGE", "ECO", totcode = tot5, hrcfiles = hrc5),
         length_tabs(dfs5, "ACT", "GEO", "AGE", "ECO", totcode = tot5, hrcfiles = hrc5))
bench("length_tabs_5D_ACT_GEO_AGE_ECO", "length_tabs_5D_ACT_GEO_AGE_ECO_opt",
      rtauargus:::length_tabs(dfs5, "ACT", "GEO", "AGE", "ECO", totcode = tot5, hrcfiles = hrc5),
      length_tabs(dfs5, "ACT", "GEO", "AGE", "ECO", totcode = tot5, hrcfiles = hrc5), times = 5)

# ---- 3.2 var_to_merge (smart automatique) ----
cat("\n========== var_to_merge (smart) ==========\n")

validate("var_to_merge 4D smart",
         rtauargus:::var_to_merge(dfs4, tot4, hrc4, nb_var = 2, nb_tab_option = 'smart', limit = 2000),
         var_to_merge(dfs4, tot4, hrc4, nb_var = 2, nb_tab_option = 'smart', limit = 2000))
bench("var_to_merge_4D_smart", "var_to_merge_4D_smart_opt",
      rtauargus:::var_to_merge(dfs4, tot4, hrc4, nb_var = 2, nb_tab_option = 'smart', limit = 2000),
      var_to_merge(dfs4, tot4, hrc4, nb_var = 2, nb_tab_option = 'smart', limit = 2000), times = 5)

validate("var_to_merge 5D 4var smart",
         rtauargus:::var_to_merge(dfs5, tot5, hrc5, nb_var = 4, nb_tab_option = 'smart', limit = 5000),
         var_to_merge(dfs5, tot5, hrc5, nb_var = 4, nb_tab_option = 'smart', limit = 5000))
bench("var_to_merge_5D_4var_smart", "var_to_merge_5D_4var_smart_opt",
      rtauargus:::var_to_merge(dfs5, tot5, hrc5, nb_var = 4, nb_tab_option = 'smart', limit = 5000),
      var_to_merge(dfs5, tot5, hrc5, nb_var = 4, nb_tab_option = 'smart', limit = 5000), times = 5)

validate("var_to_merge 5D 3var smart",
         rtauargus:::var_to_merge(dfs5, tot5, hrc5, nb_var = 3, nb_tab_option = 'smart', limit = 5000),
         var_to_merge(dfs5, tot5, hrc5, nb_var = 3, nb_tab_option = 'smart', limit = 5000))
bench("var_to_merge_5D_3var_smart", "var_to_merge_5D_3var_smart_opt",
      rtauargus:::var_to_merge(dfs5, tot5, hrc5, nb_var = 3, nb_tab_option = 'smart', limit = 5000),
      var_to_merge(dfs5, tot5, hrc5, nb_var = 3, nb_tab_option = 'smart', limit = 5000), times = 5)

# ---- 3.3 reduce_dims 4D en mode SMART (cas réel) ----
cat("\n========== reduce_dims 4D SMART ==========\n")

validate("reduce_dims 4D smart (limit=2000, pas d'over_split)",
         rtauargus:::reduce_dims(dfs4, "tab", tot4, hrc4,
                                 vars_to_merge = NULL,
                                 nb_tab_option = "smart", limit = 2000,
                                 over_split = FALSE, sep_dir = TRUE, hrc_dir = tempdir()),
         reduce_dims(dfs4, "tab", tot4, hrc4,
                     vars_to_merge = NULL,
                     nb_tab_option = "smart", limit = 2000,
                     over_split = FALSE, sep_dir = TRUE, hrc_dir = tempdir()))
bench("reduce_dims_4D_smart", "reduce_dims_4D_smart_opt",
      rtauargus:::reduce_dims(dfs4, "tab", tot4, hrc4,
                              vars_to_merge = NULL,
                              nb_tab_option = "smart", limit = 2000,
                              over_split = FALSE, sep_dir = TRUE, hrc_dir = tempdir()),
      reduce_dims(dfs4, "tab", tot4, hrc4,
                  vars_to_merge = NULL,
                  nb_tab_option = "smart", limit = 2000,
                  over_split = FALSE, sep_dir = TRUE, hrc_dir = tempdir()),
      times = 5)   # 10 répétitions seulement car déjà très lourd

# ---- 3.4 reduce_dims 5D en mode SMART (cas réel) ----
cat("\n========== reduce_dims 5D SMART ==========\n")

validate("reduce_dims 5D smart (limit=5000, pas d'over_split)",
         rtauargus:::reduce_dims(dfs5, "tab", tot5, hrc5,
                                 vars_to_merge = NULL,
                                 nb_tab_option = "smart", limit = 5000,
                                 over_split = FALSE, sep_dir = TRUE, hrc_dir = tempdir()),
         reduce_dims(dfs5, "tab", tot5, hrc5,
                     vars_to_merge = NULL,
                     nb_tab_option = "smart", limit = 5000,
                     over_split = FALSE, sep_dir = TRUE, hrc_dir = tempdir()))
bench("reduce_dims_5D_smart", "reduce_dims_5D_smart_opt",
      rtauargus:::reduce_dims(dfs5, "tab", tot5, hrc5,
                              vars_to_merge = NULL,
                              nb_tab_option = "smart", limit = 5000,
                              over_split = FALSE, sep_dir = TRUE, hrc_dir = tempdir()),
      reduce_dims(dfs5, "tab", tot5, hrc5,
                  vars_to_merge = NULL,
                  nb_tab_option = "smart", limit = 5000,
                  over_split = FALSE, sep_dir = TRUE, hrc_dir = tempdir()),
      times = 5)

cat("\n===== Fin des tests. =====\n")

# =============================================================================
# RÉCAPITULATIF FINAL (tous les benchmarks en ms)
# =============================================================================
cat("\n\n========== RÉCAPITULATIF DE TOUS LES BENCHMARKS (ms) ==========\n")
for (nom in names(all_benchmarks)) {
  cat("\n---", nom, "---\n")
  print(all_benchmarks[[nom]], unit = "ms")
}


# -------------------------------------------------------------------------

# =============================================================================
# Three-Way Comparison Script: Real Output vs. Package Version vs. Local Version
# Verifying table counts and sorted sizes for 3 non-hierarchical variables
# =============================================================================

run_three_way_comparison <- function() {
  cat("\n=======================================================================\n")
  cat("          THREE-WAY COMPARISON: REAL vs. PACKAGE vs. LOCAL             \n")
  cat("=======================================================================\n")

  # ---------------------------------------------------------------------------
  # TEST CASE 1: n_mod_v1 = 3, n_mod_v2 = 4, n_mod_v3 = 3
  # Unmerged variables multiplier (V4 x V5) = 2 x 2 = 4
  # ---------------------------------------------------------------------------
  cat("\n--- Test Case 1: n_mod_v1 = 3, n_mod_v2 = 4, n_mod_v3 = 3 ---\n")

  test_data_1 <- expand.grid(
    V1 = c("Total", "A1", "A2"),              # n_mod_v1 = 3
    V2 = c("Total", "B1", "B2", "B3"),        # n_mod_v2 = 4
    V3 = c("Total", "C1", "C2"),              # n_mod_v3 = 3
    V4 = c("Total", "D1"),                    # n_mod_v4 = 2
    V5 = c("Total", "E1"),                    # n_mod_v5 = 2
    stringsAsFactors = FALSE
  )
  test_data_1$VALUE <- 1
  totcode_1 <- c(V1 = "Total", V2 = "Total", V3 = "Total", V4 = "Total", V5 = "Total")

  # 1. Real ground truth: count and sizes of generated data frames
  real_res_1 <- reduce_dims(
    dfs = test_data_1,
    dfs_name = "test1",
    totcode = totcode_1,
    hrcfiles = NULL,
    vars_to_merge = c("V1", "V2", "V3"),
    sep_dir = TRUE,
    hrc_dir = tempdir()
  )
  real_sizes_1 <- sort(sapply(real_res_1$tabs, nrow))
  real_count_1 <- length(real_sizes_1)

  # 2. Package estimation (rtauargus original buggy formula)
  pkg_sizes_1 <- sort(unlist(rtauargus:::length_tabs(
    dfs = test_data_1,
    v1 = "V1", v2 = "V2", v3 = "V3",
    totcode = totcode_1,
    hrcfiles = NULL
  )))
  pkg_count_1 <- length(pkg_sizes_1)

  # 3. Local estimation (corrected formula)
  local_sizes_1 <- sort(unlist(length_tabs(
    dfs = test_data_1,
    v1 = "V1", v2 = "V2", v3 = "V3",
    totcode = totcode_1,
    hrcfiles = NULL
  )))
  local_count_1 <- length(local_sizes_1)

  # Display results for Test Case 1
  cat("\n[Table Count Summary]\n")
  cat("  - Real generated tables :", real_count_1, "\n")
  cat("  - Package prediction    :", pkg_count_1, " (Error:", pkg_count_1 - real_count_1, ")\n")
  cat("  - Local prediction      :", local_count_1, " (Error:", local_count_1 - real_count_1, ")\n")

  cat("\n[Sorted Table Sizes Comparison]\n")
  cat("  - Real actual sizes     :", paste(real_sizes_1, collapse = ", "), "\n")
  cat("  - Package predicted     :", paste(pkg_sizes_1, collapse = ", "), "\n")
  cat("  - Local predicted       :", paste(local_sizes_1, collapse = ", "), "\n")

  # ---------------------------------------------------------------------------
  # TEST CASE 2: n_mod_v1 = 4, n_mod_v2 = 3, n_mod_v3 = 4
  # Unmerged variables multiplier (V4 x V5) = 2 x 3 = 6
  # ---------------------------------------------------------------------------
  cat("\n-----------------------------------------------------------------------\n")
  cat("\n--- Test Case 2: n_mod_v1 = 4, n_mod_v2 = 3, n_mod_v3 = 4 ---\n")

  test_data_2 <- expand.grid(
    V1 = c("Total", "A1", "A2", "A3"),        # n_mod_v1 = 4
    V2 = c("Total", "B1", "B2"),              # n_mod_v2 = 3
    V3 = c("Total", "C1", "C2", "C3"),        # n_mod_v3 = 4
    V4 = c("Total", "D1"),                    # n_mod_v4 = 2
    V5 = c("Total", "E1", "E2"),              # n_mod_v5 = 3
    stringsAsFactors = FALSE
  )
  test_data_2$VALUE <- 1
  totcode_2 <- c(V1 = "Total", V2 = "Total", V3 = "Total", V4 = "Total", V5 = "Total")

  # 1. Real ground truth: count and sizes of generated data frames
  real_res_2 <- reduce_dims(
    dfs = test_data_2,
    dfs_name = "test2",
    totcode = totcode_2,
    hrcfiles = NULL,
    vars_to_merge = c("V1", "V2", "V3"),
    sep_dir = TRUE,
    hrc_dir = tempdir()
  )
  real_sizes_2 <- sort(sapply(real_res_2$tabs, nrow))
  real_count_2 <- length(real_sizes_2)

  # 2. Package estimation (rtauargus original buggy formula)
  pkg_sizes_2 <- sort(unlist(rtauargus:::length_tabs(
    dfs = test_data_2,
    v1 = "V1", v2 = "V2", v3 = "V3",
    totcode = totcode_2,
    hrcfiles = NULL
  )))
  pkg_count_2 <- length(pkg_sizes_2)

  # 3. Local estimation (corrected formula)
  local_sizes_2 <- sort(unlist(length_tabs(
    dfs = test_data_2,
    v1 = "V1", v2 = "V2", v3 = "V3",
    totcode = totcode_2,
    hrcfiles = NULL
  )))
  local_count_2 <- length(local_sizes_2)

  # Display results for Test Case 2
  cat("\n[Table Count Summary]\n")
  cat("  - Real generated tables :", real_count_2, "\n")
  cat("  - Package prediction    :", pkg_count_2, " (Error:", pkg_count_2 - real_count_2, ")\n")
  cat("  - Local prediction      :", local_count_2, " (Error:", local_count_2 - real_count_2, ")\n")

  cat("\n[Sorted Table Sizes Comparison]\n")
  cat("  - Real actual sizes     :", paste(real_sizes_2, collapse = ", "), "\n")
  cat("  - Package predicted     :", paste(pkg_sizes_2, collapse = ", "), "\n")
  cat("  - Local predicted       :", paste(local_sizes_2, collapse = ", "), "\n")
}

# Run the three-way comparison
run_three_way_comparison()


# -------------------------------------------------------------------------

# =============================================================================
# Validation Script for nb_tab_generated: Real vs. Local (corrected)
# Verifying node-counting simplification across different triplet scenarios
# =============================================================================

verify_nb_tab_generated <- function() {
  cat("\n=======================================================================\n")
  cat("       VALIDATION OF nb_tab_generated: REAL vs. LOCAL (CORRECTED)      \n")
  cat("=======================================================================\n")

  # Configuration du jeu de données 5D (issu de votre script de benchmark)
  # dfs5, tot5, hrc5 doivent être chargés en mémoire.

  # --- Scenario 1: Triplet merging 2 hierarchical variables & 1 flat variable
  # Variables: ACT (Hierarchical), GEO (Hierarchical), SEX (Flat)
  cat("\n[Scenario 1] Merging 2 Hierarchical (ACT, GEO) & 1 Flat (SEX):\n")

  real_res_1 <- reduce_dims(
    dfs = dfs5, dfs_name = "scen1", totcode = tot5, hrcfiles = hrc5,
    vars_to_merge = c("ACT", "GEO", "SEX"),
    sep_dir = TRUE, hrc_dir = tempdir()
  )
  real_count_1 <- length(real_res_1$tabs)

  local_count_1 <- nb_tab_generated(
    v1 = "ACT", v2 = "GEO", v3 = "SEX",
    hrcfiles = hrc5, totcode = tot5, data = dfs5
  )

  cat("  - Real count    :", real_count_1, "\n")
  cat("  - Local pred    :", local_count_1, "\n")
  match_1 <- real_count_1 == local_count_1
  cat("  - Exact match   :", match_1, "\n")

  # --- Scenario 2: Triplet merging 1 hierarchical variable & 2 flat variables
  # Variables: ACT (Hierarchical), SEX (Flat), AGE (Flat)
  cat("\n[Scenario 2] Merging 1 Hierarchical (ACT) & 2 Flat (SEX, AGE):\n")

  real_res_2 <- reduce_dims(
    dfs = dfs5, dfs_name = "scen2", totcode = tot5, hrcfiles = hrc5,
    vars_to_merge = c("ACT", "SEX", "AGE"),
    sep_dir = TRUE, hrc_dir = tempdir()
  )
  real_count_2 <- length(real_res_2$tabs)

  local_count_2 <- nb_tab_generated(
    v1 = "ACT", v2 = "SEX", v3 = "AGE",
    hrcfiles = hrc5, totcode = tot5, data = dfs5
  )

  cat("  - Real count    :", real_count_2, "\n")
  cat("  - Local pred    :", local_count_2, "\n")
  match_2 <- real_count_2 == local_count_2
  cat("  - Exact match   :", match_2, "\n")

  # --- Scenario 3: Triplet merging 3 flat variables
  # Variables: SEX (Flat), AGE (Flat), ECO (Flat)
  cat("\n[Scenario 3] Merging 3 Flat variables (SEX, AGE, ECO):\n")

  real_res_3 <- reduce_dims(
    dfs = dfs5, dfs_name = "scen3", totcode = tot5, hrcfiles = hrc5,
    vars_to_merge = c("SEX", "AGE", "ECO"),
    sep_dir = TRUE, hrc_dir = tempdir()
  )
  real_count_3 <- length(real_res_3$tabs)

  local_count_3 <- nb_tab_generated(
    v1 = "SEX", v2 = "AGE", v3 = "ECO",
    hrcfiles = hrc5, totcode = tot5, data = dfs5
  )

  cat("  - Real count    :", real_count_3, "\n")
  cat("  - Local pred    :", local_count_3, "\n")
  match_3 <- real_count_3 == local_count_3
  cat("  - Exact match   :", match_3, "\n")

  cat("\n=======================================================================\n")
  if (match_1 && match_2 && match_3) {
    cat(" SYNTHESIS: The corrected local formulas are valid.\n")
    cat("            The predictions match the real generated table counts\n")
    cat("            in all test scenarios.\n")
  } else {
    cat(" SYNTHESIS: Mismatch detected. Please check the code.\n")
  }
  cat("=======================================================================\n")
}

# Run the validation
verify_nb_tab_generated()

# il y a des écarts, mais cela peut être du à une tbale sparse : des croisements sans observations => pas de tables générées


# -------------------------------------------------------------------------


# =============================================================================
# Validation Script for nb_tab_generated with FULL factorial data
# (no sparse cells – theory == practice)
# =============================================================================

verify_nb_tab_generated_full <- function() {
  cat("\n=======================================================================\n")
  cat("   VALIDATION OF nb_tab_generated: REAL vs. LOCAL (FULL FACTORIAL)    \n")
  cat("=======================================================================\n")

  # --------------------------------------------------------------------------
  # Helper: create a small hierarchy file (in memory) and return its path
  # --------------------------------------------------------------------------
  make_small_hrc <- function(root, children, subchildren = NULL) {
    tmp <- tempfile(fileext = ".hrc")
    h <- sdcHierarchies::hier_create(root = root, nodes = children)
    if (!is.null(subchildren)) {
      for (i in seq_along(children)) {
        h <- sdcHierarchies::hier_add(h, root = children[i], nodes = subchildren[[i]])
      }
    }
    h %>%
      sdcHierarchies::hier_convert(as = "argus") %>%
      slice(-1) %>%
      mutate(levels = substring(paste0(level, name), 3)) %>%
      select(levels) %>%
      write.table(file = tmp, row.names = FALSE, col.names = FALSE, quote = FALSE)
    return(tmp)
  }

  # --------------------------------------------------------------------------
  # Scenario 1: 2 hierarchical (ACT, GEO) + 1 flat (SEX)
  # --------------------------------------------------------------------------
  cat("\n[Scenario 1] 2 hierarchical + 1 flat (full factorial)\n")

  # Small hierarchies
  hrc_act <- make_small_hrc(
    root = "Total",
    children = c("A1", "A2"),
    subchildren = list(c("A1_a", "A1_b"), c("A2_a", "A2_b"))
  )
  hrc_geo <- make_small_hrc(
    root = "Total",
    children = c("G1", "G2"),
    subchildren = list(c("G1_x", "G1_y"), c("G2_x", "G2_y"))
  )

  # All modalities for each variable (including totals)
  act_mods <- c("Total", "A1", "A2", "A1_a", "A1_b", "A2_a", "A2_b")
  geo_mods <- c("Total", "G1", "G2", "G1_x", "G1_y", "G2_x", "G2_y")
  sex_mods <- c("Total", "F", "M")
  # Dummy 4th & 5th variables (not merged)
  v4_mods <- c("Total", "X")
  v5_mods <- c("Total", "Y")

  d1 <- expand.grid(
    ACT = act_mods,
    GEO = geo_mods,
    SEX = sex_mods,
    V4  = v4_mods,
    V5  = v5_mods,
    stringsAsFactors = FALSE
  )
  d1$VALUE <- 1

  tot1 <- c(ACT = "Total", GEO = "Total", SEX = "Total", V4 = "Total", V5 = "Total")
  hrc1 <- c(ACT = hrc_act, GEO = hrc_geo)

  real1 <- reduce_dims(
    dfs = d1, dfs_name = "full1", totcode = tot1, hrcfiles = hrc1,
    vars_to_merge = c("ACT", "GEO", "SEX"),
    sep_dir = TRUE, hrc_dir = tempdir()
  )
  local1 <- nb_tab_generated(
    v1 = "ACT", v2 = "GEO", v3 = "SEX",
    hrcfiles = hrc1, totcode = tot1, data = d1
  )

  cat("  - Real count    :", length(real1$tabs), "\n")
  cat("  - Local pred    :", local1, "\n")
  cat("  - Exact match   :", length(real1$tabs) == local1, "\n")

  # --------------------------------------------------------------------------
  # Scenario 2: 1 hierarchical (ACT) + 2 flat (SEX, AGE)
  # --------------------------------------------------------------------------
  cat("\n[Scenario 2] 1 hierarchical + 2 flat (full factorial)\n")

  age_mods <- c("Total", "AGE1", "AGE2")
  d2 <- expand.grid(
    ACT = act_mods,
    SEX = sex_mods,
    AGE = age_mods,
    V4  = v4_mods,
    V5  = v5_mods,
    stringsAsFactors = FALSE
  )
  d2$VALUE <- 1

  tot2 <- c(ACT = "Total", SEX = "Total", AGE = "Total", V4 = "Total", V5 = "Total")
  hrc2 <- c(ACT = hrc_act)

  real2 <- reduce_dims(
    dfs = d2, dfs_name = "full2", totcode = tot2, hrcfiles = hrc2,
    vars_to_merge = c("ACT", "SEX", "AGE"),
    sep_dir = TRUE, hrc_dir = tempdir()
  )
  local2 <- nb_tab_generated(
    v1 = "ACT", v2 = "SEX", v3 = "AGE",
    hrcfiles = hrc2, totcode = tot2, data = d2
  )

  cat("  - Real count    :", length(real2$tabs), "\n")
  cat("  - Local pred    :", local2, "\n")
  cat("  - Exact match   :", length(real2$tabs) == local2, "\n")

  # --------------------------------------------------------------------------
  # Scenario 3: 3 flat variables (already matched, but included for completeness)
  # --------------------------------------------------------------------------
  cat("\n[Scenario 3] 3 flat variables (full factorial)\n")

  eco_mods <- c("Total", "E1", "E2")
  d3 <- expand.grid(
    SEX = sex_mods,
    AGE = age_mods,
    ECO = eco_mods,
    V4  = v4_mods,
    V5  = v5_mods,
    stringsAsFactors = FALSE
  )
  d3$VALUE <- 1

  tot3 <- c(SEX = "Total", AGE = "Total", ECO = "Total", V4 = "Total", V5 = "Total")
  hrc3 <- NULL

  real3 <- reduce_dims(
    dfs = d3, dfs_name = "full3", totcode = tot3, hrcfiles = hrc3,
    vars_to_merge = c("SEX", "AGE", "ECO"),
    sep_dir = TRUE, hrc_dir = tempdir()
  )
  local3 <- nb_tab_generated(
    v1 = "SEX", v2 = "AGE", v3 = "ECO",
    hrcfiles = hrc3, totcode = tot3, data = d3
  )

  cat("  - Real count    :", length(real3$tabs), "\n")
  cat("  - Local pred    :", local3, "\n")
  cat("  - Exact match   :", length(real3$tabs) == local3, "\n")

  cat("\n=======================================================================\n")
  cat(" If all three scenarios show Exact match = TRUE,\n")
  cat(" the nb_tab_generated formulas are fully correct.\n")
  cat("=======================================================================\n")
}

# Run the full factorial validation
verify_nb_tab_generated_full() # ok, tout est bon


# -------------------------------------------------------------------------

# =============================================================================
# Validation des options heuristiques 'min' et 'max' (4D & 5D)
# =============================================================================
cat("\n=======================================================================\n")
cat("          VALIDATION DES HEURISTIQUES : OPTIONS MIN ET MAX              \n")
cat("=======================================================================\n")

# --- Validation 4D Option MIN
validate("reduce_dims 4D - Option MIN",
         rtauargus:::reduce_dims(dfs4, "tab_min4", tot4, hrc4,
                                 nb_tab_option = "min", sep_dir = TRUE, hrc_dir = tempdir()),
         reduce_dims(dfs4, "tab_min4", tot4, hrc4,
                     nb_tab_option = "min", sep_dir = TRUE, hrc_dir = tempdir()))

# --- Validation 4D Option MAX
validate("reduce_dims 4D - Option MAX",
         rtauargus:::reduce_dims(dfs4, "tab_max4", tot4, hrc4,
                                 nb_tab_option = "max", sep_dir = TRUE, hrc_dir = tempdir()),
         reduce_dims(dfs4, "tab_max4", tot4, hrc4,
                     nb_tab_option = "max", sep_dir = TRUE, hrc_dir = tempdir()))

# --- Validation 5D Option MIN
validate("reduce_dims 5D - Option MIN",
         rtauargus:::reduce_dims(dfs5, "tab_min5", tot5, hrc5,
                                 nb_tab_option = "min", sep_dir = TRUE, hrc_dir = tempdir()),
         reduce_dims(dfs5, "tab_min5", tot5, hrc5,
                     nb_tab_option = "min", sep_dir = TRUE, hrc_dir = tempdir()))

# --- Validation 5D Option MAX
validate("reduce_dims 5D - Option MAX",
         rtauargus:::reduce_dims(dfs5, "tab_max5", tot5, hrc5,
                                 nb_tab_option = "max", sep_dir = TRUE, hrc_dir = tempdir()),
         reduce_dims(dfs5, "tab_max5", tot5, hrc5,
                     nb_tab_option = "max", sep_dir = TRUE, hrc_dir = tempdir()))

# --- Benchmark comparatif (Option MIN sur données 5D volumineuses)
bench("reduce_dims_5D_min_original", "reduce_dims_5D_min_optimise",
      rtauargus:::reduce_dims(dfs5, "tab_min5", tot5, hrc5,
                              nb_tab_option = "min", sep_dir = TRUE, hrc_dir = tempdir()),
      reduce_dims(dfs5, "tab_min5", tot5, hrc5,
                  nb_tab_option = "min", sep_dir = TRUE, hrc_dir = tempdir()),
      times = 5)


# -------------------------------------------------------------------------

loc_tauargus <- "Y:/Logiciels/TauArgus/TauArgus4.2.2b1/TauArgus.exe"
options(rtauargus.tauargus_exe = loc_tauargus)


Sys.time()

# =============================================================================
# Test d'intégration de bout en bout avec tab_rtauargus4 (avec Tau-Argus)
# =============================================================================
cat("\n=======================================================================\n")
cat("        TEST D'INTÉGRATION DE BOUT EN BOUT : tab_rtauargus4            \n")
cat("=======================================================================\n")

# --- Préparation des variables requises par tab_rtauargus4
# La fonction nécessite des colonnes pour la valeur, la fréquence et le secret primaire.
dfs4$freq <- 1
dfs4$is_secret_prim <- FALSE

# On force arbitrairement quelques cellules en secret primaire
dfs4$is_secret_prim[dfs4$ACT %in% c("A1", "A2") & dfs4$GEO == "G1"] <- TRUE

dfs4_agg <- dfs4 %>%
  group_by(across(all_of(names(tot4)))) %>%
  summarise(
    VALUE = sum(VALUE),
    freq   = sum(freq),
    is_secret_prim = as.logical(max(is_secret_prim)),  # TRUE si au moins une ligne est secrète
    .groups = "drop"
  )

# --- Vérification de la configuration de l'exécutable Tau-Argus
tau_exe <- getOption("rtauargus.tauargus_exe")

if (is.null(tau_exe) || !file.exists(tau_exe)) {
  cat("\n[INFO] L'exécutable Tau-Argus n'est pas configuré ou reste introuvable.\n")
  cat("Pour lancer ce test d'intégration réel avec Tau-Argus, configurez son chemin :\n")
  cat("options(rtauargus.tauargus_exe = \"C:/Chemin/Vers/TauArgus.exe\")\n\n")
} else {
  cat("Exécutable Tau-Argus détecté :", tau_exe, "\n")
  cat("Lancement des validations et benchmarks de bout en bout...\n")

  # Dossier de sortie temporaire pour les fichiers de travail de Tau-Argus
  dir_output <- file.path(tempdir(), "tauargus_output")
  if (!dir.exists(dir_output)) dir.create(dir_output, recursive = TRUE)

  # --- Validation logique (Version originale vs Version optimisée)
  # On utilise la méthode de suppression rapide Hypercube "GH(1,100)"
  validate(
    "tab_rtauargus4 - 4D complet (Méthode Hypercube rapide GH)",
    # Version originale du package installé
    rtauargus::tab_rtauargus4(
      tabular = dfs4_agg,
      explanatory_vars = names(tot4),
      dir_name = dir_output,
      secret_var = "is_secret_prim",
      totcode = tot4,
      hrc = hrc4,
      value = "VALUE",
      freq = "freq",
      suppress = "GH(1,100)",
      nb_tab_option = "smart",
      limit = 50000,
      dfs_name = "tab_4d_int"
    ),
    # Version locale optimisée (Sourced)
    tab_rtauargus4(
      tabular = dfs4_agg,
      explanatory_vars = names(tot4),
      dir_name = dir_output,
      secret_var = "is_secret_prim",
      totcode = tot4,
      hrc = hrc4,
      value = "VALUE",
      freq = "freq",
      suppress = "GH(1,100)",
      nb_tab_option = "smart",
      limit = 50000,
      dfs_name = "tab_4d_int"
    )
  )

  # --- Benchmark du flux complet (Écriture fichiers -> Appel Tau-Argus -> Restauration)
  # On effectue 3 répétitions car l'appel externe à Tau-Argus et l'écriture disque
  # imposent un temps de traitement incompressible.
  bench(
    "tab_rtauargus4_original_full", "tab_rtauargus4_optimise_full",
    rtauargus::tab_rtauargus4(
      tabular = dfs4_agg,
      explanatory_vars = names(tot4),
      dir_name = dir_output,
      secret_var = "is_secret_prim",
      totcode = tot4,
      hrc = hrc4,
      value = "VALUE",
      freq = "freq",
      suppress = "GH(1,100)",
      nb_tab_option = "smart",
      limit = 50000,
      dfs_name = "tab_4d_int"
    ),
    tab_rtauargus4(
      tabular = dfs4_agg,
      explanatory_vars = names(tot4),
      dir_name = dir_output,
      secret_var = "is_secret_prim",
      totcode = tot4,
      hrc = hrc4,
      value = "VALUE",
      freq = "freq",
      suppress = "GH(1,100)",
      nb_tab_option = "smart",
      limit = 50000,
      dfs_name = "tab_4d_int"
    ),
    times = 2
  )
}

Sys.time()

# -------------------------------------------------------------------------

create_test_5D_triplet <- function() {
  # Variables hiérarchiques (exemple : ACT, GEO)
  # ACT : Total_A -> A1, A2 (avec sous-niveaux A1_a, A1_b...)
  hrc_act <- tempfile(fileext = ".hrc")
  sdcHierarchies::hier_create(root = "Total_A", nodes = c("A1", "A2")) %>%
    sdcHierarchies::hier_add(root = "A1", nodes = c("A1_a", "A1_b")) %>%
    sdcHierarchies::hier_add(root = "A2", nodes = c("A2_a", "A2_b")) %>%
    sdcHierarchies::hier_convert(as = "argus") %>%
    slice(-1) %>%
    mutate(levels = substring(paste0(level, name), 3)) %>%
    select(levels) %>%
    write.table(file = hrc_act, row.names = FALSE, col.names = FALSE, quote = FALSE)

  hrc_geo <- tempfile(fileext = ".hrc")
  sdcHierarchies::hier_create(root = "Total_G", nodes = c("G1", "G2")) %>%
    sdcHierarchies::hier_add(root = "G1", nodes = c("G1_x", "G1_y")) %>%
    sdcHierarchies::hier_add(root = "G2", nodes = c("G2_x", "G2_y")) %>%
    sdcHierarchies::hier_convert(as = "argus") %>%
    slice(-1) %>%
    mutate(levels = substring(paste0(level, name), 3)) %>%
    select(levels) %>%
    write.table(file = hrc_geo, row.names = FALSE, col.names = FALSE, quote = FALSE)

  # Modalities pour chaque variable
  act_mods <- c("Total_A", "A1", "A2", "A1_a", "A1_b", "A2_a", "A2_b")
  geo_mods <- c("Total_G", "G1", "G2", "G1_x", "G1_y", "G2_x", "G2_y")
  sex_mods <- c("Total_S", "F", "M")          # variable plate 1
  age_mods <- c("Total_Age", "AGE1", "AGE2")  # variable plate 2 (non fusionnée)
  eco_mods <- c("Total_E", "E1", "E2")        # variable plate 3

  # Produit cartésien complet (toutes les combinaisons)
  data <- expand.grid(
    ACT = act_mods,
    GEO = geo_mods,
    SEX = sex_mods,
    AGE = age_mods,
    ECO = eco_mods,
    stringsAsFactors = FALSE
  )
  data$VALUE <- 1

  list(
    data   = data,
    hrc    = c(ACT = hrc_act, GEO = hrc_geo),
    tot    = c(ACT = "Total_A", GEO = "Total_G", SEX = "Total_S", AGE = "Total_Age", ECO = "Total_E")
  )
}

test <- create_test_5D_triplet()
dfs5_test <- test$data
hrc5_test <- test$hrc
tot5_test <- test$tot

validate_triplet_predictions <- function(v1, v2, v3, dfs, totcode, hrcfiles) {
  cat("\n--- Validation pour le triplet", v1, v2, v3, "---\n")

  # 1. Réduction réelle
  res_real <- reduce_dims(
    dfs = dfs,
    dfs_name = "test",
    totcode = totcode,
    hrcfiles = hrcfiles,
    vars_to_merge = c(v1, v2, v3),
    sep_dir = TRUE,
    hrc_dir = tempdir()
  )
  real_sizes <- sort(sapply(res_real$tabs, nrow))
  real_count <- length(real_sizes)

  # 2. Prédiction version locale (celle que vous avez chargée)
  local_pred <- length_tabs(
    dfs = dfs, v1 = v1, v2 = v2, v3 = v3,
    totcode = totcode, hrcfiles = hrcfiles
  )
  local_sizes <- sort(unlist(local_pred))
  local_count <- length(local_sizes)

  # 3. Prédiction version package (originale)
  pkg_pred <- rtauargus:::length_tabs(
    dfs = dfs, v1 = v1, v2 = v2, v3 = v3,
    totcode = totcode, hrcfiles = hrcfiles
  )
  pkg_sizes <- sort(unlist(pkg_pred))
  pkg_count <- length(pkg_sizes)

  cat("\n  Réel  :", real_count, "tableaux, tailles :", paste(real_sizes, collapse = ","))
  cat("\n  Local :", local_count, "tableaux, tailles :", paste(local_sizes, collapse = ","))
  cat("\n  Package:", pkg_count, "tableaux, tailles :", paste(pkg_sizes, collapse = ","))

  # Vérification
  local_ok <- isTRUE(all.equal(real_sizes, local_sizes, check.attributes = FALSE))
  pkg_ok   <- isTRUE(all.equal(real_sizes, pkg_sizes, check.attributes = FALSE))

  cat("\n  => Local correct :", local_ok, "| Package correct :", pkg_ok, "\n")
  return(list(local = local_ok, pkg = pkg_ok))
}

# Cas 1 : deux hiérarchiques + une plate (ACT, GEO, SEX)
validate_triplet_predictions("ACT", "GEO", "SEX", dfs5_test, tot5_test, hrc5_test)

# Cas 2 : une hiérarchique + deux plates (ACT, SEX, ECO)
validate_triplet_predictions("ACT", "SEX", "ECO", dfs5_test, tot5_test, hrc5_test)

# Cas 3 : trois plates (SEX, AGE, ECO)
validate_triplet_predictions("SEX", "AGE", "ECO", dfs5_test, tot5_test, hrc5_test)


# -------------------------------------------------------------------------

# =============================================================================
# Targeted validation and benchmark for split_tab
# =============================================================================
cat("\n========== split_tab (Table Splitting) ==========\n")

# To test split_tab, we first generate a baseline reduced dimension object
# with over_split = FALSE. This creates the exact data structure (res)
# that split_tab expects, allowing us to test it in isolation.
res_base_4D <- reduce_dims(
  dfs = dfs4,
  dfs_name = "tab_base",
  totcode = tot4,
  hrcfiles = hrc4,
  over_split = FALSE, # Crucial: prevent split_tab from running inside reduce_dims
  nb_tab_option = "min",
  sep_dir = TRUE,
  hrc_dir = tempdir()
)

# Reconstruct the merged variable name from the baseline results (e.g., "ACT_GEO")
var_fusionnee <- paste(res_base_4D$fus_vars[1], res_base_4D$fus_vars[2], sep = res_base_4D$sep)

# 1. Logical Validation: Ensure both functions yield mathematically identical lists of dataframes
# We set a low limit (200 rows) to force the splitting loop to run on multiple large tables
validate(
  desc = "split_tab 4D (with a strict limit of 200 rows)",
  expr_orig = rtauargus:::split_tab(res = res_base_4D, var_fus = var_fusionnee, limit = 200),
  expr_opt  = split_tab(res = res_base_4D, var_fus = var_fusionnee, limit = 200)
)

# 2. Performance Benchmark: Measure execution times between the original and optimized versions
bench(
  name_o = "split_tab_original",
  name_n = "split_tab_optimise",
  expr_o = rtauargus:::split_tab(res = res_base_4D, var_fus = var_fusionnee, limit = 200),
  expr_n = split_tab(res = res_base_4D, var_fus = var_fusionnee, limit = 200),
  times = 10) # 10 runs are sufficient to confirm the memory footprint and CPU speed difference


# -------------------------------------------------------------------------

# =============================================================================
# Validation et Benchmark de restore_format (Original vs Optimisé)
# =============================================================================

# --- 1. Génération préalable des objets réduits avec reduce_dims
cat("\nGénération des objets réduits (reduce_dims 4D et 5D)...\n")

res_4D <- reduce_dims(
  dfs = dfs4,
  dfs_name = "tab_4D",
  totcode = tot4,
  hrcfiles = hrc4,
  nb_tab_option = "max",
  sep_dir = TRUE,
  hrc_dir = tempdir()
)

res_5D <- reduce_dims(
  dfs = dfs5,
  dfs_name = "tab_5D",
  totcode = tot5,
  hrcfiles = hrc5,
  nb_tab_option = "max",
  sep_dir = TRUE,
  hrc_dir = tempdir()
)

# On renomme temporairement la fonction optimisée locale pour la distinguer du package installé
restore_format_local <- restore_format

# --- 2. Fonction de validation de l'égalité logique des jeux de données
validate_restore <- function(desc, masq, res) {
  cat("\n--- Validation logique :", desc, "---\n")

  # Version officielle installée du package
  res_orig <- rtauargus::restore_format(masq = masq, res = res)

  # Nouvelle version locale optimisée
  res_opt  <- restore_format_local(masq = masq, res = res)

  # Standardisation (conversion en data.frame classique et nettoyage des rownames)
  res_orig_clean <- as.data.frame(res_orig)
  rownames(res_orig_clean) <- NULL

  res_opt_clean <- as.data.frame(res_opt)
  rownames(res_opt_clean) <- NULL

  # Comparaison stricte du contenu des données
  eq <- identical(res_orig_clean, res_opt_clean)
  if (eq) {
    cat("Résultats strictement identiques (après nettoyage des attributs R).\n")
  } else {
    cat("Résultats DIFFÉRENTS. Vérifiez la validité des découpages.\n")
  }
  return(eq)
}

# Nombre de tables pour le cas 4D
length(res_4D$tabs)

# Nombre de tables pour le cas 5D
length(res_5D$tabs)

# Validation sur les structures 4D et 5D
v_4d <- validate_restore("restore_format 4D (Paires)", res_4D$tabs, res_4D)
v_5d <- validate_restore("restore_format 5D (Triplets/Paires)", res_5D$tabs, res_5D)

# --- 3. Mesures de performances si la validation logique est validée
if (v_4d && v_5d) {
  cat("\n=======================================================================\n")
  cat("          BENCHMARKS COMPARATIFS : restore_format                      \n")
  cat("=======================================================================\n")

  cat("\n--- Benchmark 1 : restore_format (Cas 4D - 50 000 lignes) ---\n")
  bench_4D <- microbenchmark::microbenchmark(
    original = rtauargus::restore_format(masq = res_4D$tabs, res = res_4D),
    optimise = restore_format_local(masq = res_4D$tabs, res = res_4D),
    times = 10
  )
  print(bench_4D, unit = "ms")

  cat("\n--- Benchmark 2 : restore_format (Cas 5D - 80 000 lignes) ---\n")
  bench_5D <- microbenchmark::microbenchmark(
    original = rtauargus::restore_format(masq = res_5D$tabs, res = res_5D),
    optimise = restore_format_local(masq = res_5D$tabs, res = res_5D),
    times = 10
  )
  print(bench_5D, unit = "ms")
} else {
  cat("\n[ERREUR] Impossible de lancer le benchmark : les résultats divergent.\n")
}


# -------------------------------------------------------------------------

# =============================================================================
# Validation et Benchmark de chose_sep (Original vs Optimisé)
# =============================================================================
cat("\n=======================================================================\n")
cat("          VALIDATION ET BENCHMARK : chose_sep                         \n")
cat("=======================================================================\n")

# 1. Création d'un grand jeu de données avec beaucoup de modalités distinctes
# En cumulant VAR1 à VAR4, nous obtenons environ 18 000 modalités uniques sur 100 000 observations.
set.seed(456)
n_obs <- 100000
gros_df_modalites <- data.frame(
  VAR1 = sample(paste0("M1_", 1:5000), n_obs, replace = TRUE),
  VAR2 = sample(paste0("M2_", 10000:20000), n_obs, replace = TRUE),
  VAR3 = sample(paste0("M3_", 1:2000), n_obs, replace = TRUE),
  VAR4 = sample(paste0("M4_", 1:1000), n_obs, replace = TRUE),
  stringsAsFactors = FALSE
)

separateurs_candidats <- c("___", "_XXX_", "_YYY_", "_TTT_", "_UVW_")

# 2. Validation logique : s'assurer que le séparateur trouvé est identique
cat("\n--- Validation logique de chose_sep ---\n")
sep_orig <- rtauargus:::chose_sep(gros_df_modalites, separateurs_candidats)
sep_opt  <- chose_sep(gros_df_modalites, separateurs_candidats)

if (identical(sep_orig, sep_opt)) {
  cat("Résultats identiques : le séparateur sélectionné est '", sep_opt, "'\n", sep = "")

  # 3. Benchmark comparatif de performance
  cat("\n--- Benchmark : chose_sep (18 000 modalités) ---\n")
  bench_sep <- microbenchmark::microbenchmark(
    original = rtauargus:::chose_sep(gros_df_modalites, separateurs_candidats),
    optimise = chose_sep(gros_df_modalites, separateurs_candidats),
    times = 5 # 5 répétitions suffisent car la version originale est longue à s'exécuter
  )
  print(bench_sep, unit = "ms")

} else {
  cat("\n[ERREUR] Résultats DIFFÉRENTS. Vérifiez l'implémentation de votre version locale.\n")
}
