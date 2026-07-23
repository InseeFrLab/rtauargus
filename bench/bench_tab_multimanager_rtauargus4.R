library(microbenchmark)
library(data.table)
library(purrr)
library(rlang)
library(dplyr)
library(sdcHierarchies)

library(rtauargus)

# 1. Sauvegarde des fonctions du package original avant load_all
reduce_dims_old       <- rtauargus:::reduce_dims
tab_multi_manager_old <- rtauargus::tab_multi_manager
tab_rtauargus4_old    <- rtauargus::tab_rtauargus4


loc_tauargus <- "Y:/Logiciels/TauArgus/TauArgus4.2.2b1/TauArgus.exe"
options(rtauargus.tauargus_exe = loc_tauargus)

set.seed(42)

# =============================================================================
# PRÉPARATION DU JEU DE DONNÉES 4D AGRÉGÉ
# =============================================================================
cat("======================================================================\n")
cat("TEST D'INTÉGRATION 4D : DÉCOMPOSITION DES TEMPS DE CALCUL             \n")
cat("======================================================================\n")

# --- 4D volumineux (ACT hiérarchique, GEO hiérarchique, SEX & AGE non hiérarchiques)
create_big_4D <- function(
    n = 200000,
    n_act_groups = 30,   # Nombre de groupes d'activité (Niveau 1 ACT)
    n_act_sub = 50,      # Nombre de sous-niveaux par groupe d'activité (Niveau 2 ACT)
    n_geo_regions = 50,  # Nombre de régions (Niveau 1 GEO)
    n_geo_sub = 50,      # Nombre de sous-régions par région (Niveau 2 GEO)
    n_sex = 2,           # Nombre de modalités de SEX (ex: F, M si n_sex=2, sinon S01, S02...)
    n_age = 4            # Nombre de modalités d'AGE (ex: AGE01 à AGE04)
) {
  set.seed(123)

  # Calcul dynamique de la largeur des chiffres (Zero-padding)
  w_act_g <- max(2, nchar(n_act_groups))
  w_act_s <- max(2, nchar(n_act_sub))
  w_geo_r <- max(2, nchar(n_geo_regions))
  w_geo_s <- max(2, nchar(n_geo_sub))
  w_age   <- max(2, nchar(n_age))

  fmt_act_g <- paste0("A%0", w_act_g, "d")
  fmt_act_s <- paste0("A%0", w_act_g, "d_%0", w_act_s, "d")

  fmt_geo_r <- paste0("G%0", w_geo_r, "d")
  fmt_geo_s <- paste0("G%0", w_geo_r, "d_%0", w_geo_s, "d")

  fmt_age   <- paste0("AGE%0", w_age, "d")

  # 1. Définition des niveaux ACT
  act_level1 <- sprintf(fmt_act_g, 1:n_act_groups)
  act_level2 <- unlist(lapply(1:n_act_groups, function(i) sprintf(fmt_act_s, i, 1:n_act_sub)))

  # 2. Définition des niveaux GEO
  geo_level1 <- sprintf(fmt_geo_r, 1:n_geo_regions)
  geo_level2 <- unlist(lapply(1:n_geo_regions, function(i) sprintf(fmt_geo_s, i, 1:n_geo_sub)))

  # 3. Définition des modalités SEX (F, M si n_sex = 2, sinon S01, S02...)
  sex_mods <- if (n_sex == 2) {
    c("Total", "F", "M")
  } else {
    w_sex <- max(2, nchar(n_sex))
    c("Total", sprintf(paste0("S%0", w_sex, "d"), 1:n_sex))
  }

  # 4. Définition des modalités AGE (AGE01, AGE02...)
  age_mods <- c("Total", sprintf(fmt_age, 1:n_age))

  # 5. Génération du jeu de données
  data <- data.frame(
    ACT  = sample(c("Total", act_level1, act_level2), n, replace = TRUE),
    GEO  = sample(c("Total", geo_level1, geo_level2), n, replace = TRUE),
    SEX  = sample(sex_mods, n, replace = TRUE),
    AGE  = sample(age_mods, n, replace = TRUE),
    stringsAsFactors = FALSE
  )
  data$VALUE <- 1

  # Hiérarchie ACT
  hrc_act <- tempfile(fileext = ".hrc")
  sdcHierarchies::hier_create(root = "Total", nodes = act_level1) %>%
    {
      for (i in 1:n_act_groups) {
        root_node <- sprintf(fmt_act_g, i)
        children  <- sprintf(fmt_act_s, i, 1:n_act_sub)
        . <- sdcHierarchies::hier_add(., root = root_node, nodes = children)
      }
      .
    } %>%
    sdcHierarchies::hier_convert(as = "argus") %>%
    slice(-1) %>%
    mutate(levels = substring(paste0(level, name), 3)) %>%
    select(levels) %>%
    write.table(file = hrc_act, row.names = FALSE, col.names = FALSE, quote = FALSE)

  # Hiérarchie GEO
  hrc_geo <- tempfile(fileext = ".hrc")
  sdcHierarchies::hier_create(root = "Total", nodes = geo_level1) %>%
    {
      for (i in 1:n_geo_regions) {
        root_node <- sprintf(fmt_geo_r, i)
        children  <- sprintf(fmt_geo_s, i, 1:n_geo_sub)
        . <- sdcHierarchies::hier_add(., root = root_node, nodes = children)
      }
      .
    } %>%
    sdcHierarchies::hier_convert(as = "argus") %>%
    slice(-1) %>%
    mutate(levels = substring(paste0(level, name), 3)) %>%
    select(levels) %>%
    write.table(file = hrc_geo, row.names = FALSE, col.names = FALSE, quote = FALSE)

  list(data = data,
       hrcfiles = c(ACT = hrc_act, GEO = hrc_geo),
       totcode  = c(SEX = "Total", AGE = "Total", GEO = "Total", ACT = "Total"))
}

calc_4D_dimensions <- function(
    n_act_groups = 7,  # Nombre de groupes d'activité (Niveau 1 ACT)
    n_act_sub = 11,     # Nombre de sous-niveaux par groupe d'activité (Niveau 2 ACT)
    n_geo_regions = 8,  # Nombre de régions (Niveau 1 GEO)
    n_geo_sub = 10,     # Nombre de sous-régions par région (Niveau 2 GEO)
    n_sex = 20,         # Nombre de modalités fines de SEX
    n_age = 15          # Nombre de modalités fines d'AGE
) {
  # 1. Calcul du nombre total de modalités par variable (y compris les totaux)
  n_mod_act <- (n_act_groups * n_act_sub) + n_act_groups + 1
  n_mod_geo <- (n_geo_regions * n_geo_sub) + n_geo_regions + 1
  n_mod_sex <- n_sex + 1
  n_mod_age <- n_age + 1

  # 2. Calcul du nombre de sous-tableaux générés (Option MAX / découpage par noeuds)
  # Formule : 2 x (noeuds ACT + Total) x (noeuds GEO + Total)
  nb_tables <- 2 * (n_act_groups + 1) * (n_geo_regions + 1)

  # 3. Calcul du nombre total de croisements possibles dans la table 4D
  nb_croisements <- n_mod_act * n_mod_geo * n_mod_sex * n_mod_age

  # Affichage structuré
  cat("======================================================================\n")
  cat("          ESTIMATION DE LA COMPLEXITÉ DU DÉCOUPAGE 4D                 \n")
  cat("======================================================================\n")
  cat("1. Nombre de sous-tableaux générés :", format(nb_tables, big.mark = " "), "\n")
  cat("2. Nombre total de croisements     :", format(nb_croisements, big.mark = " "), "\n\n")
  cat("Détail des modalités par variable (Totaux inclus) :\n")
  cat("  - ACT :", n_mod_act, "modalités (1 Total +", n_act_groups, "groupes +", n_act_groups * n_act_sub, "sous-niveaux)\n")
  cat("  - GEO :", n_mod_geo, "modalités (1 Total +", n_geo_regions, "régions +", n_geo_regions * n_geo_sub, "sous-régions)\n")
  cat("  - SEX :", n_mod_sex, "modalités (1 Total +", n_sex, "modalités fines)\n")
  cat("  - AGE :", n_mod_age, "modalités (1 Total +", n_age, "modalités fines)\n")
  cat("======================================================================\n")

  # Retourne les résultats sous forme de liste
  invisible(list(
    nb_tables = nb_tables,
    nb_croisements = nb_croisements,
    modalites = c(ACT = n_mod_act, GEO = n_mod_geo, SEX = n_mod_sex, AGE = n_mod_age)
  ))
}


# 1. Génération des microdonnées 4D
big4 <- create_big_4D(
  n = 50000000,
  n_act_groups = 4,
  n_act_sub = 11,
  n_geo_regions = 5,
  n_geo_sub = 10,
  n_sex = 20,
  n_age = 15
)

# Exécution avec vos valeurs par défaut
res <- calc_4D_dimensions(
  n_act_groups = 4,
  n_act_sub = 11,
  n_geo_regions = 5,
  n_geo_sub = 10,
  n_sex = 20,
  n_age = 15
)

dfs4 <- big4$data

tot4 <- big4$totcode
hrc4 <- big4$hrcfiles

# 2. AGRÉGATION : Elimination des doublons pour obtenir des cellules uniques
dfs4_agg <- dfs4 %>%
  group_by(across(all_of(names(tot4)))) %>%
  summarise(
    VALUE = sum(VALUE),
    freq  = n(),
    .groups = "drop"
  )

# 3. POSE DU SECRET PRIMAIRE : 10% de secret aléatoire déterministe
set.seed(42)
dfs4_agg$is_secret_prim <- sample(c(TRUE, FALSE), nrow(dfs4_agg), replace = TRUE, prob = c(0.10, 0.90))
dfs4_agg$secret_no_pl <- FALSE

# AFFICHAGE DU SECRET AVANT TAU-ARGUS
n_total_cellules <- nrow(dfs4_agg)
n_secret_primaire <- sum(dfs4_agg$is_secret_prim)

cat("\n----------------------------------------------------------------------\n")
cat("=== BILAN DU SECRET PRIMAIRE (AVANT TAU-ARGUS) ===\n")
cat("Nombre total de cellules uniques dans la table 4D :", n_total_cellules, "\n")
cat("Nombre de cellules en secret primaire             :", n_secret_primaire,
    sprintf("(%.2f%%)\n", 100 * n_secret_primaire / n_total_cellules))
cat("----------------------------------------------------------------------\n")

dir_out_old <- file.path(tempdir(), "tau_4d_old")
dir_out_new <- file.path(tempdir(), "tau_4d_new")
if (!dir.exists(dir_out_old)) dir.create(dir_out_old, recursive = TRUE)
if (!dir.exists(dir_out_new)) dir.create(dir_out_new, recursive = TRUE)



# =============================================================================
# 1. MESURES SUR LA VERSION ORIGINALE (PACKAGE)
# =============================================================================
cat("\n--- 1. VERSION ORIGINALE (PACKAGE) ---\n")

# A1. Mesure isolée de reduce_dims originale
# t_red_old <- system.time({
#   res_red_old <- reduce_dims_old(
#     dfs = dfs4_agg, dfs_name = "tab_4d_int", totcode = tot4, hrcfiles = hrc4,
#     nb_tab_option = "smart", limit = 50000, over_split = TRUE, sep_dir = TRUE,
#     hrc_dir = file.path(dir_out_old, "hrc")
#   )
# })

t_red_old <- system.time({
  res_red_old <- reduce_dims_old(
    dfs = dfs4_agg, dfs_name = "tab_4d_int", totcode = tot4, hrcfiles = hrc4,
    nb_tab_option = "max", sep_dir = TRUE,
    hrc_dir = file.path(dir_out_old, "hrc")
  )
})

print(t_red_old)

cat("Nombre de tableaux générés :", length(res_red_old$tabs), "\n")

# Récupération de la taille (nombre de lignes) de chaque sous-tableau
tailles_tables <- sapply(res_red_old$tabs, nrow)
cat("Statistiques sur le nombre de lignes par tableau (Min, Q1, Médiane, Q3, Max) :\n")
print(summary(tailles_tables))

# A2. Mesure isolée de restore_format sur la sortie de reduce_dims
t_rest_old <- system.time({
  dummy_rest_old <- rtauargus::restore_format(masq = res_red_old$tabs, res = res_red_old)
})

print(t_rest_old)

# B. Mesure du flux complet tab_rtauargus4 originale
# t_full_old <- system.time({
#   res_4d_old <- tab_rtauargus4_old(
#     tabular          = dfs4_agg,
#     explanatory_vars = names(tot4),
#     dir_name         = dir_out_old,
#     secret_var       = "is_secret_prim",
#     totcode          = tot4,
#     hrc              = hrc4,
#     value            = "VALUE",
#     freq             = "freq",
#     suppress         = "GH(1,100)",
#     nb_tab_option    = "smart",
#     limit            = 50000,
#     dfs_name         = "tab_4d_int"
#   )
# })

t_full_old <- system.time({
  res_4d_old <- tab_rtauargus4_old(
    tabular          = dfs4_agg,
    explanatory_vars = names(tot4),
    dir_name         = dir_out_old,
    secret_var       = "is_secret_prim",
    totcode          = tot4,
    hrc              = hrc4,
    value            = "VALUE",
    freq             = "freq",
    suppress         = "GH(1,100)",
    nb_tab_option    = "max",
    dfs_name         = "tab_4d_int",
    limit = 1000000L # très grand nombre, pour ne pas avoir d'oversplit
  )
})

print(t_full_old)

# =============================================================================
# 2. MESURES SUR LA VERSION OPTIMISÉE (LOCALE)
# =============================================================================
cat("\n--- 2. VERSION OPTIMISÉE (LOCALE) ---\n")

# Chargement du code local APRÈS l'exécution de la version originale
devtools::load_all("Z:/rtauargus")

# ESSENTIEL : Redéfinir l'option Tau-Argus car load_all() l'a réinitialisée
options(rtauargus.tauargus_exe = loc_tauargus)

# A1. Mesure isolée de reduce_dims optimisée
# t_red_new <- system.time({
#   res_red_new <- reduce_dims(
#     dfs = dfs4_agg, dfs_name = "tab_4d_int", totcode = tot4, hrcfiles = hrc4,
#     nb_tab_option = "smart", limit = 50000, over_split = TRUE, sep_dir = TRUE,
#     hrc_dir = file.path(dir_out_new, "hrc")
#   )
# })

t_red_new <- system.time({
  res_red_new <- reduce_dims(
    dfs = dfs4_agg, dfs_name = "tab_4d_int", totcode = tot4, hrcfiles = hrc4,
    nb_tab_option = "max", sep_dir = TRUE,
    hrc_dir = file.path(dir_out_new, "hrc")
  )
})

print(t_red_new)

cat("Nombre de tableaux générés :", length(res_red_new$tabs), "\n")

# Récupération de la taille (nombre de lignes) de chaque sous-tableau
tailles_tables <- sapply(res_red_new$tabs, nrow)
cat("Statistiques sur le nombre de lignes par tableau (Min, Q1, Médiane, Q3, Max) :\n")
print(summary(tailles_tables))

# A2. Mesure isolée de restore_format sur la sortie de reduce_dims
t_rest_new <- system.time({
  dummy_rest_new <- restore_format(masq = res_red_new$tabs, res = res_red_new)
})

print(t_rest_new)

# B. Mesure du flux complet tab_rtauargus4 optimisée
# t_full_new <- system.time({
#   res_4d_new <- tab_rtauargus4(
#     tabular          = dfs4_agg,
#     explanatory_vars = names(tot4),
#     dir_name         = dir_out_new,
#     secret_var       = "is_secret_prim",
#     totcode          = tot4,
#     hrc              = hrc4,
#     value            = "VALUE",
#     freq             = "freq",
#     suppress         = "GH(1,100)",
#     nb_tab_option    = "smart",
#     limit            = 50000,
#     dfs_name         = "tab_4d_int"
#   )
# })

t_full_new <- system.time({
  res_4d_new <- tab_rtauargus4(
    tabular          = dfs4_agg,
    explanatory_vars = names(tot4),
    dir_name         = dir_out_new,
    secret_var       = "is_secret_prim",
    totcode          = tot4,
    hrc              = hrc4,
    value            = "VALUE",
    freq             = "freq",
    suppress         = "GH(1,100)",
    nb_tab_option    = "max",
    dfs_name         = "tab_4d_int",
    limit = 1000000L # très grand nombre, pour ne pas avoir d'oversplit
  )
})

print(t_full_new)

# =============================================================================
# 3. BILAN DES SECRETS ET DÉCOMPOSITION DES TEMPS DE CALCUL
# =============================================================================
cat("\n======================================================================\n")
cat("=== BILAN DU SECRET APPRÈS TAU-ARGUS ===\n")
cat("======================================================================\n")

# Fonction d'extraction des métriques de secret
get_secret_stats <- function(df) {
  # La dernière colonne is_secret_* contient le secret total final
  sec_cols <- names(df)[grep("^is_secret_[1-9]", names(df))]
  sec_cols <- sec_cols[order(as.integer(gsub("is_secret_", "", sec_cols)))]
  last_col <- if(length(sec_cols) > 0) tail(sec_cols, 1) else "Status"

  tot_sec  <- if(last_col == "Status") df$Status != "V" else df[[last_col]]
  prim_sec <- df$is_secret_prim
  sec_sec  <- tot_sec & !prim_sec

  data.frame(
    "Total_Cellules"    = nrow(df),
    "Secret_Primaire"   = sum(prim_sec),
    "Secret_Secondaire" = sum(sec_sec),
    "Total_Masque"      = sum(tot_sec),
    "Pourcentage_Masque" = sprintf("%.2f%%", 100 * sum(tot_sec) / nrow(df))
  )
}

cat("\n[Statistiques du secret - Version Originale] :\n")
print(get_secret_stats(res_4d_old))

cat("\n[Statistiques du secret - Version Optimisée] :\n")
print(get_secret_stats(res_4d_new))

# Décomposition fine des temps
pure_multi_old <- t_full_old["elapsed"] - t_red_old["elapsed"] - t_rest_old["elapsed"]
pure_multi_new <- t_full_new["elapsed"] - t_red_new["elapsed"] - t_rest_new["elapsed"]

cat("\n======================================================================\n")
cat("          BILAN COMPARATIF DES TEMPS (en secondes)                    \n")
cat("======================================================================\n")

cat("\n[1. Temps Total tab_rtauargus4]\n")
cat("  - Original :", round(t_full_old["elapsed"], 2), "s | Optimisé :", round(t_full_new["elapsed"], 2), "s\n")

cat("\n[2. Part reduce_dims (Amont)]\n")
cat("  - Original :", round(t_red_old["elapsed"], 2), "s | Optimisé :", round(t_red_new["elapsed"], 2), "s\n")

cat("\n[3. Part restore_format (Aval)]\n")
cat("  - Original :", round(t_rest_old["elapsed"], 2), "s | Optimisé :", round(t_rest_new["elapsed"], 2), "s\n")

cat("\n[4. Temps PUR tab_multi_manager (Moteur + Tau-Argus)]\n")
cat("  - Original :", round(pure_multi_old, 2), "s | Optimisé :", round(pure_multi_new, 2), "s\n")
cat("  - Gain net  :", round(pure_multi_old - pure_multi_new, 2), "secondes économisées\n")

# Égalité logique finale
normalize_for_compare <- function(df, key_cols) {
  df <- as.data.frame(df)
  df <- df[, order(names(df)), drop = FALSE]
  df <- df[do.call(order, df[key_cols]), , drop = FALSE]
  row.names(df) <- NULL
  return(df)
}

b4_is_identical <- identical(
  normalize_for_compare(res_4d_old, names(tot4)),
  normalize_for_compare(res_4d_new, names(tot4))
)

cat("\n======================================================================\n")
cat("Égalité stricte du tableau 4D final reconstitué :", b4_is_identical, "\n")
cat("======================================================================\n")
