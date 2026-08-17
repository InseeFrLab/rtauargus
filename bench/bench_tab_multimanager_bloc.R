library(microbenchmark)
library(data.table)
library(purrr)
library(rlang)
library(dplyr)
library(sdcHierarchies)

library(rtauargus) # package original
# on fait une copie de la fonction du package, car sera remplacé lors du load_all
tab_multi_manager_old <- rtauargus::tab_multi_manager

# pour charger reduce_dims et le package rtauargus local
devtools::load_all("Z:/rtauargus")  # path où est la version split_perf de github : https://github.com/InseeFrLab/rtauargus/tree/split_perf_improvement

set.seed(42)

# =============================================================================
# 0. ÉTAPE PRÉALABLE : Création du jeu 5D et découpage par reduce_dims
# =============================================================================
cat("======================================================================\n")
cat("0. INITIALISATION : Génération du jeu 5D et exécution de reduce_dims  \n")
cat("======================================================================\n")

# 1. Fonction create_big_5D corrigée (largeur de codes fixe)
create_big_5D <- function(n = 80000) {
  set.seed(234)

  # Codes de largeur fixe pour CHAQUE niveau :
  # Level 1 ACT: A01_0 (5 caracteres)
  # Level 2 ACT: A01_1 (5 caracteres)
  act_level1 <- sprintf("A%02d_0", 1:20)
  act_level2 <- unlist(lapply(1:20, function(i) sprintf("A%02d_%d", i, 1:5)))

  # Level 1 GEO: GA_ (3 caracteres)
  # Level 2 GEO: GA1 (3 caracteres)
  geo_level1 <- c("GA_", "GB_")
  geo_level2 <- c("GA1", "GA2", "GA3", "GB1", "GB2", "GB3")

  data <- data.frame(
    ACT  = sample(c("Total_A", act_level1, act_level2), n, replace = TRUE),
    GEO  = sample(c("Total_G", geo_level1, geo_level2), n, replace = TRUE),
    SEX  = sample(c("Total_S", "F0", "M0", "F1", "F2", "M1", "M2"), n, replace = TRUE),
    AGE  = sample(c("Ensemble", "AGE01", "AGE02", "AGE11", "AGE12", "AGE21", "AGE22"), n, replace = TRUE),
    ECO  = sample(c("PIB", "Ménages", "Entreprises"), n, replace = TRUE),
    stringsAsFactors = FALSE
  )
  data$VALUE <- 1

  # Hiérarchie ACT
  hrc_act <- tempfile(fileext = ".hrc")
  sdcHierarchies::hier_create(root = "Total_A", nodes = act_level1) %>%
    {
      for (i in 1:20) {
        root_node <- sprintf("A%02d_0", i)
        children <- sprintf("A%02d_%d", i, 1:5)
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
  sdcHierarchies::hier_create(root = "Total_G", nodes = geo_level1) %>%
    sdcHierarchies::hier_add(root = "GA_", nodes = c("GA1", "GA2", "GA3")) %>%
    sdcHierarchies::hier_add(root = "GB_", nodes = c("GB1", "GB2", "GB3")) %>%
    sdcHierarchies::hier_convert(as = "argus") %>%
    slice(-1) %>%
    mutate(levels = substring(paste0(level, name), 3)) %>%
    select(levels) %>%
    write.table(file = hrc_geo, row.names = FALSE, col.names = FALSE, quote = FALSE)

  list(data = data,
       hrcfiles = c(ACT = hrc_act, GEO = hrc_geo),
       totcode  = c(SEX = "Total_S", AGE = "Ensemble", GEO = "Total_G", ACT = "Total_A", ECO = "PIB"))
}

# 1. Création du jeu 5D
big5 <- create_big_5D(80000)
dfs5 <- big5$data

# 2. AGRÉGATION PRÉALABLE DES DONNÉES (Pour obtenir un vrai tableau statistique de cellules uniques)
dfs5_agg <- dfs5 %>%
  group_by(ACT, GEO, SEX, AGE, ECO) %>%
  summarise(
    VALUE = sum(VALUE),
    freq = n(),
    .groups = "drop"
  )

cat("Nombre de lignes tableau avant reduce dims :", nrow(dfs5_agg), "\n")


dfs5_agg$is_secret_prim <- sample(c(TRUE, FALSE), nrow(dfs5_agg), replace = TRUE, prob = c(0.05, 0.95))
dfs5_agg$secret_no_pl <- FALSE

tot5 <- big5$totcode
hrc5 <- big5$hrcfiles

cat("=== CONTENU DU FICHIER HRC GEO ===\n")
print(readLines(hrc5[["GEO"]]))

cat("\n=== CONTENU DU FICHIER HRC ACT ===\n")
print(readLines(hrc5[["ACT"]]))

# 3. Réduction de dimension sur les données agrégées
# res5 <- reduce_dims(
#   dfs = dfs5_agg,
#   dfs_name = "tab_5D",
#   totcode = tot5,
#   hrcfiles = hrc5,
#   nb_tab_option = "smart",
#   limit = 50000,
#   over_split = TRUE,
#   # nb_tab_option = "max",
#   sep_dir = TRUE,
#   hrc_dir = tempdir()
# )

res5 <- reduce_dims(
  dfs = dfs5_agg,
  dfs_name = "tab_5D",
  totcode = tot5,
  hrcfiles = hrc5,
  vars_to_merge = c("ACT", "GEO", "SEX", "AGE"), # <-- Force 2 couples (ACT_GEO et SEX_AGE)
  sep_dir = TRUE,
  hrc_dir = tempdir()
)

cat("Nombre de tableaux générés :", length(res5$tabs), "\n")

list_tables_5d <- res5$tabs
list_expl_5d <- res5$vars

all_expl_vars <- unique(unname(unlist(list_expl_5d)))
noms_tbx <- names(list_tables_5d)
noms_col_T <- stats::setNames(paste0("T_", noms_tbx), noms_tbx)



# Bench bloc 1 ------------------------------------------------------------


# Construction de table_majeure_list
table_majeure_list <- purrr::imap(
  .x = list_tables_5d,
  .f = function(tableau, nom_tab){
    tableau <- as.data.frame(tableau)[, c(list_expl_5d[[nom_tab]], "VALUE", "freq", "secret_no_pl", "is_secret_prim")]

    var_a_ajouter <- setdiff(all_expl_vars, names(tableau))
    for (nom_col in var_a_ajouter){
      tableau[[nom_col]] <- unname(
        purrr::keep(res5$alt_totcode, function(x) nom_col %in% names(x))[[1]][nom_col]
      )
    }
    tableau[[noms_col_T[[nom_tab]]]] <- TRUE

    for (col in list_expl_5d[[nom_tab]]) {
      tableau[[col]] <- as.character(tableau[[col]])
    }
    return(tableau)
  }
)

by_vars <- purrr::reduce(purrr::map(table_majeure_list, names), intersect)

# Helper de normalisation pour la comparaison exacte
normalize_for_compare <- function(df, key_cols) {
  df <- as.data.frame(df)
  df <- df[, order(names(df)), drop = FALSE]
  df <- df[do.call(order, df[key_cols]), , drop = FALSE]
  row.names(df) <- NULL
  return(df)
}

# --- CODE EXACT D'ORIGINE DU PACKAGE ---
run_old_block1 <- function(table_majeure_list, by_vars, noms_col_T) {
  table_majeure <- purrr::reduce(
    .x = table_majeure_list,
    .f = merge,
    by = by_vars,
    all = TRUE
  )

  table_majeure$secret_no_pl_iter <- table_majeure$secret_no_pl
  secret_no_pl_iter <- "secret_no_pl_iter"

  purrr::walk(
    noms_col_T,
    function(col_T){
      e_par <- rlang::env_parent()
      e_par$table_majeure[[col_T]] <- ifelse(
        is.na(e_par$table_majeure[[col_T]]),
        FALSE,
        e_par$table_majeure[[col_T]]
      )
    }
  )
  return(table_majeure)
}

# --- CODE PROPOSÉ OPTIMISÉ ---
run_new_block1 <- function(table_majeure_list, by_vars, noms_col_T) {
  dt_list <- lapply(table_majeure_list, data.table::as.data.table)
  dt_all <- data.table::rbindlist(dt_list, use.names = TRUE, fill = TRUE)
  table_majeure <- dt_all[, lapply(.SD, function(col) any(!is.na(col))), by = by_vars, .SDcols = noms_col_T]
  data.table::setDT(table_majeure)

  table_majeure[, secret_no_pl_iter := secret_no_pl]
  secret_no_pl_iter <- "secret_no_pl_iter"

  for (col_T in noms_col_T) {
    data.table::set(table_majeure, i = which(is.na(table_majeure[[col_T]])), j = col_T, value = FALSE)
  }
  return(as.data.frame(table_majeure))
}

t_res_old_b1 <- system.time({
  res_old_b1 <- run_old_block1(table_majeure_list, by_vars, noms_col_T)
})
cat("Temps res_old_b1 :\n")
print(t_res_old_b1)

t_res_new_b1 <- system.time({
  res_new_b1 <- run_new_block1(table_majeure_list, by_vars, noms_col_T)
})
cat("Temps res_new_b1 :\n")
print(t_res_new_b1)

b1_is_identical <- identical(
  normalize_for_compare(res_old_b1, by_vars),
  normalize_for_compare(res_new_b1, by_vars)
)
cat("Nombre de lignes Original :", nrow(res_old_b1), "\n")
cat("Nombre de lignes Optimisé :", nrow(res_new_b1), "\n")
cat("Block 1 Verification: Are outputs 100% identical?", b1_is_identical, "\n\n")

benchmark_b1 <- microbenchmark(
  "Old: purrr::reduce(merge)" = run_old_block1(table_majeure_list, by_vars, noms_col_T),
  "New: rbindlist + group-by" = run_new_block1(table_majeure_list, by_vars, noms_col_T),
  times = 3
)
print(benchmark_b1)
cat("\n----------------------------------------------------------------------\n\n")


# Bloc 2 ------------------------------------------------------------------

cat("======================================================================\n")
cat("2. BENCHMARK BLOCK 2: In-place table_majeure Update (Processing Phase)\n")
cat("======================================================================\n")

# Prise de la table_majeure generee au Bloc 1
tm_base <- res_old_b1
nom_tab_1 <- noms_tbx[1]

# Identification des lignes de la premiere sous-table et extraction de ses variables explicatives
active_idx <- which(tm_base[[paste0("T_", nom_tab_1)]])
ex_var_1 <- list_expl_5d[[nom_tab_1]]

# Simulation du resultat renvoye par Tau-Argus pour ce sous-tableau
res_tau <- tm_base[active_idx, ex_var_1, drop = FALSE]
set.seed(42)
res_tau$is_secret <- sample(c(TRUE, FALSE), length(active_idx), replace = TRUE, prob = c(0.2, 0.8))

# Noms des variables de secret pour l'iteration 1
var_sec_apriori <- "is_secret_prim"
var_sec_new <- "is_secret_1"
secret_var_name <- "is_secret_prim"

# Préparation préalable de table_majeure au format data.table
# Reflète fidèlement le déroulement réel où table_majeure est DÉJÀ un data.table dans la boucle
t_conv <- system.time({
  tm_dt_base <- data.table::as.data.table(tm_base)
})

cat("Temps de conversion data.frame -> data.table :\n")
print(t_conv)

# --- CODE EXACT D'ORIGINE DU PACKAGE ---
run_old_block2 <- function(table_majeure, res, var_secret_apriori, var_secret, secret_var) {
  table_majeure <- merge(table_majeure, res, all = TRUE)
  table_majeure[[var_secret]] <- table_majeure$is_secret
  table_majeure <- subset(
    table_majeure,
    select = setdiff(names(table_majeure), "is_secret")
  )

  table_majeure[[var_secret]] <- ifelse(
    is.na(table_majeure[[var_secret]]),
    table_majeure[[var_secret_apriori]],
    table_majeure[[var_secret]]
  )

  table_majeure$secret_no_pl_iter <- ifelse(
    table_majeure[[secret_var]],
    table_majeure$secret_no_pl,
    table_majeure[[var_secret]]
  )
  return(table_majeure)
}

# --- CODE PROPOSÉ OPTIMISÉ (data.table) ---
run_new_block2 <- function(table_majeure_dt, res, var_secret_apriori, var_secret, secret_var, all_expl, list_tot) {

  # NEW 2 : Seule une copie très légère de sécurité est faite pour éviter que les répétitions
  # NEW 2 : du microbenchmark ne modifient l'objet initial entre deux répétitions
  tm_dt <- data.table::copy(table_majeure_dt)

  # NEW 2 : Passage de res en data.table par référence (aligné sur le code réel)
  res_dt <- data.table::as.data.table(res)

  # Correction mode générique : compléter res avec les variables explicatives absentes de res (fixées à leur total)
  missing_expl <- setdiff(all_expl, names(res_dt))
  for (v in missing_expl) {
    val_tot <- unname(purrr::keep(list_tot, function(x) v %in% names(x))[[1]][v])
    data.table::set(res_dt, j = v, value = as.character(val_tot))
  }

  # Initialisation de la colonne d'itération avec l'état précédent
  tm_dt[, (var_secret) := get(var_secret_apriori)]

  # Mise à jour en place par référence sur all_expl_vars
  tm_dt[res_dt, (var_secret) := i.is_secret, on = all_expl]

  # Mise à jour de la colonne de gestion du PL
  tm_dt[, secret_no_pl_iter := data.table::fifelse(get(secret_var), secret_no_pl, get(var_secret))]

  return(as.data.frame(tm_dt))
}

# Exécution des deux versions
t_res_old_b2 <- system.time({
  res_old_b2 <- run_old_block2(tm_base, res_tau, var_sec_apriori, var_sec_new, secret_var_name)
})
cat("Temps res_old_b2 :\n")
print(t_res_old_b2)

# On passe tm_dt_base (déjà converti en data.table) à la version optimisée
t_res_new_b2<- system.time({
  res_new_b2 <- run_new_block2(tm_dt_base, res_tau, var_sec_apriori, var_sec_new, secret_var_name, all_expl_vars, res5$alt_totcode)
})
cat("Temps res_new_b2 :\n")
print(t_res_new_b2)


# Vérification de l'égalité exacte des résultats
b2_is_identical <- identical(
  normalize_for_compare(res_old_b2, by_vars),
  normalize_for_compare(res_new_b2, by_vars)
)

cat("Nombre de lignes Original :", nrow(res_old_b2), "\n")
cat("Nombre de lignes Optimisé :", nrow(res_new_b2), "\n")
cat("Block 2 Verification: Are outputs 100% identical?", b2_is_identical, "\n\n")

# Benchmark de performance
benchmark_b2 <- microbenchmark(
  "Old: merge + ifelse" = run_old_block2(tm_base, res_tau, var_sec_apriori, var_sec_new, secret_var_name),

  # NEW 2 : Le benchmark mesure la fonction opérant directement sur data.table
  "New: in-place data.table" = run_new_block2(tm_dt_base, res_tau, var_sec_apriori, var_sec_new, secret_var_name, all_expl_vars, res5$alt_totcode),
  times = 3
)
print(benchmark_b2)
cat("\n----------------------------------------------------------------------\n\n")


# Bloc 3 ------------------------------------------------------------------

cat("======================================================================\n")
cat("3. BENCHMARK BLOCK 3: Common Cell Detection & Queue Update\n")
cat("======================================================================\n")

# Prise de la table_majeure issue du Bloc 2
tm_b3 <- res_old_b2
nom_tab_1 <- noms_tbx[1]

# On simule un changement de statut de secret sur 15 cellules du sous-tableau courant
active_idx <- which(tm_b3[[paste0("T_", nom_tab_1)]])
tm_b3$is_secret_2 <- tm_b3$is_secret_1
changed_rows <- active_idx[1:15]
tm_b3$is_secret_2[changed_rows] <- !tm_b3$is_secret_1[changed_rows]

# Todolist et remainlist initiales fictives
todolist_init <- noms_tbx[1:3]
remainlist_init <- noms_tbx[4:length(noms_tbx)]

var_sec_apriori <- "is_secret_1"
var_sec_new <- "is_secret_2"

# --- CODE EXACT D'ORIGINE DU PACKAGE ---
run_old_block3 <- function(table_majeure, var_secret_apriori, var_secret, noms_col_T, num_tableau, all_expl, todolist, remainlist) {
  lignes_modifs <- which(table_majeure[[var_secret_apriori]] != table_majeure[[var_secret]])

  cur_tab <- paste0("T_", num_tableau)
  other_tabs <- setdiff(noms_col_T, cur_tab)
  cur_cells <- rowSums(table_majeure[, cur_tab, drop=FALSE])
  other_cells <- rowSums(table_majeure[, other_tabs, drop=FALSE])

  common_cells_rows <- which(cur_cells == 1 & other_cells > 0)
  common_cells <- table_majeure[common_cells_rows, , drop=FALSE]

  modified <- common_cells[common_cells[[var_secret_apriori]] != common_cells[[var_secret]], all_expl, drop=FALSE]

  noms_tbx_local <- names(noms_col_T)
  for(tab in noms_tbx_local){
    nom_col_identifiante <- paste0("T_", tab)
    if( !(tab %in% todolist)
        & (any(table_majeure[[nom_col_identifiante]][lignes_modifs]))
    ){
      todolist <- append(todolist, tab)
      remainlist <- remainlist[remainlist != tab]
    }
  }

  return(list(modified = modified, todolist = todolist, remainlist = remainlist))
}

# --- CODE PROPOSÉ OPTIMISÉ (data.table) ---
run_new_block3 <- function(table_majeure, var_secret_apriori, var_secret, noms_col_T, num_tableau, all_expl, active_rows, todolist, remainlist) {
  tm_dt <- data.table::as.data.table(table_majeure)
  nom_col_identifiante <- paste0("T_", num_tableau)
  all_col_T <- unname(noms_col_T)

  # Restricton stricte de la recherche aux seules lignes de la sous-table courante
  idx_changed <- tm_dt[[var_secret_apriori]][active_rows] != tm_dt[[var_secret]][active_rows]
  lignes_modifs <- active_rows[idx_changed]

  other_tabs <- setdiff(all_col_T, nom_col_identifiante)
  common_modified_idx <- integer(0)

  if (length(lignes_modifs) > 0) {
    if (length(other_tabs) > 0) {
      # rowSums calcule uniquement sur les 15 lignes modifiees (au lieu des 41 817 lignes)
      is_common <- rowSums(as.matrix(tm_dt[lignes_modifs, ..other_tabs])) > 0
      common_modified_idx <- lignes_modifs[is_common]
    }

    noms_tbx_local <- names(noms_col_T)
    for(tab in noms_tbx_local){
      nom_col_identifiante_tab <- paste0("T_", tab)
      if( !(tab %in% todolist)
          && any(tm_dt[[nom_col_identifiante_tab]][lignes_modifs])
      ){
        todolist <- c(todolist, tab)
        remainlist <- remainlist[remainlist != tab]
      }
    }
  }

  if (length(common_modified_idx) > 0) {
    modified <- as.data.frame(tm_dt[common_modified_idx, ..all_expl])
  } else {
    modified <- as.data.frame(tm_dt[0, ..all_expl])
  }

  return(list(modified = modified, todolist = todolist, remainlist = remainlist))
}

# Execution des deux versions
t_res_old_b3 <- system.time({
  res_old_b3 <- run_old_block3(tm_b3, var_sec_apriori, var_sec_new, noms_col_T, nom_tab_1, all_expl_vars, todolist_init, remainlist_init)
})
cat("Temps t_res_old_b3 :\n")
print(t_res_old_b3)

t_res_new_b3 <- system.time({
  res_new_b3 <- run_new_block3(tm_b3, var_sec_apriori, var_sec_new, noms_col_T, nom_tab_1, all_expl_vars, active_idx, todolist_init, remainlist_init)
})
cat("Temps t_res_new_b3 :\n")
print(t_res_new_b3)

# Verification de l'egalite exacte du tableau "modified" et de la file "todolist"
b3_mod_identical <- identical(
  normalize_for_compare(res_old_b3$modified, all_expl_vars),
  normalize_for_compare(res_new_b3$modified, all_expl_vars)
)
b3_todo_identical <- identical(res_old_b3$todolist, res_new_b3$todolist)

cat("Block 3 Verification: Are modified cells 100% identical?", b3_mod_identical, "\n")
cat("Block 3 Verification: Are todolists 100% identical?", b3_todo_identical, "\n\n")

# Benchmark de performance
benchmark_b3 <- microbenchmark(
  "Old: rowSums on full 41k rows" = run_old_block3(tm_b3, var_sec_apriori, var_sec_new, noms_col_T, nom_tab_1, all_expl_vars, todolist_init, remainlist_init),
  "New: rowSums on modified rows" = run_new_block3(tm_b3, var_sec_apriori, var_sec_new, noms_col_T, nom_tab_1, all_expl_vars, active_idx, todolist_init, remainlist_init),
  times = 3
)
print(benchmark_b3)
cat("\n----------------------------------------------------------------------\n\n")

# Bloc 4 ------------------------------------------------------------------

cat("======================================================================\n")
cat("4. BENCHMARK BLOCK 4: Final Sub-table Assembly (Processing Phase)\n")
cat("======================================================================\n")

# Prise de la table_majeure mise a jour issue du Bloc 3
tm_b4 <- tm_b3

# --- CODE EXACT D'ORIGINE DU PACKAGE ---
run_old_block4 <- function(tbl_list, expl_list, tm) {
  purrr::imap(
    tbl_list,
    function(tab, nom){
      expl_vars <- expl_list[[nom]]
      tab_rows <- tm[[paste0("T_", nom)]]
      secret_vars <- names(tm)[grep("^is_secret_[1-9]", names(tm))]
      secret_vars <- secret_vars[order(as.integer(gsub("is_secret_", "", secret_vars)))]
      res <- merge(
        tab,
        tm[tab_rows, c(expl_vars, secret_vars)],
        all.x = TRUE, all.y = FALSE, by = expl_vars
      )
    }
  )
}

# --- CODE PROPOSÉ OPTIMISÉ (data.table) ---
run_new_block4 <- function(tbl_list, expl_list, tm) {
  tm_dt <- data.table::as.data.table(tm)
  secret_vars <- names(tm_dt)[grep("^is_secret_[1-9]", names(tm_dt))]
  secret_vars <- secret_vars[order(as.integer(gsub("is_secret_", "", secret_vars)))]

  purrr::imap(
    tbl_list,
    function(tab, nom){
      expl_vars <- expl_list[[nom]]
      tab_rows <- tm_dt[[paste0("T_", nom)]]

      sub_majeure <- tm_dt[tab_rows, c(expl_vars, secret_vars), with = FALSE]

      tab_is_dt <- data.table::is.data.table(tab)
      # Copy si deja un data.table pour eviter toute mutation de memoire utilisateur
      tab_dt <- if (tab_is_dt) data.table::copy(tab) else data.table::as.data.table(tab)

      # Sauvegarde des types d'origine
      original_classes <- vapply(expl_vars, function(v) class(tab[[v]])[1], character(1))

      for (v in expl_vars) {
        if (v %in% names(tab_dt)) data.table::set(tab_dt, j = v, value = as.character(tab_dt[[v]]))
      }

      res_dt <- merge(tab_dt, sub_majeure, by = expl_vars, all.x = TRUE, sort = FALSE)

      # Restauration des types d'origine (factor, integer, etc.)
      for (v in expl_vars) {
        cls <- original_classes[v]
        if (cls == "factor") {
          data.table::set(res_dt, j = v, value = factor(res_dt[[v]]))
        } else if (cls == "integer") {
          data.table::set(res_dt, j = v, value = as.integer(res_dt[[v]]))
        }
      }

      if (!tab_is_dt) {
        return(as.data.frame(res_dt))
      } else {
        return(res_dt)
      }
    }
  )
}

# Execution des deux versions avec system.time
t_res_old_b4 <- system.time({
  res_old_b4 <- run_old_block4(list_tables_5d, list_expl_5d, tm_b4)
})
cat("Temps t_res_old_b4 :\n")
print(t_res_old_b4)

t_res_new_b4 <- system.time({
  res_new_b4 <- run_new_block4(list_tables_5d, list_expl_5d, tm_b4)
})
cat("Temps t_res_new_b4 :\n")
print(t_res_new_b4)

# Verification de l'egalite exacte pour chacun des sous-tableaux restitues
b4_results_identical <- vapply(seq_along(res_old_b4), function(i) {
  nom_t <- names(res_old_b4)[i]
  expl_v <- list_expl_5d[[nom_t]]
  identical(
    normalize_for_compare(res_old_b4[[i]], expl_v),
    normalize_for_compare(res_new_b4[[i]], expl_v)
  )
}, logical(1))

cat("\nBlock 4 Verification: Are all output tables 100% identical?", all(b4_results_identical), "\n\n")

# Benchmark de performance
benchmark_b4 <- microbenchmark(
  "Old: sequential base merges" = run_old_block4(list_tables_5d, list_expl_5d, tm_b4),
  "New: data.table merges" = run_new_block4(list_tables_5d, list_expl_5d, tm_b4),
  times = 3
)
print(benchmark_b4)
cat("\n----------------------------------------------------------------------\n\n")

# Bloc 5 ------------------------------------------------------------------

cat("======================================================================\n")
cat("5. BENCHMARK BLOCK 5: Full End-to-End Multi-Iteration Integration Test \n")
cat("======================================================================\n")

Sys.time()
loc_tauargus <- "Y:/Logiciels/TauArgus/TauArgus4.2.2b1/TauArgus.exe"
options(rtauargus.tauargus_exe = loc_tauargus)

# 1. Reprise des sous-tables et ajout d'une variable de cout (Inclusion de cost_var)
list_tables_5d_cost <- lapply(list_tables_5d, function(df) {
  df$cost_col <- round(runif(nrow(df), 1, 100))
  df
})

dir_old <- file.path(tempdir(), "test_old_real")
dir_new <- file.path(tempdir(), "test_new_real")
if (!dir.exists(dir_old)) dir.create(dir_old, recursive = TRUE)
if (!dir.exists(dir_new)) dir.create(dir_new, recursive = TRUE)

# Initialisation des résultats
res_full_old <- NULL
t_full_old   <- NULL
res_full_new <- NULL
t_full_new   <- NULL

# 2. Exécution version ORIGINALE -------------------------------------------
cat("\nLancement de tab_multi_manager version ORIGINALE (Multi-itérations)...\n")
tryCatch({
  t_full_old <- system.time({
    res_full_old <- tab_multi_manager_old(
      list_tables           = list_tables_5d_cost,
      list_explanatory_vars = list_expl_5d,
      totcode               = res5$totcode,
      hrc                   = res5$hrc,
      alt_totcode           = res5$alt_totcode,
      alt_hrc               = res5$alt_hrc,
      value                 = "VALUE",
      freq                  = "freq",
      secret_var            = "is_secret_prim",
      cost_var              = "cost_col",
      suppress              = "GH(1,100)", # Hypercube rapide
      dir_name              = dir_old
    )
  })
  cat("Temps version ORIGINALE :\n")
  print(t_full_old)
}, error = function(e) {
  cat("\n[ERREUR] Échec de tab_multi_manager_old :\n")
  message(e$message)
})

# 3. Exécution version OPTIMISÉE ------------------------------------------
cat("\nLancement de tab_multi_manager version OPTIMISÉE (Multi-itérations)...\n")
tryCatch({
  t_full_new <- system.time({
    res_full_new <- tab_multi_manager(
      list_tables           = list_tables_5d_cost,
      list_explanatory_vars = list_expl_5d,
      totcode               = res5$totcode,
      hrc                   = res5$hrc,
      alt_totcode           = res5$alt_totcode,
      alt_hrc               = res5$alt_hrc,
      value                 = "VALUE",
      freq                  = "freq",
      secret_var            = "is_secret_prim",
      cost_var              = "cost_col",
      suppress              = "GH(1,100)", # Hypercube rapide
      dir_name              = dir_new
    )
  })
  cat("Temps version OPTIMISÉE :\n")
  print(t_full_new)
}, error = function(e) {
  cat("\n[ERREUR] Échec de tab_multi_manager :\n")
  message(e$message)
})

# 4. VERIFICATION INTEGRALE ET RIGOUREUSE DE BOUT EN BOUT
if (!is.null(res_full_old) && !is.null(res_full_new)) {

  check_e2e <- vapply(seq_along(res_full_old), function(i) {
    nom_t <- names(res_full_old)[i]
    expl_v <- list_expl_5d[[nom_t]]

    df_o <- res_full_old[[i]]
    df_n <- res_full_new[[i]]

    identical(
      normalize_for_compare(df_o, expl_v),
      normalize_for_compare(df_n, expl_v)
    )
  }, logical(1))

  cat("\n======================================================================\n")
  cat("RÉSULTAT DU TEST D'INTÉGRATION GLOBAL (END-TO-END)\n")
  cat("======================================================================\n")
  cat("1. Nombre de tableaux traites (", length(res_full_old), ") : TRUE\n", sep = "")
  cat("2. Egalite stricte de TOUTES les itérations et colonnes :", all(check_e2e), "\n")

  if (all(check_e2e)) {
    cat("\n>>> SUCCÈS TOTAL : 100% d'équivalence logique sur toutes les itérations !\n")
    cat(">>> Gain de temps global de bout en bout : x", round(t_full_old["elapsed"] / t_full_new["elapsed"], 1), "\n")
  } else {
    cat("\n>>> ERREUR : Divergence detectee dans les tables :", names(res_full_old)[!check_e2e], "\n")
  }

} else {
  cat("\n======================================================================\n")
  cat("COMPARAISON IMPOSSIBLE : Au moins une des deux fonctions a échoué.\n")
  cat("======================================================================\n")
}


# Bloc 6 - unif_labels=FALSE ------------------------------------------------------------------

cat("======================================================================\n")
cat("6. BENCHMARK BLOCK 5: Full End-to-End Multi-Iteration Integration Test \n")
cat("======================================================================\n")

Sys.time()
loc_tauargus <- "Y:/Logiciels/TauArgus/TauArgus4.2.2b1/TauArgus.exe"
options(rtauargus.tauargus_exe = loc_tauargus)

# 1. Reprise des sous-tables et ajout d'une variable de cout (Inclusion de cost_var)
list_tables_5d_cost <- lapply(list_tables_5d, function(df) {
  df$cost_col <- round(runif(nrow(df), 1, 100))
  df
})

dir_old <- file.path(tempdir(), "test_old_real")
dir_new <- file.path(tempdir(), "test_new_real")
if (!dir.exists(dir_old)) dir.create(dir_old, recursive = TRUE)
if (!dir.exists(dir_new)) dir.create(dir_new, recursive = TRUE)

# Initialisation des résultats
res_full_old <- NULL
t_full_old   <- NULL
res_full_new <- NULL
t_full_new   <- NULL

# 2. Exécution version ORIGINALE -------------------------------------------
cat("\nLancement de tab_multi_manager version ORIGINALE (Multi-itérations)...\n")
tryCatch({
  t_full_old <- system.time({
    res_full_old <- tab_multi_manager_old(
      list_tables           = list_tables_5d_cost,
      list_explanatory_vars = list_expl_5d,
      totcode               = res5$totcode,
      hrc                   = res5$hrc,
      alt_totcode           = res5$alt_totcode,
      alt_hrc               = res5$alt_hrc,
      value                 = "VALUE",
      freq                  = "freq",
      secret_var            = "is_secret_prim",
      cost_var              = "cost_col",
      suppress              = "GH(1,100)", # Hypercube rapide
      unif_labels           = FALSE,
      dir_name              = dir_old
    )
  })
  cat("Temps version ORIGINALE :\n")
  print(t_full_old)
}, error = function(e) {
  cat("\n[ERREUR] Échec de tab_multi_manager_old :\n")
  message(e$message)
})

# 3. Exécution version OPTIMISÉE ------------------------------------------
cat("\nLancement de tab_multi_manager version OPTIMISÉE (Multi-itérations)...\n")
tryCatch({
  t_full_new <- system.time({
    res_full_new <- tab_multi_manager(
      list_tables           = list_tables_5d_cost,
      list_explanatory_vars = list_expl_5d,
      totcode               = res5$totcode,
      hrc                   = res5$hrc,
      alt_totcode           = res5$alt_totcode,
      alt_hrc               = res5$alt_hrc,
      value                 = "VALUE",
      freq                  = "freq",
      secret_var            = "is_secret_prim",
      cost_var              = "cost_col",
      suppress              = "GH(1,100)", # Hypercube rapide
      unif_labels           = FALSE,
      dir_name              = dir_new
    )
  })
  cat("Temps version OPTIMISÉE :\n")
  print(t_full_new)
}, error = function(e) {
  cat("\n[ERREUR] Échec de tab_multi_manager :\n")
  message(e$message)
})

# 4. VERIFICATION INTEGRALE ET RIGOUREUSE DE BOUT EN BOUT
if (!is.null(res_full_old) && !is.null(res_full_new)) {

  check_e2e <- vapply(seq_along(res_full_old), function(i) {
    nom_t <- names(res_full_old)[i]
    expl_v <- list_expl_5d[[nom_t]]

    df_o <- res_full_old[[i]]
    df_n <- res_full_new[[i]]

    identical(
      normalize_for_compare(df_o, expl_v),
      normalize_for_compare(df_n, expl_v)
    )
  }, logical(1))

  cat("\n======================================================================\n")
  cat("RÉSULTAT DU TEST D'INTÉGRATION GLOBAL (END-TO-END)\n")
  cat("======================================================================\n")
  cat("1. Nombre de tableaux traites (", length(res_full_old), ") : TRUE\n", sep = "")
  cat("2. Egalite stricte de TOUTES les itérations et colonnes :", all(check_e2e), "\n")

  if (all(check_e2e)) {
    cat("\n>>> SUCCÈS TOTAL : 100% d'équivalence logique sur toutes les itérations !\n")
    cat(">>> Gain de temps global de bout en bout : x", round(t_full_old["elapsed"] / t_full_new["elapsed"], 1), "\n")
  } else {
    cat("\n>>> ERREUR : Divergence detectee dans les tables :", names(res_full_old)[!check_e2e], "\n")
  }

} else {
  cat("\n======================================================================\n")
  cat("COMPARAISON IMPOSSIBLE : Au moins une des deux fonctions a échoué.\n")
  cat("======================================================================\n")
}

