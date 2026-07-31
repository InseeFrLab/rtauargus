create_big_4D <- function(
    n = 200000,
    n_act_groups = 30,
    n_act_sub = 50,
    n_geo_regions = 50,
    n_geo_sub = 50,
    n_sex = 2,
    n_age = 4,
    add_dim5 = FALSE,
    complete_grid = TRUE
){
  set.seed(123)

  w_act_g <- max(2, nchar(n_act_groups))
  w_act_s <- max(2, nchar(n_act_sub))
  w_geo_r <- max(2, nchar(n_geo_regions))
  w_geo_s <- max(2, nchar(n_geo_sub))
  w_age   <- max(2, nchar(n_age))

  fmt_act_g <- paste0("A%0", w_act_g, "d")
  fmt_act_s <- paste0("A%0", w_act_g, "d_%0", w_act_s, "d")   # typo corrigée
  fmt_geo_r <- paste0("G%0", w_geo_r, "d")
  fmt_geo_s <- paste0("G%0", w_geo_r, "d_%0", w_geo_s, "d")
  fmt_age   <- paste0("AGE%0", w_age, "d")

  act_level1 <- sprintf(fmt_act_g, 1:n_act_groups)
  act_level2 <- unlist(lapply(1:n_act_groups, function(i) sprintf(fmt_act_s, i, 1:n_act_sub)))
  geo_level1 <- sprintf(fmt_geo_r, 1:n_geo_regions)
  geo_level2 <- unlist(lapply(1:n_geo_regions, function(i) sprintf(fmt_geo_s, i, 1:n_geo_sub)))

  sex_leaves <- if (n_sex == 2) c("F", "M") else {
    w_sex <- max(2, nchar(n_sex))
    sprintf(paste0("S%0", w_sex, "d"), 1:n_sex)
  }
  age_leaves <- sprintf(fmt_age, 1:n_age)

  dim_vars <- if (add_dim5) c("ACT", "GEO", "SEX", "AGE", "ECO")
  else          c("ACT", "GEO", "SEX", "AGE")

  # ---- 1. Microdonnées FEUILLES uniquement --------------------------------
  dt <- data.table(
    ACT = sample(act_level2, n, replace = TRUE),
    GEO = sample(geo_level2, n, replace = TRUE),
    SEX = sample(sex_leaves, n, replace = TRUE),
    AGE = sample(age_leaves, n, replace = TRUE)
  )
  if (add_dim5) dt[, ECO := sample(c("mod1", "mod2"), n, replace = TRUE)]

  # Agrégation feuilles : libère immédiatement les 50M lignes
  dt_leaf <- dt[, .(VALUE = .N), by = dim_vars]
  rm(dt); invisible(gc())

  # ---- 2. Tables de correspondance hiérarchiques --------------------------
  # mk_map : leaf → {leaf, parent (si fourni), "Total"}
  mk_map <- function(leaves, parents = NULL) {
    rows <- list(data.table(from = leaves, to = leaves))
    if (!is.null(parents)) rows[[2]] <- data.table(from = leaves, to = parents)
    rows[[length(rows) + 1]] <- data.table(from = leaves, to = "Total")
    rbindlist(rows)
  }

  map_act <- mk_map(act_level2, sub("_[0-9]+$", "", act_level2))
  map_geo <- mk_map(geo_level2, sub("_[0-9]+$", "", geo_level2))
  map_sex <- mk_map(sex_leaves)
  map_age <- mk_map(age_leaves)

  # ---- 3. Expansion dimension par dimension (join data.table) -------------
  # Pour chaque dimension : on remplace la valeur feuille par TOUTES ses
  # valeurs hiérarchiques (leaf, parent, Total) via un join one-to-many.
  rollup_dim <- function(dt, col, map) {
    # map[dt, ...] : right-join — pour chaque ligne de dt, trouve les
    # correspondances dans map (3 ou 2 lignes par valeur feuille)
    result <- map[dt, on = c(from = col), allow.cartesian = TRUE]
    # result a : from | to | [autres colonnes de dt sauf col]
    result[, (col) := to][, c("from", "to") := NULL]
    result
  }

  dt_exp <- rollup_dim(dt_leaf, "ACT", map_act)
  dt_exp <- rollup_dim(dt_exp,  "GEO", map_geo)
  dt_exp <- rollup_dim(dt_exp,  "SEX", map_sex)
  dt_exp <- rollup_dim(dt_exp,  "AGE", map_age)

  if (add_dim5) dt_exp <- rollup_dim(dt_exp, "ECO", mk_map(c("mod1", "mod2")))

  rm(dt_leaf); invisible(gc())

  # ---- 4. Agrégation finale -----------------------------------------------
  # À ce stade dt_exp a : 96 000 × 3 × 3 × 2 × 2 ≈ 3,5 M lignes (4D)
  dfs_agg <- dt_exp[, .(VALUE = sum(VALUE)), by = dim_vars]
  rm(dt_exp); invisible(gc())

  # ---- 4b. COMPLÉTION DE LA GRILLE AVEC LES 0 (Si complete_grid = TRUE) ---
  if (complete_grid) {
    grid_all <- list(
      ACT = unique(map_act$to),
      GEO = unique(map_geo$to),
      SEX = unique(map_sex$to),
      AGE = unique(map_age$to)
    )
    if (add_dim5) grid_all$ECO <- c("mod1", "mod2", "Total")

    # Produit cartésien de toutes les modalités (feuilles, parents, Total)
    full_grid <- do.call(CJ, c(grid_all, list(sorted = FALSE)))

    # Jointure avec la grille complète et remplacement des NA par 0
    dfs_agg <- dfs_agg[full_grid, on = dim_vars]
    dfs_agg[is.na(VALUE), VALUE := 0]
  }

  # ---- 5. Fichiers .hrc ---------------------------------------------------
  write_hrc <- function(hier, path) {
    hier %>%
      sdcHierarchies::hier_convert(as = "argus") %>%
      dplyr::slice(-1) %>%
      dplyr::mutate(levels = substring(paste0(level, name), 3)) %>%
      dplyr::select(levels) %>%
      write.table(file = path, row.names = FALSE, col.names = FALSE, quote = FALSE)
  }

  hrc_act <- tempfile(fileext = ".hrc")
  h_act <- sdcHierarchies::hier_create(root = "Total", nodes = act_level1)
  for (i in seq_len(n_act_groups))
    h_act <- sdcHierarchies::hier_add(h_act, root = sprintf(fmt_act_g, i),
                                      nodes = sprintf(fmt_act_s, i, 1:n_act_sub))
  write_hrc(h_act, hrc_act)

  hrc_geo <- tempfile(fileext = ".hrc")
  h_geo <- sdcHierarchies::hier_create(root = "Total", nodes = geo_level1)
  for (i in seq_len(n_geo_regions))
    h_geo <- sdcHierarchies::hier_add(h_geo, root = sprintf(fmt_geo_r, i),
                                      nodes = sprintf(fmt_geo_s, i, 1:n_geo_sub))
  write_hrc(h_geo, hrc_geo)

  totcode <- if (add_dim5)
    c(SEX = "Total", AGE = "Total", GEO = "Total", ACT = "Total", ECO = "Total")
  else
    c(SEX = "Total", AGE = "Total", GEO = "Total", ACT = "Total")

  list(data = dfs_agg, hrcfiles = c(ACT = hrc_act, GEO = hrc_geo), totcode = totcode)
}
# =============================================================================
# 00_setup_bench_options
# Génère données 5D pour le benchmark min/smart/max
# =============================================================================

BENCH_DIR <- "benchmark_options_5D"
dir.create(file.path(BENCH_DIR, "hrc"), recursive = TRUE, showWarnings = FALSE)

library(data.table); library(dplyr); library(sdcHierarchies)

set.seed(123)
big5 <- create_big_4D(
  n            = 900000,
  n_act_groups = 4,  n_act_sub    = 5,
  n_geo_regions = 5, n_geo_sub    = 6,
  n_sex        = 10, n_age        = 15,
  add_dim5     = TRUE
)

hrc5 <- c(
  ACT = file.path(BENCH_DIR, "hrc", "hrc_ACT.hrc"),
  GEO = file.path(BENCH_DIR, "hrc", "hrc_GEO.hrc")
)
file.copy(big5$hrcfiles["ACT"], hrc5["ACT"], overwrite = TRUE)
file.copy(big5$hrcfiles["GEO"], hrc5["GEO"], overwrite = TRUE)

tot5 <- big5$totcode

dfs5_agg = big5$data

dfs5_agg %>% tally()
dfs5_agg %>% arrange(VALUE)

dfs5_agg %>% summarise(secret_prim = mean(as.integer(VALUE > 0 & VALUE <= 3)))

dfs5_agg$is_secret_prim <- dfs5_agg$VALUE  > 0 & dfs5_agg$VALUE  <= 3
dfs5_agg$secret_no_pl <- FALSE
dfs5_agg$freq <- dfs5_agg$VALUE

cat("Nombre de cellules :", nrow(dfs5_agg), "\n")
cat("Secret primaire    :", sum(dfs5_agg$is_secret_prim),
    sprintf("(%.2f%%)\n", 100 * mean(dfs5_agg$is_secret_prim)))

save(dfs5_agg, tot5, hrc5,
     file = file.path(BENCH_DIR, "input_data_5d.RData"))
cat("Setup 5D OK →", BENCH_DIR, "\n")
cat("Redémarrer R, puis lancer bench_nb_tab_option.R\n")

# =============================================================================
# bench_nb_tab_option.R
# Compare nb_tab_option "min" / "smart" / "max"
# =============================================================================

BENCH_DIR    <- "benchmark_options_5D"
loc_tauargus <- "Y:/Logiciels/TauArgus/TauArgus4.2.2b1/TauArgus.exe"

library(data.table); library(dplyr)
devtools::load_all("Z:/rtauargus")
options(rtauargus.tauargus_exe = loc_tauargus)

load(file.path(BENCH_DIR, "input_data_5d.RData"))  # dfs5_agg, tot5, hrc5

# =============================================================================
# UTILITAIRES
# =============================================================================

capture_gc <- function(label) {
  g <- gc()
  data.frame(
    label = label, type = rownames(g),
    used_mb = g[, 2], trigger_mb = g[, 4], max_mb = g[, 6],
    row.names = NULL
  )
}

get_last_secret_col <- function(df) {
  cols <- names(df)[grepl("^is_secret_[0-9]+$", names(df))]
  if (!length(cols)) return(NULL)
  cols[order(as.integer(sub("is_secret_", "", cols)))] |> tail(1)
}

get_secret_stats <- function(df, label) {
  last_col <- get_last_secret_col(df)
  final    <- df[[last_col]]
  prim     <- df$is_secret_prim
  sec      <- final & !prim
  data.frame(
    label              = label,
    n_prim             = sum(prim),
    value_prim         = sum(df$VALUE[prim],  na.rm = TRUE),
    n_sec              = sum(sec),
    value_sec          = sum(df$VALUE[sec],   na.rm = TRUE),
    n_total_masque     = sum(final),
    value_total_masque = sum(df$VALUE[final], na.rm = TRUE),
    pct_masque_cells   = round(100 * mean(final), 2),
    pct_masque_value   = round(100 * sum(df$VALUE[final], na.rm = TRUE) /
                                 sum(df$VALUE, na.rm = TRUE), 2)
  )
}

extract_mask <- function(df, tot) {
  last_col <- get_last_secret_col(df)
  df_min   <- as.data.frame(df)[, c(names(tot), last_col)]
  names(df_min)[ncol(df_min)] <- "final_secret"
  df_min   <- df_min[do.call(order, df_min[names(tot)]), ]
  rownames(df_min) <- NULL
  df_min
}

# =============================================================================
# VARIANTS
# =============================================================================

explore_reduce_dims(dfs = dfs5_agg,
                    totcode = tot5,
                    hrcfiles = hrc5)

# Pour "min" et "max", limit très grand = pas d'oversplit parasite
# Pour "smart", limit change de fonction du scénario
variants <- list(
  list(label = "smart_100000",    nb_tab_option = "smart", limit = 100000L),
  list(label = "smart_45000",    nb_tab_option = "smart", limit = 45000L),
  list(label = "smart_20000",     nb_tab_option = "smart", limit = 20000L),
  list(label = "smart_18000",     nb_tab_option = "smart", limit = 18000L),
  list(label = "smart_13000",     nb_tab_option = "smart", limit = 13000L)
)

# pre-run de reduce dims pour voir combien de tables génèrent les configs
# et si certaines sont redondantes entre elles (même résultat)
for (v in variants) {

  cat("\n======================================================================\n")
  cat("  Variant :", v$label, "(nb_tab_option =", v$nb_tab_option,
      "| limit =", v$limit, ")\n")
  cat("======================================================================\n")

  # --- Pré-mesure : nombre de sous-tables générées ---
  # Appel isolé à reduce_dims pour capturer n_tables avant le run complet.
  # Overhead ~5-10s, négligeable face au run complet.
  dir_v <- file.path(BENCH_DIR, paste0("tauargus_", v$label))
  dir_count <- file.path(dir_v, "count_hrc")
  dir.create(dir_count, recursive = TRUE, showWarnings = FALSE)

  res_count <- reduce_dims(
    dfs           = dfs5_agg,
    dfs_name      = "tab_5d",
    totcode       = tot5,
    hrcfiles      = hrc5,
    nb_tab_option = v$nb_tab_option,
    limit         = v$limit,
    over_split    = TRUE,
    sep_dir       = TRUE,
    hrc_dir       = dir_count,
    verbose       = TRUE # affichera le nombre de tables générées ainsi que leur distribution
  )
  rm(res_count); gc()

}


# HYPERCUBE-------------------------------------------------------------------------
BENCH_DIR <- "Z:/benchmark_options_5D_HYPERCUBE"
dir.create(file.path(BENCH_DIR, "hrc"), recursive = TRUE, showWarnings = FALSE)

results_raw  <- list()
stats_list   <- list()
timings_list <- list()
gc_list      <- list()
ntables_list <- list()

# =============================================================================
# BOUCLE PRINCIPALE
# =============================================================================

for (v in variants) {

  cat("\n======================================================================\n")
  cat("  Variant :", v$label, "(nb_tab_option =", v$nb_tab_option,
      "| limit =", v$limit, ")\n")
  cat("======================================================================\n")

  dir_v <- file.path(BENCH_DIR, paste0("tauargus_", v$label))
  dir.create(file.path(dir_v, "hrc"), recursive = TRUE, showWarnings = FALSE)

  # --- Pré-mesure : nombre de sous-tables générées ---
  # Appel isolé à reduce_dims pour capturer n_tables avant le run complet.
  # Overhead ~5-10s, négligeable face au run complet.
  dir_count <- file.path(dir_v, "count_hrc")
  dir.create(dir_count, recursive = TRUE, showWarnings = FALSE)

  res_count <- reduce_dims(
    dfs           = dfs5_agg,
    dfs_name      = "tab_5d",
    totcode       = tot5,
    hrcfiles      = hrc5,
    nb_tab_option = v$nb_tab_option,
    limit         = v$limit,
    over_split    = TRUE,
    sep_dir       = TRUE,
    hrc_dir       = dir_count,
    verbose       = FALSE
  )
  n_tables <- length(res_count$tabs)
  tailles  <- sapply(res_count$tabs, nrow)

  rm(res_count); gc()

  ntables_list[[v$label]] <- data.frame(
    label    = v$label,
    n_tables = n_tables,
    min_rows = if (length(tailles) > 0) min(tailles) else 0,
    med_rows = if (length(tailles) > 0) median(tailles) else 0,
    max_rows = if (length(tailles) > 0) max(tailles) else 0
  )

  # --- Run complet ---
  gc(reset = TRUE)

  t_v <- system.time({
    res_v <- tab_rtauargus4(
      tabular          = dfs5_agg,
      explanatory_vars = names(tot5),
      dir_name         = dir_v,
      secret_var       = "is_secret_prim",
      totcode          = tot5,
      hrc              = hrc5,
      value            = "VALUE",
      freq             = "freq",
      suppress         = "GH(1,100)",
      nb_tab_option    = v$nb_tab_option,
      dfs_name         = "tab_5d",
      limit            = v$limit,
      keep_history     = FALSE
    )
  })

  results_raw[[v$label]]  <- res_v
  stats_list[[v$label]]   <- get_secret_stats(res_v, v$label)
  timings_list[[v$label]] <- data.frame(
    label   = v$label,
    elapsed = t_v["elapsed"],
    user    = t_v["user.self"],
    sys     = t_v["sys.self"]
  )
  gc_list[[v$label]] <- capture_gc(v$label)

  # Sauvegarde intermédiaire (filet de sécurité)
  write.csv(stats_list[[v$label]],
            file.path(BENCH_DIR, paste0("secret_", v$label, ".csv")),
            row.names = FALSE)
  write.csv(timings_list[[v$label]],
            file.path(BENCH_DIR, paste0("timing_", v$label, ".csv")),
            row.names = FALSE)
  write.csv(gc_list[[v$label]],
            file.path(BENCH_DIR, paste0("gc_", v$label, ".csv")),
            row.names = FALSE)

  # ======================================================================== =
  # 📊 AFFICHAGE IMMÉDIAT EN CONSOLE
  # ======================================================================== =
  cat("\n----------------------------------------------------------------------\n")
  cat("  📊 RÉSULTATS OBTENUS POUR :", v$label, "\n")
  cat("----------------------------------------------------------------------\n")

  cat("⏱️  [Timing] :\n")
  print(timings_list[[v$label]])

  cat("\n🔒 [Statistiques de secret] :\n")
  print(stats_list[[v$label]])

  cat("\n💾 [Mémoire (Vcells)] :\n")
  print(gc_list[[v$label]] %>% filter(type == "Vcells"))

  cat("\n✓", v$label, "terminé avec succès en", round(t_v["elapsed"] / 60, 2), "minutes.\n")
}

# =============================================================================
# RECHARGEMENT DES RÉSULTATS DEPUIS LE DISQUE
# =============================================================================

results_raw  <- list()
stats_list   <- list()
timings_list <- list()
gc_list      <- list()
ntables_list <- list()

cat("Rechargement des fichiers CSV enregistrés...\n")

for (v in variants) {
  lbl <- v$label

  # 1. Lecture des fichiers CSV sauvegardés
  file_secret <- file.path(BENCH_DIR, paste0("secret_", lbl, ".csv"))
  file_timing <- file.path(BENCH_DIR, paste0("timing_", lbl, ".csv"))
  file_gc     <- file.path(BENCH_DIR, paste0("gc_", lbl, ".csv"))

  if (file.exists(file_secret)) stats_list[[lbl]]   <- read.csv(file_secret)
  if (file.exists(file_timing)) timings_list[[lbl]] <- read.csv(file_timing)
  if (file.exists(file_gc))     gc_list[[lbl]]      <- read.csv(file_gc)

  # 2. Recalcul rapide du nombre de sous-tables (reduce_dims seul est très rapide, ~5s)
  dir_v     <- file.path(BENCH_DIR, paste0("tauargus_", lbl))
  dir_count <- file.path(dir_v, "count_hrc")
  dir.create(dir_count, recursive = TRUE, showWarnings = FALSE)

  res_count <- reduce_dims(
    dfs           = dfs5_agg,
    dfs_name      = "tab_5d",
    totcode       = tot5,
    hrcfiles      = hrc5,
    nb_tab_option = v$nb_tab_option,
    limit         = v$limit,
    over_split    = TRUE,
    sep_dir       = TRUE,
    hrc_dir       = dir_count,
    verbose       = FALSE
  )

  n_tables <- length(res_count$tabs)
  tailles  <- sapply(res_count$tabs, nrow)

  ntables_list[[lbl]] <- data.frame(
    label    = lbl,
    n_tables = n_tables,
    min_rows = if (length(tailles) > 0) min(tailles) else 0,
    med_rows = if (length(tailles) > 0) median(tailles) else 0,
    max_rows = if (length(tailles) > 0) max(tailles) else 0
  )

  rm(res_count); gc()
}


# =============================================================================
# BILAN FINAL
# =============================================================================

cat("\n======================================================================\n")
cat("  BILAN COMPARATIF : min / smart_* / max                                \n")
cat("======================================================================\n")

cat("\n[1. Nombre et dimensions des sous-tables générées]\n")
df_tables_summary <- bind_rows(ntables_list) %>%
  rename(
    `Variant`        = label,
    `Nb tables`      = n_tables,
    `Taille min (lignes)` = min_rows,
    `Taille med (lignes)` = med_rows,
    `Taille max (lignes)` = max_rows
  )
print(as.data.frame(df_tables_summary))

cat("\n[2. Timings (elapsed, secondes)]\n")
print(bind_rows(timings_list))

cat("\n[3. Pic mémoire Vcells]\n")
gc_all <- bind_rows(gc_list)
print(gc_all |> filter(type == "Vcells") |> select(label, used_mb, max_mb))

cat("\n[4. Statistiques de secret]\n")
print(bind_rows(stats_list))

# MODULAR -------------------------------------------------------------------------

BENCH_DIR <- "Z:/benchmark_options_5D_MODULAR"
dir.create(file.path(BENCH_DIR, "hrc"), recursive = TRUE, showWarnings = FALSE)

results_raw_mod  <- list()
stats_list_mod   <- list()
timings_list_mod <- list()
gc_list_mod      <- list()
ntables_list_mod <- list()

# =============================================================================
# BOUCLE PRINCIPALE
# =============================================================================

for (v in variants) {

  cat("\n======================================================================\n")
  cat("  Variant :", v$label, "(nb_tab_option =", v$nb_tab_option,
      "| limit =", v$limit, ")\n")
  cat("======================================================================\n")

  dir_v <- file.path(BENCH_DIR, paste0("tauargus_", v$label))
  dir.create(file.path(dir_v, "hrc"), recursive = TRUE, showWarnings = FALSE)

  # --- Pré-mesure : nombre de sous-tables générées ---
  # Appel isolé à reduce_dims pour capturer n_tables avant le run complet.
  # Overhead ~5-10s, négligeable face au run complet.
  dir_count <- file.path(dir_v, "count_hrc")
  dir.create(dir_count, recursive = TRUE, showWarnings = FALSE)

  res_count <- reduce_dims(
    dfs           = dfs5_agg,
    dfs_name      = "tab_5d",
    totcode       = tot5,
    hrcfiles      = hrc5,
    nb_tab_option = v$nb_tab_option,
    limit         = v$limit,
    over_split    = TRUE,
    sep_dir       = TRUE,
    hrc_dir       = dir_count,
    verbose       = FALSE
  )
  n_tables <- length(res_count$tabs)
  tailles  <- sapply(res_count$tabs, nrow)

  rm(res_count); gc()

  ntables_list_mod[[v$label]] <- data.frame(
    label    = v$label,
    n_tables = n_tables,
    min_rows = if (length(tailles) > 0) min(tailles) else 0,
    med_rows = if (length(tailles) > 0) median(tailles) else 0,
    max_rows = if (length(tailles) > 0) max(tailles) else 0
  )

  # --- Run complet ---
  gc(reset = TRUE)

  t_v <- system.time({
    res_v <- tab_rtauargus4(
      tabular          = dfs5_agg,
      explanatory_vars = names(tot5),
      dir_name         = dir_v,
      secret_var       = "is_secret_prim",
      totcode          = tot5,
      hrc              = hrc5,
      value            = "VALUE",
      freq             = "freq",
      nb_tab_option    = v$nb_tab_option,
      dfs_name         = "tab_5d",
      limit            = v$limit,
      keep_history     = FALSE
    )
  })

  results_raw_mod[[v$label]]  <- res_v
  stats_list_mod[[v$label]]   <- get_secret_stats(res_v, v$label)
  timings_list_mod[[v$label]] <- data.frame(
    label   = v$label,
    elapsed = t_v["elapsed"],
    user    = t_v["user.self"],
    sys     = t_v["sys.self"]
  )
  gc_list_mod[[v$label]] <- capture_gc(v$label)

  # Sauvegarde intermédiaire (filet de sécurité)
  write.csv(stats_list_mod[[v$label]],
            file.path(BENCH_DIR, paste0("secret_", v$label, ".csv")),
            row.names = FALSE)
  write.csv(timings_list_mod[[v$label]],
            file.path(BENCH_DIR, paste0("timing_", v$label, ".csv")),
            row.names = FALSE)
  write.csv(gc_list_mod[[v$label]],
            file.path(BENCH_DIR, paste0("gc_", v$label, ".csv")),
            row.names = FALSE)

  # ======================================================================== =
  # 📊 AFFICHAGE IMMÉDIAT EN CONSOLE
  # ======================================================================== =
  cat("\n----------------------------------------------------------------------\n")
  cat("  📊 RÉSULTATS OBTENUS POUR :", v$label, "\n")
  cat("----------------------------------------------------------------------\n")

  cat("⏱️  [Timing] :\n")
  print(gc_list_mod[[v$label]])

  cat("\n🔒 [Statistiques de secret] :\n")
  print(timings_list_mod[[v$label]])

  cat("\n💾 [Mémoire (Vcells)] :\n")
  print(gc_list_mod[[v$label]] %>% filter(type == "Vcells"))

  cat("\n✓", v$label, "terminé avec succès en", round(t_v["elapsed"] / 60, 2), "minutes.\n")
}

# =============================================================================
# RECHARGEMENT DES RÉSULTATS DEPUIS LE DISQUE
# =============================================================================

results_raw_mod  <- list()
stats_list_mod   <- list()
timings_list_mod <- list()
gc_list_mod      <- list()
ntables_list_mod <- list()

cat("Rechargement des fichiers CSV enregistrés...\n")

for (v in variants) {
  lbl <- v$label

  # 1. Lecture des fichiers CSV sauvegardés
  file_secret <- file.path(BENCH_DIR, paste0("secret_", lbl, ".csv"))
  file_timing <- file.path(BENCH_DIR, paste0("timing_", lbl, ".csv"))
  file_gc     <- file.path(BENCH_DIR, paste0("gc_", lbl, ".csv"))

  if (file.exists(file_secret)) stats_list_mod[[lbl]]   <- read.csv(file_secret)
  if (file.exists(file_timing)) timings_list_mod[[lbl]] <- read.csv(file_timing)
  if (file.exists(file_gc))     gc_list_mod[[lbl]]      <- read.csv(file_gc)

  # 2. Recalcul rapide du nombre de sous-tables (reduce_dims seul est très rapide, ~5s)
  dir_v     <- file.path(BENCH_DIR, paste0("tauargus_", lbl))
  dir_count <- file.path(dir_v, "count_hrc")
  dir.create(dir_count, recursive = TRUE, showWarnings = FALSE)

  res_count <- reduce_dims(
    dfs           = dfs5_agg,
    dfs_name      = "tab_5d",
    totcode       = tot5,
    hrcfiles      = hrc5,
    nb_tab_option = v$nb_tab_option,
    limit         = v$limit,
    over_split    = TRUE,
    sep_dir       = TRUE,
    hrc_dir       = dir_count,
    verbose       = FALSE
  )

  n_tables <- length(res_count$tabs)
  tailles  <- sapply(res_count$tabs, nrow)

  ntables_list_mod[[lbl]] <- data.frame(
    label    = lbl,
    n_tables = n_tables,
    min_rows = if (length(tailles) > 0) min(tailles) else 0,
    med_rows = if (length(tailles) > 0) median(tailles) else 0,
    max_rows = if (length(tailles) > 0) max(tailles) else 0
  )

  rm(res_count); gc()
}


# =============================================================================
# BILAN FINAL
# =============================================================================

cat("\n======================================================================\n")
cat("  BILAN COMPARATIF : min / smart_* / max                                \n")
cat("======================================================================\n")

cat("\n[1. Nombre et dimensions des sous-tables générées]\n")
df_tables_summary <- bind_rows(ntables_list_mod) %>%
  rename(
    `Variant`        = label,
    `Nb tables`      = n_tables,
    `Taille min (lignes)` = min_rows,
    `Taille med (lignes)` = med_rows,
    `Taille max (lignes)` = max_rows
  )
print(as.data.frame(df_tables_summary))

cat("\n[2. Timings (elapsed, secondes)]\n")
print(bind_rows(timings_list_mod))

cat("\n[3. Pic mémoire Vcells]\n")
gc_all <- bind_rows(gc_list_mod)
print(gc_all |> filter(type == "Vcells") |> select(label, used_mb, max_mb))

cat("\n[4. Statistiques de secret]\n")
print(bind_rows(stats_list_mod))



