# =============================================================================
# 00_setup.R — Session fraîche obligatoire
# Génère les données + hrc persistants. Redémarrer R après exécution.
# =============================================================================

BENCH_DIR <- "Z:/benchmark_4d"
dir.create(file.path(BENCH_DIR, "hrc"), recursive = TRUE, showWarnings = FALSE)

library(data.table)
library(dplyr)
library(sdcHierarchies)

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

# --- Génération ---
big4 <- create_big_4D(
  n = 50000000,
  n_act_groups = 4, n_act_sub = 11,
  n_geo_regions = 5, n_geo_sub = 10,
  n_sex = 20, n_age = 15
)

# Copie des hrc dans un dossier persistant (tempdir() disparaît au restart)
hrc4 <- c(
  ACT = file.path(BENCH_DIR, "hrc", "hrc_ACT.hrc"),
  GEO = file.path(BENCH_DIR, "hrc", "hrc_GEO.hrc")
)
file.copy(big4$hrcfiles["ACT"], hrc4["ACT"], overwrite = TRUE)
file.copy(big4$hrcfiles["GEO"], hrc4["GEO"], overwrite = TRUE)

tot4 <- big4$totcode

dfs4_agg <- big4$data %>%
  group_by(across(all_of(names(tot4)))) %>%
  summarise(VALUE = sum(VALUE), freq = n(), .groups = "drop")

rm(big4); gc()

set.seed(42)
dfs4_agg$is_secret_prim <- sample(
  c(TRUE, FALSE), nrow(dfs4_agg),
  replace = TRUE, prob = c(0.01, 0.99)
)
dfs4_agg$secret_no_pl <- FALSE

save(dfs4_agg, tot4, hrc4, file = file.path(BENCH_DIR, "input_data.RData"))
cat("Setup OK →", BENCH_DIR, "\n")
cat("Redémarrer R, puis lancer 01_bloc_old.R\n")


# BLOC 1 -------------------------------------------------------------------------


# =============================================================================
# 01_bloc_old.R — Session fraîche obligatoire
# Version originale (package installé). Redémarrer R après exécution.
# =============================================================================
set.seed(42)
BENCH_DIR <- "Z:/benchmark_4d"
loc_tauargus <- "Y:/Logiciels/TauArgus/TauArgus4.2.2b1/TauArgus.exe"

# Capture gc() sous forme de data.frame propre
capture_gc <- function(label) {
  g <- gc()
  data.frame(
    label      = label,
    type       = rownames(g),
    used_mb    = g[, 2],
    trigger_mb = g[, 4],
    max_mb     = g[, 6],   # pic depuis le dernier gc(reset=TRUE)
    row.names  = NULL
  )
}

# Extrait la dernière colonne is_secret_N
get_last_secret_col <- function(df) {
  cols <- names(df)[grepl("^is_secret_[0-9]+$", names(df))]
  if (!length(cols)) return(NULL)
  cols[order(as.integer(sub("is_secret_", "", cols)))] |> tail(1)
}

# Sauvegarde le résultat minimal pour comparaison (clés + secret final)
save_result_minimal <- function(df, tot4, label) {
  cols_base <- c(names(tot4), "is_secret_prim")
  last_col  <- get_last_secret_col(df)
  df_min    <- as.data.frame(df)[, c(cols_base, last_col)]
  names(df_min)[ncol(df_min)] <- "final_secret"
  write.csv(df_min, file.path(BENCH_DIR, paste0("result_", label, ".csv")),
            row.names = FALSE)
  invisible(df_min)
}


BENCH_DIR    <- "Z:/benchmark_4d"
loc_tauargus <- "Y:/Logiciels/TauArgus/TauArgus4.2.2b1/TauArgus.exe"

library(data.table); library(dplyr); library(rtauargus)
options(rtauargus.tauargus_exe = loc_tauargus)

load(file.path(BENCH_DIR, "input_data.RData"))  # dfs4_agg, tot4, hrc4

# Reset du compteur de pic mémoire
gc(reset = TRUE)

dir_old <- file.path(BENCH_DIR, "tauargus_old")
dir.create(file.path(dir_old, "hrc"), recursive = TRUE, showWarnings = FALSE)

# --- reduce_dims ---
t_red <- system.time({
  res_red <- reduce_dims(
    dfs = dfs4_agg, dfs_name = "tab_4d_int",
    totcode = tot4, hrcfiles = hrc4,
    nb_tab_option = "max", sep_dir = TRUE,
    hrc_dir = file.path(dir_old, "hrc")
  )
})

# --- restore_format ---
t_rest <- system.time({
  rtauargus::restore_format(masq = res_red$tabs, res = res_red)
})

# --- flux complet ---
t_full <- system.time({
  res_old <- tab_rtauargus4(
    tabular = dfs4_agg, explanatory_vars = names(tot4),
    dir_name = dir_old, secret_var = "is_secret_prim",
    totcode = tot4, hrc = hrc4,
    value = "VALUE", freq = "freq",
    suppress = "GH(1,100)", nb_tab_option = "max",
    dfs_name = "tab_4d_int", limit = 1000000L
  )
})

# --- Sauvegarde ---
timing <- data.frame(
  label   = "old",
  phase   = c("reduce_dims", "restore_format", "tab_rtauargus4"),
  elapsed = c(t_red["elapsed"], t_rest["elapsed"], t_full["elapsed"]),
  user    = c(t_red["user.self"], t_rest["user.self"], t_full["user.self"]),
  sys     = c(t_red["sys.self"], t_rest["sys.self"], t_full["sys.self"])
)
write.csv(timing, file.path(BENCH_DIR, "timing_old.csv"), row.names = FALSE)

gc_stats <- capture_gc("old")
write.csv(gc_stats, file.path(BENCH_DIR, "gc_old.csv"), row.names = FALSE)

save_result_minimal(res_old, tot4, "old")

cat("Bloc 01 terminé. Redémarrer R, puis lancer 02_bloc_new_true.R\n")


# BLOC 2-------------------------------------------------------------------------

# =============================================================================
# 02_bloc_new_true.R — Session fraîche obligatoire
# Version optimisée, keep_history = TRUE. Redémarrer R après exécution.
# =============================================================================
set.seed(42)
BENCH_DIR <- "Z:/benchmark_4d"
loc_tauargus <- "Y:/Logiciels/TauArgus/TauArgus4.2.2b1/TauArgus.exe"

# Capture gc() sous forme de data.frame propre
capture_gc <- function(label) {
  g <- gc()
  data.frame(
    label      = label,
    type       = rownames(g),
    used_mb    = g[, 2],
    trigger_mb = g[, 4],
    max_mb     = g[, 6],   # pic depuis le dernier gc(reset=TRUE)
    row.names  = NULL
  )
}

# Extrait la dernière colonne is_secret_N
get_last_secret_col <- function(df) {
  cols <- names(df)[grepl("^is_secret_[0-9]+$", names(df))]
  if (!length(cols)) return(NULL)
  cols[order(as.integer(sub("is_secret_", "", cols)))] |> tail(1)
}

# Sauvegarde le résultat minimal pour comparaison (clés + secret final)
save_result_minimal <- function(df, tot4, label) {
  cols_base <- c(names(tot4), "is_secret_prim")
  last_col  <- get_last_secret_col(df)
  df_min    <- as.data.frame(df)[, c(cols_base, last_col)]
  names(df_min)[ncol(df_min)] <- "final_secret"
  write.csv(df_min, file.path(BENCH_DIR, paste0("result_", label, ".csv")),
            row.names = FALSE)
  invisible(df_min)
}

BENCH_DIR    <- "Z:/benchmark_4d"
loc_tauargus <- "Y:/Logiciels/TauArgus/TauArgus4.2.2b1/TauArgus.exe"

library(data.table); library(dplyr)
devtools::load_all("Z:/rtauargus")
options(rtauargus.tauargus_exe = loc_tauargus)

load(file.path(BENCH_DIR, "input_data.RData"))
gc(reset = TRUE)

dir_new_t <- file.path(BENCH_DIR, "tauargus_new_true")
dir.create(file.path(dir_new_t, "hrc"), recursive = TRUE, showWarnings = FALSE)

t_red <- system.time({
  res_red <- reduce_dims(
    dfs = dfs4_agg, dfs_name = "tab_4d_int",
    totcode = tot4, hrcfiles = hrc4,
    nb_tab_option = "max", sep_dir = TRUE,
    hrc_dir = file.path(dir_new_t, "hrc")
  )
})

t_rest <- system.time({
  restore_format(masq = res_red$tabs, res = res_red)
})

t_full <- system.time({
  res_new_true <- tab_rtauargus4(
    tabular = dfs4_agg, explanatory_vars = names(tot4),
    dir_name = dir_new_t, secret_var = "is_secret_prim",
    totcode = tot4, hrc = hrc4,
    value = "VALUE", freq = "freq",
    suppress = "GH(1,100)", nb_tab_option = "max",
    dfs_name = "tab_4d_int", limit = 1000000L,
    keep_history = TRUE
  )
})

timing <- data.frame(
  label   = "new_true",
  phase   = c("reduce_dims", "restore_format", "tab_rtauargus4"),
  elapsed = c(t_red["elapsed"], t_rest["elapsed"], t_full["elapsed"]),
  user    = c(t_red["user.self"], t_rest["user.self"], t_full["user.self"]),
  sys     = c(t_red["sys.self"], t_rest["sys.self"], t_full["sys.self"])
)
write.csv(timing, file.path(BENCH_DIR, "timing_new_true.csv"), row.names = FALSE)

gc_stats <- capture_gc("new_true")
write.csv(gc_stats, file.path(BENCH_DIR, "gc_new_true.csv"), row.names = FALSE)

save_result_minimal(res_new_true, tot4, "new_true")

cat("Bloc 02 terminé. Redémarrer R, puis lancer 03_bloc_new_false.R\n")


# BLOC 3 -------------------------------------------------------------------------

# =============================================================================
# 03_bloc_new_false.R — Session fraîche obligatoire
# Version optimisée, keep_history = FALSE. Redémarrer R après exécution.
# =============================================================================
set.seed(42)
BENCH_DIR    <- "Z:/benchmark_4d"
loc_tauargus <- "Y:/Logiciels/TauArgus/TauArgus4.2.2b1/TauArgus.exe"

# Capture gc() sous forme de data.frame propre
capture_gc <- function(label) {
  g <- gc()
  data.frame(
    label      = label,
    type       = rownames(g),
    used_mb    = g[, 2],
    trigger_mb = g[, 4],
    max_mb     = g[, 6],
    row.names  = NULL
  )
}

# Extrait la dernière colonne is_secret_N
get_last_secret_col <- function(df) {
  cols <- names(df)[grepl("^is_secret_[0-9]+$", names(df))]
  if (!length(cols)) return(NULL)
  cols[order(as.integer(sub("is_secret_", "", cols)))] |> tail(1)
}

# Sauvegarde le résultat minimal pour comparaison (clés + secret final)
save_result_minimal <- function(df, tot4, label) {
  cols_base <- c(names(tot4), "is_secret_prim")
  last_col  <- get_last_secret_col(df)
  df_min    <- as.data.frame(df)[, c(cols_base, last_col)]
  names(df_min)[ncol(df_min)] <- "final_secret"
  write.csv(df_min, file.path(BENCH_DIR, paste0("result_", label, ".csv")),
            row.names = FALSE)
  invisible(df_min)
}

library(data.table); library(dplyr)
devtools::load_all("Z:/rtauargus")
options(rtauargus.tauargus_exe = loc_tauargus)

load(file.path(BENCH_DIR, "input_data.RData"))
gc(reset = TRUE)

dir_new_f <- file.path(BENCH_DIR, "tauargus_new_false")
dir.create(file.path(dir_new_f, "hrc"), recursive = TRUE, showWarnings = FALSE)

t_red <- system.time({
  res_red <- reduce_dims(
    dfs = dfs4_agg, dfs_name = "tab_4d_int",
    totcode = tot4, hrcfiles = hrc4,
    nb_tab_option = "max", sep_dir = TRUE,
    hrc_dir = file.path(dir_new_f, "hrc")
  )
})

t_rest <- system.time({
  restore_format(masq = res_red$tabs, res = res_red)
})

t_full <- system.time({
  res_new_false <- tab_rtauargus4(
    tabular = dfs4_agg, explanatory_vars = names(tot4),
    dir_name = dir_new_f, secret_var = "is_secret_prim",
    totcode = tot4, hrc = hrc4,
    value = "VALUE", freq = "freq",
    suppress = "GH(1,100)", nb_tab_option = "max",
    dfs_name = "tab_4d_int", limit = 1000000L,
    keep_history = FALSE
  )
})

timing <- data.frame(
  label   = "new_false",
  phase   = c("reduce_dims", "restore_format", "tab_rtauargus4"),
  elapsed = c(t_red["elapsed"], t_rest["elapsed"], t_full["elapsed"]),
  user    = c(t_red["user.self"], t_rest["user.self"], t_full["user.self"]),
  sys     = c(t_red["sys.self"], t_rest["sys.self"], t_full["sys.self"])
)
write.csv(timing, file.path(BENCH_DIR, "timing_new_false.csv"), row.names = FALSE)

gc_stats <- capture_gc("new_false")
write.csv(gc_stats, file.path(BENCH_DIR, "gc_new_false.csv"), row.names = FALSE)

save_result_minimal(res_new_false, tot4, "new_false")

cat("Bloc 03 terminé. Redémarrer R, puis lancer 04_summary.R\n")

# =============================================================================
# 04_summary.R — Lecture des résultats, pas besoin de session fraîche
# =============================================================================

BENCH_DIR <- "Z:/benchmark_4d"
library(dplyr)

# --- Timings ---
timing <- lapply(
  c("old", "new_true", "new_false"),
  function(l) read.csv(file.path(BENCH_DIR, paste0("timing_", l, ".csv")))
) |> bind_rows()

cat("\n=== TIMINGS (elapsed, secondes) ===\n")
print(
  timing |>
    select(label, phase, elapsed) |>
    tidyr::pivot_wider(names_from = label, values_from = elapsed)
)

# --- Mémoire peak ---
gc_all <- lapply(
  c("old", "new_true", "new_false"),
  function(l) read.csv(file.path(BENCH_DIR, paste0("gc_", l, ".csv")))
) |> bind_rows()

cat("\n=== PIC MÉMOIRE max_mb (Vcells) ===\n")
print(gc_all |> filter(type == "Vcells") |> select(label, used_mb, max_mb))

# --- Égalité des résultats ---
results <- lapply(
  c("old", "new_true", "new_false"),
  function(l) read.csv(file.path(BENCH_DIR, paste0("result_", l, ".csv")))
)
names(results) <- c("old", "new_true", "new_false")

normalize <- function(df) {
  df <- df[do.call(order, df[c("ACT", "GEO", "SEX", "AGE")]), ]
  rownames(df) <- NULL
  df
}

cat("\n=== ÉGALITÉ DES MASQUES DE SECRET FINAL ===\n")
cat("old == new_true  :", identical(normalize(results$old), normalize(results$new_true)), "\n")
cat("old == new_false :", identical(normalize(results$old), normalize(results$new_false)), "\n")

# --- Stats secret ---
secret_stats <- lapply(names(results), function(l) {
  df <- results[[l]]
  data.frame(
    label              = l,
    n_prim             = sum(df$is_secret_prim),
    n_sec              = sum(df$final_secret & !df$is_secret_prim),
    n_total_masque     = sum(df$final_secret),
    pct_masque         = round(100 * mean(df$final_secret), 2)
  )
}) |> bind_rows()

cat("\n=== STATISTIQUES DE SECRET ===\n")
print(secret_stats)
