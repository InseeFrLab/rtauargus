# =============================================================================
# bench_sort_table.R — Une seule session suffit (keep_history = FALSE)
# Compare sort_table = NULL / "ASC" / "DESC"
# =============================================================================

BENCH_DIR    <- "Z:/benchmark_4d"
loc_tauargus <- "Y:/Logiciels/TauArgus/TauArgus4.2.2b1/TauArgus.exe"

library(data.table); library(dplyr)
devtools::load_all("Z:/rtauargus")
options(rtauargus.tauargus_exe = loc_tauargus)

load(file.path(BENCH_DIR, "input_data.RData"))  # dfs4_agg, tot4, hrc4

# =============================================================================
# UTILITAIRES
# =============================================================================

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
  valid    <- !final

  data.frame(
    label              = label,
    n_prim             = sum(prim),
    value_prim         = sum(df$VALUE[prim],  na.rm = TRUE),
    n_sec              = sum(sec),
    value_sec          = sum(df$VALUE[sec],   na.rm = TRUE),
    n_total_masque     = sum(final),
    value_total_masque = sum(df$VALUE[final], na.rm = TRUE),
    n_valid            = sum(valid),
    value_valid        = sum(df$VALUE[valid], na.rm = TRUE),
    pct_masque_cells   = round(100 * mean(final), 2),
    pct_masque_value   = round(100 * sum(df$VALUE[final], na.rm = TRUE) /
                                 sum(df$VALUE,        na.rm = TRUE), 2)
  )
}

# Normalise un df pour comparaison cellule par cellule
normalize <- function(df, key_cols) {
  df <- as.data.frame(df)[, c(key_cols, "final_secret")]
  df <- df[do.call(order, df[key_cols]), ]
  rownames(df) <- NULL
  df
}

# Extrait le masque final sous forme normalisée
extract_mask <- function(df, tot4) {
  last_col <- get_last_secret_col(df)
  df_min   <- as.data.frame(df)[, c(names(tot4), last_col)]
  names(df_min)[ncol(df_min)] <- "final_secret"
  normalize(df_min, names(tot4))
}

# =============================================================================
# PARAMÈTRES COMMUNS AUX 3 RUNS
# =============================================================================

common_args <- list(
  tabular          = dfs4_agg,
  explanatory_vars = names(tot4),
  secret_var       = "is_secret_prim",
  totcode          = tot4,
  hrc              = hrc4,
  value            = "VALUE",
  freq             = "freq",
  suppress         = "GH(1,100)",
  nb_tab_option    = "max",
  dfs_name         = "tab_4d_int",
  limit            = 1000000L,
  keep_history     = FALSE
)

variants <- list(
  list(label = "sort_NULL", sort_table = NULL),
  list(label = "sort_ASC",  sort_table = "ASC"),
  list(label = "sort_DESC", sort_table = "DESC")
)

# Stockage des résultats en mémoire (~50 MB × 3 = négligeable)
results_raw  <- list()
stats_list   <- list()
timings_list <- list()
gc_list      <- list()

# =============================================================================
# BOUCLE PRINCIPALE
# =============================================================================

for (v in variants) {

  cat("\n======================================================================\n")
  cat("  Variant :", v$label, "\n")
  cat("======================================================================\n")

  dir_v <- file.path(BENCH_DIR, paste0("tauargus_", v$label))
  dir.create(file.path(dir_v, "hrc"), recursive = TRUE, showWarnings = FALSE)

  gc(reset = TRUE)

  t_v <- system.time({
    res_v <- do.call(
      tab_rtauargus4,
      c(common_args, list(dir_name = dir_v, sort_table = v$sort_table))
    )
  })

  # Résultats bruts conservés en mémoire pour comparaison cellule par cellule
  results_raw[[v$label]] <- res_v

  # Stats secret
  stats_list[[v$label]] <- get_secret_stats(res_v, v$label)

  # Timing
  timings_list[[v$label]] <- data.frame(
    label   = v$label,
    elapsed = t_v["elapsed"],
    user    = t_v["user.self"],
    sys     = t_v["sys.self"]
  )

  # Mémoire (après la fin du run, gc() déclenché dans capture_gc)
  gc_list[[v$label]] <- capture_gc(v$label)

  # Sauvegarde intermédiaire (sécurité en cas de crash session)
  write.csv(stats_list[[v$label]],
            file.path(BENCH_DIR, paste0("secret_", v$label, ".csv")),
            row.names = FALSE)
  write.csv(timings_list[[v$label]],
            file.path(BENCH_DIR, paste0("timing_", v$label, ".csv")),
            row.names = FALSE)
  write.csv(gc_list[[v$label]],
            file.path(BENCH_DIR, paste0("gc_", v$label, ".csv")),
            row.names = FALSE)

  cat("✓", v$label, "terminé en", round(t_v["elapsed"], 1), "s\n")
}

# =============================================================================
# BILAN FINAL
# =============================================================================

cat("\n======================================================================\n")
cat("  BILAN COMPARATIF : IMPACT DE sort_table                             \n")
cat("======================================================================\n")

cat("\n[1. Statistiques de secret]\n")
print(bind_rows(stats_list))

cat("\n[2. Timings (elapsed, secondes)]\n")
print(bind_rows(timings_list))

cat("\n[3. Pic mémoire Vcells (max_mb, MB)]\n")
gc_all <- bind_rows(gc_list)
print(gc_all |> filter(type == "Vcells") |> select(label, used_mb, max_mb))

# =============================================================================
# COMPARAISON CELLULE PAR CELLULE
# =============================================================================

cat("\n[4. Comparaison cellule par cellule des masques de secret final]\n")

mask_null <- extract_mask(results_raw[["sort_NULL"]], tot4)
mask_asc  <- extract_mask(results_raw[["sort_ASC"]],  tot4)
mask_desc <- extract_mask(results_raw[["sort_DESC"]], tot4)

cat("NULL == ASC  :", identical(mask_null, mask_asc),  "\n")
cat("NULL == DESC :", identical(mask_null, mask_desc), "\n")
cat("ASC  == DESC :", identical(mask_asc,  mask_desc), "\n")

# Si des différences existent, les quantifier
if (!identical(mask_null, mask_asc)) {
  n_diff <- sum(mask_null$final_secret != mask_asc$final_secret)
  cat("  → Cellules différentes NULL vs ASC  :", n_diff,
      sprintf("(%.3f%%)\n", 100 * n_diff / nrow(mask_null)))
}
if (!identical(mask_null, mask_desc)) {
  n_diff <- sum(mask_null$final_secret != mask_desc$final_secret)
  cat("  → Cellules différentes NULL vs DESC :", n_diff,
      sprintf("(%.3f%%)\n", 100 * n_diff / nrow(mask_null)))
}
if (!identical(mask_asc, mask_desc)) {
  n_diff <- sum(mask_asc$final_secret != mask_desc$final_secret)
  cat("  → Cellules différentes ASC  vs DESC :", n_diff,
      sprintf("(%.3f%%)\n", 100 * n_diff / nrow(mask_asc)))
}
