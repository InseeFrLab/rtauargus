# Benchmarking and Verification script for Z:/rtauargus/R optimizations
library(microbenchmark)

# ==============================================================================
# 1. Benchmark & Verification for Priority 1: Subsetting (from_4_to_3_case_1_hr logic)
# ==============================================================================
set.seed(42)
n_rows <- 1e5
unique_codes <- paste0("CODE_", 1:50)
dfs_test <- data.frame(
  v2 = sample(unique_codes, n_rows, replace = TRUE),
  VALUE = runif(n_rows),
  stringsAsFactors = FALSE
)
codes_split_test <- lapply(1:50, function(i) sample(unique_codes, 10))

# --- Package original logic ---
run_old_p1 <- function(dfs, codes_split) {
  lapply(codes_split, function(codes) {
    dfs[dfs$v2 %in% codes, , drop = FALSE]
  })
}

# --- Package optimized logic ---
run_new_p1 <- function(dfs, codes_split) {
  # OPTIMIZATION: Physically pre-index positions once
  row_indices <- split(seq_len(nrow(dfs)), dfs$v2)

  lapply(codes_split, function(codes) {
    # OPTIMIZATION: Combine indices and sort to preserve exact original row order
    idx <- sort(unlist(row_indices[codes], use.names = FALSE))
    dfs[idx, , drop = FALSE]
  })
}

# VERIFICATION: Assert that both implementations return 100% identical dataframes
res_old_p1 <- run_old_p1(dfs_test, codes_split_test)
res_new_p1 <- run_new_p1(dfs_test, codes_split_test)
p1_is_identical <- identical(res_old_p1, res_new_p1)
cat("Priority 1 (Subsetting): Are outputs identical?", p1_is_identical, "\n\n")

benchmark_p1 <- microbenchmark(
  "Old %in% subsetting" = {
    run_old_p1(dfs_test, codes_split_test)
  },
  "New Index-based (Sorted)" = {
    run_new_p1(dfs_test, codes_split_test)
  },
  times = 10
)
print(benchmark_p1)
cat("\n----------------------------------------------------------------------\n\n")


# ==============================================================================
# 2. Benchmark & Verification for Priority 2: Stride-based extraction (separer4_3 logic)
# ==============================================================================
# Generate a realistic dataframe of 100,000 rows with a concatenated column
set.seed(42)
n_splits <- 1e5
df_base <- data.frame(
  v1_v2 = paste(sample(paste0("A", 1:500), n_splits, replace = TRUE),
                sample(paste0("B", 1:500), n_splits, replace = TRUE),
                sep = "_"),
  OTHER_VAL = runif(n_splits),
  stringsAsFactors = FALSE
)

# --- Original separer4_3 function ---
separer4_3_old <- function(df, v1, v2, v1_v2, sep) {
  splits <- strsplit(df[[v1_v2]], split = sep, fixed = TRUE)
  mat <- matrix(unlist(splits, use.names = FALSE), ncol = 2, byrow = TRUE)

  df[[v1]] <- mat[, 1]
  df[[v2]] <- mat[, 2]
  df[[v1_v2]] <- NULL

  # Reorder columns
  new_order <- c(v1, v2, setdiff(names(df), c(v1, v2)))
  df[, new_order]
}

# --- Optimized separer4_3 function ---
separer4_3_new <- function(df, v1, v2, v1_v2, sep) {
  splits <- strsplit(df[[v1_v2]], split = sep, fixed = TRUE)

  # OPTIMIZATION: Stride-based extraction bypassing 2D matrix allocation
  unlisted <- unlist(splits, use.names = FALSE)
  n_rows <- length(splits)

  df[[v1]] <- unlisted[seq(1, by = 2, length.out = n_rows)]
  df[[v2]] <- unlisted[seq(2, by = 2, length.out = n_rows)]
  df[[v1_v2]] <- NULL

  # Reorder columns
  new_order <- c(v1, v2, setdiff(names(df), c(v1, v2)))
  df[, new_order]
}

# VERIFICATION: Assert that both dataframes are 100% identical after columns splitting
df_old_res <- separer4_3_old(df_base, "v1", "v2", "v1_v2", "_")
df_new_res <- separer4_3_new(df_base, "v1", "v2", "v1_v2", "_")
p2_is_identical <- identical(df_old_res, df_new_res)
cat("Priority 2 (separer4_3): Are outputs identical?", p2_is_identical, "\n\n")

benchmark_p2 <- microbenchmark(
  "Old separer4_3 (Matrix)" = {
    separer4_3_old(df_base, "v1", "v2", "v1_v2", "_")
  },
  "New separer4_3 (Stride)" = {
    separer4_3_new(df_base, "v1", "v2", "v1_v2", "_")
  },
  times = 10
)
print(benchmark_p2)
