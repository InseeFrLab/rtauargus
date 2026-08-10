# Cache environment to store imported sdcHierarchies levels and prevent redundant disk parsing.
.hrc_cache <- new.env(parent = emptyenv())

#' Count the number of nodes in a hierarchical file
#'
#' @param hrcfiles named vector of hrc files
#' @param v variable name
#' @param hrc_name logical, if TRUE v is a name in hrcfiles
#' @param totcode named vector of totals
#' @param total total code if totcode is NULL
#'
#' @return integer number of nodes
#' @keywords internal
#' @noRd
nb_nodes <- function(hrcfiles = NULL, v = NULL, hrc_name = TRUE,
                     totcode = NULL, total = NULL) {
  if (hrc_name && !is.null(hrcfiles) && v %in% names(hrcfiles)) {
    # Fallback to "Total" if totcode is NULL or if v is not present in totcode
    total_val <- if (!is.null(totcode) && v %in% names(totcode)) totcode[[v]] else "Total"
    return(length(import_hierarchy(hrcfiles[[v]], total_val)))
  } else if (!hrc_name && !is.null(hrcfiles)) {
    # Fallback to "Total" if total is NULL
    total_val <- if (!is.null(total)) total else "Total"
    return(length(import_hierarchy(hrcfiles, total_val)))
  } else {
    return(1)
  }
}

#' Import a hierarchy and cache its levels
#'
#' @param hrcfile character path to a .hrc file
#' @param total character, the root total for the hierarchy
#'
#' @return A list of character vectors representing hierarchy nodes.
#' @keywords internal
#' @noRd
import_hierarchy <- function(hrcfile, total) {
  # Cache key combines filepath and root total code
  cache_key <- paste0(hrcfile, "|||", total)
  if (exists(cache_key, envir = .hrc_cache)) {
    return(get(cache_key, envir = .hrc_cache))
  }
  res_sdc <- sdcHierarchies::hier_import(inp = hrcfile, from = "hrc", root = total) %>%
    sdcHierarchies::hier_convert(as = "sdc")
  levels <- lapply(res_sdc$dims, names)
  assign(cache_key, levels, envir = .hrc_cache)
  return(levels)
}

#' General function to choose variables to merge
#'
#' @param dfs data.frame
#' @param totcode named vector of totals for categorical variables
#' @param hrcfiles named vector of hrc files
#' @param nb_var number of variables to merge
#' @param nb_tab_option strategy: 'min', 'max', or 'smart'
#' @param limit maximum allowed row count in the 'smart' case
#'
#' @return A list with `vars`, `max_row`, and `nb_tab`.
#' @keywords internal
#' @noRd
var_to_merge <- function(
    dfs,
    totcode,
    hrcfiles = NULL,
    nb_var = 4,
    nb_tab_option = "min",
    limit = 150)
{

  # Precompute unique values across all categorical columns once (O(N) cost)
  # to avoid repeating expensive unique() calls inside evaluation loops.
  unique_mods <- lapply(dfs[names(totcode)], unique)

  # Generate candidate combinations based on target dimension reduction
  if (nb_var == 4){
    result_comb <- generate_two_pairs(totcode) # 5D -> 3D (2 pairs merged)

    # Case of a triplet in dimension 5
  } else if (nb_var == 3){
    result_comb <- generate_a_triplet(totcode) # 5D -> 3D (1 triplet merged)

    # Case of dimension 4
  } else {
    result_comb <- generate_a_pair(totcode) # 4D -> 3D (1 pair merged)
  }

  return(var_to_merge_fragment(dfs = dfs,
                               result_comb = result_comb,
                               totcode = totcode,
                               hrcfiles = hrcfiles,
                               limit = limit,
                               nb_tab_option = nb_tab_option,
                               unique_mods = unique_mods)
         )
}

#' Evaluate combinations and select optimal variable merge strategy
#'
#' @keywords internal
#' @noRd
var_to_merge_fragment <- function(
    dfs,
    result_comb,
    totcode,
    hrcfiles = NULL,
    limit = 150,
    nb_tab_option = "smart",
    unique_mods = NULL)
{

  # Fallback initialization for unique_mods if called directly without precomputed list.
  if (is.null(unique_mods)) {
    unique_mods <- lapply(dfs[names(totcode)], unique)
  }

  # Simulate generated table sizes and counts for each candidate combination
  res_func <- lapply(result_comb, function(x) length_tabs(
    dfs = dfs,
    v1 = x[1],
    v2 = x[2],
    v3 = x[3],
    v4 = x[4],
    totcode = totcode,
    hrcfiles = hrcfiles,
    unique_mods = unique_mods))

  # Get the maximum rows and number of created tables
  res_max <- sapply(res_func, function(x) max(unlist(x)))
  res_len <- sapply(res_func, function(x) length(unlist(x)))

  # Create a dataframe for better filtering
  df <- data.frame(res_max = res_max, res_len = res_len)

  # Save the row number by adding a column
  df$original_index <- seq(nrow(df))

  # Selection logic according to 'nb_tab_option'
  if (nb_tab_option == "min"){
    # Minimize number of tables, break ties with smallest max table size
    min_nb_tab <-  min(df$res_len)
    filtered_df <- df[df$res_len == min_nb_tab, ]

    # Get the index of the filtered table
    min_index <- which.min(filtered_df$res_max)
    # Print the original index
    i <- filtered_df$original_index[min_index]

    return(list(vars = result_comb[[i]],
                max_row = filtered_df$res_max[min_index],
                nb_tab = filtered_df$res_len[min_index])
           )

  } else if (nb_tab_option == "max"){
    # Maximize number of tables, break ties with smallest max table size
    max_nb_tab <-  max(df$res_len)
    filtered_df <- df[df$res_len == max_nb_tab, ]

    # Get the index of the filtered table
    min_index <- which.min(filtered_df$res_max)
    # Print the original index
    i <- filtered_df$original_index[min_index]

    return(list(vars = result_comb[[i]],
                max_row = filtered_df$res_max[min_index],
                nb_tab = filtered_df$res_len[min_index])
    )

  } else {
    # 'smart' option: Minimize table count under constraint max_row < limit

    # Filter based on the maximum rows condition
    filtered_df <- df[df$res_max < limit, ]

    # If at least one case satisfies this condition
    if (nrow(filtered_df) > 0){
      # Get the index of the filtered table
      min_index <- which.min(filtered_df$res_len)

      # Print the original index
      i <- filtered_df$original_index[min_index]

      return(list(vars = result_comb[[i]],
                  max_row = filtered_df$res_max[min_index],
                  nb_tab = filtered_df$res_len[min_index])
      )

    } else {
      # Fallback: If no combination is under limit, pick candidate with smallest max size

      min_res_max <- min(df$res_max)
      filtered_df <- df[df$res_max == min_res_max, ]

      # Get the index of the filtered table
      min_index <- which.min(filtered_df$res_len)
      i <- filtered_df$original_index[min_index]

      return(list(vars = result_comb[[i]],
                  max_row = filtered_df$res_max[min_index],
                  nb_tab = filtered_df$res_len[min_index])
      )
    }
  }
}

#' Generate all candidate pairs of variables
#' @keywords internal
#' @noRd
#' @importFrom utils combn
generate_a_pair <- function(totcode) {
  # Retrieve the categorical variables from the dataframe
  cat_vars <- names(totcode)

  # Use combn to get all combinations of two elements
  comb <- combn(cat_vars, 2)

  # Transform the results into a list of vectors
  result <- split(t(comb), seq(ncol(comb)))

  return(result)
}

#' Generate candidate disjoint double pairs of variables
#' @keywords internal
#' @noRd
#' @importFrom utils combn
generate_two_pairs <- function(totcode) {
  # Retrieve the categorical variables from the dataframe
  cat_vars <- names(totcode)

  # Get all combinations of four elements
  comb <- combn(cat_vars, 4)

  # For each combination, obtain two disjoint pairs
  result <- lapply(seq(ncol(comb)), function(i) {
    quad <- comb[, i]
    pair_comb <- t(combn(quad, 2))

    # Create two disjoint pairs for each combination
    pairs <- lapply(seq(nrow(pair_comb)), function(j) {
      pair1 <- pair_comb[j, ]
      pair2 <- setdiff(quad, pair1)

      # Convert the pairs to strings
      pair1_str <- paste(sort(pair1), collapse = ",")
      pair2_str <- paste(sort(pair2), collapse = ",")

      # Create a string representing both pairs
      both_pairs_str <- paste(sort(c(pair1_str, pair2_str)), collapse = ",")
      return(both_pairs_str)
    })
    return(pairs)
  })

  # Flatten the result
  result <- unlist(result, recursive = FALSE)

  # Remove duplicates
  unique_pairs <- unique(result)

  # Convert the strings back to vectors
  result <- lapply(unique_pairs, function(pair_str) {
    pairs <- strsplit(pair_str, ",")[[1]]
    return(pairs)
  })

  return(result)
}

#' Generate all candidate triplets of variables
#' @keywords internal
#' @noRd
#' @importFrom utils combn
generate_a_triplet <- function(totcode) {
  # Retrieve the categorical variables from the dataframe
  cat_vars <- names(totcode)

  # Get all combinations of three elements
  comb <- combn(cat_vars, 3)

  # Transform the result into a list of vectors
  result <- split(t(comb), seq(ncol(comb)))

  return(result)
}

#' Simulate generated table sizes for dimension reduction
#'
#' @param dfs data.frame
#' @param v1,v2 primary pair of variables
#' @param v3,v4 optional 3rd and 4th variables for 5D cases
#' @param totcode named vector of totals
#' @param hrcfiles named vector of hrc files
#' @param unique_mods precomputed unique modalities
#'
#' @return A list of expected row counts.
#' @keywords internal
#' @noRd
length_tabs <- function(
  dfs,
  v1,
  v2,
  v3 = NULL,
  v4 = NULL,
  totcode,
  hrcfiles = NULL,
  unique_mods = NULL)
{

  # To generalize the function to handle NA for an external function
  v3 <- if (!is.null(v3) && is.na(v3)) NULL else v3
  v4 <- if (!is.null(v4) && is.na(v4)) NULL else v4

  # Security in case the function is called outside var_to_merge
  if (is.null(unique_mods)) {
    unique_mods <- lapply(dfs[names(totcode)], unique)
  }

  if (!is.null(v4)) {
    # 5D -> 3D (2 pairs)
    return(length_tabs_5_4_var(dfs = dfs,
                               hrcfiles = hrcfiles,
                               v1 = v1, v2 = v2,
                               v3 = v3, v4 = v4,
                               totcode = totcode,
                               unique_mods = unique_mods))
  } else if (!is.null(v3)) {
    # 5D -> 3D (1 triplet)
    return(length_tabs_5_3_var(dfs = dfs,
                               hrcfiles = hrcfiles,
                               v1 = v1, v2 = v2, v3 = v3,
                               totcode = totcode,
                               unique_mods = unique_mods))
  } else {
    # 4D -> 3D (1 pair)
    return(length_tabs_4(dfs = dfs,
                         hrcfiles = hrcfiles,
                         v1 = v1, v2 = v2,
                         totcode = totcode,
                         unique_mods = unique_mods))
  }
}

# Simulate table sizes for 4D -> 3D transition
length_tabs_4 <- function(dfs, v1, v2, totcode, hrcfiles = NULL, unique_mods = NULL){

  if (v1 %in% names(hrcfiles)) {
    level_v1 <- import_hierarchy(hrcfiles[[v1]], totcode[[v1]])
  } else {
    level_v1 <- list(unique_mods[[v1]])
  }

  if (v2 %in% names(hrcfiles)) {
    level_v2 <- import_hierarchy(hrcfiles[[v2]], totcode[[v2]])
  } else {
    level_v2 <- list(unique_mods[[v2]])
  }

  # Ensure flat variable is first if only one of {v1, v2} is hierarchical
  if (!(v2 %in% names(hrcfiles)) & (v1 %in% names(hrcfiles))) {
    tmp <- level_v1
    level_v1 <- level_v2
    level_v2 <- tmp
  }

  # Vectorized calculation of sub-table row counts for each node pair (i, j)
  len1 <- sapply(level_v1, length)
  len2 <- sapply(level_v2, length)

  L1 <- length(len1)
  L2 <- length(len2)

  len1_grid <- rep(len1, each = L2)
  len2_grid <- rep(len2, times = L1)

  val1 <- (len1_grid - 1) * len2_grid + 1
  val2 <- len1_grid * (len2_grid - 1) + 1

  nb_rows <- as.vector(rbind(val1, val2))

  # Multiply by number of modalities of non-merged variables
  list_non_merged_vars <- names(totcode[!(names(totcode) %in% c(v1, v2))])

  # Get the number of non-merged modalities using unique_mods lengths (much faster than lapply on unique).
  mod_non_merged_vars <- sapply(list_non_merged_vars,
                                function(x) length(unique_mods[[x]]))

  prod_numbers <- prod(unlist(mod_non_merged_vars))

  # Direct vector multiplication and conversion to a list for optimization.
  nb_rows_tot <- as.list(nb_rows * prod_numbers)

  return(nb_rows_tot)
}

# Simulate table sizes for 5D -> 3D transition with 2 merged pairs
length_tabs_5_4_var <- function(dfs, v1, v2, v3, v4, totcode, hrcfiles = NULL, unique_mods = NULL){

  if (is.null(unique_mods)) {
    unique_mods <- lapply(dfs[names(totcode)], unique)
  }

  if (v1 %in% names(hrcfiles)) {
    level_v1 <- import_hierarchy(hrcfiles[[v1]], totcode[[v1]])
  } else {
    level_v1 <- list(unique_mods[[v1]])
  }

  if (v2 %in% names(hrcfiles)) {
    level_v2 <- import_hierarchy(hrcfiles[[v2]], totcode[[v2]])
  } else {
    level_v2 <- list(unique_mods[[v2]])
  }

  # Swap hierarchy levels so the flat variable comes first (convention for 4D -> 3D steps).
  # Variable name strings themselves (v1/v2, v3/v4) are not swapped because c(v1,v2,v3,v4)
  # is used as an unordered set downstream.
  if (!(v2 %in% names(hrcfiles)) & (v1 %in% names(hrcfiles))) {
    tmp <- level_v1
    level_v1 <- level_v2
    level_v2 <- tmp
  }

  if (v3 %in% names(hrcfiles)) {
    level_v3 <- import_hierarchy(hrcfiles[[v3]], totcode[[v3]])
  } else {
    level_v3 <- list(unique_mods[[v3]])
  }

  if (v4 %in% names(hrcfiles)) {
    level_v4 <- import_hierarchy(hrcfiles[[v4]], totcode[[v4]])
  } else {
    level_v4 <- list(unique_mods[[v4]])
  }

  if (!(v4 %in% names(hrcfiles)) & (v3 %in% names(hrcfiles))) {
    tmp <- level_v3
    level_v3 <- level_v4
    level_v4 <- tmp
  }

  # Fully vectorized 5D -> 4D row count computation across all combinations
  len1 <- sapply(level_v1, length)
  len2 <- sapply(level_v2, length)
  len3 <- sapply(level_v3, length)
  len4 <- sapply(level_v4, length)

  L1 <- length(len1)
  L2 <- length(len2)
  L3 <- length(len3)
  L4 <- length(len4)

  len1_ij <- rep(len1, each = L2)
  len2_ij <- rep(len2, times = L1)
  A_ij <- (len1_ij - 1) * len2_ij + 1
  B_ij <- len1_ij * (len2_ij - 1) + 1

  len3_kl <- rep(len3, each = L4)
  len4_kl <- rep(len4, times = L3)
  C_kl <- (len3_kl - 1) * len4_kl + 1
  D_kl <- len3_kl * (len4_kl - 1) + 1

  CD_woven <- as.vector(rbind(C_kl, D_kl))
  AB_woven <- as.vector(rbind(A_ij, B_ij))

  multipliers <- rep(AB_woven, each = length(CD_woven))
  nb_rows <- multipliers * rep(CD_woven, times = 2 * L1 * L2)


  # Calculate the total number of rows by multiplying with the unique modalities of non-merged variables.

  list_non_fused_vars <- names(totcode[!(names(totcode) %in% c(v1, v2, v3, v4))])

  # Get the number of non-merged modalities using unique_mods lengths (much faster than lapply on unique).
  non_fused_vars_mod <- sapply(list_non_fused_vars,
                               function(x) length(unique_mods[[x]]))

  prod_numbers <- prod(unlist(non_fused_vars_mod))

  nb_rows_tot <- as.list(nb_rows * prod_numbers)


  return(nb_rows_tot)
}

# Simulate table sizes for 5D -> 3D transition with a merged triplet
length_tabs_5_3_var <- function(dfs, v1, v2, v3, totcode, hrcfiles = NULL, unique_mods = NULL) {

  # Fallback security in case unique_mods is not provided.
  if (is.null(unique_mods)) {
    unique_mods <- lapply(dfs[names(totcode)], unique)
  }

  # Case: At least 1 variable in triplet is hierarchical
  if (length(setdiff(names(hrcfiles), c(v1, v2, v3))) != length(hrcfiles)) {

    if (v1 %in% names(hrcfiles)) {
      level_v1 <- import_hierarchy(hrcfiles[[v1]], totcode[[v1]])
    } else {
      level_v1 <- list(unique_mods[[v1]])
    }

    if (v2 %in% names(hrcfiles)) {
      level_v2 <- import_hierarchy(hrcfiles[[v2]], totcode[[v2]])
    } else {
      level_v2 <- list(unique_mods[[v2]])
    }

    # Ensure v1 is non-hierarchical if only one of {v1, v2} has a hierarchy.
    # The merging algorithm always places the non-hierarchical variable first
    # for the 4‑D to 3‑D step, so we swap if necessary.
    if (!(v2 %in% names(hrcfiles)) & (v1 %in% names(hrcfiles))) {
      tmp <- level_v1
      level_v1 <- level_v2
      level_v2 <- tmp
    }

    if (v3 %in% names(hrcfiles)) {
      level_v3 <- import_hierarchy(hrcfiles[[v3]], totcode[[v3]])
    } else {
      level_v3 <- list(unique_mods[[v3]])
    }

    # Number of modalities within each hierarchy node
    len1 <- sapply(level_v1, length)
    len2 <- sapply(level_v2, length)
    len3 <- sapply(level_v3, length)
    L1 <- length(len1)
    L2 <- length(len2)
    L3 <- length(len3)


    # --- Build all (i, j) pairs ---
    # Each pair corresponds to a combination of a node from level_v1 (i)
    # and a node from level_v2 (j). These represent the tables created during
    # the 5‑D to 4‑D merging step.
    grid_ij <- expand.grid(i = seq_len(L1), j = seq_len(L2))
    i <- grid_ij$i
    j <- grid_ij$j

    len1_i <- len1[i]   # len1 for each pair
    len2_j <- len2[j]   # len2 for each pair

    # --- Calculate the four pattern matrices ---
    # These formulas come from the original nested lapply structure:
    #   For a given (i, j, k), the original code generates four numbers:
    #     (len2_j - 1) * len3[k] + 1
    #     len2_j * (len3[k] - 1) + 1
    #     (len1_i - 1) * len3[k] + 1
    #     len1_i * (len3[k] - 1) + 1
    # We compute them all at once for every (i, j) and every k using
    # matrix multiplication (%*%) to obtain matrices of size (n_pairs, L3).
    n_pairs <- length(i)

    # Pattern A: based on v2 & v3
    A1 <- (len2_j - 1) %*% t(len3) + 1      # (n_pairs x L3)
    A2 <-  len2_j      %*% t(len3 - 1) + 1  # (n_pairs x L3)

    # Pattern B: based on v1 & v3
    B1 <- (len1_i - 1) %*% t(len3) + 1
    B2 <-  len1_i      %*% t(len3 - 1) + 1

    # --- Replicate rows according to the original repetition logic ---
    # In the original algorithm, for a given (i, j):
    #   - A1 and A2 are repeated len1_i times (once for each element of the v1 node)
    #   - B1 and B2 are repeated len2_j times (once for each element of the v2 node)
    # They are then interleaved column‑wise (i.e., by k) in the order:
    #   A1, A2, B1, B2, A1, A2, B1, B2, ... (for each k)
    # Here we stack all repetitions row‑wise first, then interleave by rows.

    # Indices to repeat each pair's row in A1/A2 len1_i times
    idx_A <- rep(seq_len(n_pairs), times = len1_i)
    # Indices to repeat each pair's row in B1/B2 len2_j times
    idx_B <- rep(seq_len(n_pairs), times = len2_j)

    # Stack the matrices with the appropriate row repetitions
    stack_A1 <- A1[idx_A, , drop = FALSE]
    stack_A2 <- A2[idx_A, , drop = FALSE]
    stack_B1 <- B1[idx_B, , drop = FALSE]
    stack_B2 <- B2[idx_B, , drop = FALSE]

    # --- Interleave the stacked rows in the correct order ---
    # For each pair (i, j), we must place:
    #   len1_i rows of A1, len1_i rows of A2, len2_j rows of B1, len2_j rows of B2.
    # This small loop iterates only over n_pairs, not over the data size,
    # so it remains negligible while keeping the code readable.
    total_rows <- nrow(stack_A1) + nrow(stack_A2) + nrow(stack_B1) + nrow(stack_B2)
    res_mat <- matrix(0, nrow = total_rows, ncol = L3)

    pos <- 1
    start_A <- 1   # current position in stack_A1 / stack_A2
    start_B <- 1   # current position in stack_B1 / stack_B2
    for (k in seq_len(n_pairs)) {
      rA <- len1_i[k]   # number of rows for this pair from A1/A2
      rB <- len2_j[k]   # number of rows for this pair from B1/B2

      # Copy the rA rows of A1, then rA rows of A2
      res_mat[pos:(pos + rA - 1), ] <- stack_A1[start_A:(start_A + rA - 1), ]
      pos <- pos + rA
      res_mat[pos:(pos + rA - 1), ] <- stack_A2[start_A:(start_A + rA - 1), ]
      pos <- pos + rA

      # Copy the rB rows of B1, then rB rows of B2
      res_mat[pos:(pos + rB - 1), ] <- stack_B1[start_B:(start_B + rB - 1), ]
      pos <- pos + rB
      res_mat[pos:(pos + rB - 1), ] <- stack_B2[start_B:(start_B + rB - 1), ]
      pos <- pos + rB

      start_A <- start_A + rA
      start_B <- start_B + rB
    }

    # Flatten the matrix column‑wise (by k) to obtain the final vector of sizes
    nb_rows <- as.vector(t(res_mat))
  } else {
    # -------------------------------------------------------------------
    # 3 non‑hierarchical variables: exact result (the length of table i is known)
    # The formulas below come from the analytical derivation for flat variables.
    # They are already fully vectorized and correctly predict every table size.
    # -------------------------------------------------------------------

    # Fetch unique modality counts directly from unique_mods list rather than executing raw unique() calls.
    n_mod_v1 <- length(unique_mods[[v1]])
    n_mod_v2 <- length(unique_mods[[v2]])
    n_mod_v3 <- length(unique_mods[[v3]])

    # RATIONALE FOR THE 14-TABLE FORMULA (e.g., n_mod_v1=3, n_mod_v2=4, n_mod_v3=3):
    # Merging 3 flat variables v1, v2, v3 in a 5D-to-3D reduction is a nested process:
    #
    # Step 1: Merges v1 and v2, generating 2 intermediate 4D tables (tab1 and tab2).
    #   - tab1: v1 is the primary split variable. Its custom hierarchy has:
    #           * 1 Root Split (total "Total_Total" cut into v1 level 1 children)
    #           * (n_mod_v1 - 1) Intermediate Splits (v1 node cut into v2 children)
    #   - tab2: v2 is the primary split variable. Its custom hierarchy has:
    #           * 1 Root Split (total "Total_Total" cut into v2 level 1 children)
    #           * (n_mod_v2 - 1) Intermediate Splits (v2 node cut into v1 children)
    #
    # Step 2: Merges the newly hierarchical V1_V2 with v3 using 'from_4_to_3_case_1_hr'.
    # Every single split from Step 1 (both Root and Intermediate splits) is further
    # split and crossed with v3, generating TWO 3D tables per split.
    #
    # Summing all generated tables:
    #   - From tab1:
    #     * 1 pair from the Root Split (sizes based on v1 and v3)
    #     * (n_mod_v1 - 1) pairs from the Intermediate Splits (sizes based on v2 and v3)
    #   - From tab2:
    #     * 1 pair from the Root Split (sizes based on v2 and v3)
    #     * (n_mod_v2 - 1) pairs from the Intermediate Splits (sizes based on v1 and v3)
    #
    # Grouping these symmetrically (which yields the original package's formula):
    #   - Root Split of tab1 + Intermediate Splits of tab2 (both based on v1 & v3)
    #     Total splits = 1 + (n_mod_v2 - 1) = n_mod_v2 splits (2 * n_mod_v2 tables).
    #   - Root Split of tab2 + Intermediate Splits of tab1 (both based on v2 & v3)
    #     Total splits = 1 + (n_mod_v1 - 1) = n_mod_v1 splits (2 * n_mod_v1 tables).
    nb_rows <- c(
      # Standalone: 1 pair of tables from tab1's Root Split (sizes based on v1 and v3)
      1 + (n_mod_v3 - 1) * n_mod_v1,
      1 + n_mod_v3 * (n_mod_v1 - 1),

      # Rep 1: n_mod_v1 pairs of tables representing:
      #   - (n_mod_v1 - 1) Intermediate Splits from tab1
      #   - 1 Root Split from tab2
      # (All of these have sizes based on v2 and v3)
      rep(c(1 + (n_mod_v3 - 1) * n_mod_v2,
            1 + n_mod_v3 * (n_mod_v2 - 1)),
          times = n_mod_v1),

      # Rep 2: (n_mod_v2 - 1) pairs of tables representing:
      #   - (n_mod_v2 - 1) Intermediate Splits from tab2
      # (All of these have sizes based on v1 and v3)
      rep(c(1 + (n_mod_v3 - 1) * n_mod_v1,
            1 + n_mod_v3 * (n_mod_v1 - 1)),
          times = n_mod_v2 - 1)
    )

  }

  # Calculate the total number of rows by multiplying with the unique modalities of non-merged variables.

  list_non_fused_vars <- names(totcode[!(names(totcode) %in% c(v1, v2, v3))])

  # Get the number of non-merged modalities using unique_mods lengths (much faster than lapply on unique).
  non_fused_vars_mod <- sapply(list_non_fused_vars,
                               function(x) length(unique_mods[[x]]))

  prod_numbers <- prod(unlist(non_fused_vars_mod))

  nb_rows_tot <- as.list(nb_rows * prod_numbers)

  return(nb_rows_tot)
}

#' Calculate total count of generated tables
#'
#' @keywords internal
#' @noRd
nb_tab_generated <- function(
  v1,
  v2,
  v3 = NULL,
  v4 = NULL,
  hrcfiles = NULL,
  totcode = NULL,
  data = NULL,
  unique_mods = NULL)
{

  # Fallback setup to minimize duplicate calculations in nb_tab_generated
  if (is.null(unique_mods) && !is.null(data)) {
    needed_vars <- c(v1, v2, v3, v4)
    needed_vars <- needed_vars[!is.null(needed_vars) & !is.na(needed_vars)]
    unique_mods <- lapply(data[needed_vars], unique)
  }

  # Case dimension 5: 2 couples created
  if (!is.null(v4)) {
    return(4 * nb_nodes(hrcfiles = hrcfiles, v = v1, totcode = totcode) *
             nb_nodes(hrcfiles = hrcfiles, v = v2, totcode = totcode) *
             nb_nodes(hrcfiles = hrcfiles, v = v3, totcode = totcode) *
             nb_nodes(hrcfiles = hrcfiles, v = v4, totcode = totcode))

    # Case dimension 5: one triplet merged
  } else if (!is.null(v3)) {

    # 2 hierarchical variables merged
    if (!is.null(hrcfiles) & v1 %in% names(hrcfiles) & v2 %in% names(hrcfiles)) {

      # The hierarchy of each variable
      level_v1 <- import_hierarchy(hrcfiles[[v1]], totcode[[v1]])
      level_v2 <- import_hierarchy(hrcfiles[[v2]], totcode[[v2]])

      # ALGEBRAIC SIMPLIFICATION JUSTIFICATION:
      # The original nested sapply loops computed:
      #   sum_{i=1}^{L1} [ sum_{j=1}^{L2} ( length(level_v1[[i]]) + length(level_v2[[j]]) ) ]
      # By linearity of summation, this can be distributed as:
      #   L2 * sum_{i=1}^{L1} length(level_v1[[i]]) + L1 * sum_{j=1}^{L2} length(level_v2[[j]])
      # Where L1 = length(level_v1) and L2 = length(level_v2).
      # This removes all nested loops and executes instantaneously.
      len1 <- sapply(level_v1, length)
      len2 <- sapply(level_v2, length)

      nb_noeuds_var <- length(level_v2) * sum(len1) + length(level_v1) * sum(len2)

      # 2 non-hierarchical variables merged
    } else if (is.null(hrcfiles) | !(v1 %in% names(hrcfiles)) & !(v2 %in% names(hrcfiles))) {
      # There is only one table in the end
      # which can have two hierarchies
      # totals on v1, or totals on v2
      # the number of nodes is equivalent to the number of modalities
      nb_noeuds_var <- length(unique_mods[[v1]]) + length(unique_mods[[v2]])

      # 1 hierarchical variable and 1 non-hierarchical variable merged
    } else {
      var_hier <- ifelse(v1 %in% names(hrcfiles), v1, v2)
      mod_var_non_hier <- ifelse(var_hier == v1,
                                 length(unique_mods[[v2]]),
                                 length(unique_mods[[v1]]))

      # Analysis of the hierarchy of var_hier
      level_var_hier <- import_hierarchy(hrcfiles[[var_hier]], totcode[[var_hier]])

      # ALGEBRAIC SIMPLIFICATION JUSTIFICATION:
      # The original single sapply computed:
      #   sum_{i=1}^{L_hier} ( length(level_var_hier[[i]]) + mod_var_non_hier )
      # Since mod_var_non_hier is a constant across all iterations, this is equivalent to:
      #   sum_{i=1}^{L_hier} length(level_var_hier[[i]]) + L_hier * mod_var_non_hier
      # Where L_hier = length(level_var_hier).
      len_hier <- sapply(level_var_hier, length)

      nb_noeuds_var <- sum(len_hier) + length(level_var_hier) * mod_var_non_hier
    }

    # nb_nodes corresponds to the number of tables that need to be created
    # to make v1_v2 non-hierarchical
    # for each of these tables, v3 needs to be made non-hierarchical
    # and we create as many tables as its hierarchy has nodes
    # finally, for each created table, two hierarchies are possible
    # totals on v1_v2 and totals on v3
    return(2 * nb_noeuds_var * nb_nodes(hrcfiles, v = v3, totcode = totcode))

    # Case dimension 4
  } else {
    return(2 * nb_nodes(hrcfiles = hrcfiles, v = v1, totcode = totcode) *
             nb_nodes(hrcfiles = hrcfiles, v = v2, totcode = totcode))
  }
}

#' Analytically computes all possible table splits and summarizes their statistics.
#'
#' @param dfs data.frame containing 4 or 5 categorical variables
#' @param totcode named vector of totals for categorical variables
#' @param hrcfiles named vector of hrc files (optional)
#'
#' @return A deduplicated data.frame containing the following columns:
#' \itemize{
#'   \item `nb_tab`: number of generated tables
#'   \item `nb_hrc`: number of remaining hierarchical variables
#'   \item `min_size`: minimum table size (rows)
#'   \item `med_size`: median table size (rows, rounded to integer)
#'   \item `max_size`: maximum table size (rows)
#' }
#' @export
explore_reduce_dims <- function(dfs, totcode, hrcfiles = NULL) {

  # Ensure dfs is a standard data.frame (handles data.table input)
  dfs <- as.data.frame(dfs)

  # 1. Check dimensions (4D or 5D)
  num_dims <- length(totcode)
  if (!num_dims %in% c(4, 5)) {
    stop("Please provide a totcode object with 4 or 5 dimensions!")
  }

  # Initial number of hierarchical variables
  hrc_var_names <- intersect(names(hrcfiles), names(totcode))
  nb_hrc_initial <- length(hrc_var_names)

  # Precompute unique_mods to speed up computation (as in var_to_merge)
  unique_mods <- lapply(dfs[names(totcode)], unique)

  # 2. Generate possible variable combinations
  if (num_dims == 4) {
    # 4D: 6 combinations of 2 variables
    result_comb <- generate_a_pair(totcode)
  } else {
    # 5D: 10 triplets (3-var merge) + 15 double pairs (4-var merge) = 25 cases
    triplets  <- generate_a_triplet(totcode)
    two_pairs <- generate_two_pairs(totcode)
    result_comb <- c(triplets, two_pairs)
  }

  # 3. Analytically compute table lengths using length_tabs
  results <- lapply(result_comb, function(x) {
    # x contains 2, 3, or 4 variables.
    # x[3] and x[4] return NA if the index exceeds length(x),
    # which length_tabs() automatically converts to NULL.
    tab_sizes <- unlist(
      length_tabs(
        dfs = dfs,
        v1 = x[1],
        v2 = x[2],
        v3 = x[3],
        v4 = x[4],
        totcode = totcode,
        hrcfiles = hrcfiles,
        unique_mods = unique_mods
      )
    )

    # Calculate remaining hierarchical variables
    merged_vars <- x[!is.na(x)]
    nb_hrc_merged <- sum(merged_vars %in% hrc_var_names)
    nb_created <- if (length(x) == 4) 2L else 1L
    nb_hrc_val <- as.integer(nb_hrc_initial - nb_hrc_merged + nb_created)

    data.frame(
      nb_tab   = as.integer(length(tab_sizes)),
      nb_hrc   = nb_hrc_val,
      min_size = as.integer(min(tab_sizes)),
      med_size = as.integer(round(stats::median(tab_sizes))),
      max_size = as.integer(max(tab_sizes))
    )
  })

  # 4. Combine, deduplicate (distinct), and sort by ascending nb_tab
  res_df <- dplyr::bind_rows(results) %>%
    dplyr::distinct() %>%
    dplyr::arrange(nb_tab, max_size, min_size)

  return(res_df)
}
