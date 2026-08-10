#' Transition from 4 to 3 variables via the merging of two hierarchical variables
#'
#' @param dfs data.frame with 4 categorical variables
#' @param dfs_name name of the data.frame in the user list
#' @param v1,v2 hierarchical categorical variables to merge
#' @param totcode named vector of totals for categorical variables
#' @param hrcfiles named vector of hrc file paths
#' @param dir_name directory where to write generated hrc files
#' @param sep separator used for merging variables
#'
#' @return A list with `tabs`, `hrcs`, `alt_tot` and `vars`.
#'
#' @keywords internal
#' @noRd
from_4_to_3_case_2_hr <- function(
  dfs,
  dfs_name,
  v1,
  v2,
  totcode,
  hrcfiles,
  dir_name,
  sep = "_"){

  # ----------------------------------------------------------------------------
  # STRATEGY (2 Hierarchical Variables -> 1 Hierarchical Variable):
  # 1. Split 'dfs' into sub-tables corresponding to each node/level of 'v1'.
  # 2. Within each sub-table, 'v1' becomes fixed to its local total, leaving only
  #    'v2' as hierarchical.
  # 3. Delegate each sub-table to 'from_4_to_3_case_1_hr' (the 1-HR solver).
  # 4. Combine generated 3D tables, HRC files, and alt_tot metadata.
  # ----------------------------------------------------------------------------

  hrc1 <- hrcfiles[[v1]]
  total1 <- totcode[[v1]]

  # Retrieve vector of code groupings defining the hierarchy levels of v1
  codes_split_1 <- import_hierarchy(hrc1, total1)

  # Pre-index physical row positions by 'v1' value to perform lookup in integer space.
  # 'sort()' guarantees that row ordering within sub-dataframes matches the original 'dfs'.
  row_indices <- split(seq_len(nrow(dfs)), dfs[[v1]])

  liste_df_4_var_1_hr <- lapply(codes_split_1, function(codes) {
    idx <- sort(unlist(row_indices[codes], use.names = FALSE))
    dfs[idx, , drop = FALSE]
  })

  # Sparse hierarchies may produce empty sub-tables. Filter them out to avoid:
  # 1. Failure in 'write_hrc2' (cannot construct hierarchy from 0 rows).
  # 2. Mismatch between 'codes_split_1' and 'liste_df_4_var_1_hr' indices.
  is_empty <- vapply(liste_df_4_var_1_hr, function(df) nrow(df) == 0, logical(1))
  valid_idx <- which(!is_empty)

  # If all tables are empty, return an empty result early
  if (length(valid_idx) == 0) {
    return(list(
      tabs = list(),
      hrcs = list(),
      alt_tot = list(),
      vars = c(v1, v2)
    ))
  }

  # Keep only non-empty tables and corresponding hierarchy nodes
  liste_df_4_var_1_hr <- liste_df_4_var_1_hr[valid_idx]
  codes_split_1 <- codes_split_1[valid_idx]


  # Helper: Update 'totcode' and table name for sub-domain i of 'v1',
  # then delegate to the 1-hierarchical variable solver ('case_1_hr').
  call_4_to_3_1_hr <- function(dfs, i){

    if (i <= length(codes_split_1)) {
      totcode[v1] <- codes_split_1[[i]][1]
      dfs_name <- paste(dfs_name, totcode[v1], sep = "_")

      from_4_to_3_case_1_hr(dfs = dfs,
                               dfs_name = dfs_name,
                               v1 = v1,
                               v2 = v2,
                               totcode = totcode,
                               hrcfiles = hrcfiles,
                               dir_name = dir_name,
                               sep = sep)
    }
    else {
      print(paste("Index", i, "is out of bounds for codes_split."))
      return(NULL)
    }
  }

  # Process all sub-tables and aggregate results
  res <- lapply(seq_along(liste_df_4_var_1_hr), function(i) {
    call_4_to_3_1_hr(liste_df_4_var_1_hr[[i]], i)
  })

  tabs <- unlist(lapply(res, function(x) x$tabs), recursive = FALSE)
  hrcs <- unlist(lapply(res, function(x) x$hrcs), recursive = FALSE)
  alt_tot <- unlist(lapply(res, function(x) x$alt_tot), recursive = FALSE)

  return(
    list(
      tabs = tabs,
      hrcs = hrcs,
      alt_tot = alt_tot,
      vars = c(v1, v2))
  )
}
