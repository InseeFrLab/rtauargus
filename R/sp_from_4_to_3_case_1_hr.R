#' Transition from 4 to 3 variables by merging a hierarchical
#' and a non-hierarchical variable
#'
#' @param dfs data.frame with 4 categorical variables
#' @param dfs_name name of the data.frame in the user list
#' @param v1 non-hierarchical categorical variable
#' @param v2 hierarchical categorical variable
#' @param totcode named vector of totals for categorical variables
#' @param hrcfiles named vector of hrc file paths
#' @param dir_name directory where to write generated hrc files
#' @param sep separator used when concatenating variables
#'
#' @return A list with `tabs`, `hrcs`, `alt_tot` and `vars`.
#'
#' @keywords internal
#' @noRd
from_4_to_3_case_1_hr <- function(
  dfs,
  dfs_name,
  v1,
  v2,
  totcode,
  hrcfiles,
  dir_name,
  sep = "_")
{

  # ----------------------------------------------------------------------------
  # STRATEGY (1 Hierarchical Variable v2 + 1 Flat Variable v1):
  # 1. Split 'dfs' along the nodes of hierarchy 'v2'.
  # 2. In each sub-table, 'v2' becomes fixed to its local total, meaning both
  #    'v1' and 'v2' are now non-hierarchical (0 HR variables remaining).
  # 3. Delegate each sub-table to 'from_4_to_3_case_0_hr' (merging 2 flat vars).
  # 4. Aggregate and return 3D tables, generated HRC files, and alt_tot metadata.
  # ----------------------------------------------------------------------------


  hrc <- hrcfiles[[v2]]
  total <- totcode[[v2]]

  # Retrieve vector of code groupings defining the hierarchy levels of v2
  codes_split <- import_hierarchy(hrc, total)


  # Pre-index physical row positions by 'v2' value to filter in integer space.
  # 'sort()' guarantees that row ordering within sub-dataframes matches original 'dfs'.
  row_indices <- split(seq_len(nrow(dfs)), dfs[[v2]])

  liste_df_4_var_0_hr <- lapply(codes_split, function(codes) {
    idx <- sort(unlist(row_indices[codes], use.names = FALSE))
    dfs[idx, , drop = FALSE]
  })

  # Sparse hierarchies may produce empty sub-tables. Filter them out to avoid:
  # 1. Failure in 'from_4_to_3_case_0_hr' / 'write_hrc2' on 0 rows.
  # 2. Mismatch between 'codes_split' and 'liste_df_4_var_0_hr' indices.
  is_empty <- vapply(liste_df_4_var_0_hr, function(df) nrow(df) == 0, logical(1))
  valid_idx <- which(!is_empty)

  if (length(valid_idx) == 0) {
    return(list(
      tabs = list(),
      hrcs = list(),
      alt_tot = list(),
      vars = c(v1, v2)
    ))
  }

  liste_df_4_var_0_hr <- liste_df_4_var_0_hr[valid_idx]
  codes_split <- codes_split[valid_idx]

  # Helper: Update 'totcode' for local v2 total, then delegate to 0-HR solver
  call_4_to_3_0_hr <- function(dfs, i){

    if (i <= length(codes_split)) {
      totcode[v2] <- codes_split[[i]][1]
      dfs_name <- paste(dfs_name, totcode[v2], sep = "_")

      from_4_to_3_case_0_hr(dfs = dfs,
                               dfs_name = dfs_name,
                               v1 = v1,
                               v2 = v2,
                               totcode = totcode,
                               dir_name = dir_name,
                               sep = sep)
    }
    else {
      print(paste("Index", i, "is out of bounds for codes_split."))
      return(NULL)
    }
  }

  # Process all sub-tables and aggregate results
  res <- lapply(seq_along(liste_df_4_var_0_hr), function(i) {
    call_4_to_3_0_hr(liste_df_4_var_0_hr[[i]], i)
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
