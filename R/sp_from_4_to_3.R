# Small helper functions for variable selection in from_4_to_3()

# Return the hierarchical variable with the fewest nodes/subtotals
smallest_hrc <- function(hrcfiles, totcode) {
  v <- list()
  for (i in seq_along(hrcfiles)) {
    v <- append(v, nb_nodes(hrcfiles, names(hrcfiles)[i], totcode = totcode))
  }
  index_smaller_hrc <- which.min(v)
  name_smaller_hrc <- names(hrcfiles)[index_smaller_hrc]
  return(name_smaller_hrc)
}

# Return the hierarchical variable with the most nodes/subtotals
bigger_hrc <- function(hrcfiles, totcode) {
  v <- list()
  for (i in seq_along(hrcfiles)) {
    v <- append(v, nb_nodes(hrcfiles, names(hrcfiles)[i], totcode = totcode))
  }
  index_bigger_hrc <- which.max(v)
  name_bigger_hrc <- names(hrcfiles)[index_bigger_hrc]
  return(name_bigger_hrc)
}

# Return the categorical variable with the fewest unique modalities
smallest_mod <- function(dfs) {
  v <- sapply(dfs, function(col) length(unique(col)))
  names(which.min(v))
}

# Return the categorical variable with the most unique modalities
bigger_mod <- function(dfs) {
  v <- sapply(dfs, function(col) length(unique(col)))
  names(which.max(v))
}

# Priority 1: Pick non-hierarchical variable with fewest modalities (fewest tables created).
# Priority 2: If no flat variable, pick hierarchical variable with fewest nodes.
choose_var_priority_non_hierarchical <- function(dfs,totcode,hrcfiles){
  # The categorical variables without hierarchy
  cat_vars <- names(totcode)

  non_hier_vars <- intersect(
    setdiff(names(dfs), names(hrcfiles)),
    cat_vars
  )

  nb_non_hier_vars<-length(non_hier_vars)

  # Principle: preferably choose non-hierarchical variables

  # If more than 1, look at the variables with the fewest modalities
  # to create fewer dataframes later
  if (nb_non_hier_vars > 1){
    dfs_vars_non_hier <- subset(dfs,select = non_hier_vars)
    return (smallest_mod(dfs_vars_non_hier))
  }
  else if(nb_non_hier_vars == 1){
    return (non_hier_vars[1])
  }
  # Otherwise choose the hierarchical variable with the fewest subtotals
  else {
    return (smallest_hrc(hrcfiles, totcode))
  }
}

# Priority 1: Pick hierarchical variable with most nodes (smaller sub-tables created).
# Priority 2: If no hierarchical variable, pick flat variable with most modalities.
choose_var_priority_hierarchical <- function(dfs, totcode, hrcfiles) {
  # Principle: preferably choose hierarchical variables

  # If no hierarchical variable, choose non-hierarchical variable with the most modalities
  if (length(hrcfiles) == 0) {
    return(bigger_mod(dfs[names(dfs) %in% names(totcode)]))
    # Otherwise, choose the hierarchical variable with the most subtotals
  } else {
    return(bigger_hrc(hrcfiles, totcode))
  }
}

# Heuristic selector for merging variables according to 'maximize_nb_tabs'
chose_var_to_merge <- function(dfs, totcode, hrcfiles, maximize_nb_tabs = FALSE) {
  if(maximize_nb_tabs){
    return(choose_var_priority_hierarchical(dfs, totcode, hrcfiles))
  } else {
    return(choose_var_priority_non_hierarchical(dfs, totcode, hrcfiles))
  }
}

#' Transition from 4 to 3 categorical variables
#'
#' @param dfs data.frame with 4 categorical variables
#' @param dfs_name name of the data.frame
#' @param totcode named vector of totals for categorical variables
#' @param hrcfiles named vector of hrc file paths
#' @param sep_dir logical, if TRUE forces writing hrc files into hrc_dir
#' @param hrc_dir folder to write hrc files
#' @param v1 optional first variable to merge
#' @param v2 optional second variable to merge
#' @param sep separator used during variable concatenation
#' @param maximize_nb_tabs logical, whether to prefer selecting hierarchical variables
#'
#' @return A list with `tabs`, `hrcs`, `alt_tot` and `vars`.
#'
#' @keywords internal
#' @noRd
from_4_to_3 <- function(
  dfs,
  dfs_name,
  totcode,
  hrcfiles = NULL,
  sep_dir = FALSE,
  hrc_dir = "hrc_alt",
  v1 = NULL,
  v2 = NULL,
  sep = "_",
  maximize_nb_tabs = FALSE)
{

  # ----------------------------------------------------------------------------
  # ROUTER: 4D -> 3D transition
  # 1. Determine target directory for generated .hrc files.
  # 2. Select / validate candidate variables v1 and v2 to merge.
  # 3. Route execution to dedicated sub-method based on count of flat variables:
  #      - 2 flat vars -> from_4_to_3_case_0_hr
  #      - 1 flat + 1 HR var -> from_4_to_3_case_1_hr (forces v1=flat, v2=HR)
  #      - 2 HR vars -> from_4_to_3_case_2_hr
  # ----------------------------------------------------------------------------

  # Update the output directory containing the hierarchies
  if( (length(hrcfiles) != 0) & !sep_dir){
    dir_name <- dirname(hrcfiles[[1]])
  } else {
    dir_name <- hrc_dir
  }

  # Categorical variables without hierarchy
  cat_vars <- names(totcode)

  non_hier_vars <- intersect(
    setdiff(names(dfs), names(hrcfiles)),
    cat_vars
  )

  # Choice of variables and verification of those given as arguments

  nb_non_hier_vars <- 0 # Hierarchical variable selected so far

  # First variable
  if (!is.null(v1)){
    if (!(v1 %in% cat_vars)){
      stop(paste("v1 is not a categorical variable, v1 = ", v1,
                 "Categorical variables are: ",paste(cat_vars, collapse = ", ")), sep = "")
    }
  } else {
    # a variable is chosen, avoiding v2
    v1 <- chose_var_to_merge(dfs = dfs[setdiff(names(dfs),v2)],
                      totcode = totcode[setdiff(names(totcode),v2)],
                      hrcfiles = hrcfiles[setdiff(names(hrcfiles),v2)],
                      maximize_nb_tabs = maximize_nb_tabs)
  }

  if (v1 %in% non_hier_vars){
    # Update the number of selected hierarchical variables
    nb_non_hier_vars <- nb_non_hier_vars + 1
  }

  # Second variable
  if (!is.null(v2)){
    if (!(v2 %in% cat_vars)){
      stop(paste("v2 is not a categorical variable, v2 = ", v2,
                 "Categorical variables are: ",paste(cat_vars, collapse = ", ")), sep = "")
    }
    if (v1 == v2){
      stop("Error. You are trying to merge a variable with itself")
    }

  } else {
    # a variable is chosen, avoiding v1
    v2 <- chose_var_to_merge(dfs = dfs[setdiff(names(dfs),v1)],
                      totcode = totcode[setdiff(names(totcode),v1)],
                      hrcfiles = hrcfiles[!(names(hrcfiles) == v1)],
                      maximize_nb_tabs = maximize_nb_tabs)
  }

  if (v2 %in% non_hier_vars){
    # Update the number of selected hierarchical variables
    nb_non_hier_vars <- nb_non_hier_vars + 1
  }

  # The corresponding function is called

  # Case 2 non-hierarchical variables
  if(nb_non_hier_vars == 2){
    return(from_4_to_3_case_0_hr(dfs = dfs,
                                    dfs_name = dfs_name,
                                    v1 = v1,
                                    v2 = v2,
                                    totcode = totcode,
                                    dir_name = dir_name,
                                    sep = sep)
           )

  # Case 1 non-hierarchical variable
  }else if(nb_non_hier_vars == 1){
    # v2 must be hierarchical, v1 non-hierarchical
    # So the variables are put in the right order
    if (v2 %in% non_hier_vars){
      tmp <- v2
      v2 <- v1
      v1 <- tmp
    }
    return(from_4_to_3_case_1_hr(dfs = dfs,
                                    dfs_name = dfs_name,
                                    v1 = v1,
                                    v2 = v2,
                                    totcode = totcode,
                                    hrcfiles = hrcfiles,
                                    dir_name = dir_name,
                                    sep = sep)
           )

  # Case 0 non-hierarchical variable
  }else{
    return(from_4_to_3_case_2_hr(dfs = dfs,
                                    dfs_name = dfs_name,
                                    v1 = v1,
                                    v2 = v2,
                                    totcode = totcode,
                                    hrcfiles = hrcfiles,
                                    dir_name = dir_name,
                                    sep = sep)
           )
  }
}
