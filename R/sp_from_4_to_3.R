# Small functions for use in from_4_to_3()

# Returns the hierarchical variable with the fewest nodes (= subtotals)
smallest_hrc <- function(hrcfiles) {
  v <- list()
  for (i in 1:length(hrcfiles)) {
    v <- append(v, nb_nodes(hrcfiles, names(hrcfiles[i])))
  }
  index_smaller_hrc <- which.min(v)
  name_smaller_hrc <- names(hrcfiles)[index_smaller_hrc]
  return(name_smaller_hrc)
}

# Returns the variable with the fewest modalities
smallest_mod <- function(dfs) {
  v <- list()
  for (colonne in dfs) {
    v <- append(v,length(unique(colonne)))
  }
  index_smaller_mod <- which.min(v)
  name_smaller_mod <- names(dfs)[index_smaller_mod]
  return(name_smaller_mod)
}

# Choose a categorical variable
# Preferably the non-hierarchical one with the fewest modalities
# If not available, the hierarchical variable with the fewest nodes
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
    return (smallest_hrc(hrcfiles))
  }
}

# Returns the hierarchical variable with the most nodes
bigger_hrc <- function(hrcfiles) {
  v <- list()
  for (i in 1:length(hrcfiles)) {
    v <- append(v, nb_nodes(hrcfiles, names(hrcfiles[i])))
  }
  index_bigger_hrc <- which.max(v)
  name_bigger_hrc <- names(hrcfiles)[index_bigger_hrc]
  return(name_bigger_hrc)
}

# Returns the variable with the most modalities
bigger_mod <- function(dfs) {
  v <- list()
  for (colonne in dfs) {
    v <- append(v, length(unique(colonne)))
  }
  index_bigger_mod <- which.max(v)
  name_bigger_mod <- names(dfs)[index_bigger_mod]
  return(name_bigger_mod)
}

# Choose a categorical variable
# Preferably the hierarchical one with the most nodes
# If not available, the non-hierarchical variable with the most modalities
choose_var_priority_hierarchical <- function(dfs, totcode, hrcfiles) {
  # Principle: preferably choose hierarchical variables

  # If no hierarchical variable, choose non-hierarchical variable with the most modalities
  if (length(hrcfiles) == 0) {
    return(bigger_mod(dfs[names(dfs) %in% names(totcode)]))
    # Otherwise, choose the hierarchical variable with the most subtotals
  } else {
    return(bigger_hrc(hrcfiles))
  }
}

chose_var_to_merge <- function(dfs, totcode, hrcfiles, maximize_nb_tabs = FALSE) {
  if(maximize_nb_tabs){
    return(choose_var_priority_hierarchical(dfs, totcode, hrcfiles))
  } else {
    return(choose_var_priority_non_hierarchical(dfs, totcode, hrcfiles))
  }
}

#' Function reducing from 4 to 3 categorical variables
#'
#' @param dfs data.frame with 4 categorical variables (n >= 2 in the general case)
#' @param dfs_name name of the dataframe
#' @param totcode named vector of totals for categorical variables
#' @param hrcfiles named vector indicating the hrc files of hierarchical variables
#' among the categorical variables of dfs
#' @param sep_dir allows forcing the writing of hrc into a separate folder,
#' default is FALSE
#' @param hrc_dir folder to write hrc files if writing to a new folder is forced
#' or if no folder is specified in hrcfiles
#' @param v1 allows forcing the value of the first variable to merge,
#' not specified by default (NULL)
#' @param v2 allows forcing the value of the second variable to merge,
#' not specified by default (NULL)
#' @param sep separator used during concatenation of variables
#' @param maximize_nb_tabs specifies whether to prefer selecting hierarchical variables with
#' the most nodes in priority (TRUE), generating more tables but with smaller sizes,
#' or non-hierarchical variables with the fewest modalities (FALSE) to create fewer tables
#'
#' @return A list containing the following components:
#' \itemize{
#'   \item \code{tabs}: named list of 3-dimensional dataframes
#'   (n-1 dimensions in the general case) with nested hierarchies
#'   \item \code{hrc}: named list of hrc specific to the variable created
#'   through merging
#'   \item \code{alt_tot}: named list of totals
#'   \item \code{vars}: named list of vectors representing the merged variables
#'   during the two stages of dimension reduction
#' }
#'
#' @examples
#' library(dplyr)
#' data <- expand.grid(
#'   ACT = c("Total", "A", "B", "A1", "A2", "B1", "B2"),
#'   GEO = c("Total", "G1", "G2"),
#'   SEX = c("Total", "F", "M"),
#'   AGE = c("Total", "AGE1", "AGE2"),
#'   stringsAsFactors = FALSE
#' ) %>%
#'   as.data.frame()
#'
#' data <- data %>% mutate(VALUE = 1)
#'
#' hrc_act <- "hrc_ACT.hrc"
#'
#' sdcHierarchies::hier_create(root = "Total", nodes = c("A","B")) %>%
#'   sdcHierarchies::hier_add(root = "A", nodes = c("A1","A2")) %>%
#'   sdcHierarchies::hier_add(root = "B", nodes = c("B1","B2")) %>%
#'   sdcHierarchies::hier_convert(as = "argus") %>%
#'   slice(-1) %>%
#'   mutate(levels = substring(paste0(level,name),3)) %>%
#'   select(levels) %>%
#'   write.table(file = hrc_act, row.names = FALSE, col.names = FALSE, quote = FALSE)
#'
#' # Results of the function
#' res1 <- from_4_to_3(
#'   dfs = data,
#'   dfs_name = "tab",
#'   totcode = c(SEX = "Total", AGE = "Total", GEO = "Total", ACT = "Total"),
#'   hrcfiles = c(ACT = hrc_act),
#'   sep_dir = TRUE,
#'   hrc_dir = "output"
#' )
#'
#' # Maximize the number of tables
#' res2 <- from_4_to_3(
#'   dfs = data,
#'   dfs_name = "tab",
#'   totcode = c(SEX = "Total", AGE = "Total", GEO = "Total", ACT = "Total"),
#'   hrcfiles = c(ACT = hrc_act),
#'   sep_dir = TRUE,
#'   hrc_dir = "output",
#'   maximize_nb_tabs = TRUE
#' )
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
