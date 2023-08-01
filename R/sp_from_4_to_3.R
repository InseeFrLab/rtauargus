# Small functions for use in from_4_to_3()

# Returns the hierarchical variable with the fewest nodes (= subtotals)
plus_petit_hrc <- function(hrcfiles) {
  v <- list()
  for (i in 1:length(hrcfiles)) {
    v <- append(v, nb_nodes(hrcfiles, names(hrcfiles[i])))
  }
  indice_petit_hrc <- which.min(v)
  nom_plus_petit_hrc <- names(hrcfiles)[indice_petit_hrc]
  return(nom_plus_petit_hrc)
}

# Returns the variable with the fewest modalities
plus_petit_mod <- function(tab_to_split) {
  v <- list()
  for (colonne in tab_to_split) {
    v <- append(v,length(unique(colonne)))
  }
  indice_petit_mod <- which.min(v)
  nom_plus_petit_mod <- names(tab_to_split)[indice_petit_mod]
  return(nom_plus_petit_mod)
}

# Choose a categorical variable
# Preferably the non-hierarchical one with the fewest modalities
# If not available, the hierarchical variable with the fewest nodes
choisir_var_priorite_non_hierarchique <- function(tab_to_split,totcode,hrcfiles){
  # The categorical variables without hierarchy
  var_cat <- names(totcode)

  var_sans_hier <- intersect(
    setdiff(names(tab_to_split), names(hrcfiles)),
    var_cat
  )

  n_vars_sans_hier<-length(var_sans_hier)

  # Principle: preferably choose non-hierarchical variables

  # If more than 1, look at the variables with the fewest modalities
  # to create fewer dataframes later
  if (n_vars_sans_hier > 1){
    dfs_var_sans_hier <- subset(tab_to_split,select = var_sans_hier)
    return (plus_petit_mod(dfs_var_sans_hier))
  }
  else if(n_vars_sans_hier == 1){
    return (var_sans_hier[1])
  }
  # Otherwise choose the hierarchical variable with the fewest subtotals
  else {
    return (plus_petit_hrc(hrcfiles))
  }
}

# Returns the hierarchical variable with the most nodes
plus_grand_hrc <- function(hrcfiles) {
  v <- list()
  for (i in 1:length(hrcfiles)) {
    v <- append(v, nb_nodes(hrcfiles, names(hrcfiles[i])))
  }
  indice_grand_hrc <- which.max(v)
  nom_plus_grand_hrc <- names(hrcfiles)[indice_grand_hrc]
  return(nom_plus_grand_hrc)
}

# Returns the variable with the most modalities
plus_grand_mod <- function(tab_to_split) {
  v <- list()
  for (colonne in tab_to_split) {
    v <- append(v, length(unique(colonne)))
  }
  indice_grand_mod <- which.max(v)
  nom_plus_grand_mod <- names(tab_to_split)[indice_grand_mod]
  return(nom_plus_grand_mod)
}

# Choose a categorical variable
# Preferably the hierarchical one with the most nodes
# If not available, the non-hierarchical variable with the most modalities
choisir_var_priorite_hierarchique <- function(tab_to_split, totcode, hrcfiles) {
  # Principle: preferably choose hierarchical variables

  # If no hierarchical variable, choose non-hierarchical variable with the most modalities
  if (length(hrcfiles) == 0) {
    return(plus_grand_mod(tab_to_split[names(tab_to_split) %in% names(totcode)]))
    # Otherwise, choose the hierarchical variable with the most subtotals
  } else {
    return(plus_grand_hrc(hrcfiles))
  }
}

choisir_var <- function(tab_to_split, totcode, hrcfiles, maximize_nb_tabs = FALSE) {
  if(maximize_nb_tabs){
    return(choisir_var_priorite_hierarchique(tab_to_split, totcode, hrcfiles))
  } else {
    return(choisir_var_priorite_non_hierarchique(tab_to_split, totcode, hrcfiles))
  }
}

#' Function reducing from 4 to 3 categorical variables
#'
#' @param tab_to_split data.frame with 4 categorical variables (n >= 2 in the general case)
#' @param nom_dfs name of the dataframe
#' @param totcode named vector of totals for categorical variables
#' @param hrcfiles named vector indicating the hrc files of hierarchical variables
#' among the categorical variables of tab_to_split
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
#' @return list(tabs, hrcs, alt_tot, vars)
#' tab : named list of 3-dimensional dataframes (n-1 dimensions in the general case)
#' with nested hierarchies
#' hrc : named list of hrc specific to the variable created through merging
#' alt_tot : named list of totals
#' vars : named list of vectors representing the merged variables
#' during the two stages of dimension reduction
#'
#' @examples
#'
#' library(dplyr)
#'
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
#' hrc_act <- "output/hrc_ACT.hrc"
#'
#' sdcHierarchies::hier_create(root = "Total", nodes = c("A","B")) %>%
#'   sdcHierarchies::hier_add(root = "A", nodes = c("A1","A2")) %>%
#'   sdcHierarchies::hier_add(root = "B", nodes = c("B1","B2")) %>%
#'   sdcHierarchies::hier_convert(as = "argus") %>%
#'   slice(-1) %>%
#'   mutate(levels = substring(paste0(level,name),3)) %>%
#'   select(levels) %>%
#'   write.table(file = hrc_act, row.names = F, col.names = F, quote = F)
#'
#' # Results of the function
#' res1 <- from_4_to_3(
#'   tab_to_split = data,
#'   nom_dfs = "tab",
#'   totcode = c(SEX = "Total", AGE = "Total", GEO = "Total", ACT = "Total"),
#'   hrcfiles = c(ACT = hrc_act),
#'   sep_dir = TRUE,
#'   hrc_dir = "output"
#' )
#'
#' # Maximize the number of tables
#' res2 <- from_4_to_3(
#'   tab_to_split = data,
#'   nom_dfs = "tab",
#'   totcode = c(SEX = "Total", AGE = "Total", GEO = "Total", ACT = "Total"),
#'   hrcfiles = c(ACT = hrc_act),
#'   sep_dir = TRUE,
#'   hrc_dir = "output",
#'   maximize_nb_tabs = TRUE
#' )
from_4_to_3 <- function(
  tab_to_split,
  nom_dfs,
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
  var_cat <- names(totcode)

  var_sans_hier <- intersect(
    setdiff(names(tab_to_split), names(hrcfiles)),
    var_cat
  )

  # Choice of variables and verification of those given as arguments

  n_vars_sans_hier <- 0 # Hierarchical variable selected so far

  # First variable
  if (!is.null(v1)){
    if (!(v1 %in% var_cat)){
      stop(paste("v1 is not a categorical variable, v1 = ", v1,
                 "Categorical variables are: ",paste(var_cat, collapse = ", ")), sep = "")
    }
  } else {
    # a variable is chosen, avoiding v2
    v1 <- choisir_var(tab_to_split = tab_to_split[setdiff(names(tab_to_split),v2)],
                      totcode = totcode[setdiff(names(totcode),v2)],
                      hrcfiles = hrcfiles[setdiff(names(hrcfiles),v2)],
                      maximize_nb_tabs = maximize_nb_tabs)
  }

  if (v1 %in% var_sans_hier){
    # Update the number of selected hierarchical variables
    n_vars_sans_hier <- n_vars_sans_hier + 1
  }

  # Second variable
  if (!is.null(v2)){
    if (!(v2 %in% var_cat)){
      stop(paste("v2 is not a categorical variable, v2 = ", v2,
                 "Categorical variables are: ",paste(var_cat, collapse = ", ")), sep = "")
    }
    if (v1 == v2){
      stop("Error. You are trying to merge a variable with itself")
    }

  } else {
    # a variable is chosen, avoiding v1
    v2 <- choisir_var(tab_to_split = tab_to_split[setdiff(names(tab_to_split),v1)],
                      totcode = totcode[setdiff(names(totcode),v1)],
                      hrcfiles = hrcfiles[!(names(hrcfiles) == v1)],
                      maximize_nb_tabs = maximize_nb_tabs)
  }

  if (v2 %in% var_sans_hier){
    # Update the number of selected hierarchical variables
    n_vars_sans_hier <- n_vars_sans_hier + 1
  }

  # The corresponding function is called

  # Case 2 non-hierarchical variables
  if(n_vars_sans_hier == 2){
    return(from_4_to_3_case_0_hr(tab_to_split = tab_to_split,
                                    nom_dfs = nom_dfs,
                                    v1 = v1,
                                    v2 = v2,
                                    totcode = totcode,
                                    dir_name = dir_name,
                                    sep = sep)
           )

  # Case 1 non-hierarchical variable
  }else if(n_vars_sans_hier == 1){
    # v2 must be hierarchical, v1 non-hierarchical
    # So the variables are put in the right order
    if (v2 %in% var_sans_hier){
      tmp <- v2
      v2 <- v1
      v1 <- tmp
    }
    return(from_4_to_3_case_1_hr(tab_to_split = tab_to_split,
                                    nom_dfs = nom_dfs,
                                    v1 = v1,
                                    v2 = v2,
                                    totcode = totcode,
                                    hrcfiles = hrcfiles,
                                    dir_name = dir_name,
                                    sep = sep)
           )

  # Case 0 non-hierarchical variable
  }else{
    return(from_4_to_3_case_2_hr(tab_to_split = tab_to_split,
                                    nom_dfs = nom_dfs,
                                    v1 = v1,
                                    v2 = v2,
                                    totcode = totcode,
                                    hrcfiles = hrcfiles,
                                    dir_name = dir_name,
                                    sep = sep)
           )
  }
}
