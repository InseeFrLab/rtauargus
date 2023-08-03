#' Transition from 4 to 3 variables via the merging of two hierarchical variables
#'
#' @param dfs data.frame with 4 categorical variables (n >= 2 in the general case)
#' @param nom_dfs name of the data.frame in the list provided by the user
#' @param v1 hierarchical categorical variable
#' @param v2 hierarchical categorical variable
#' @param totcode named vector of totals for categorical variables
#' @param hrcfiles named vector indicating the hrc files of hierarchical variables
#' among the categorical variables of dfs
#' @param dir_name folder where to write the hrc files
#' if no folder is specified in hrcfiles
#' @param sep separator used during the concatenation of variables
#'
#' @return list(tabs, hrcs, alt_tot, vars)
#' tabs : named list of 3-dimensional dataframes (n-1 dimensions in the general case)
#' with nested hierarchies
#' hrcs : named list of hrc specific to the variable created via the merge
#' alt_tot : named list of totals
#' vars : named list of vectors representing the merged variables
#' during the two stages of dimension reduction
#'
#' @examples
#' library(dplyr)
#'
#' data <- expand.grid(
#'   ACT = c("Total", "A", "B", "A1", "A2", "B1", "B2"),
#'   SEX = c("Total", "F", "M","F1","F2","M1","M2"),
#'   AGE = c("Total", "AGE1", "AGE2", "AGE11", "AGE12", "AGE21", "AGE22"),
#'   ECO = c("PIB","Ménages","Entreprises"),
#'   stringsAsFactors = FALSE,
#'   KEEP.OUT.ATTRS = FALSE
#' ) %>%
#'   as.data.frame()
#'
#' data <- data %>% mutate(VALUE = 1:n())
#'
#' hrc_act <- "output/hrc_ACT.hrc"
#' sdcHierarchies::hier_create(root = "Total", nodes = c("A","B")) %>%
#'   sdcHierarchies::hier_add(root = "A", nodes = c("A1","A2")) %>%
#'   sdcHierarchies::hier_convert(as = "argus") %>%
#'   slice(-1) %>%
#'   mutate(levels = substring(paste0(level,name),3)) %>%
#'   select(levels) %>%
#'   write.table(file = hrc_act, row.names = F, col.names = F, quote = F)
#'
#' hrc_sex <- "output/hrc_SEX.hrc"
#' sdcHierarchies::hier_create(root = "Total", nodes = c("F","M")) %>%
#'   sdcHierarchies::hier_add(root = "F", nodes = c("F1","F2")) %>%
#'   sdcHierarchies::hier_add(root = "M", nodes = c("M1","M2")) %>%
#'   sdcHierarchies::hier_convert(as = "argus") %>%
#'   slice(-1) %>%
#'   mutate(levels = substring(paste0(level,name),3)) %>%
#'   select(levels) %>%
#'   write.table(file = hrc_sex, row.names = F, col.names = F, quote = F)
#'
#' res <- from_4_to_3_case_2_hr(dfs = data,
#'                                 nom_dfs = "nom_dfs",
#'                                 v1 = "ACT",v2 = "SEX",
#'                                 totcode = c(ACT = "Total",SEX = "Total",
#'                                             AGE = "Total",ECO = "PIB"),
#'                                 hrcfiles = c(ACT = hrc_act, SEX = hrc_sex),
#'                                 dir_name = "output")
from_4_to_3_case_2_hr <- function(
  dfs,
  nom_dfs,
  v1,
  v2,
  totcode,
  hrcfiles,
  dir_name,
  sep = "_")
{
  #############################
  ## Creating code_split ##
  #############################
  hrc1 <- hrcfiles[[v1]]
  total1 <- totcode[[v1]]

  # Creating the table providing the hierarchy levels
  res_sdc <- sdcHierarchies::hier_import(inp = hrc1, from = "hrc", root = total1) %>%
    sdcHierarchies::hier_convert(as = "sdc")

  codes_split_1 <- lapply(
    res_sdc$dims,
    names
  )

  ###########################
  # Hierarchy Reduction #
  ###########################

  fonc_liste_df_4_var_1_non_hr <- function(codes_split,dfs){
    lapply(
      codes_split_1,
      function(codes){
        res <- dfs %>%
          filter(dfs[[v1]] %in% codes)
      }
    )
  }

  liste_df_4_var_1_non_hr <- fonc_liste_df_4_var_1_non_hr(codes_split_1,dfs)
  # We now have data.frames with 1 non-hierarchical variables (v1)
  # therefore we can apply the dedicated method

  # Update arguments then call the function cas_2_non_hrc
  appel_4_3_non_hier <- function(dfs, i){

    if (i <= length(codes_split_1)) {
      totcode[v1] <- codes_split_1[[i]][1]
      nom_dfs <- paste(nom_dfs, totcode[v1], sep = "_")

      from_4_to_3_case_1_hr(dfs = dfs,
                               nom_dfs = nom_dfs,
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

  # We transform all our 4-var tables into 3-var tables
  res <- lapply(seq_along(liste_df_4_var_1_non_hr), function(i) {
    appel_4_3_non_hier(liste_df_4_var_1_non_hr[[i]], i)
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
