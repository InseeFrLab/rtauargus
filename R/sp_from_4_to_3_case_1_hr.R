#' Transition from 4 to 3 variables by merging a hierarchical
#' and a non-hierarchical variable
#'
#' @param dfs data.frame with 4 categorical variables (n >= 2 in the general case)
#' @param dfs_name name of the data.frame in the list provided by the user
#' @param v1 non-hierarchical categorical variable
#' @param v2 hierarchical categorical variable
#' @param totcode named vector of totals for categorical variables
#' @param hrcfiles named vector indicating the hrc files of hierarchical variables
#' among the categorical variables of dfs
#' @param dir_name directory where to write the hrc files
#' if no folder is specified in hrcfiles
#' @param sep separator used when concatenating variables
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{tabs}: named list of 3-dimensional dataframes
#'   (n-1 dimensions in the general case) with nested hierarchies
#'   \item \code{hrc}: named list of hrc specific to the variable created by fusion
#'   \item \code{alt_tot}: named list of totals
#'   \item \code{vars}: named list of vectors representing the merged variables
#'   during the two stages of dimension reduction
#' }
#'
#' @examples
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
#' res1 <- from_4_to_3_case_1_hr(dfs = data,
#'                                 dfs_name = "dfs_name",
#'                                 v1 = "ECO",v2 = "SEX",
#'                                 totcode = c(ACT = "Total",SEX = "Total",
#'                                             AGE = "Total",ECO = "PIB"),
#'                                 hrcfiles = c(ACT = hrc_act, SEX = hrc_sex),
#'                                 dir_name = "output")
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
  #############################
  ## Creation of code_split ##
  #############################
  hrc <- hrcfiles[[v2]]
  total <- totcode[[v2]]

  res_sdc <- sdcHierarchies::hier_import(inp = hrc, from = "hrc", root = total) %>%
    sdcHierarchies::hier_convert(as = "sdc")

  # Code split gives us the hierarchies as well as the hierarchy levels
  # Allows to select a node of the tree and its direct branches
  codes_split <- lapply(
    res_sdc$dims,
    names
  )

  ###########################
  # Reduction of hierarchy #
  ###########################

  liste_df_4_var_0_hr <- lapply(
    codes_split,
    function(codes){
      res <- dfs %>%
        filter(dfs[[v2]] %in% codes)
    }
  )
  # We now have data.frames with 0 hierarchical variables
  # therefore we can apply the dedicated method

  # Updating the arguments then call the function from_4_to_3_case_0_hr
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

  # We transform all our 4 var tables into 3 var
  res <- lapply(seq_along(liste_df_4_var_0_hr), function(i) {
    call_4_to_3_0_hr(liste_df_4_var_0_hr[[i]], i)
  })


  # We change the object so that it is the same as in the other cases
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
