# Count the number of nodes in a hierarchical file
# Expects 2 arguments:
# - Either a named list and a variable,
# - Or an hrc (hierarchical file) and hrc_name = FALSE
nb_noeuds <- function(hrcfiles, v = NULL, hrc_name = TRUE) {
  # Check if the variable has an associated hrc file or if hrc_name == FALSE
  if (hrc_name && !(v %in% names(hrcfiles)) || (!hrc_name && is.null(hrcfiles))) {
    # Non-hierarchical variable or hrcfiles == NULL
    return(1)
  }
  
  # Take the specified file if hrc_name = TRUE, otherwise take the hrc directly provided
  hrc <- ifelse(hrc_name, hrcfiles[[v]], hrcfiles)
  
  # Unimportant value for the following steps
  total <- "This_Is_My_Total"
  
  # Convert to hierarchy
  res_sdc <- sdcHierarchies::hier_import(inp = hrc, from = "hrc", root = total) %>%
    sdcHierarchies::hier_convert(as = "sdc")
  
  # Return the number of nodes
  return(length(res_sdc$dims))
}

#' Function reducing from 5 to 3 categorical variables
#'
#' @param dfs data.frame with 5 categorical variables (n >= 3 in the general case)
#' @param nom_dfs name of the data.frame in the list provided by the user
#' @param totcode named vector of totals for categorical variables
#' @param hrcfiles named vector indicating the hrc files of hierarchical variables
#' among the categorical variables of dfs
#' @param sep_dir allows forcing the writing of hrc files in a separate folder
#' defaulted to FALSE
#' @param hrc_dir folder where to write the hrc files if forcing the writing
#' in a new folder or if no folder is specified in hrcfiles
#' @param v1 allows forcing the value of the first variable to merge
#' when reducing from 5 to 4 dimensions, not specified by default (NULL)
#' @param v2 allows forcing the value of the second variable to merge
#' when reducing from 5 to 4 dimensions, not specified by default (NULL)
#' @param v3 allows forcing the value of the first variable to merge
#' when reducing from 4 to 3 dimensions, not specified by default (NULL)
#' @param v4 allows forcing the value of the second variable to merge
#' when reducing from 4 to 3 dimensions, not specified by default (NULL)
#' @param sep separator used during concatenation of variables
#' @param maximize_nb_tabs specifies whether to prefer selecting hierarchical variables with
#' the most nodes as a priority (TRUE), which generates more tables
#' but of smaller size, or non-hierarchical variables with the least modality (FALSE)
#' to create fewer tables
#' @param verbose prints the different steps of the function to notify
#' the user of the progress, mainly for the general function gen_tabs_5_4_to_3()
#' 
#' @return a list(tabs, hrcs5_4, hrcs4_3, alt_tot5_4, alt_tot4_3, vars)
#' tabs: named list of dataframes with 3 dimensions (n-2 dimensions in the general case)
#'       endowed with nested hierarchies
#' hrcs5_4: named list of hrc specific to the variable created via the merge
#'          when reducing from 5 to 4 dimensions
#' hrcs4_3: named list of hrc specific to the variable created via the merge
#'          when reducing from 4 to 3 dimensions
#' alt_tot5_4: named list of totals when reducing from 5 to 4 dimensions
#' alt_tot4_3: named list of totals when reducing from 4 to 3 dimensions
#' vars: named list of vectors representing the merged variables
#'       during the two steps of dimension reduction
#'
#' @examples
#' 
#' library(dplyr)
#' 
#' source("R/passage_4_3_cas_0_non_hrc.R",encoding = "UTF-8")
#' source("R/passage_4_3_cas_1_non_hrc.R",encoding = "UTF-8")
#' source("R/passage_4_3_cas_2_non_hrc.R",encoding = "UTF-8")
#' source("R/passage_4_3.R",encoding = "UTF-8")
#' source("R/passage_5_3.R",encoding = "UTF-8")
#' 
#' data <- expand.grid(
#'   ACT = c("Total", "A", "B", "A1", "A2", "B1", "B2"),
#'   GEO = c("Total", "GA", "GB", "GA1", "GA2", "GB1", "GB2"),
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
#' hrc_geo <- "output/hrc_GEO.hrc"
#' sdcHierarchies::hier_create(root = "Total", nodes = c("GA","GB")) %>% 
#'   sdcHierarchies::hier_add(root = "GA", nodes = c("GA1","GA2")) %>% 
#'   sdcHierarchies::hier_add(root = "GB", nodes = c("GB1","GB2")) %>% 
#'   sdcHierarchies::hier_convert(as = "argus") %>%
#'   slice(-1) %>% 
#'   mutate(levels = substring(paste0(level,name),3)) %>% 
#'   select(levels) %>% 
#'   write.table(file = hrc_geo, row.names = F, col.names = F, quote = F)
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
#' # Results of the function
#' res1 <- passer_de_5_a_3_var(
#'   dfs = data,
#'   nom_dfs = "tab",
#'   totcode = c(SEX="Total",AGE="Total", GEO="Total", ACT="Total", ECO = "PIB"), 
#'   hrcfiles = c(ACT = hrc_act, GEO = hrc_geo, SEX = hrc_sex),
#'   sep_dir = TRUE,
#'   hrc_dir = "output",
#'   v1 = "ACT",
#'   v2 = "AGE",
#'   v3 = "SEX",
#'   v4 = "ECO"
#' )
#' 
#' res2 <- passer_de_5_a_3_var(
#'   dfs = data,
#'   nom_dfs = "tab",
#'   totcode = c(SEX="Total",AGE="Total", GEO="Total", ACT="Total", ECO = "PIB"), 
#'   hrcfiles = c(ACT = hrc_act, GEO = hrc_geo, SEX = hrc_sex),
#'   sep_dir = TRUE,
#'   hrc_dir = "output",
#'   verbose = TRUE
#' )
passer_de_5_a_3_var <- function(
    dfs,
    nom_dfs,
    totcode,
    hrcfiles = NULL, 
    sep_dir = FALSE,
    hrc_dir = "hrc_alt",
    v1 = NULL,
    v2 = NULL,
    v3 = NULL,
    v4 = NULL, 
    sep = "_",
    maximize_nb_tabs = FALSE,
    verbose = FALSE)
{
  # Update the output folder containing the hierarchies
  if( (length(hrcfiles) != 0) & !sep_dir){
    dir_name <- dirname(hrcfiles[[1]])
  } else {
    dir_name <- hrc_dir
  }
  
  # We remove a dimension from our starting dataframe
  res_5_4 <- passer_de_4_a_3_var(dfs = dfs,
                                 nom_dfs = nom_dfs,
                                 totcode = totcode,
                                 hrcfiles = hrcfiles,
                                 sep_dir = TRUE,
                                 hrc_dir = dir_name,
                                 v1 = v1,
                                 v2 = v2,
                                 sep = sep,
                                 maximize_nb_tabs = maximize_nb_tabs)
  if (verbose){
    print(paste(length(res_5_4$tabs),"tables created"))
    print(c("Reducing from 4 to 3..."))
  }
  
  # Retrieving the merged variables
  v1f <- res_5_4$vars[[1]]
  v2f <- res_5_4$vars[[2]]
  new_var = paste(v1f, v2f, sep=sep)
  
  # Updating the totals
  totcode2 <- totcode
  totcode2 <- totcode2[!(names(totcode2) %in% c(v1f, v2f))]
  # totcode2[[new_var]] <- 1
  
  # Updating hrc files
  hrcfiles2 <- hrcfiles
  hrcfiles2 <- hrcfiles2[!(names(hrcfiles2) %in% c(v1f, v2f))]
  
  # Categorical variables without hierarchy in our 4D tables
  var_cat <- c(names(totcode2),new_var)
  
  var_sans_hier <- intersect(
    setdiff(names(dfs), names(hrcfiles2)),
    var_cat
  )
  
  # Choice of variables for the 4 -> 3 transition and verification of those provided in argument
  # We now choose v3 and v4 to be sure that the same variable
  # is created within all the sub-tables
  
  # First variable for the 4 to 3 transition
  if (!is.null(v3)){
    if (!(v3 %in% var_cat)){
      stop(paste("v3 is not a categorical variable, v3 = ", v3,
                 "The categorical variables are: ",paste(var_cat, collapse = ", ")), sep = "")
    }
  } else {
    # we choose a variable avoiding v4
    v3 <- choisir_var(dfs = dfs[setdiff(names(dfs),v4)],
                      totcode = totcode2[setdiff(names(totcode2),v4)],
                      hrcfiles = hrcfiles2[setdiff(names(hrcfiles2),v4)],
                      maximize_nb_tabs = maximize_nb_tabs)
    
    # We check if the merged variable has fewer nodes than the selected variable
    nb_noeuds_v3 <- nb_noeuds(hrcfiles2, v=v3)
    if (!is.null(v4)){
      # We need to do two different if statements otherwise NULL != new_var crashes!
      if (v4 != new_var & maximize_nb_tabs == TRUE){
        v3 <- new_var
      }
      # If v4 = NULL no need to compare v4 != new_var
    } else if (maximize_nb_tabs == TRUE){
      v3 <- new_var
    }
  }
  
  # Second variable for the 4 to 3 transition
  if (!is.null(v4)){
    if (!(v4 %in% var_cat)){
      stop(paste("v4 is not a categorical variable, v4 = ", v4,
                 "The categorical variables are: ",paste(var_cat, collapse = ", ")), sep = "")
    }
    if (v3 == v4){
      stop("Error. You are trying to merge a variable with itself")
    }
    
  } else {
    # we choose a variable avoiding v3
    v4 <- choisir_var(dfs = dfs[setdiff(names(dfs),v3)],
                      totcode = totcode2[setdiff(names(totcode2),v3)],
                      hrcfiles = hrcfiles2[setdiff(names(hrcfiles2),v3)],
                      maximize_nb_tabs = maximize_nb_tabs)
    
    # We check if the merged variable has fewer nodes than the selected variable
    nb_noeuds_v4 <- nb_noeuds(hrcfiles2, v=v4)
    # Rq : v3 can not be NULL
    if (v3 != new_var & maximize_nb_tabs == TRUE){
      v4 <- new_var
    }
  }
  
  appel_4_3_gen <- function(nom_dfsb){
    # Update the arguments of the function
    dfsb <- res_5_4$tabs[[nom_dfsb]]
    
    hrcfiles2b <-  c(hrcfiles2, res_5_4$hrcs[[nom_dfsb]])
    names(hrcfiles2b)[length(hrcfiles2b)] <- new_var
    
    totcode2[[new_var]] <- res_5_4$alt_tot[[nom_dfsb]]
    
    passer_de_4_a_3_var(dfs = dfsb,
                        nom_dfs = nom_dfsb,
                        totcode = totcode2,
                        hrcfiles = hrcfiles2b,
                        sep_dir = TRUE,
                        hrc_dir = dir_name,
                        v1 = v3,
                        v2 = v4, 
                        sep = sep)
  }
  
  # Transform all our 4-var tables into 3-var tables
  res_5_3 <- lapply(
    names(res_5_4$tabs),
    appel_4_3_gen
  )
  
  tabs <- unlist(lapply(res_5_3, function(x) x$tabs), recursive = FALSE)
  hrcs4_3 <- unlist(lapply(res_5_3, function(x) x$hrcs), recursive = FALSE)
  alt_tot4_3 <- unlist(lapply(res_5_3, function(x) x$alt_tot), recursive = FALSE)
  
  vars1 <- res_5_4$vars
  vars2 <- res_5_3[[1]]$vars # merged variables are always the same
  vars_tot <- list(vars1,vars2)
  names(vars_tot) <- c("five_to_three","four_to_three")
  
  # Memorization of res5_4
  
  # Case we merge 4 different variables 
  if (!(new_var %in% c(v3,v4))){
    # We repeat as many times res5_4[i] as the table will create 
    # 3-dimensional tables
    
    # Each 4-dimensional table will create the same number of 3-dimensional tables
    # because the selected variables have the same modes in each of them
    nb_rep <- length(tabs) / length(res_5_4$tabs)
    hrcs5_4 <- as.list(unlist(lapply(res_5_4$hrcs,
                                     function(x) rep(x,nb_rep))))
    
    alt_tot5_4 <- as.list(unlist(lapply(res_5_4$alt_tot,
                                        function(x) rep(x,nb_rep))))
    
    # If we merge 3 variables into one, the number of tables
    # created by each table changes!
  } else {
    # Store the name of the variable that is not new_var in a new object
    non_fused_var <- ifelse(v3 == new_var, v4, v3)
    
    # Calculate the value of nb_noeuds once for each res_5_4$hrcs[[x]]
    # to avoid calculating the same quantity twice
    results <- lapply(1:length(res_5_4$hrcs), function(x) {
      nb_noeuds_val <- 2 * nb_noeuds(res_5_4$hrcs[[x]], hrc_name = FALSE) * 
                           nb_noeuds(hrcfiles2, non_fused_var)
      
      # Use the calculated value for hrcs5_4 and alt_tot5_4
      list(
        hrcs = rep(res_5_4$hrcs[[x]], nb_noeuds_val),
        alt_tot = rep(res_5_4$alt_tot[[x]], nb_noeuds_val)
      )
    })
    
    # Extract the values for hrcs5_4 and alt_tot5_4
    hrcs5_4 <- as.list(unlist(lapply(results, function(x) x$hrcs)))
    alt_tot5_4 <- as.list(unlist(lapply(results, function(x) x$alt_tot)))
  }
  
  return(list(tabs = tabs,
              hrcs5_4 = hrcs5_4,
              hrcs4_3 = hrcs4_3,
              alt_tot5_4 = alt_tot5_4,
              alt_tot4_3 = alt_tot4_3,
              vars = vars_tot)
  )
}
