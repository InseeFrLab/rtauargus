#' General function that selects the appropriate separator and applies dimension reduction.
#'
#' @param dfs data.frame with 4 or 5 categorical variables
#' @param nom_dfs name of the data.frame in the list provided by the user
#' @param totcode named vector of totals for categorical variables
#' @param hrcfiles named vector indicating the hrc files of hierarchical variables
#' among the categorical variables of dfs
#' @param sep_dir allows forcing the writing of hrc into a separate folder,
#' default is FALSE
#' @param hrc_dir folder to write hrc files if writing to a new folder is forced
#' or if no folder is specified in hrcfiles
#' @param vars_a_fusionner NULL or vector of variables to be merged:
#' 2 in dimension 4; 3 or 4 in dimension 5
#' @param nb_tab strategy to automatically choose variables:
#' min: minimize the number of tables;
#' max: maximize the number of tables;
#' smart: minimize the number of tables under the constraint of their number of rows
#' @param LIMIT maximum allowed number of rows in the smart or split case 
#' @param split indicate if we split in several tables the table bigger than LIMIT at the end
#' it decreases the number of hierarchy of these tables
#' @param vec_sep vector of candidate separators to use
#' @param verbose print the different steps of the function to inform the user of progress
#'
#' @return list(tabs, hrcs, alt_tot, vars, sep, totcode, hrcfiles, fus_vars)
#' tabs: named list of 3-dimensional dataframes with nested hierarchies
#' hrcs: named list of hrc specific to the variables created during merging to go to dimension 3
#' alt_tot: named list of totals specific to the variables created during merging to go to dimension 3
#' vars: categorical variables of the output dataframes
#' sep: separator used to link the variables
#' totcode: named vector of totals for all categorical variables
#' hrcfiles: named vector of hrc for categorical variables (except the merged one)
#' fus_vars: named vector of vectors representing the merged variables during dimension reduction
#' @export
#'
#' TODO: 
#' to save time: parallelize the lapply for variable selection
#'                                     lapply for reducing from 4 to 3 dimensions
#'                                     in the case of dimension 5
#'                                     
#' @examples
#' library(dplyr)
#' library(stringr)
#' 
#' source("R/reduce_dims.R")
#' source("R/passage_5_3.R", encoding = "UTF-8")
#' source("R/passage_4_3_cas_0_non_hrc.R", encoding = "UTF-8")
#' source("R/passage_4_3_cas_1_non_hrc.R", encoding = "UTF-8")
#' source("R/passage_4_3_cas_2_non_hrc.R", encoding = "UTF-8")
#' source("R/passage_4_3.R", encoding = "UTF-8")
#' source("R/choisir_sep.R", encoding = "UTF-8")
#' source("R/format.R", encoding = "UTF-8")
#' source("R/length_tabs.R", encoding = "UTF-8")
#' source("R/nb_tab.R", encoding = "UTF-8")
#' source("R/chercher_combinaison_variable_a_fusionner.R", encoding = "UTF-8")
#' source("R/split_table.R",encoding = "UTF-8")
#' 
#' # Examples for dimension 4
#' 
#' data <- expand.grid(
#'   ACT = c("Total", "A", "B", "A1", "A2","A3", "B1", "B2","B3","B4","C","D","E","F","G","B5"),
#'   GEO = c("Total", "G1", "G2"),
#'   SEX = c("Total", "F", "M"),
#'   AGE = c("Total", "AGE1", "AGE2"),
#'   stringsAsFactors = FALSE
#' ) %>%
#'   as.data.frame()
#' 
#' data <- data %>% mutate(VALUE = 1)
#' 
#' 
#' hrc_act <- "output/hrc_ACT.hrc"
#' 
#' sdcHierarchies::hier_create(root = "Total", nodes = c("A","B","C","D","E","F","G")) %>%
#'   sdcHierarchies::hier_add(root = "A", nodes = c("A1","A2","A3")) %>%
#'   sdcHierarchies::hier_add(root = "B", nodes = c("B1","B2","B3","B4","B5")) %>%
#'   sdcHierarchies::hier_convert(as = "argus") %>%
#'   slice(-1) %>%
#'   mutate(levels = substring(paste0(level,name),3)) %>%
#'   select(levels) %>%
#'   write.table(file = hrc_act, row.names = F, col.names = F, quote = F)
#' 
#' # Reduce dim by forcing variables to be merged
#' res1 <- gen_tabs_5_4_to_3(
#'   dfs = data,
#'   nom_dfs = "tab",
#'   totcode = c(SEX = "Total", AGE = "Total", GEO = "Total", ACT = "Total"),
#'   hrcfiles = c(ACT = hrc_act),
#'   sep_dir = TRUE,
#'   vars_a_fusionner = c("ACT", "GEO"),
#'   hrc_dir = "output"
#' )
#' 
#' # Split the output in order to be under the limit & forcing variables to be merged
#' res1b <- gen_tabs_5_4_to_3(
#'   dfs = data,
#'   nom_dfs = "tab",
#'   totcode = c(SEX = "Total", AGE = "Total", GEO = "Total", ACT = "Total"),
#'   hrcfiles = c(ACT = hrc_act),
#'   sep_dir = TRUE,
#'   hrc_dir = "output",
#'   nb_tab = 'max',
#'   verbose = TRUE
#' )
#' 
#' # Result of the function (minimizes the number of created tables by default)
#' res2 <- gen_tabs_5_4_to_3(
#'   dfs = data,
#'   nom_dfs = "tab",
#'   totcode = c(SEX = "Total", AGE = "Total", GEO = "Total", ACT = "Total"),
#'   hrcfiles = c(ACT = hrc_act),
#'   sep_dir = TRUE,
#'   hrc_dir = "output"
#' )
#' 
#' # Result of the function (maximize the number of created tables)
#' res3 <- gen_tabs_5_4_to_3(
#'   dfs = data,
#'   nom_dfs = "tab",
#'   totcode = c(SEX = "Total", AGE = "Total", GEO = "Total", ACT = "Total"),
#'   hrcfiles = c(ACT = hrc_act),
#'   sep_dir = TRUE,
#'   hrc_dir = "output",
#'   nb_tab = "smart",
#'   LIMIT = 1
#' )
#' 
#' # Example for dimension 5
#' 
#' data <- expand.grid(
#'   ACT = c("Total_A", paste0("A", seq(1,5),"_"),paste0("A1_", seq(1,7)),paste0("A2_", seq(1,9))),
#'   GEO = c("Total_G", "GA", "GB", "GA1", "GA2", "GB1", "GB2","GA3","GB3","GB4"),
#'   SEX = c("Total_S", "F", "M","F1","F2","M1","M2"),
#'   AGE = c("Ensemble", "AGE1", "AGE2", "AGE11", "AGE12", "AGE21", "AGE22"),
#'   ECO = c("PIB","Ménages","Entreprises"),
#'   stringsAsFactors = FALSE,
#'   KEEP.OUT.ATTRS = FALSE
#' ) %>% 
#'   as.data.frame()
#' 
#' data <- data %>% mutate(VALUE = 1:n())
#' 
#' hrc_act <- "output/hrc_ACT.hrc"
#' sdcHierarchies::hier_create(root = "Total_A", nodes = paste0("A", seq(1,5),"_")) %>% 
#'   sdcHierarchies::hier_add(root = "A1_", nodes = paste0("A1_", seq(1,7))) %>% 
#'   sdcHierarchies::hier_add(root = "A2_", nodes = paste0("A2_", seq(1,9))) %>% 
#'   sdcHierarchies::hier_convert(as = "argus") %>%
#'   slice(-1) %>% 
#'   mutate(levels = substring(paste0(level,name),3)) %>% 
#'   select(levels) %>% 
#'   write.table(file = hrc_act, row.names = F, col.names = F, quote = F)
#' 
#' hrc_geo <- "output/hrc_GEO.hrc"
#' sdcHierarchies::hier_create(root = "Total_G", nodes = c("GA","GB")) %>% 
#'   sdcHierarchies::hier_add(root = "GA", nodes = c("GA1","GA2","GA3")) %>% 
#'   sdcHierarchies::hier_add(root = "GB", nodes = c("GB1","GB2","GB3","GB4")) %>% 
#'   sdcHierarchies::hier_convert(as = "argus") %>%
#'   slice(-1) %>% 
#'   mutate(levels = substring(paste0(level,name),3)) %>% 
#'   select(levels) %>% 
#'   write.table(file = hrc_geo, row.names = F, col.names = F, quote = F)
#' 
#' # Results of the function
#' res4 <- gen_tabs_5_4_to_3(
#'   dfs = data,
#'   nom_dfs = "tab",
#'   totcode = c(SEX = "Total_S", AGE = "Ensemble", GEO = "Total_G", ACT = "Total_A", ECO = "PIB"), 
#'   hrcfiles = c(ACT = hrc_act, GEO = hrc_geo),
#'   sep_dir = TRUE,
#'   hrc_dir = "output"
#' )
#' 
#' res5 <- gen_tabs_5_4_to_3(
#'   dfs = data,
#'   nom_dfs = "tab",
#'   totcode = c(SEX = "Total_S", AGE = "Ensemble", GEO = "Total_G", ACT = "Total_A", ECO = "PIB"), 
#'   hrcfiles = c(ACT = hrc_act, GEO = hrc_geo),
#'   sep_dir = TRUE,
#'   hrc_dir = "output",
#'   nb_tab = 'smart',
#'   LIMIT = 1300,
#'   verbose = TRUE,
#' )
#' 
#' res6 <- gen_tabs_5_4_to_3(
#'   dfs = data,
#'   nom_dfs = "tab",
#'   totcode = c(SEX = "Total_S", AGE = "Ensemble", GEO = "Total_G", ACT = "Total_A", ECO = "PIB"), 
#'   hrcfiles = c(ACT = hrc_act, GEO = hrc_geo),
#'   sep_dir = TRUE,
#'   hrc_dir = "output",
#'   nb_tab = 'min',
#'   verbose = TRUE,
#'   LIMIT = 4470,
#'   split = TRUE
#' )
gen_tabs_5_4_to_3 <- function(
    dfs,
    nom_dfs,
    totcode,
    hrcfiles = NULL,
    sep_dir = FALSE, 
    hrc_dir = "hrc_alt",
    vars_a_fusionner = NULL,
    nb_tab = "min",
    LIMIT = NULL,
    split = FALSE,
    vec_sep = c("\\_+_", "\\_!_", "\\_?_","___","_z_z_z_z"),
    verbose = FALSE
){
  require(sdcHierarchies)
  dfs <- as.data.frame(dfs)
  
  # Check if nom_dfs is a character string
  if (!is.character(nom_dfs)){
    stop("nom_dfs must be a character string.")
  }
  
  # Check if all modalities of totcode are present in dfs
  if (any(!names(totcode) %in% names(dfs))){
    stop("At least one modality in totcode is not present in dfs!")
  }
  
  # Check if the number of dimensions in totcode is either 4 or 5
  if (!(length(totcode) %in% c(4,5))){
    stop("Please provide a dataframe with 4 or 5 categorical variables!")
  }
  
  # Check if the number of variables to merge is valid for 4-dimensional data
  if (length(totcode) == 4 & !length(vars_a_fusionner) %in% c(0,2)){
    stop("For 4-dimensional data, please specify 2 variables or leave vars_a_fusionner as NULL!")
  }
  
  # Check if the number of variables to merge is valid for 5-dimensional data
  if (length(totcode) == 5 & !length(vars_a_fusionner) %in% c(0,3,4)){
    stop("For 5-dimensional data, please specify 2 or 3 variables or leave vars_a_fusionner as NULL!")
  }
  
  # Check if all modalities of hrcfiles are present in dfs
  if (any(!names(hrcfiles) %in% names(dfs))){
    stop("At least one modality in hrcfiles is not present in dfs!")
  }
  
  # Check if sep_dir is a logical value
  if (!is.logical(sep_dir)){
    stop("sep_dir must be a logical value.")
  }
  
  # Check if hrc_dir is a character string
  if (!is.character(hrc_dir)){
    stop("hrc_dir must be a character string.")
  }
  
  # Check if nb_tab is one of the valid options
  if (!nb_tab %in% c('min', 'max', 'smart')){
    stop("nb_tab must be 'min', 'max', or 'smart'!")
  }
  
  # If vars_a_fusionner is specified, check if all variables are present in totcode
  if (!is.null(vars_a_fusionner)){
    if (any(!vars_a_fusionner %in% names(totcode))){
      stop("vars_a_fusionner contains at least one variable that is not in totcode!")
    }
  }
  
  # Check if verbose is a logical value
  if (!is.logical(verbose)){
    stop("verbose must be a logical value.")
  }
  
  # Check if verbose is a logical value
  if (!is.logical(split)){
    stop("split must be a logical value.")
  }
  
  # LIMIT is not used if the user does not use split or nb_tab
  # we consider it to be an error if the users specifies it
  if (split | nb_tab == "smart"){
    if (is.null(LIMIT)){
      stop("You must specify a LIMIT (number) if you use split = TRUE or nb_tab = \"smart\"")
    }
    
    # Convert LIMIT to numeric
    LIMIT <- as.numeric(LIMIT)
    
  } else {
    if (!is.null(LIMIT)){
      stop("You must not specify a LIMIT (number) if you do not use split = TRUE or nb_tab = \"smart\"")
    }
  }
  
  
  
  # Choose the separator
  data_var_cat <- dfs[names(dfs) %in% names(totcode)]
  sep <- choisir_sep(data_var_cat, vec_sep)
  
  if (length(totcode) == 5) {
    # If the user specified the variables to merge
    if (length(vars_a_fusionner) == 3) {
      v1 <- vars_a_fusionner[[1]]
      v2 <- vars_a_fusionner[[2]]
      v3 <- vars_a_fusionner[[3]]
      v4 <- paste(v1, v2, sep = sep)
      
    } else if (length(vars_a_fusionner) == 4) {
      v1 <- vars_a_fusionner[[1]]
      v2 <- vars_a_fusionner[[2]]
      v3 <- vars_a_fusionner[[3]]
      v4 <- vars_a_fusionner[[4]]
      
    } else {
      # If the user did not specify the variables to merge, we need to calculate them
      
      if (nb_tab == 'smart') {
        
        if (verbose) {
          print("Choosing variables...")
        }
        
        # Propose combinations of variables to merge
        choix_3_var <- choisir_var_a_fusionner_general(dfs = data,
                                                       totcode = totcode,
                                                       hrcfiles = hrcfiles,
                                                       nb_var = 3,
                                                       LIMIT = LIMIT,
                                                       nb_tab = nb_tab)
        
        choix_4_var <- choisir_var_a_fusionner_general(dfs = data,
                                                       totcode = totcode,
                                                       hrcfiles = hrcfiles,
                                                       nb_var = 4,
                                                       LIMIT = LIMIT,
                                                       nb_tab = nb_tab)
        
        # Choose the best combination
        # The less nb of tab is the row limit is respected
        # or the less nb or row if the limit cannot be respected
        if (
            (choix_3_var$nb_tab < choix_4_var$nb_tab & 
              max(choix_4_var$max_row,choix_3_var$max_row) < LIMIT) |
            
            (choix_3_var$max_row < choix_4_var$max_row & 
              choix_4_var$max_row > LIMIT)
            )
          {
          
          v1 <- choix_3_var$vars[[1]]
          v2 <- choix_3_var$vars[[2]]
          v3 <- choix_3_var$vars[[3]]
          v4 <- paste(v1, v2, sep = sep)
        } else {
          v1 <- choix_4_var$vars[[1]]
          v2 <- choix_4_var$vars[[2]]
          v3 <- choix_4_var$vars[[3]]
          v4 <- choix_4_var$vars[[4]]
        }
        
        # Return to the primitive implementation to minimize or maximize
        # the number of tables since the old implementation is not bad and is
        # faster than calculating the size and number of generated tables
      } else {
        v1 <- NULL
        v2 <- NULL
        v3 <- NULL
        v4 <- NULL
        maximize_nb_tabs <- if (nb_tab == 'max') TRUE else FALSE
      }
    }
    
    if (verbose) {
      print("Reducing from 5 to 4...")
    }
    
    res <- passer_de_5_a_3_var(dfs = dfs,
                               nom_dfs = nom_dfs,
                               totcode = totcode,
                               hrcfiles = hrcfiles,
                               sep_dir = sep_dir,
                               hrc_dir = hrc_dir,
                               v1 = v1, v2 = v2,
                               v3 = v3, v4 = v4,
                               sep = sep,
                               maximize_nb_tabs = maximize_nb_tabs,
                               verbose = verbose)
    
  } else if (length(totcode) == 4) {
    
    # If the user specified the variables to merge
    if (length(vars_a_fusionner) == 2) {
      v1 <- vars_a_fusionner[[1]]
      v2 <- vars_a_fusionner[[2]]
      
    } else {
      # If the user did not specify the variables to merge, we need to calculate them
      
      if (nb_tab == 'smart') {
        
        if (verbose) {
          print("Choosing variables...")
        }
        
        choix_2_var <- choisir_var_a_fusionner_general(dfs = data,
                                                       totcode = totcode,
                                                       hrcfiles = hrcfiles,
                                                       nb_var = 2,
                                                       LIMIT = LIMIT,
                                                       nb_tab = nb_tab)
        v1 <- choix_2_var$vars[[1]]
        v2 <- choix_2_var$vars[[2]]
        
        # Return to the primitive implementation to minimize or maximize
        # the number of tables since the old implementation is not bad and is
        # faster than calculating the size and number of generated tables
      } else {
        v1 <- NULL
        v2 <- NULL
        maximize_nb_tabs <- if (nb_tab == 'max') TRUE else FALSE
      }
    }
    
    if (verbose) {
      print("Reducing from 4 to 3...")
    }
    
    res <- passer_de_4_a_3_var(dfs = dfs,
                               nom_dfs = nom_dfs,
                               totcode = totcode,
                               hrcfiles = hrcfiles,
                               sep_dir = sep_dir,
                               hrc_dir = hrc_dir,
                               v1 = v1, v2 = v2,
                               sep = sep,
                               maximize_nb_tabs = maximize_nb_tabs)
  }
  
  if (verbose) {
    print(paste(length(res$tabs), "tables created"))
  }
  
  # Put a format usable by rtauargus
  res <- format(res = res,
                nom_dfs = nom_dfs,
                sep = sep,
                totcode = totcode,
                hrcfiles = hrcfiles)
  
  # Split too big table
  if (split) {
    
    if (verbose) {
      print("Spliting...")
    }
    
    # Collect of created vars
    if (length(totcode) == 4){
      liste_var_fus <- paste(res$fus_vars[1],
                             res$fus_vars[2],
                             sep = res$sep)
    } else {
      v1 <- res$fus_vars[[1]][1]
      v2 <- res$fus_vars[[1]][2]
      
      v1_v2 <- paste(v1,v2, sep = res$sep)
      
      v3 <- res$fus_vars[[2]][1]
      v4 <- res$fus_vars[[2]][2]
      
      # 3 variables merged together
      if (v1_v2 %in% c(v3,v4)){
        liste_var_fus <- list(paste(v3,v4, sep = res$sep))
        
      # 2 couples created
      } else {
        liste_var_fus <- list(v1_v2,
                              paste(v3,v4, sep = res$sep))
      }
    }
    
    for (var_fus in liste_var_fus){
      
      if (verbose) {
        print(var_fus)
      }
      
      res <- split_tab(res = res,
                       LIMIT = LIMIT,
                       var_fus = var_fus)
    }
    
    if (verbose) {
      print(paste(length(res$tabs), "tables created"))
    }
  }
  
  # The user specified a LIMIT (smart or split case)
  if (!is.null(LIMIT)){
    max_row <- max(sapply(res$tabs, nrow))
    
    if (max_row > LIMIT){
      warning(c("
      The limit of ",LIMIT," cannot be achieved.
      The largest table has ",max_row," rows."))
    }
  }
  
  return(res)
}
