#' General function that selects the appropriate separator and applies dimension reduction.
#'
#' @param dfs data.frame with 4 or 5 categorical variables
#' @param dfs_name name of the data.frame in the list provided by the user
#' @param totcode named vector of totals for categorical variables
#' @param hrcfiles named vector indicating the hrc files of hierarchical variables
#' among the categorical variables of dfs
#' @param sep_dir allows forcing the writing of hrc into a separate folder,
#' default is FALSE
#' @param hrc_dir folder to write hrc files if writing to a new folder is forced
#' or if no folder is specified in hrcfiles
#' @param vars_to_merge NULL or vector of variables to be merged:
#' 2 in dimension 4; 3 or 4 in dimension 5
#' @param nb_tab_option strategy to follow for choosing variables automatically:
#' \itemize{
#'   \item `'min'`: minimize the number of tables;
#'   \item `'max'`: maximize the number of tables;
#'   \item `'smart'`: minimize the number of tables under the constraint
#'   of their row count.
#' }
#' @param limit maximum allowed number of rows in the smart or over_split = TRUE case
#' @param over_split indicates if we split in several tables the tables bigger than
#' limit at the end of the reduction process ; it decreases the number
#' of hierarchy of these tables
#' @param vec_sep vector of candidate separators to use
#' @param verbose print the different steps of the function to inform the user
#' of progress
#'
#' @return A list containing:
#' \itemize{
#'   \item `tabs`: named list of 3-dimensional dataframes
#'   with nested hierarchies
#'   \item `alt_hrc`: named list of hrc specific to the variables created
#'   during merging to go to dimension 3
#'   \item `alt_totcode`: named list of totals specific to the variables
#'   created during merging to go to dimension 3
#'   \item `vars`: categorical variables of the output dataframes
#'   \item `sep`: separator used to link the variables
#'   \item `totcode`: named vector of totals for all categorical variables
#'   \item `hrcfiles`: named vector of hrc for categorical variables
#'   (except the merged one)
#'   \item `fus_vars`: named vector of vectors representing the merged
#'   variables during dimension reduction
#' }
#'
#' @importFrom sdcHierarchies hier_import hier_convert
#' @importFrom stringr str_detect
#' @importFrom dplyr select mutate filter
#'
#' @examples
#' library(dplyr)
#' # Examples for dimension 4
#'
#' data <- expand.grid(
#'   ACT = c("Total", "A", "B", "A1", "A2","A3", "B1",
#'   "B2","B3","B4","C","D","E","F","G","B5"),
#'   GEO = c("Total", "G1", "G2"),
#'   SEX = c("Total", "F", "M"),
#'   AGE = c("Total", "AGE1", "AGE2"),
#'   stringsAsFactors = FALSE
#' ) %>%
#'   as.data.frame() %>%
#'   mutate(VALUE = 1)
#'
#' if(!dir.exists("hrc")) dir.create("hrc")
#' hrc_act <- "hrc/hrc_ACT4.hrc"
#'
#' sdcHierarchies::hier_create(
#'   root = "Total",
#'   nodes = c("A","B","C","D","E","F","G")
#' ) %>%
#'   sdcHierarchies::hier_add(root = "A", nodes = c("A1","A2","A3")) %>%
#'   sdcHierarchies::hier_add(root = "B", nodes = c("B1","B2","B3","B4","B5")) %>%
#'   sdcHierarchies::hier_convert(as = "argus") %>%
#'   slice(-1) %>%
#'   mutate(levels = substring(paste0(level,name),3)) %>%
#'   select(levels) %>%
#'   write.table(
#'     file = hrc_act, row.names = FALSE, col.names = FALSE, quote = FALSE
#'   )
#'
#' # Reduce dim by forcing variables to be merged
#' res1 <- reduce_dims(
#'   dfs = data,
#'   dfs_name = "tab",
#'   totcode = c(SEX = "Total", AGE = "Total", GEO = "Total", ACT = "Total"),
#'   hrcfiles = c(ACT = hrc_act),
#'   sep_dir = TRUE,
#'   vars_to_merge = c("ACT", "GEO"),
#'   hrc_dir = "output",
#'   verbose = TRUE
#' )
#'
#' # Split the output in order to be under the limit & forcing variables to be merged
#' res1b <- reduce_dims(
#'   dfs = data,
#'   dfs_name = "tab",
#'   totcode = c(SEX = "Total", AGE = "Total", GEO = "Total", ACT = "Total"),
#'   hrcfiles = c(ACT = hrc_act),
#'   sep_dir = TRUE,
#'   hrc_dir = "output",
#'   nb_tab_option = 'smart',
#'   over_split = TRUE,
#'   verbose = TRUE,
#'   limit = 100
#' )
#'
#' # Result of the function (minimizes the number of created tables by default)
#' res2 <- reduce_dims(
#'   dfs = data,
#'   dfs_name = "tab",
#'   totcode = c(SEX = "Total", AGE = "Total", GEO = "Total", ACT = "Total"),
#'   hrcfiles = c(ACT = hrc_act),
#'   sep_dir = TRUE,
#'   hrc_dir = "output",
#'   verbose = TRUE
#' )
#'
#' # Result of the function (maximize the number of created tables)
#' res3 <- reduce_dims(
#'   dfs = data,
#'   dfs_name = "tab",
#'   totcode = c(SEX = "Total", AGE = "Total", GEO = "Total", ACT = "Total"),
#'   hrcfiles = c(ACT = hrc_act),
#'   sep_dir = TRUE,
#'   hrc_dir = "output",
#'   nb_tab_option = "max",
#'   verbose = TRUE
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
#'   as.data.frame() %>%
#'   mutate(VALUE = 1:n())
#'
#' hrc_act <- "hrc/hrc_ACT5.hrc"
#' sdcHierarchies::hier_create(root = "Total_A", nodes = paste0("A", seq(1,5),"_")) %>%
#'   sdcHierarchies::hier_add(root = "A1_", nodes = paste0("A1_", seq(1,7))) %>%
#'   sdcHierarchies::hier_add(root = "A2_", nodes = paste0("A2_", seq(1,9))) %>%
#'   sdcHierarchies::hier_convert(as = "argus") %>%
#'   slice(-1) %>%
#'   mutate(levels = substring(paste0(level,name),3)) %>%
#'   select(levels) %>%
#'   write.table(file = hrc_act, row.names = FALSE, col.names = FALSE, quote = FALSE)
#'
#' hrc_age <- "hrc/hrc_AGE5.hrc"
#' sdcHierarchies::hier_create(root = "Ensemble", nodes = c("AGE1", "AGE2")) %>%
#'   sdcHierarchies::hier_add(root = "AGE1", nodes = c("AGE11", "AGE12")) %>%
#'   sdcHierarchies::hier_add(root = "AGE2", nodes = c("AGE21", "AGE22")) %>%
#'   sdcHierarchies::hier_convert(as = "argus") %>%
#'   slice(-1) %>%
#'   mutate(levels = substring(paste0(level,name),3)) %>%
#'   select(levels) %>%
#'   write.table(file = hrc_age, row.names = FALSE, col.names = FALSE, quote = FALSE)
#'
#' hrc_geo <- "hrc/hrc_GEO5.hrc"
#' sdcHierarchies::hier_create(root = "Total_G", nodes = c("GA","GB")) %>%
#'   sdcHierarchies::hier_add(root = "GA", nodes = c("GA1","GA2","GA3")) %>%
#'   sdcHierarchies::hier_add(root = "GB", nodes = c("GB1","GB2","GB3","GB4")) %>%
#'   sdcHierarchies::hier_convert(as = "argus") %>%
#'   slice(-1) %>%
#'   mutate(levels = substring(paste0(level,name),3)) %>%
#'   select(levels) %>%
#'   write.table(file = hrc_geo, row.names = FALSE, col.names = FALSE, quote = FALSE)
#'
#' # Results of the function
#' res4 <- reduce_dims(
#'   dfs = data,
#'   dfs_name = "tab",
#'   totcode = c(SEX = "Total_S", AGE = "Ensemble", GEO = "Total_G", ACT = "Total_A", ECO = "PIB"),
#'   hrcfiles = c(ACT = hrc_act, GEO = hrc_geo, AGE = hrc_age),
#'   sep_dir = TRUE,
#'   hrc_dir = "output",
#'   verbose = TRUE
#' )
#'
#' res5 <- reduce_dims(
#'   dfs = data,
#'   dfs_name = "tab",
#'   totcode = c(SEX = "Total_S", AGE = "Ensemble", GEO = "Total_G", ACT = "Total_A", ECO = "PIB"),
#'   hrcfiles = c(ACT = hrc_act, GEO = hrc_geo),
#'   sep_dir = TRUE,
#'   hrc_dir = "output",
#'   nb_tab_option = 'smart',
#'   limit = 1300,
#'   verbose = TRUE
#' )
#'
#' res6 <- reduce_dims(
#'   dfs = data,
#'   dfs_name = "tab",
#'   totcode = c(SEX = "Total_S", AGE = "Ensemble", GEO = "Total_G", ACT = "Total_A", ECO = "PIB"),
#'   hrcfiles = c(ACT = hrc_act, GEO = hrc_geo),
#'   sep_dir = TRUE,
#'   hrc_dir = "output",
#'   nb_tab_option = 'min',
#'   verbose = TRUE,
#'   limit = 4470,
#'   over_split = TRUE
#' )
#' @keywords internal
#' @export
reduce_dims <- function(
    dfs,
    dfs_name,
    totcode,
    hrcfiles = NULL,
    sep_dir = FALSE,
    hrc_dir = "hrc_alt",
    vars_to_merge = NULL,
    nb_tab_option = "min",
    limit = NULL,
    over_split = FALSE,
    vec_sep = c("___","_XXX_","_YYY_", "_TTT_", "_UVW_"),
    verbose = FALSE
){

  # TODO OR NOT:
  # to save time: parallelize the lapply for variable selection
  #                                     lapply for reducing from 4 to 3 dimensions
  #                                    in the case of dimension 5

  dfs <- as.data.frame(dfs)


  # Check if dfs_name is a character string
  if (!is.character(dfs_name)){
    stop("dfs_name must be a character string.")
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
  if (length(totcode) == 4 & !length(vars_to_merge) %in% c(0,2)){
    stop("For 4-dimensional data, please specify 2 variables or leave vars_to_merge as NULL!")
  }

  # Check if the number of variables to merge is valid for 5-dimensional data
  if (length(totcode) == 5 & !length(vars_to_merge) %in% c(0,3,4)){
    stop("For 5-dimensional data, please specify 2 or 3 variables or leave vars_to_merge as NULL!")
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

  # Check if nb_tab_option is one of the valid options
  if (!nb_tab_option %in% c('min', 'max', 'smart')){
    stop("nb_tab_option must be 'min', 'max', or 'smart'!")
  }

  # If vars_to_merge is specified, check if all variables are present in totcode
  if (!is.null(vars_to_merge)){
    if (any(!vars_to_merge %in% names(totcode))){
      stop("vars_to_merge contains at least one variable that is not in totcode!")
    }
  }

  # Check if verbose is a logical value
  if (!is.logical(verbose)){
    stop("verbose must be a logical value.")
  }

  # Check if verbose is a logical value
  if (!is.logical(over_split)){
    stop("over_split must be a logical value.")
  }

  # limit is not used if the user does not use over_split or nb_tab_option
  # we consider it to be an error if the users specifies it
  if (over_split | nb_tab_option == "smart"){
    if (is.null(limit)){
      stop("You must specify a limit (number) if you use over_split = TRUE or nb_tab_option = \"smart\"")
    }

    # Convert limit to numeric
    limit <- as.numeric(limit)

  } else {
    if (!is.null(limit)){
      stop("You must not specify a limit (number) if you do not use over_split = TRUE or nb_tab_option = \"smart\"")
    }
  }



  # Choose the separator
  data_var_cat <- dfs[names(dfs) %in% names(totcode)]
  sep <- chose_sep(data_var_cat, vec_sep)

  if (length(totcode) == 5) {
    # If the user specified the variables to merge
    if (length(vars_to_merge) == 3) {
      v1 <- vars_to_merge[[1]]
      v2 <- vars_to_merge[[2]]
      v3 <- vars_to_merge[[3]]
      v4 <- paste(v1, v2, sep = sep)

    } else if (length(vars_to_merge) == 4) {
      v1 <- vars_to_merge[[1]]
      v2 <- vars_to_merge[[2]]
      v3 <- vars_to_merge[[3]]
      v4 <- vars_to_merge[[4]]

    } else {
      # If the user did not specify the variables to merge, we need to calculate them

      if (nb_tab_option == 'smart') {

        if (verbose) {
          cat("Choosing variables...\n")
        }

        # Propose combinations of variables to merge
        choice_3_var <- var_to_merge(dfs = dfs,
                                     totcode = totcode,
                                     hrcfiles = hrcfiles,
                                     nb_var = 3,
                                     limit = limit,
                                     nb_tab_option = nb_tab_option)

        choice_4_var <- var_to_merge(dfs = dfs,
                                     totcode = totcode,
                                     hrcfiles = hrcfiles,
                                     nb_var = 4,
                                     limit = limit,
                                     nb_tab_option = nb_tab_option)

        # Choose the best combination
        # The less nb of tab is the row limit is respected
        # or the less nb or row if the limit cannot be respected
        if (
          (choice_3_var$nb_tab < choice_4_var$nb_tab &
           max(choice_4_var$max_row,choice_3_var$max_row) < limit) |

          (choice_3_var$max_row < choice_4_var$max_row &
           choice_4_var$max_row > limit)
        )
        {

          v1 <- choice_3_var$vars[[1]]
          v2 <- choice_3_var$vars[[2]]
          v3 <- choice_3_var$vars[[3]]
          v4 <- paste(v1, v2, sep = sep)

          if (choice_3_var$max_row > limit){
            cat(c("Warning when choosing variables:
The limit of ",limit," cannot be achieved.
The largest table has ",choice_3_var$max_row," rows.\n"))
          }

        } else {
          v1 <- choice_4_var$vars[[1]]
          v2 <- choice_4_var$vars[[2]]
          v3 <- choice_4_var$vars[[3]]
          v4 <- choice_4_var$vars[[4]]

          if (choice_3_var$max_row > limit){
            cat(c("Warning when choosing variables:
The limit of ",limit," cannot be achieved.
The largest table has ",choice_3_var$max_row," rows.\n"))
          }
        }

        # Return to the primitive implementation to minimize or maximize
        # the number of tables since the old implementation is not bad and is
        # faster than calculating the size and number of generated tables
      } else {
        v1 <- NULL
        v2 <- NULL
        v3 <- NULL
        v4 <- NULL
        maximize_nb_tabs <- if (nb_tab_option == 'max') TRUE else FALSE
      }
    }

    if (verbose) {
      cat("
Reducing from 5 to 4...\n")
    }

    res <- from_5_to_3(dfs = dfs,
                       dfs_name = dfs_name,
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
    if (length(vars_to_merge) == 2) {
      v1 <- vars_to_merge[[1]]
      v2 <- vars_to_merge[[2]]

    } else {
      # If the user did not specify the variables to merge, we need to calculate them

      if (nb_tab_option == 'smart') {

        if (verbose) {
          cat("Choosing variables...\n")
        }


        choice_2_var <- var_to_merge(dfs = dfs,
                                     totcode = totcode,
                                     hrcfiles = hrcfiles,
                                     nb_var = 2,
                                     limit = limit,
                                     nb_tab_option = nb_tab_option)
        v1 <- choice_2_var$vars[[1]]
        v2 <- choice_2_var$vars[[2]]

        if (choice_2_var$max_row > limit){
          cat(c("Warning when choosing variables:
The limit of ",limit," cannot be achieved.
The largest table has ",choice_2_var$max_row," rows.\n"))
        }

        # Return to the primitive implementation to minimize or maximize
        # the number of tables since the old implementation is not bad and is
        # faster than calculating the size and number of generated tables
      } else {
        v1 <- NULL
        v2 <- NULL
        maximize_nb_tabs <- if (nb_tab_option == 'max') TRUE else FALSE
      }
    }

    if (verbose) {
      cat("
Reducing from 4 to 3...\n")
    }

    res <- from_4_to_3(dfs = dfs,
                       dfs_name = dfs_name,
                       totcode = totcode,
                       hrcfiles = hrcfiles,
                       sep_dir = sep_dir,
                       hrc_dir = hrc_dir,
                       v1 = v1, v2 = v2,
                       sep = sep,
                       maximize_nb_tabs = maximize_nb_tabs)
  }

  if (verbose) {
    cat(paste(dfs_name,"has generated",length(res$tabs),"tables in total\n\n"))
  }

  # Put a format usable by rtauargus
  res <- sp_format(res = res,
                   dfs_name = dfs_name,
                   sep = sep,
                   totcode = totcode,
                   hrcfiles = hrcfiles)

  # Split too big table
  if (over_split) {

    if (verbose) {
      cat("Spliting...\n")
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
        cat(paste("",var_fus,"\n"))
      }

      res <- split_tab(res = res,
                       limit = limit,
                       var_fus = var_fus)
    }

    if (verbose) {
      cat(paste(dfs_name,"has generated",length(res$tabs),"tables in total\n\n"))
    }

    # The user specified a limit (smart or over_split case)
    if (!is.null(limit)){
      max_row <- max(sapply(res$tabs, nrow))

      if (max_row > limit){
        cat(c("Warning after splitting :
The limit of ",limit," cannot be achieved.
The largest table has ",max_row," rows.\n\n"))
      }
    }
  }

  return(res)
}

# split tables according to var_fuse if the nb of row exceed limit
# it creates smaller tabs with a hier variable less
#' @importFrom stats setNames
split_tab <- function(res, var_fus, limit) {
  # table to split because they are too big

  res$to_split <- sapply(res$tabs, function(x) nrow(x) > limit)
  table_to_split <-names(res$to_split[res$to_split == TRUE])

  # data to stock

  all_tot_stock <- list()
  tabs2 <- list()
  list_vars <- list()
  list_alt_hrcs <- list()

  # loop for table to treat

  for (t in table_to_split) {

    # Create of how to split

    hrc <- res$alt_hrc[[t]][[var_fus]]
    total <- res$alt_totcode[[t]][[var_fus]]
    other_total <-res$alt_totcode[[t]][names(res$alt_totcode[[t]]) != (var_fus)]

    res_sdc <-sdcHierarchies::hier_import(inp = hrc, from = "hrc",root = total) %>%
      sdcHierarchies::hier_convert(as = "sdc")

    codes_split <- lapply(res_sdc$dims,names)
    n <- length(codes_split)

    # Names use for tauargus
    new_names <- lapply(1:n, function(i) paste(t, i, sep = "_"))

    # Create tabs by filtering
    tabs <- lapply(codes_split,
                   function(codes) {
                     res <- res$tabs[[t]] %>%
                       filter(res$tabs[[t]][[var_fus]] %in% codes)
                   })

    names(tabs) <- new_names
    tabs2 <- append(tabs2, tabs)

    # alt_totcode for tauargus

    liste_alt_tot <- setNames(lapply(1:n, function(i) {
      totali <- c(codes_split[[i]][1])
      totali <- setNames(list(totali), var_fus)
      totali <- c(totali, other_total)
      return(totali) }), new_names)
    all_tot_stock <- append(all_tot_stock, liste_alt_tot)

    # list of variables for the created tables

    var <- replicate(n, list(res$vars[[1]]))
    list_add <- replicate(n, list(res$vars[[1]]))
    names(list_add) <- new_names
    list_vars <- append(list_vars, list_add)

    # remove hierarchies from the variable we split and naming it

    res$alt_hrc[[t]][[var_fus]] <- NULL

    if (length(res$alt_hrc[[t]]) != 0) {

      hrc_e <- list(res$alt_hrc[[t]])
      names(hrc_e) <- names(res$alt_hrc[[t]])

      alt_hrcs <- replicate(n, hrc_e)
      names(alt_hrcs) <- new_names

      list_alt_hrcs <- append(list_alt_hrcs, alt_hrcs)
    }
  }

  # adding the names tables we created to the already existing tables

  table <- names(res$tabs[!(names(res$tabs) %in% table_to_split)])
  tabs_tot <- append(res$tabs[table], tabs2)
  alt_totcode <- append(res$alt_totcode[table],all_tot_stock)
  vars <- append(res$vars[table], list_vars)
  hrcs <- append( res$alt_hrc[table],list_alt_hrcs)
  if (length(hrcs) == 0) { hrcs <- NULL }


  res = list(
    tabs = tabs_tot,
    vars = vars,
    sep = res$sep,
    alt_hrc = hrcs,
    totcode = res$totcode,
    alt_totcode = alt_totcode,
    hrc = res$hrc,
    fus_vars = res$fus_vars
  )
  return(res)
}

chose_sep <- function(
    data,
    liste_sep)
{

  liste_var <- names(data)
  liste_mod <- unique(unlist(lapply(data, unique)))
  liste_mod <- c(liste_mod, liste_var)
  n_sep <- length(liste_sep)

  i = 0
  is_in_mod = TRUE
  while (i <= n_sep & is_in_mod) {
    i <- i + 1
    sep <- liste_sep[i]
    is_in_mod = sum(unlist(lapply(liste_mod, function(x) stringr::str_detect(x, sep)))) > 0
  }

  # We have a working separator!
  if (i <= n_sep) {
    # Remove the "\" in front of the separator
    #sep <- stringr::str_sub(liste_sep[i], start = 2)
    sep <- liste_sep[i]

    # Return the concatenated separator thrice
    return(paste0(sep,
                  collapse = ""))
  } else {
    # Return a default separator (four underscores)
    return(paste(rep("_AZERTY_", 2),
                 collapse = ""))
  }
}

#' Change the result of dimension reduction to be directly usable
#' in rtauargus
#'
#' @param res result of variable merging composed of name_non_changed_vars, a list of lists of tables,
#' a list of hierarchical files, a list of subtotals associated with these files,
#' and a list of vectors of variables or a vector of variables depending on the base size
#' of the dataframes
#' @param dfs_name the name of the entered dataframes
#' @param sep character
#' @param totcode character named vector
#' @param hrcfiles character named vector
#'
#' @return A list containing:
#' \itemize{
#'   \item `tabs`: named list of 3-dimensional dataframes
#'   with nested hierarchies
#'   \item `alt_hrc`: named list of hrc specific to the variables
#'   created during merging to go to dimension 3
#'   \item `alt_totcode`: named list of totals specific to the variables
#'   created during merging to go to dimension 3
#'   \item `vars`: categorical variables of the output dataframes
#'   \item `sep`: separator used to link the variables
#'   \item `totcode`: named vector of totals for all categorical variables
#'   \item `hrcfiles`: named vector of hrc for categorical variables
#'   (except the merged one)
#'   \item `fus_vars`: named vector of vectors representing the merged
#'   variables during dimension reduction
#' }
#' @importFrom stats setNames
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
#' res <- sp_format(res1,
#'         dfs_name = "tab",
#'         sep = "_",
#'         totcode = c(SEX="Total",AGE="Total",
#'                    GEO="Total", ACT="Total"),
#'        hrcfiles = c(ACT = hrc_act)
#'        )
#' @keywords internal
#' @export
sp_format <- function(
    res,
    dfs_name,
    sep,
    totcode,
    hrcfiles)
{
  if (is.character(res$vars[1])) {
    return(format4(res, dfs_name, sep, totcode, hrcfiles))
  }
  if (is.list(res$vars)) {
    return(format5(res, dfs_name, sep, totcode, hrcfiles))
  }
}

# Format for tables with 4 variables
format4 <- function(res, dfs_name, sep, totcode, hrcfiles) {
  # Data

  v1 <- res$vars[1]
  v2 <- res$vars[2]
  tabs <- res$tabs
  n <- length(tabs)
  var_cross <- paste(v1, v2, sep = sep)

  if (v1 %in% names(totcode)) {
    tot1 <- totcode[[v1]]
  } else
    tot1 <- paste(res$fus_vars[1], res$fus_vars[2], sep = sep)
  if (v2 %in% names(totcode)) {
    tot2 <- totcode[[v2]]
  } else
    tot2 <- paste(res$fus_vars[1], res$fus_vars[2], sep = sep)

  tot_cross <- paste(tot1, tot2, sep = sep)

  name_non_changed_vars <- intersect(names(res$tabs[[1]]), names(totcode))
  old_totcode <- totcode[names(totcode) %in% name_non_changed_vars]
  names(tot_cross) <- var_cross
  totcode_2 <- c(old_totcode, tot_cross)

  v <- c(name_non_changed_vars, var_cross)
  list_vars <- replicate(n, v, simplify = FALSE)
  names(list_vars) <- c(paste0(dfs_name, 1:n, sep = ""))

  names(tabs) <- c(paste0(dfs_name, 1:n, sep = ""))


  # new_names of alt_hrc
  res2 <- setNames(
    lapply(
      seq_along(res$tabs),
      function(i) setNames(list(res$hrcs[[i]]), var_cross)
    ),
    paste(dfs_name, seq_along(res$tabs), sep = "")
  )

  # new_names of subtotals
  res3 <- setNames(
    lapply(
      seq_along(res$tabs),
      function(i) setNames(list(res$alt_tot[[i]]), var_cross)
    ),
    paste(dfs_name, seq_along(res$tabs), sep = "")
  )
  hrcfiles <- hrcfiles[(names(hrcfiles) %in% names(totcode_2))]
  if (length(hrcfiles) == 0) {hrcfiles <- NULL}

  return (
    list(
      tabs = tabs,
      alt_hrc = res2,
      alt_totcode = res3,
      vars = list_vars,
      sep = sep,
      totcode = totcode_2,
      hrc = hrcfiles,
      fus_vars = res$vars
    )
  )
}

# Format for tables with 5 variables
#' @importFrom stats setNames
format5 <- function(res, dfs_name, sep, totcode, hrcfiles) {
  if (is.list(res$vars)) {
    # Retrieve the different variables
    v1 <- res$vars[[2]][1]
    v2 <- res$vars[[2]][2]
    v3 <- res$vars[[1]][1]
    v4 <- res$vars[[1]][2]
    var_cross <- paste(v1, v2, sep = sep)
    var_cross2 <- paste(v3, v4, sep = sep)

    # Merging 3 variables into one
    # So the information related to two merged variables during 5->4
    # is no longer useful to us since the variable no longer exists in dimension 3
    if (var_cross2 %in% c(v1, v2)) {
      res2 <- list(
        tabs = res$tabs,
        hrcs = res$hrcs4_3,
        alt_tot = res$alt_tot4_3,
        vars = res$vars[[2]],
        sep = sep,
        fus_vars = c(v3, v4)
      )
      res2 <- sp_format(res2, dfs_name, sep, totcode, hrcfiles)

      # Keep the information of the merged variables at each step
      res2$fus_vars <- res$vars
      return(res2)
    }

    tot_cross <- paste(totcode[[v1]], totcode[[v2]], sep = sep)
    tot_cross2 <- paste(totcode[[v3]], totcode[[v4]], sep = sep)
    tabs <- res$tabs
    name_non_changed_vars <- intersect(names(res$tabs[[1]]), names(totcode))
    old_totcode <- totcode[names(totcode) %in% name_non_changed_vars]

    names(tot_cross) <- var_cross
    names(tot_cross2) <- var_cross2
    totcode_2 <- c(old_totcode, tot_cross, tot_cross2)

    n <- length(res$tabs)
    v <- c(name_non_changed_vars, var_cross, var_cross2)
    list_vars <- replicate(n, v, simplify = FALSE)
    names(list_vars) <- c(paste0(dfs_name, 1:n, sep = ""))
    names(tabs) <- c(paste0(dfs_name, 1:n, sep = ""))

    # new_names of alt_hrc

    res2 <- setNames(lapply(seq_along(res$tabs), function(i) {
      list1 <- setNames(list(res$hrcs4_3[[i]]), var_cross)
      list2 <- setNames(list(res$hrcs5_4[[i]]), var_cross2)
      c(list1, list2)
    }),
    paste(dfs_name, seq_along(res$tabs), sep = ""))

    # new_names of subtotals

    res3 <- setNames(lapply(seq_along(res$tabs), function(i) {
      list1 <- setNames(list(res$alt_tot4_3[[i]]), var_cross)
      list2 <- setNames(list(res$alt_tot5_4[[i]]), var_cross2)
      c(list1, list2)
    }),
    paste(dfs_name, seq_along(res$tabs), sep = ""))

  }
  hrcfiles <- hrcfiles[(names(hrcfiles) %in% names(totcode_2))]
  if (length(hrcfiles) == 0) {hrcfiles <- NULL}
  return (
    list(
      tabs = tabs,
      alt_hrc = res2,
      alt_totcode = res3,
      vars = list_vars,
      sep = sep,
      totcode = totcode_2,
      hrc = hrcfiles,
      fus_vars = res$vars
    )
  )
}
