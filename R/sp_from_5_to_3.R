#' Function reducing from 5 to 3 categorical variables
#'
#' @param dfs data.frame with 5 categorical variables
#' @param dfs_name name of the data.frame in the list provided by the user
#' @param totcode named vector of totals for categorical variables
#' @param hrcfiles named vector of hrc file paths
#' @param sep_dir logical, if TRUE forces writing hrc files into hrc_dir
#' @param hrc_dir folder where to write hrc files
#' @param v1,v2 optional variables to merge when reducing from 5 to 4 dimensions
#' @param v3,v4 optional variables to merge when reducing from 4 to 3 dimensions
#' @param sep separator used during concatenation of variables
#' @param maximize_nb_tabs logical, whether to prefer selecting hierarchical variables
#' @param verbose logical, print progress steps
#'
#' @return A list containing `tabs`, `hrcs5_4`, `hrcs4_3`, `alt_tot5_4`, `alt_tot4_3`, and `vars`.
#'
#' @keywords internal
#' @noRd
from_5_to_3 <- function(
    dfs,
    dfs_name,
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

  # ----------------------------------------------------------------------------
  # STRATEGY (5D -> 3D Two-Step Transition):
  # Step 1: Reduce 5D -> 4D using 'from_4_to_3()' on pair (v1, v2).
  #         This generates intermediate 4D tables and a new merged variable 'new_var'.
  # Step 2: Select pair (v3, v4) for the 4D -> 3D step, ensuring a uniform choice
  #         across all generated 4D sub-tables.
  # Step 3: Call 'from_4_to_3()' on each 4D sub-table to reach 3D tables.
  # Step 4: Replicate 5D->4D metadata (HRCs and totals) to match final 3D tables,
  #         enabling 'restore_format()' to reconstruct the full 5D dataset.
  # ----------------------------------------------------------------------------

  # Update the output folder containing the hierarchies
  if( (length(hrcfiles) != 0) & !sep_dir){
    dir_name <- dirname(hrcfiles[[1]])
  } else {
    dir_name <- hrc_dir
  }

  # We remove a dimension from our starting dataframe
  res_5_4 <- from_4_to_3(dfs = dfs,
                                 dfs_name = dfs_name,
                                 totcode = totcode,
                                 hrcfiles = hrcfiles,
                                 sep_dir = TRUE,
                                 hrc_dir = dir_name,
                                 v1 = v1,
                                 v2 = v2,
                                 sep = sep,
                                 maximize_nb_tabs = maximize_nb_tabs)
  if (verbose){
    cat(paste(dfs_name,"has generated",length(res_5_4$tabs),"tables in total\n"))
    cat("Reducing from 4 to 3...\n")
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
  cat_vars <- c(names(totcode2),new_var)

  non_hier_vars <- intersect(
    setdiff(names(dfs), names(hrcfiles2)),
    cat_vars
  )

  # Choice of variables for the 4 -> 3 transition and verification of those provided in argument
  # We now choose v3 and v4 to be sure that the same variable
  # is created within all the sub-tables

  # First variable for the 4 to 3 transition
  if (!is.null(v3)){
    if (!(v3 %in% cat_vars)){
      stop(paste("v3 is not a categorical variable, v3 = ", v3,
                 "The categorical variables are: ",paste(cat_vars, collapse = ", ")), sep = "")
    }
  } else {
    # we choose a variable avoiding v4
    v3 <- chose_var_to_merge(dfs = dfs[setdiff(names(dfs),v4)],
                      totcode = totcode2[setdiff(names(totcode2),v4)],
                      hrcfiles = hrcfiles2[setdiff(names(hrcfiles2),v4)],
                      maximize_nb_tabs = maximize_nb_tabs)

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
    if (!(v4 %in% cat_vars)){
      stop(paste("v4 is not a categorical variable, v4 = ", v4,
                 "The categorical variables are: ",paste(cat_vars, collapse = ", ")), sep = "")
    }
    if (v3 == v4){
      stop("Error. You are trying to merge a variable with itself")
    }

  } else {
    # we choose a variable avoiding v3
    v4 <- chose_var_to_merge(dfs = dfs[setdiff(names(dfs),v3)],
                      totcode = totcode2[setdiff(names(totcode2),v3)],
                      hrcfiles = hrcfiles2[setdiff(names(hrcfiles2),v3)],
                      maximize_nb_tabs = maximize_nb_tabs)

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

    from_4_to_3(dfs = dfsb,
                        dfs_name = nom_dfsb,
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

    # Calculate the value of nb_nodes once for each res_5_4$hrcs[[x]]
    # to avoid calculating the same quantity twice
    results <- lapply(seq_along(res_5_4$hrcs), function(x) {
      nb_node_value <- 2 * nb_nodes(res_5_4$hrcs[[x]], hrc_name = FALSE,
                                    total = res_5_4$alt_tot[[x]]) *
        nb_nodes(hrcfiles2, non_fused_var, totcode = totcode2)
      list(
        hrcs = rep(res_5_4$hrcs[[x]], nb_node_value),
        alt_tot = rep(res_5_4$alt_tot[[x]], nb_node_value)
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
