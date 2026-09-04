# Utility function to clear hrc cache
clear_hrc_cache <- function() {
  rm(list = ls(envir = .hrc_cache, all.names = TRUE), envir = .hrc_cache)
}

#' General function that selects the appropriate separator and applies dimension reduction
#'
#' @param dfs data.frame with 4 or 5 categorical variables
#' @param dfs_name name of the data.frame
#' @param totcode named vector of totals for categorical variables
#' @param hrcfiles named vector of hrc file paths
#' @param sep_dir logical, if TRUE forces writing hrc into hrc_dir
#' @param hrc_dir folder to write hrc files
#' @param vars_to_merge NULL or vector of variables to be merged
#' @param nb_tab_option strategy: 'min', 'max', or 'smart'
#' @param limit maximum allowed number of rows
#' @param over_split logical, whether to split tables exceeding limit
#' @param vec_sep vector of candidate separators
#' @param verbose logical, print progress steps
#'
#' @return A list formatted for tab_multi_manager compatibility.
#'
#' @keywords internal
#' @noRd
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

  # ----------------------------------------------------------------------------
  # MAIN ENTRY POINT: Dimension Reduction Pipeline
  # 1. Validate inputs and dimensions (4D or 5D).
  # 2. Clear hierarchy cache and select a collision-free separator.
  # 3. Determine merge variables (enforced by user or computed via 'var_to_merge').
  # 4. Delegate dimension reduction to 'from_4_to_3' or 'from_5_to_3'.
  # 5. Format metadata via 'sp_format()' for compatibility with 'tab_multi_manager'.
  # 6. Fallback safety: Split oversized tables if 'over_split = TRUE' and max_row > limit.
  # ----------------------------------------------------------------------------

  # Rq for later: using data.table may speed up the process in from_4_to_3_case_*
  # for merging / filtering etc ?

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

  # Check if over_split is a logical value
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

  # ============================================================================
  # Safety initialization to prevent the latent undefined variable bug
  # in 'smart' modes or when merge variables are enforced.
  # ============================================================================
  maximize_nb_tabs <- FALSE

  # clear the hierarchy cache at the very beginning of the dimension reduction process
  clear_hrc_cache()

  # Choose the separator
  data_var_cat <- dfs[names(dfs) %in% names(totcode)]
  sep <- chose_sep(data_var_cat, vec_sep)

  if (length(totcode) == 5) {
    # If the user specified the variables to merge
    if (length(vars_to_merge) == 3) {
      v1 <- vars_to_merge[[1]]
      v2 <- vars_to_merge[[2]]
      v3 <- vars_to_merge[[3]]

      # Predict the actual merged variable name, accounting for possible swap
      # in from_4_to_3 (non-hierarchical variable always comes first).
      non_hier_vars <- setdiff(names(totcode), names(hrcfiles))
      if (v1 %in% names(hrcfiles) && v2 %in% non_hier_vars) {
        # v2 is non-hierarchical, v1 is hierarchical => swap
        v4 <- paste(v2, v1, sep = sep)
      } else {
        v4 <- paste(v1, v2, sep = sep)
      }

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

          # Correct the swap prediction (non-hierarchical must come first)
          non_hier_vars <- setdiff(names(totcode), names(hrcfiles))
          if (v1 %in% names(hrcfiles) && v2 %in% non_hier_vars) {
            v4 <- paste(v2, v1, sep = sep)
          } else {
            v4 <- paste(v1, v2, sep = sep)
          }

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

          if (choice_4_var$max_row > limit){
            cat(c("Warning when choosing variables:
The limit of ",limit," cannot be achieved.
The largest table has ",choice_4_var$max_row," rows.\n"))
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

    table_sizes  <- sapply(res$tabs, nrow)
    cat("  Table size distribution :\n")
    print(summary(table_sizes))
    cat("\n\n")

  }

  # Put a format usable by rtauargus
  res <- sp_format(res = res,
                   dfs_name = dfs_name,
                   sep = sep,
                   totcode = totcode,
                   hrcfiles = hrcfiles)

  max_row <- max(sapply(res$tabs, nrow))

  # Split too big table if we didn't achieve the target value of "limit"
  if (over_split && !is.null(limit) && max_row > limit) {

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

      table_sizes  <- sapply(res$tabs, nrow)
      cat("  Table size distribution :\n")
      print(summary(table_sizes))
      cat("\n\n")
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

# Split oversized tables exceeding 'limit' by decomposing the merged hierarchical variable 'var_fus'
#' @importFrom stats setNames
split_tab <- function(res, var_fus, limit) {
  # todo: actuellement split_tab est plutôt lent
  # en effet, sur une réduction complète avec splitage d'environ 51s,
  # 50s sont dûes à split_tab à la fin de la réduction

  # cela est dûe à import_hierarchy qui fail sur le cache
  # car on utilise les sous totaux intermédiaire

  # il faudrait donc maj import_hierarchy pour ajouter cette fonctionnalité
  # Ce n'est pas la priorité cependant : split_tab n'est pas utilisé courament
  # il faut que over_split = TRUE (TRUE mpar défaut)
  # + que la limite soit très basse (très rare en pratique)
  # Et même dans ce cas, split_tab sera négligeable devant tab_multimanager
  # Ce n'est donc pas une priorité

  # 1. Calcul rapide des tailles de tableaux et détection précoce
  tab_sizes <- vapply(res$tabs, nrow, FUN.VALUE = integer(1L))
  to_split  <- tab_sizes > limit

  if (!any(to_split)) return(res)

  table_to_split <- names(res$tabs)[to_split]
  table_keep     <- names(res$tabs)[!to_split]
  n_to_split     <- length(table_to_split)

  # 2. Pré-allocation des listes
  tabs_split        <- vector("list", n_to_split)
  alt_totcode_split <- vector("list", n_to_split)
  vars_split        <- vector("list", n_to_split)
  hrcs_split        <- vector("list", n_to_split)

  # 3. Boucle sur les tableaux à découper
  for (i in seq_len(n_to_split)) {
    t     <- table_to_split[i]
    df_t  <- res$tabs[[t]]
    v_col <- df_t[[var_fus]]

    # Import spécifique à chaque table t (car total varie selon le sous-tableau)
    hrc         <- res$alt_hrc[[t]][[var_fus]]
    total       <- res$alt_totcode[[t]][[var_fus]]
    codes_split <- import_hierarchy(hrc, total)
    n           <- length(codes_split)

    new_names <- paste0(t, "_", seq_len(n))

    # OPTIMISATION MAJEURE : Filtrage C direct %in% (remplace split + unlist + sort.int)
    tabs <- lapply(codes_split, function(codes) {
      df_t[v_col %in% codes, , drop = FALSE]
    })
    names(tabs) <- new_names
    tabs_split[[i]] <- tabs

    # alt_totcode
    alt_tot_t   <- res$alt_totcode[[t]]
    other_total <- alt_tot_t[names(alt_tot_t) != var_fus]
    first_codes <- lapply(codes_split, `[[`, 1L)

    liste_alt_tot <- setNames(lapply(first_codes, function(code_1) {
      c(setNames(list(code_1), var_fus), other_total)
    }), new_names)
    alt_totcode_split[[i]] <- liste_alt_tot

    # vars
    var_t <- res$vars[[t]]
    if (is.null(var_t)) var_t <- res$vars[[1L]]
    vars_split[[i]] <- setNames(rep(list(var_t), n), new_names)

    # hrcs
    res$alt_hrc[[t]][[var_fus]] <- NULL
    if (length(res$alt_hrc[[t]]) != 0L) {
      hrc_e        <- list(res$alt_hrc[[t]])
      names(hrc_e) <- names(res$alt_hrc[[t]])
      hrcs_split[[i]] <- setNames(rep(list(hrc_e), n), new_names)
    }
  }

  # 4. Fusion des résultats
  tabs2         <- do.call(c, unname(tabs_split))
  all_tot_stock <- do.call(c, unname(alt_totcode_split))
  list_vars     <- do.call(c, unname(vars_split))

  has_hrcs      <- any(vapply(hrcs_split, function(x) !is.null(x), logical(1L)))
  list_alt_hrcs <- if (has_hrcs) do.call(c, unname(hrcs_split)) else list()

  tabs_tot    <- c(res$tabs[table_keep], tabs2)
  alt_totcode <- c(res$alt_totcode[table_keep], all_tot_stock)
  vars        <- c(res$vars[table_keep], list_vars)
  hrcs        <- c(res$alt_hrc[table_keep], list_alt_hrcs)
  if (length(hrcs) == 0L) hrcs <- NULL

  return(list(
    tabs        = tabs_tot,
    vars        = vars,
    sep         = res$sep,
    alt_hrc     = hrcs,
    totcode     = res$totcode,
    alt_totcode = alt_totcode,
    hrc         = res$hrc,
    fus_vars    = res$fus_vars
  ))
}

# Select an unused separator guaranteed not to collide with modalities or column names
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
  while (i < n_sep & is_in_mod) {
    i <- i + 1
    sep <- liste_sep[i]
    is_in_mod = any(stringr::str_detect(liste_mod, stringr::fixed(sep)))
  }

  # We have a working separator!
  if (!is_in_mod) {
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

#' Format dimension reduction outputs for tab_multi_manager compatibility
#'
#' @param res result from dimension reduction
#' @param dfs_name name of input dataframes
#' @param sep separator string
#' @param totcode named vector of totals
#' @param hrcfiles named vector of hrc file paths
#'
#' @return Formatted list compatible with tab_multi_manager.
#' @keywords internal
#' @noRd
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

# Format outputs for 4D -> 3D transition
#' @importFrom stats setNames
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

# Format outputs for 5D -> 3D transition
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
