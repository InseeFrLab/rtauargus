#' Automatically computes an intermediate size limit for table reduction
#'
#' @description
#' Uses `explore_reduce_dims()` to find the median configuration in terms of
#' generated tables, and returns the minimum `max_size` among the configurations
#' having that number of tables.
#'
#' @details
#' The current implementation uses a default heuristic designed to strike a simple
#' balance between the number of generated tables and their maximum size.
#' This selection logic is subject to future refinements
#'
#' @inheritParams explore_reduce_dims
#'
#' @return An integer representing the computed row limit.
#'
#' @keywords internal
#' @noRd
auto_limit <- function(dfs, totcode, hrcfiles = NULL) {

  # 1. Explore candidate configurations for table reduction
  exp_df <- explore_reduce_dims(dfs = dfs, totcode = totcode, hrcfiles = hrcfiles)

  if (nrow(exp_df) == 0) {
    stop("`explore_reduce_dims` returned an empty data frame.")
  }

  # 2. Pick the median configuration index in terms of generated table count
  mid_idx <- ceiling(nrow(exp_df) / 2)
  target_nb_tab <- exp_df$nb_tab[mid_idx]

  # 3. Filter candidates matching this table count and select the minimum max_size
  sub_df <- exp_df[exp_df$nb_tab == target_nb_tab, ]
  calculated_limit <- min(sub_df$max_size)

  return(as.integer(calculated_limit))
}

#' Call Tau-Argus to protect a 4 or 5 dimensions table by splitting it
#' in several 3 dimensions table.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @inheritParams tab_rtauargus
#'
#' @param limit numeric or NULL, default `NULL`. Used to choose which variable
#'   to merge (if nb_tab_option = 'smart') and split table with a number of row
#'   above this limit in order to avoid tauargus failures.
#'   If `NULL`, an automatic limit is calculated using \code{auto_limit}.
#' @param nb_tab_option `r lifecycle::badge("superseded")` character, default `"smart"`.
#'   Strategy used for dimension reduction. **Note:** Only `"smart"` is recommended.
#'   Historical options (`"min"` and `"max"`) are legacy heuristics kept for backward
#'   compatibility:
#'   \itemize{
#'     \item `'smart'` *(default & recommended)*: minimizes the number of tables under
#'     the constraint of their row count;
#'     \item `'min'` *(legacy)*: minimizes the number of tables;
#'     \item `'max'` *(legacy)*: maximizes the number of tables.
#'   }
#' @param dfs_name name used to write hrc files when reducing dims
#' @param sort_table character or NULL, default `NULL`.
#'   *(Exploratory / Experimental)* Used to assess the impact of table ordering on
#'   secondary secret selection when cells are shared across sub-tables.
#'   If `"ASC"`, sub-tables are processed in ascending order of their total cell value
#'   (smallest first); if `"DESC"`, in descending order.
#'   `NULL` preserves the default order produced by `reduce_dims()`.
#'   *Note: this parameter is experimental and may be modified or removed in future versions.*
#' @param ... additional parameters
#'
#' @return The original tabular is returned with additional variables indicating
#' whether or not the cell has to be masked according to Tau-Argus
#'
#' @examples
#'\dontrun{
#' #Please don't forget to specify the localisation of Tau-Argus in your computer
#' options(
#'   rtauargus.tauargus_exe =
#'     "Y:/Logiciels/TauArgus/TauArgus4.2.3/TauArgus.exe"
#' )
#'
#' data(datatest1)
#' expl_vars <- c("A10", "treff","type_distrib","cj")
#'
#' res_dim4 <- tab_rtauargus4(
#'   tabular = datatest1,
#'   files_name = "datatest1",
#'   dir_name = "tauargus_files",
#'   explanatory_vars = expl_vars,
#'   totcode = setNames(rep("Total", 4), expl_vars),
#'   secret_var = "is_secret_prim",
#'   value = "pizzas_tot_abs",
#'   freq = "nb_obs_rnd",
#'   verbose = TRUE,
#'   nb_tab_option = "min",
#'   verbose = TRUE
#' )
#'
#' # With a data of 5 variables
#'
#' data(datatest2)
#' expl_vars <- c("A10", "treff","type_distrib","cj","nuts1")
#'
#' res_dim5 <- tab_rtauargus4(
#'   tabular = datatest2,
#'   files_name = "datatest2",
#'   dir_name = "tauargus_files",
#'   explanatory_vars = expl_vars,
#'   totcode = setNames(rep("Total", 5), expl_vars),
#'   secret_var = "is_secret_prim",
#'   value = "pizzas_tot_abs",
#'   freq = "nb_obs_rnd",
#'   verbose = TRUE,
#'   nb_tab_option = "min", # split into the minimum of tables.
#'   verbose = TRUE,
#'   suppress = "GH(1,100)" # We use hypercube to save time.
#' )
#' }
#' @importFrom stats setNames
#' @importFrom dplyr select filter mutate
#' @importFrom sdcHierarchies hier_import hier_convert
#' @export
tab_rtauargus4 <- function(
    tabular,
    explanatory_vars,
    dir_name,
    secret_var,
    totcode,
    files_name = NULL,
    hrc = NULL,
    secret_no_pl = NULL,
    cost_var = NULL,
    value = "value",
    freq = "freq",
    ip = 10,
    suppress = "MOD(1,5,1,0,0)",
    safety_rules = paste0("MAN(",ip,")"),
    nb_tab_option = "smart",
    limit = NULL,
    dfs_name = 'tab',
    sort_table = NULL,
    ...
){

  if (!is.null(sort_table) && !sort_table %in% c("ASC", "DESC")) {
    stop('`sort_table` must be NULL, "ASC" or "DESC".')
  }

  .dots = list(...)

  hrc_path <- file.path(dir_name, "hrc")
  if (!dir.exists(hrc_path)){
    dir.create(hrc_path, recursive = TRUE)
  }

  # TODO:
  # deleting created hrc files at the end of the function ?

  # Reduce dims for 4 or 5 dimensions table
  if (length(explanatory_vars) %in% c(4, 5)) {

    # Calcul automatique de la limite si limit = NULL
    if (is.null(limit) && nb_tab_option == "smart") {
      if (isTRUE(.dots[["verbose"]])) {
        cat("`limit` est NULL, calcul automatique en cours via `auto_limit()`...\n")
      }
      limit <- auto_limit(dfs = tabular, totcode = totcode, hrcfiles = hrc)

      if (isTRUE(.dots[["verbose"]])) {
        cat("Limite calcul\u00e9e automatiquement :", limit, "\n\n") # \u00e9 pour &
      }
    }

    cat("\nReducing dims...\n",dfs_name,"\n\n")

    list_tables <- reduce_dims(
      dfs = tabular,
      dfs_name = dfs_name,
      totcode = totcode,
      hrcfiles = hrc,
      hrc_dir = hrc_path,
      nb_tab_option = nb_tab_option,
      limit = limit,
      over_split = FALSE,
      verbose = TRUE, # to generalize later
      sep_dir = TRUE
    )

    # Tri des sous-tables par somme de la colonne value
    if (!is.null(sort_table)) {
      totals <- sapply(list_tables$tabs, function(t) sum(t[[value]], na.rm = TRUE))
      ord <- if (sort_table == "ASC") order(totals) else order(totals, decreasing = TRUE)

      list_tables$tabs        <- list_tables$tabs[ord]
      list_tables$vars        <- list_tables$vars[ord]
      list_tables$alt_hrc     <- list_tables$alt_hrc[ord]
      list_tables$alt_totcode <- list_tables$alt_totcode[ord]

      if (isTRUE(.dots[["verbose"]])){
        cat("Ordre de traitement des tables (sort_table =", sort_table, ") :\n")
        print(data.frame(table = names(list_tables$tabs), total_value = totals[ord]))
      }
    }

    params_multi <- formals(fun = "tab_multi_manager")
    params_multi <- params_multi[1:(length(params_multi)-1)]
    call <- sys.call(); call[[1]] <- as.name('list')
    new_params <- eval.parent(call)

    for(param in intersect(names(params_multi), names(new_params))){
      params_multi[[param]] <- new_params[[param]]
    }

    params_multi$list_tables = list_tables$tabs
    params_multi$list_explanatory_vars = list_tables$vars
    params_multi$hrc = list_tables$hrc
    params_multi$totcode = list_tables$totcode
    params_multi$alt_hrc = list_tables$alt_hrc
    params_multi$alt_totcode = list_tables$alt_totcode

    masq_list <- do.call("tab_multi_manager", params_multi)


    result <- restore_format(masq_list, list_tables)

    return(result)
  } else {
    stop("Do not use table with more than 5 dimensions.
         Split_tab = TRUE is not compatible with these large tables.")
  }
}

