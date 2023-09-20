#' Call Tau-Argus to protect a 4 or 5 dimensions table by splitting it
#' in several 3 dimensions table.
#'
#' @inheritParams tab_rtauargus
#'
#' @param limit numeric, used to choose which variable to merge (if nb_tab_option = 'smart')
#' and split table with a number of row above this limit in order to avoid
#' tauargus failures
#' @param nb_tab_option strategy to follow for choosing variables automatically:
#' \itemize{
#'   \item \code{'min'}: minimize the number of tables;
#'   \item \code{'max'}: maximize the number of tables;
#'   \item \code{'smart'}: minimize the number of tables under the constraint
#'   of their row count.
#' }
#' @param dfs_name name used to write hrc files when reducing dims
#' @param ... additional parameters#'
#'
#' @return The original tabular is returned with additional variables indicating
#' whether or not the cell has to be masked according to Tau-Argus
#'
#' @examples
#'\dontrun{
#' library(rtauargus)
#' #Please don't forget to specify the localisation of Tau-Argus in your computer
#' options(
#'   rtauargus.tauargus_exe =
#'     "Y:/Logiciels/TauArgus/TauArgus4.2.3/TauArgus.exe"
#' )
#'
#' res_dim4 <- tab_rtauargus4(
#'   tabular = datatest1,
#'   files_name = "datatest1",
#'   dir_name = "tauargus_files",
#'   explanatory_vars = c("A10", "treff","type_distrib","cj"),
#'   totcode = c(A10 = "Total", treff = "Total",type_distrib = "Total",cj = "Total"),
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
#'   suppress = "GH(1,100)" # We use hyerpcube to save time.
#' )
#' }
#' @importFrom stats setNames
#' @export
tab_rtauargus4 <- function(
    tabular,
    files_name = NULL,
    dir_name,
    explanatory_vars,
    totcode = getOption("rtauargus.totcode"),
    hrc = NULL,
    secret_var,
    secret_no_pl = NULL,
    cost_var = NULL,
    value = "value",
    freq = "freq",
    ip = 10,
    suppress = "MOD(1,5,1,0,0)",
    safety_rules = paste0("MAN(",ip,")"),
    nb_tab_option = "smart",
    limit = 14700L,
    dfs_name = 'tab',
    ...
){

  .dots = list(...)

  hrc_path <- file.path(dir_name, "hrc")
  if (!dir.exists(hrc_path)){
    dir.create(hrc_path, recursive = TRUE)
  }

  # TODO:
  # deleting created hrc files at the end of the function ?

  # Reduce dims for 4 or 5 dimensions table
  if (length(explanatory_vars) %in% c(4, 5)) {

    cat("\nReducing dims...\n",dfs_name,"\n\n")

    list_tables <- reduce_dims(
      dfs = tabular,
      dfs_name = dfs_name,
      totcode = totcode,
      hrcfiles = hrc,
      hrc_dir = hrc_path,
      nb_tab_option = nb_tab_option,
      limit = limit,
      over_split = TRUE,
      verbose = TRUE, # to generalize later
      sep_dir = TRUE
    )

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
