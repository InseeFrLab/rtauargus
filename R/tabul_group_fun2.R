#' Compute margins for a contingency table
#'
#' @param inner_cells data.table. Inner cells of the table
#' @param margins Character vector. Margin variables
#' @param resp_var Character. Response variable to aggregate
#' @param marge_label Character. Label for margin cells
#'
#' @return data.table with computed margins
#' @keywords internal
#' @export
compute_margins <- function(
    inner_cells,
    margins,
    resp_var = NULL,
    marge_label
) {
  
  if(is.null(resp_var)){

    res <- data.table::cube(
      inner_cells,
      j = lapply(.SD, sum),
      by = c(margins),
      .SDcols = c("nb_obs"),
      label = marge_label
    )
  } else {

    res_sum <- data.table::cube(
      inner_cells,
      j = lapply(.SD, sum),
      by = c(margins),
      .SDcols = c("nb_obs", paste0(resp_var, "_tot")),
      label = marge_label
    )

    res_max <- suppressWarnings({
      data.table::cube(
        inner_cells[, .SD, .SDcols = c(margins, paste0(resp_var, "_max"))],
        j = lapply(.SD, \(x) max(x, na.rm = TRUE)),
        by = c(margins),
        .SDcols = c(paste0(resp_var, "_max")),
        label = marge_label
      )
      res <- merge(res_sum, res_max, by = margins)
    })
  }

  return(res)

}

#' Build contingency table with margins from microdata
#'
#' Constructs a complete contingency table with all possible margins from
#' microdata, including cell keys and optional numerical aggregation.
#'
#' @param df data.frame. Input microdata
#' @param cat_vars Character vector. Categorical variables
#' @param hrc_vars Named list. Hierarchical variables
#' @param resp_var Character vector. Numerical variable(s) to aggregate
#' @param pond_var Character. Numerical variable to use as weights
#' @param marge_label Character. Margin label (default: "Total")
#'
#' @return Tibble or list with table and frequencies
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' data("dtest")
#' tab_comptage <- tabulate_cnt_micro_data(
#'   df = dtest,
#'   cat_vars = c("DIPLOME", "SEXE", "AGE"),
#'   hrc_vars = list(GEO = c("REG", "DEP")),
#'   marge_label = "Total"
#' )
#'
#' # With numerical variable to aggregate
#' tab_comptage_num <- tabulate_cnt_micro_data(
#'   df = dtest |> mutate(NUM = 12),
#'   cat_vars = c("DIPLOME", "SEXE", "AGE"),
#'   hrc_vars = list(GEO = c("REG", "DEP")),
#'   resp_var = "NUM",
#'   marge_label = "Total"
#' )
#' }
tabulate_micro_data_2 <- function(
    df,
    cat_vars = NULL,
    hrc_vars = NULL,
    pond_var = NULL,
    resp_var = NULL,
    marge_label = "Total"
) {
  assertthat::assert_that(
    is.data.frame(df),
    msg = "The input data must be a data frame."
  )
  assertthat::assert_that(
    is.null(cat_vars) || all(cat_vars %in% names(df)),
    msg = "The specified categorical variables are missing from your data."
  )
  assertthat::assert_that(
    is.null(hrc_vars) || all(unlist(hrc_vars) %in% names(df)),
    msg = "The specified hierarchical variables are missing from your data."
  )
  assertthat::assert_that(
    is.null(resp_var) || all(resp_var %in% names(df)),
    msg = "The specified numerical variable is missing from your data."
  )
  assertthat::assert_that(
    is.null(pond_var) || pond_var %in% names(df),
    msg = "The specified weight variable is missing from your data."
  )
  assertthat::assert_that(
    is.character(marge_label) && length(marge_label) == 1,
    msg = "The margin label must be a single character string."
  )
  
  # If no categorical or hierarchical variables are provided, use all character columns
  if (is.null(cat_vars) & is.null(hrc_vars)) {
    all_cat_vars <- df |> dplyr::select(dplyr::where(is.character)) |> names()
  } else {
    all_cat_vars <- c(cat_vars, unlist(unname(hrc_vars)))
  }
  
  data_dt <- data.table::as.data.table(df) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(all_cat_vars), as.character))
  
  summary_spec <- function(x, ponderation) list(tot = sum(x*ponderation, na.rm=TRUE), max = max(x, na.rm=TRUE))

  
  if (is.null(resp_var)) {
    if (is.null(pond_var)) {
      inner_cells <- data_dt[, .(nb_obs = .N), by = c(all_cat_vars)]
    }else {
      inner_cells <- data_dt[, .(nb_obs = sum(get(pond_var))), by = c(all_cat_vars)]
    }
  } else {
    if (is.null(pond_var)) {
      inner_cells <- merge(
        data_dt[, .(nb_obs = .N), by = c(all_cat_vars)],
        data_dt[, (as.list(unlist(sapply(.SD, \(x) summary_spec(x, 1))))), by = c(all_cat_vars), .SDcols = c(resp_var)],
        by = all_cat_vars
      )
    } else {
      inner_cells <- merge(
        data_dt[, .(nb_obs = sum(get(pond_var))), by = c(all_cat_vars)],
        data_dt[, (as.list(unlist(sapply(.SD, \(x) summary_spec(x, get(pond_var)))))), by = c(all_cat_vars), .SDcols = c(resp_var)],
        by = all_cat_vars
      )
    }
    ##TODO: les resp_var à gérer (le sort ne peut pas fonctionner en général)
    names_resp_var <- map(resp_var, \(x) paste0(x, c("_tot","_max"))) |> list_c()
    data.table::setnames(inner_cells, old = paste0("V", 1:(length(resp_var)*2)), new = names_resp_var)
  }

  res <- compute_margins(
    inner_cells,
    margins = all_cat_vars,
    resp_var = resp_var,
    marge_label = marge_label
  )
  
  # Remove inconsistent margin rows for hierarchical variables
  for (hvar in hrc_vars) {
    for (j in seq_along(hvar)) {
      if (j > 1) {
        res <- res[!(get(hvar[j]) != marge_label & get(hvar[j - 1]) == marge_label), ]
      }
    }
  }
  
  res[is.na(res)] <- marge_label
  
  tab <- tibble::as_tibble(res)
    
  return(tab)
}