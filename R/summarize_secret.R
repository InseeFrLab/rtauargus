#' Provide the summary of the suppression pattern from a rtauargus result
#'
#' @param res_tau either the data.frame resulting from tab_rtauargus run or
#' the list of data.frame resulting from tab_multimanager run
#' @param var the quantitative variable name to use for values stats of suppression
#' (default to `NULL` that is the stats are only computed depending on the number of cells )
#' @param secret_var the name of the variable indicating the primary suppressed cells
#' @returns data.frame or list of data.frames
#' @export
#'
#' @examples
#'\dontrun{
#' library(dplyr)
#' data(turnover_act_size)
#'
#' # Prepare data with primary secret ----
#' turnover_act_size <- turnover_act_size %>%
#'   mutate(
#'     is_secret_freq = N_OBS > 0 & N_OBS < 3,
#'     is_secret_dom = ifelse(MAX == 0, FALSE, MAX/TOT>0.85),
#'     is_secret_prim = is_secret_freq | is_secret_dom
#'   )
#'
#' # Make hrc file of business sectors ----
#' data(activity_corr_table)
#' hrc_file_activity <- activity_corr_table %>%
#'   write_hrc2(file_name = "hrc/activity")
#'
#' # Compute the secondary secret ----
#' options(
#'   rtauargus.tauargus_exe =
#'     "Y:/Logiciels/TauArgus/TauArgus4.2.3/TauArgus.exe"
#' )
#'
#' res <- tab_rtauargus(
#'   tabular = turnover_act_size,
#'   files_name = "turn_act_size",
#'   dir_name = "tauargus_files",
#'   explanatory_vars = c("ACTIVITY", "SIZE"),
#'   hrc = c(ACTIVITY = hrc_file_activity),
#'   totcode = c(ACTIVITY = "Total", SIZE = "Total"),
#'   secret_var = "is_secret_prim",
#'   value = "TOT",
#'   freq = "N_OBS",
#'   verbose = FALSE
#' )
#'
#' summarize_secret(res, "TOT")
#' summarize_secret(res)
#' }
#' @importFrom dplyr case_when
#' @importFrom dplyr last_col
#' @importFrom dplyr tibble
#' @importFrom dplyr rename_with
#' @importFrom dplyr n
#' @importFrom purrr list_c
summarize_secret <- function(res_tau, var = NULL, secret_var = "is_secret_prim"){

  if( is.data.frame(res_tau) ) {

    if( !is.null(var)  &&  ! var %in% names(res_tau) ){

      stop("The variable has to be present in the data.frame")

    }

    if( ! secret_var %in% names(res_tau) ){

      stop("The primary secret variable has to be present in the data.frame")

    }

    tab_mod <- res_tau %>%
      {if( ! is.null(var) ) dplyr::rename_with(., ~"VALUE", all_of(var)) else .} |>
      dplyr::rename_with(~"final_status_ta", last_col()) |>
      dplyr::rename_with(~"is_secret_prim", all_of(secret_var)) |>
      dplyr::mutate(
        status = case_when(
          is_secret_prim  ~ "primary suppr.",
          final_status_ta != "V" ~ "secondary suppr.",
          TRUE ~ "published"
        )) |>
      dplyr::mutate(status = factor(
        status,
        levels = c("primary suppr.", "secondary suppr.", "published", "total"),
        ordered = TRUE)
      )

    stats <- tab_mod |>
      group_by(status) %>%
      {if( ! is.null(var) ) dplyr::summarise(., nb_cells = n(), value = sum(VALUE) ) else dplyr::summarise(., nb_cells = n()) } |>
      dplyr::bind_rows(
        dplyr::tibble(
          status = "total",
          tab_mod %>% {if( ! is.null(var) ) dplyr::summarise(., nb_cells = n(), value = sum(VALUE)) else dplyr::summarise(., nb_cells = n()) }
        )
      ) |>
      mutate(pourc_cells = round( nb_cells/nb_cells[status == "total"]*100, 2 ) ) %>%
      {if( ! is.null(var) ) dplyr::mutate(., pourc_value = round( value/value[status == "total"]*100, 2 ) ) else . }

    return(stats)

  }else if( any(! purrr::map(res_tau, is.data.frame) |> purrr::list_c()) ){

    stop("res_tau has to be a data.frame or a list of data.frames")

  }else{

    if( !( is.null(var) ) & any(! purrr::map(res_tau, \(t) var %in% names(t)) |> purrr::list_c()) ){

      stop("The variable has to be present in each of the dataframes")

    }

    if( any(! purrr::map(res_tau, \(t) secret_var %in% names(t)) |> purrr::list_c()) ){

      stop("The primary secret variable has to be present in the data.frame")

    }

    return( purrr::map(res_tau, summarize_secret, var = var) )

  }

}
