################################################################################
# Le programme ci-dessous propose une fonction pour construire un tableau
# croisant plusieurs variables catégorielles avec l'ensemble des marges
# nécessaire. Ce code devra être adapté aux cas d'usages réels
################################################################################

tabul_group_dt <- function(
    df,
    group_var = NULL,
    pond_var = NULL,
    exten_var,
    resp_var,
    marge_label = "Ensemble"
){

  ponderation = nb_obs = nb_obs.tot = NULL # due to NSE notes in R CMD check

  data_dt <- as.data.table(df)
  data_dt$ponderation <- if(is.null(pond_var)) 1 else data_dt[[pond_var]]
  data_dt$nb_obs <- 1

  summary_spec <- function(x, ponderation) list(tot = sum(x*ponderation, na.rm=TRUE), max = max(x, na.rm=TRUE))

  res <- data_dt[
    ,
    as.list(unlist(lapply(.SD, function(x) summary_spec(x, ponderation)))),
    by = group_var,
    .SDcols = c(resp_var, "nb_obs")
  ]
  res[,nb_obs := nb_obs.tot][,`:=`(nb_obs.max = NULL, nb_obs.tot = NULL)]

  names(res) <- sapply(names(res), function(x) gsub("[.]","_",x))

  var_marges <- base::setdiff(exten_var, group_var)
  for(var_cat in var_marges){
    res[[var_cat]] <- marge_label
  }

  if(is.null(resp_var)){
    res <- res[, .SD, .SDcols = c(exten_var, "nb_obs")]
  }else{
    res <- res[, .SD, .SDcols = c(exten_var, "nb_obs", paste0(resp_var,"_tot"), paste0(resp_var,"_max"))]
  }

  return(res)
}

all_croisements <- function(vec){
  l <- do.call("c", lapply(seq_along(vec), function(i) combn(vec, i, FUN = list)))
  l[[length(l)+1]] <- ""
  return(l)
}

tabul_group_all_margins <- function(
    df,
    cat_var = NULL,
    pond_var = NULL,
    resp_var = NULL,
    marge_label = "Total"
){

  croisements <- all_croisements(cat_var)

  res <- rbindlist(
    lapply(
      croisements,
      function(groups){
        tabul_group_dt(
          df = df,
          group_var = if(groups[1] == "") NULL else groups,
          pond_var = pond_var,
          exten_var = cat_var,
          resp_var = resp_var,
          marge_label = marge_label
        )
      }),
    use.names=TRUE
  )

  if(is.null(resp_var)){
    res[, .SD, .SDcols = c(cat_var, "nb_obs")]
  }else{
    res[, .SD, .SDcols = c(cat_var, "nb_obs", paste0(resp_var,"_tot"), paste0(resp_var,"_max"))]
  }

  return(res)
}

#' tabulate grouped data with all margins, handling hierarchical variables
#'
#' @param df data.frame or data.table
#' @param cat_vars vector of categorical variables but not hierarchical
#' @param hrc_vars named list (name = VAR final name, value = VAR current names)
#' @param pond_var weight (NULL if no weight is used)
#' @param resp_var vector of response variables (NULL to only compute frequency table)
#' @param marge_label label of margins (applied to all cat and hrc variables)
#'
#' @return a tibble
#' @export
#'
#' @examples
#' library(data.table)
#'
#' data("indiv_dt")
#'
#' #Non hierarchical variables
#' res_all_dtp <- tabulate_micro_data(
#'   df = indiv_dt,
#'   #categorical but not hierarchical variables
#'   cat_vars = c("A10", "SIZE","CJ"),
#'   #weight var
#'   pond_var = "WEIGHT",
#'   #response variable
#'   resp_var = "TURNOVER",
#'   # Labels of the margins
#'   marge_label = "Total"
#' )
#' str(res_all_dtp)
#'
#' #With one hierarchical variable
#' res_all_dtph <- tabulate_micro_data(
#'   df = indiv_dt,
#'   #categorical but not hierarchical variables
#'   cat_vars = c("SIZE","CJ"),
#'   #categorical nested variables
#'   hrc_vars = list(ACTIVITY = c("A10","A21")),
#'   pond_var = "WEIGHT",
#'   resp_var = c("TURNOVER","PRODUCTION"),
#'   marge_label = "Total"
#' )
#' str(res_all_dtph)
#'
#' @rawNamespace import(data.table, except = transpose)
tabulate_micro_data <- function(
    df,
    cat_vars = NULL,
    hrc_vars = NULL,
    pond_var = NULL,
    resp_var = NULL,
    marge_label = "Total"
){
  setDT(df)

  if(is.null(hrc_vars)){
    result <- tabul_group_all_margins(
      df = df,
      cat_var = cat_vars,
      pond_var,
      resp_var,
      marge_label
    )
  }else{

    hrc_vars_l <- do.call(
      "expand.grid",
      args = list(hrc_vars, stringsAsFactors = FALSE)
    ) %>%
      as.list() %>%
      purrr::transpose() %>%
      purrr::map(unlist)

    result_l <- purrr::map(
      hrc_vars_l,
      function(hrc){
        res <- tabul_group_all_margins(
          df = df,
          cat_var = unname(c(hrc, cat_vars)),
          pond_var,
          resp_var,
          marge_label
        )
        names(res)[names(res) %in% hrc] <- names(hrc)
        return(res[,.SD, .SDcols = ])
      }
    )
    result <- rbindlist(result_l, use.names = TRUE)
  }

  return(unique(result))
}

