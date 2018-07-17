param_function <- function(f, list_param) {
  # renvoie les noms des paramètres acceptés par f présents dans list_param
  f_param_names <-
    intersect(
      names(formals(f)),
      names(list_param)
    )
  list_param[f_param_names]
}


#' @export

rtauargus <- function(...) {

  # obligatoire :
  ## microdata
  ## explanatory_vars
  ## safety_rules
  ## suppress
  ## ... autres parametres à passer aux fonctions du package (cf details)

  ## 0. VERIFS .............................

  # verif poids
  # if (!is.null(weighted)) {
  #   if (is.null(weight)) stop("Définir une variable de pondération")
  # }

  ## 1. MICRO_ASC_RDA  .....................

  # parametres
  param_asc_rda <- param_function(micro_asc_rda, list(...))
  param_asc_rda$microdata <- microdata

  # appel (+ récuperation noms asc et rda)
  input <- do.call(micro_asc_rda, param_asc_rda)

  ## 2. MICRO_ARB .........................

  # parametres
  param_arb <- param_function(micro_arb, list(...))
  param_arb$asc_filename <- input$asc_filename
  param_arb$rda_filename <- input$rda_filename
  param_arb$explanatory_vars <- explanatory_vars
  param_arb$safety_rules <- safety_rules
  param_arb$suppress <- suppress

  # appel (+ récupération nom batch)
  batch <- do.call(micro_arb, param_arb)

  ## 3. RUN_TAUARGUS ......................

  # parametres
  param_run0 <- param_function(run_tauargus, list(...))
  param_system <- param_function(system, list(...))
  param_run <- c(param_run0, param_system)
  param_run$arb_filename <- batch$arb_filename

  # appel
  res <- do.call(run_tauargus, param_run)

  # RESULTAT .............................

  res

}
