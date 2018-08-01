param_function <- function(f, list_param) {
  # renvoie les noms des paramètres acceptés par f présents dans list_param
  f_param_names <-
    intersect(
      names(formals(f)),
      names(list_param)
    )
  list_param[f_param_names]
}

#' Secrétise des tableaux à partir de microdonnées
#'
#' Secrétise des tableaux construits à partir de microdonnées et des
#' spécifications des croisements. La fonction permet d'effectuer le processus
#' complet, à savoir la création des fichiers asc et rda, la construction
#' du fichier arb, le lancement effectif de Tau-Argus et la récupération
#' éventuelle des résultats dans R.
#'
#' La fonction exécute séquentiellement les fonctions : \itemize{
#'   \item{
#'     \code{\link{micro_asc_rda}} \code{->}
#'     \code{\link{micro_arb}} \code{->}
#'     \code{\link{run_tauargus}}
#'   }
#' }
#'
#' Les fichiers intermédiaires sans nom renseigné (\code{asc_filename}...) seront
#' créés dans un dossier temporaire, avec des noms générés aléatoirement. Ce
#' mécanisme permet à l'utilisateur de s'abstraire de la préparation des données
#' propre à Tau-Argus et de maintenir l'intégralité de la chaîne de traitements
#' dans R.
#'
#' @inheritParams micro_asc_rda
#' @inheritParams micro_arb
#' @param ... paramètres optionnels pour \code{micro_asc_rda}, \code{micro_arb}
#'    et \code{run_tauargus}. Voir l'aide de ces fonctions.
#'
#' @return Si \code{import = TRUE}, une liste de data.frames (tableaux
#'   secrétisés), \code{NULL} sinon.
#'
#' @examples
#' rtauargus(
#'   microdata = data.frame(V1 = c("A", "A", "B", "C", "A"), V2 = "Z"),
#'   explanatory_vars = c("V1", "V2"),
#'   safety_rules = "FREQ(3,10)",
#'   suppress = "GH(.,100)",
#'   output_options = "AS+" # (exemple de parametre optionnel pour micro_arb)
#' )
#' @export

rtauargus <- function(microdata,
                      explanatory_vars,
                      safety_rules,
                      suppress,
                      ...) {

  .dots <- list(...)

  ## 0. VERIFS .............................

  # weighted_var necessaire si un weighted = TRUE
  any_weighted <- any(getOption("rtauargus.weighted"))
  if (!is.null(.dots[["weighted"]])) any_weighted <- any(.dots[["weighted"]])
  if (any_weighted & is.null(.dots[["weight_var"]])) {
    stop(
      "\nIncoherence parametres :\n",
      "Definir une variable de ponderation (weight_var)\n",
      "ou mettre a FALSE l'indicatrice de ponderation (weighted)"
    )
  }

  ## 1. MICRO_ASC_RDA  .....................

  # parametres
  param_asc_rda <- param_function(micro_asc_rda, .dots)
  param_asc_rda$microdata <- microdata

  # appel (+ récuperation noms asc et rda)
  input <- do.call(micro_asc_rda, param_asc_rda)

  ## 2. MICRO_ARB .........................

  # parametres
  param_arb <- param_function(micro_arb, .dots)
  param_arb$asc_filename <- input$asc_filename
  param_arb$rda_filename <- input$rda_filename
  param_arb$explanatory_vars <- explanatory_vars
  param_arb$safety_rules <- safety_rules
  param_arb$suppress <- suppress

  # appel (+ récupération nom batch)
  batch <- do.call(micro_arb, param_arb)

  ## 3. RUN_TAUARGUS ......................

  # parametres
  param_run0 <- param_function(run_tauargus, .dots)
  param_system <- param_function(system, .dots)
  param_run <- c(param_run0, param_system)
  param_run$arb_filename <- batch$arb_filename

  # appel
  res <- do.call(run_tauargus, param_run)

  # RESULTAT .............................

  res

}
