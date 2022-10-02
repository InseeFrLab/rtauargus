#' Protects tables from microdata
#'
#' Protects tables built from microdata and specifications of the crossings.
#' The function allows to perform the complete process, namely the creation of
#' the asc and rda files, the construction of the arb file, the effective
#' launching of Tau-Argus and the eventual recovery of the results in R. \cr
#' (Secrétise des tableaux construits à partir de microdonnées et des
#' spécifications des croisements. La fonction permet d'effectuer le processus
#' complet, à savoir la création des fichiers asc et rda, la construction
#' du fichier arb, le lancement effectif de Tau-Argus et la récupération
#' éventuelle des résultats dans R.)
#'
#' The function executes sequentially the functions: \itemize{
#' \item{
#' \code{\link{micro_asc_rda}} \code{->}
#' \code{\link{micro_arb}} \code{->}
#' \code{\link{run_arb}}
#' }
#' }
#'
#' Intermediate files without a name entered (\code{asc_filename}...)
#' will be created in a temporary folder, with randomly generated names.
#' This mechanism allows the user to abstract from the preparation of
#' preparation of the data and to maintain the entire chain of
#' processing in R. \cr
#'
#' (La fonction exécute séquentiellement les fonctions : \itemize{
#'   \item{
#'     \code{\link{micro_asc_rda}} \code{->}
#'     \code{\link{micro_arb}} \code{->}
#'     \code{\link{run_arb}}
#'   }
#' }
#'
#' Les fichiers intermédiaires sans nom renseigné (\code{asc_filename}...)
#' seront créés dans un dossier temporaire, avec des noms générés aléatoirement.
#' Ce mécanisme permet à l'utilisateur de s'abstraire de la préparation des
#' données propre à Tau-Argus et de maintenir l'intégralité de la chaîne de
#' traitements dans R.)
#'
#' @inheritParams micro_arb
#' @param microdata [\strong{required}] data.frame containing the microdata
#' (or path to text files already present: see section
#' \emph{Microdata already as text files}). \cr
#' ([\strong{obligatoire}] data.frame contenant les microdonnées
#'   (ou chemin vers des fichiers texte déjà présents : voir section
#'   \emph{Microdata already as text files}).)
#' @param ... optional parameters for \code{micro_asc_rda}, \code{micro_arb}
#' and \code{run_arb}. See the help for these functions. \cr
#' (paramètres optionnels pour \code{micro_asc_rda}, \code{micro_arb}
#'    et \code{run_arb}. Voir l'aide de ces fonctions.)
#'
#' @inheritSection micro_arb Syntax
#'
#' @section Microdata already as text files:
#' To use existing asc and rda files already existing, it is possible to
#' provide, instead of the data.frame, a character vector indicating
#' the path of these files. The first element of this vector is the asc file,
#' the second element the rda file. The rda file can be omitted if it has the
#' same name as the asc file (except for the extension).
#' Use this option to start the whole process without the generation
#' of the text data. Do not specify \code{asc_filename} or
#' \code{rda_filename} (used to name the text files to be created, which is
#' irrelevant here). \cr
#'
#' Pour utiliser des fichiers asc et rda existant déjà, il est possible de
#' fournir à la place du data.frame un vecteur caractère indiquant le chemin
#' de ces fichiers. Le premier élément de ce vecteur est le fichier asc,
#' le deuxième élément le fichier rda. Le fichier rda peut être omis s'il
#' porte le même nom que le fichier asc (à l'extension près).
#'
#' Utiliser cette option pour lancer le processus complet sans la génération
#'  des données en texte. Ne pas spécifier \code{asc_filename} ou
#'  \code{rda_filename} (sert à nommer les fichiers texte à créer, ce qui est
#'  sans objet ici).
#'
#' @return
#' If \code{import = TRUE}, a list of data.frames (protected tables),
#' \code{NULL} otherwise. \cr
#'
#'(Si \code{import = TRUE}, une liste de data.frames (tableaux
#'   secrétisés), \code{NULL} sinon.)
#'
#' @seealso
#' \code{link{rtauargus_plus}}, a version optimized for a large
#' number of tables (at the cost of some usage restrictions). \cr
#' (\code{\link{rtauargus_plus}}, une version optimisée pour un grand
#'   nombre de tableaux (au prix de quelques restrictions d'usage).)
#'
#' @examples
#' \dontrun{
#' rtauargus(
#'   microdata = data.frame(V1 = c("A", "A", "B", "C", "A"), V2 = "Z"),
#'   explanatory_vars = c("V1", "V2"),
#'   safety_rules = "FREQ(3,10)",
#'   suppress = "GH(.,100)",
#'   output_options = "AS+" # (example of optional parameter for micro_arb)
#' )}
#' @export

rtauargus <- function(microdata,
                      explanatory_vars,
                      safety_rules,
                      suppress,
                      ...) {

  .dots <- list(...)

  if (length(explanatory_vars) > 10) {
    warning(
      "Plus de 10 croisements. ",
      "Utiliser 'rtauargus_plus' pour une execution plus rapide ?"
    )
  }

  ## 0. CONFLITS PARAMETRES .................

  # microdata pas un data.frame et asc/rda_filename spécifié
  if (!is.data.frame(microdata) &
      !(is.null(.dots$asc_filename) & is.null(.dots$rda_filename))) {
    stop(
      "\nIncoherence parametres :\n",
      " asc_filename ou rda_filename defini(s) alors que microdata sous",
      " forme de fichier texte"
    )
  }

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

  if (is.data.frame(microdata)) {
    # si microdata est un data.frame, crée les fichiers temporaires asc et rda
    # (récupère leurs noms dans liste 'input')

    # parametres
    param_asc_rda <- param_function(micro_asc_rda, .dots)
    param_asc_rda$microdata <- microdata
    # appel (+ récuperation noms asc et rda)
    input <- do.call(micro_asc_rda, param_asc_rda)

  } else if (is.character(microdata) & length(microdata) <= 2) {
    # si chemin vers fichiers texte, crée liste 'input' à partir de ceux-ci

    if (is.na(microdata[2])) { # le fichier rda porte le même nom
      microdata[2] <- sub("asc$", "rda", microdata[1])
    }
    if (!all(file.exists(microdata))) stop("fichier(s) asc ou rda manquant(s)")
    input <- list(
      asc_filename = microdata[1],
      rda_filename = microdata[2]
    )

  } else {

    stop(
      "microdata obligatoirement data.frame ou chemin(s) vers fichiers texte"
    )

  }

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

  ## 3. RUN_ARB ...........................

  # parametres
  param_run0 <- param_function(run_arb, .dots)
  param_system <- param_function(system, .dots)
  param_run <- c(param_run0, param_system)
  param_run$arb_filename <- batch$arb_filename

  # appel
  res <- do.call(run_arb, param_run)

  # RESULTAT .............................

  res

}
