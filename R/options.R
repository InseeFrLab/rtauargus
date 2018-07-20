# Valeurs par défaut (utilisées ici et dans .onLoad)
# Endroit unique où modifier ces valeurs internes aux package
# (y compris pour mise à jour tableau rubrique aide)

op.rtauargus <- list(
  # asc-rda ----------------------------------------------------- #
  rtauargus.decimals           = 0L,
  rtauargus.totcode            = "Total",
  rtauargus.missing            = "",
  # arb --------------------------------------------------------- #
  rtauargus.response_var       = "<freq>",
  rtauargus.weighted           = FALSE,
  rtauargus.output_type        = "2",
  rtauargus.output_options     = "",
  # run_tauargus ------------------------------------------------ #
  rtauargus.tauargus_exe       = "Y:/Logiciels/Tau/TauArgus.exe",
  rtauargus.show_batch_console = TRUE,
  rtauargus.import             = TRUE
)


#' Options du package rtauargus
#'
#' Gère les options du package (affiche, modifie, réinitialise).
#'
#' @usage
#' rtauargus_options()
#'
#' options(rtauargus.<opt> = <val>)
#'
#' @details
#' Les options du package définissent les comportements par défaut des
#' fonctions. Elles seront utilisées si un argument d'une fonction n'est pas
#' renseigné. Le nom de l'option est le nom du paramètre d'une fonction précédé
#' de "rtauargus.".
#'
#' Elles peuvent être redéfinies pour une session par une instruction
#' \code{options(rtauargus.}...\code{ = }...\code{)}, ou de manière globale si
#' de telles instructions sont placées dans un fichier de configuration propre à
#' l'utilisateur (fortement déconseillé si le programme a vocation à être
#' reproductible).
#'
#' @param ... noms des options à réinitialiser, séparés par des virgules. Si
#'   aucun nom n'est spécifié, toutes les options du package seront
#'   réinitialisées. Le préfixe \code{"rtauargus."} est facultatif.
#'
#' @section Liste des options:
#' \tabular{lll}{
#'  \strong{Option}                  \tab \strong{Valeur par défaut}                                            \tab \strong{Fonction}   \cr
#'  \code{------------------------}  \tab  \code{---------------------------------}                             \tab \code{-------------}\cr
#'  rtauargus.decimals               \tab  \code{\Sexpr{rtauargus:::op.rtauargus$rtauargus.decimals}}           \tab \link{micro_asc_rda}\cr
#'  rtauargus.totcode                \tab "\code{\Sexpr{rtauargus:::op.rtauargus$rtauargus.totcode}}"           \tab                     \cr
#'  rtauargus.missing                \tab "\code{\Sexpr{rtauargus:::op.rtauargus$rtauargus.missing}}"           \tab                     \cr
#'  \code{------------------------}  \tab  \code{---------------------------------}                             \tab \code{-------------}\cr
#'  rtauargus.response_var           \tab "\code{\Sexpr{rtauargus:::op.rtauargus$rtauargus.response_var}}"      \tab \link{micro_arb}    \cr
#'  rtauargus.weighted               \tab  \code{\Sexpr{rtauargus:::op.rtauargus$rtauargus.weighted}}           \tab                     \cr
#'  rtauargus.output_type            \tab "\code{\Sexpr{rtauargus:::op.rtauargus$rtauargus.output_type}}"       \tab                     \cr
#'  rtauargus.output_options         \tab "\code{\Sexpr{rtauargus:::op.rtauargus$rtauargus.output_options}}"    \tab                     \cr
#'  \code{------------------------}  \tab  \code{---------------------------------}                             \tab \code{-------------}\cr
#'  rtauargus.tauargus_exe           \tab "\code{\Sexpr{rtauargus:::op.rtauargus$rtauargus.tauargus_exe}}"      \tab \link{run_tauargus} \cr
#'  rtauargus.show_batch_console     \tab  \code{\Sexpr{rtauargus:::op.rtauargus$rtauargus.show_batch_console}} \tab                     \cr
#'  rtauargus.import                 \tab  \code{\Sexpr{rtauargus:::op.rtauargus$rtauargus.import}}             \tab
#' }
#'
#' @examples
#' rtauargus_options()
#'
#' # modifie certaines options
#' options(
#'   rtauargus.tauargus_exe = "Z:/tmp/TauArgus.exe",
#'   rtauargus.output_type = "4",
#'   rtauargus.weighted = TRUE
#' )
#' str(rtauargus_options())
#'
#' # reinitialise une partie des options (prefixe "rtauargus." facultatif)
#' reset_rtauargus_options("output_type", "rtauargus.tauargus_exe")
#' str(rtauargus_options())
#'
#' # reinitialise tout
#' reset_rtauargus_options()
#' str(rtauargus_options())
#' @seealso \link{options}, le système d'options de R dans lequel s'insèrent les
#'   options de ce package.
#' @export
#' @rdname rtauargus_options

rtauargus_options <- function() {

  # affiche les options du package
  opt <- options()
  rtauargus_options <- grep("^rtauargus\\.", names(opt))
  opt[rtauargus_options]

}

#' @export
#' @rdname rtauargus_options

reset_rtauargus_options <- function(...) {

  # reinitialise certaines options ou toutes

  modif <- c(...)

  if (length(modif) == 0) {

    modif <- names(op.rtauargus)

  } else {

    # autorise nom sans prefixe rtauargus.
    modif <-
      ifelse(
        grepl("^rtauargus\\.", modif),
        modif,
        paste0("rtauargus.", modif)
      )

    # warning si options inconnues
    inconnu <- modif[!modif %in% names(op.rtauargus)]
    if (length(inconnu)) {
      warning(
        "impossible de reinitisaliser : ",
        paste(inconnu, collapse = ", ")
      )
    }

  }

  options(op.rtauargus[modif])

  invisible(rtauargus_options())

}
