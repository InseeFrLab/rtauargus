#' Exécute un batch Tau Argus
#'
#' Exécute les instructions contenues dans un fichier .arb pour Tau Argus.
#'
#' L'emplacement du programme TauArgus.exe est défini de manière globale au
#' chargement du package et modifiable par \code{\link{set_tauargus_exe}}.
#'
#' @param arb_filename nom du fichier batch à exécuter.
#' @param logbook nom du fichier de log (optionnel).
#' @param ... paramètres supplémentaires pour \code{system()}. Par exemple,
#'   \code{show.output.on.console} = \code{FALSE}, pour ne pas afficher la log
#'   dans la console.
#'
#' @export

run_tauargus <- function(arb_filename, logbook = NULL, ...) {

  arb_full <- normalizePath(arb_filename)
  tau_full <- normalizePath(getOption("rtauargus_exe"))

  if (!is.null(logbook)) {
    file.create(logbook)
    logbook_full <- normalizePath(logbook)
  }

  commande <- paste0(
    '"',
    tau_full,
    '" "',
    arb_full,
    '"',
    if (!is.null(logbook)) paste0(' "', logbook_full, '"')
  )

  system(commande, ...)

}
