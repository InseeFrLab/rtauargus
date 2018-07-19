#' Exécute un batch Tau Argus
#'
#' Exécute les instructions contenues dans un fichier .arb pour Tau Argus.
#'
#' L'emplacement du programme TauArgus.exe est défini de manière globale au
#' chargement du package et modifiable par \code{\link{set_tauargus_exe}}.
#'
#' @param arb_filename nom du fichier batch à exécuter.
#' @param logbook nom du fichier où est enregistré le journal d'erreurs
#'   (optionnel).
#' @param import pour importer dans R les fichiers produits, \code{TRUE} par
#'   défaut.
#' @param ... paramètres supplémentaires pour \code{system()}. Par exemple,
#'   \code{show.output.on.console} = \code{FALSE}, pour ne pas afficher le
#'   déroulement du batch dans la console.
#'
#' @return \itemize{
#'   \item{une liste de data.frame contenant les résultats si
#'     \code{import = TRUE} (via la fonction \code{\link{import}})} ;
#'   \item{\code{NULL} sinon}.
#' }
#'
#' @export

run_tauargus <- function(arb_filename,
                         logbook = NULL,
                         import = TRUE,
                         ...) {

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

  if (import) import(arb_filename) else invisible(NULL)

}
