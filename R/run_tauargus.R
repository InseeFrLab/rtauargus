#' Exécute un batch Tau-Argus
#'
#' Exécute les instructions contenues dans un fichier .arb pour Tau-Argus.
#'
#' L'emplacement du programme TauArgus.exe est défini de manière globale au
#' chargement du package. De fait, l'argument \code{tauargus_exe} n'aura
#' normalement pas à être spécifié (sauf pour surcharger l'option globale le
#' temps de l'exécution de la foncion).
#'
#' @param arb_filename nom du fichier batch à exécuter.
#' @param tauargus_exe répertoire et nom du logiciel Tau-Argus.
#' @param logbook nom du fichier où est enregistré le journal d'erreurs
#'   (optionnel).
#' @param show_batch_console pour afficher le déroulement du batch dans la
#'   console.
#' @param import pour importer dans R les fichiers produits, \code{TRUE} par
#'   défaut.
#' @param ... paramètres supplémentaires pour \code{system()}.
#'
#' @return \itemize{
#'   \item{une liste de data.frame contenant les résultats si
#'     \code{import = TRUE} (via la fonction \code{\link{import}})} ;
#'   \item{\code{NULL} sinon}.
#' }
#'
#' @inheritSection micro_asc_rda Voir aussi
#'
#' @export

run_tauargus <- function(arb_filename,
                         tauargus_exe = getOption("rtauargus.tauargus_exe"),
                         logbook = NULL,
                         show_batch_console = getOption("rtauargus.show_batch_console"),
                         import = getOption("rtauargus.import"),
                         ...) {

  # valeur par défaut du package si option vide
  if (is.null(tauargus_exe)) tauargus_exe <- op.rtauargus$rtauargus.tauargus_exe
  if (is.null(show_batch_console)) {
    show_batch_console <- op.rtauargus$rtauargus.show_batch_console
  }
  if (is.null(import)) import <- op.rtauargus$rtauargus.import

  arb_full <- normalizePath(arb_filename)
  tau_full <- normalizePath(tauargus_exe)

  commande <- paste0(
    '"',
    tau_full,
    '" "',
    arb_full,
    '"',
    if (!is.null(logbook)) paste0(' "', normPath2(logbook), '"')
  )

  system(
    commande,
    show.output.on.console = show_batch_console,
    ...
  )

  if (import) import(arb_filename) else invisible(NULL)

}
