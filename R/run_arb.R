#' Exécute un batch Tau-Argus
#'
#' Exécute les instructions contenues dans un fichier .arb pour Tau-Argus.
#'
#' Seul l’argument \code{arb_filename} est obligatoire, car toutes les
#' informations nécessaires sont présentes dans ce fichier.
#'
#' Il s'agit de la seule fonction du package qui exécute Tau-Argus. Elle
#' nécessite donc que le logiciel soit accessible depuis le poste de travail.
#'
#' L'emplacement du programme TauArgus.exe est défini de manière globale au
#' chargement du package. De fait, l'argument \code{tauargus_exe} n'aura
#' normalement pas à être spécifié (sauf pour surcharger l'option globale le
#' temps de l'exécution de la foncion).
#'
#' Des vérifications sont effectuées avant le lancement effectif de Tau-Argus :
#' existence du logiciel sur le poste, des fichiers asc et rda, des dossiers où
#' écrire les résultats, des variables à utiliser (croisements, variable de
#' réponse) dans les métadonnées (fichier rda).
#'
#' @param arb_filename nom du fichier batch à exécuter.
#' @param missing_dir action si les dossiers où seront écrits les
#'   résultats n'existent pas ("stop" pour déclencher une erreur, "create" pour
#'   créer les dossiers manquants).
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
#' @examples
#' \dontrun{
#'
#' micro_arb("my_batch.arb")
#' }
#'
#' @aliases run_tauargus
#'
#' @importFrom dplyr %>%
#'
#' @export

run_arb <- function(arb_filename,
                    missing_dir = getOption("rtauargus.missing_dir"),
                    tauargus_exe = getOption("rtauargus.tauargus_exe"),
                    logbook = NULL,
                    show_batch_console = getOption("rtauargus.show_batch_console"),
                    import = getOption("rtauargus.import"),
                    ...) {

  # valeur par défaut du package si option vide ................

  if (is.null(missing_dir)) missing_dir <- op.rtauargus$rtauargus.missing_dir
  if (is.null(tauargus_exe)) tauargus_exe <- op.rtauargus$rtauargus.tauargus_exe
  if (is.null(show_batch_console)) {
    show_batch_console <- op.rtauargus$rtauargus.show_batch_console
  }
  if (is.null(import)) import <- op.rtauargus$rtauargus.import

  # présence TauArgus.exe ......................................

  if (!file.exists(tauargus_exe)) {
    stop(
     "Tau-Argus introuvable (", tauargus_exe, ")\n  ",
     "renseigner le parametre tauargus_exe ou l'option rtauargus.tauargus_exe"
    )
  }

  # lecture contenu fichier arb pour vérifications .............

  if (!file.exists(arb_filename)) {
    stop(
      "Fichier introuvable : ", arb_filename, "\n",
      "(utiliser `micro_arb` pour creer un fichier batch)"
    )
  }
  infos_arb <- arb_contents(arb_filename)

  # présence des fichiers (input) ..............................

  if (!file.exists(infos_arb$openmicrodata)) {
    stop(
      "Fichier asc introuvable : ", infos_arb$openmicrodata, "\n",
      "(utiliser `micro_asc_rda` pour creer un fichier asc)"
    )
  }
  if (!file.exists(infos_arb$openmetadata)) {
    stop(
      "Fichier rda introuvable : ", infos_arb$openmetadata, "\n",
      "(utiliser `micro_asc_rda` pour creer un fichier rda)"
    )
  }
  if (!is.null(infos_arb$apriori)) {
    hst <- infos_arb$apriori$file
    if (any(manq <- !file.exists(hst))) {
      stop(
        "Fichier(s) d'apriori introuvable(s) : ",
        paste(unique(hst[manq]), collapse = "\n")
      )
    }
  }

  # gestion dossiers manquants (output) ........................

  output_names <- infos_arb$writetable$output_names

  ouput_dirs <- dirname(output_names)
  if (any(manq <- !dir.exists(ouput_dirs))) {
    missd <- unique(ouput_dirs[manq])
    if (missing_dir == "stop") {
      stop(
        "\nDossiers introuvable(s) :\n  ",
        paste(missd, collapse = "\n  "),
        "\n(utiliser missing_dir = \"create\" ?)"
      )
    } else if (missing_dir == "create") {
      warning(
        "Dossier(s) cree(s) :\n    ",
        paste(missd, collapse = "\n    ")
      )
      purrr::walk(missd, dir.create, recursive = TRUE)
    } else {
      stop("'missing_dir' incorrect. Valeurs permises : \"stop\", \"create\".")
    }
  }

  # coherence explanatory_vars et variables rda ...............

  used_vars <-
    with(
      infos_arb$specifytable,
      c(explanatory, response, shadow, cost)
    ) %>%
    unlist() %>%
    unique()
  used_vars <- used_vars[!used_vars %in% c("Freq", "")]

  rda_vars <- vars_micro_rda(infos_arb$openmetadata)

  vars_manq <- setdiff(used_vars, rda_vars)
  if (length(vars_manq)) {
    stop(
      "Variable(s) specifiee(s) absente(s) des metadonnees (rda) :\n    ",
      paste(vars_manq, collapse = "\n    ")
    )
  }

  # construction commande .....................................

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

  # appel .....................................................

  system(
    commande,
    show.output.on.console = show_batch_console,
    ...
  )

  if (import) import(arb_filename) else invisible(NULL)

}

# changement de nom
# slow deprecation : dans un premier temps juste un avertissement,
# remplacer par .Defunct à terme, puis supprimer définitivement

#' @export
#' @keywords internal

run_tauargus <- function(arb_filename,
                         missing_dir = getOption("rtauargus.missing_dir"),
                         tauargus_exe = getOption("rtauargus.tauargus_exe"),
                         logbook = NULL,
                         show_batch_console = getOption("rtauargus.show_batch_console"),
                         import = getOption("rtauargus.import"),
                         ...) {

  .Deprecated( # puis .Defunct
    new = "run_arb",
    old = "run_tauargus", # (a supprimer si .Defunct)
    package = "rtauargus",
    msg = paste0("Utiliser maintenant 'run_arb' a la place de 'run_tauargus'\n",
                 "(seul le nom change, la syntaxe reste la meme)")
  )

  run_arb(
    arb_filename,
    missing_dir,
    tauargus_exe,
    logbook,
    show_batch_console,
    import,
    ...
  )

}
