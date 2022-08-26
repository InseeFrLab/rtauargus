# Fonction extrayant les informations contenues dans un fichier batch (arb)
# Structure de la liste en sortie (tous les items pas forcément présents)
# $ openmicrodata     [chr]
# $ openmetadata      [chr]
# $ specifytable      [list]
#  ..$ explanatory    [list]
#  ..$ response       [chr]
#  ..$ shadow         [chr]
#  ..$ cost           [chr]
# $ apriori           [list]
#  ..$ file           [chr]
#  ..$ sep            [chr]
#  ..$ ignore_err     [chr]
#  ..$ exp_triv       [chr]
# $ safetyrule        [chr]
# $ suppress          [chr]
# $ writetable        [list]
#  ..$ output_types   [chr]
#  ..$ output_options [chr]
#  ..$ output_names   [chr]
# $ gointeractive     [logi]
# $ tab_id            [chr]

#' @importFrom purrr map_at
#' @importFrom dplyr %>%

# Décompose commandes avec plusieurs arguments ----------------------------

#' @importFrom purrr map
#' @importFrom purrr map_at
#' @importFrom purrr transpose
#' @importFrom dplyr %>%

specif_decompose <- function(specif) {

  ## extrait les infos contenus dans instructions <SPECIFYTABLE> d'un batch
  ## pour par exemple les utiliser dans reimport des données
  ## param specif : un vecteur de commandes <SPECIFYTABLE>

  # menage debut fin
  specif <- sub("^<SPECIFYTABLE> +", "", trimws(specif))

  # variable comptage
  specif <- sub("<freq>", "Freq", specif)

  # separe differentes parties
  specif <- paste0(specif, "|") # pour dernière valeur vide (cf. ?strsplit)
  specif_split <- strsplit(specif, "|", fixed = TRUE)

  # reclasse par type de variable
  type_var <- c("explanatory", "response", "shadow", "cost")
  res <- transpose(specif_split, type_var)

  # supprime guillements et espaces (sauf dans explanatory)
  res <-
    res %>%
    map_at(
      c("response", "shadow", "cost"),
      ~ gsub("(\"| )", "", .x)
    )

  # separe variables dans explanatory
  res %>%
    map_at(
      "explanatory",
      ~ .x %>%
        stringr::str_match_all('[^" ]+') %>%
        map(~ .[ , 1])
    )

}

write_decompose <- function(write_cmd) {

  ## extrait les infos contenus dans instructions <WRITETABLE> d'un batch
  ## pour par exemple les utiliser dans reimport des données
  ## param write_cmd : un vecteur de commandes <WRITETABLE>

  res <- NULL

  infos_writetable <-
    stringr::str_match(
      write_cmd,
      "\\((\\d+),([1-6]+),(.*),\"(.+)\"\\)"
    )

  res$output_types   <- infos_writetable[ , 3]
  res$output_options <- infos_writetable[ , 4]
  res$output_names   <- infos_writetable[ , 5]

  res

}

apriori_decompose <- function(apriori_cmd) {

  res <- NULL

  infos_apriori <-
    stringr::str_match(
      apriori_cmd,
      "(.+),\\d{1,2},\"(.+)\",(\\d),(\\d)"
    )

  if (!length(infos_apriori)) return(NULL)

  res$file       <- infos_apriori[ , 2] %>% rm_cmd() %>% unquote()
  res$sep        <- infos_apriori[ , 3]
  res$ignore_err <- infos_apriori[ , 4]
  res$exp_triv   <- infos_apriori[ , 5]

  res

}

#Affiche le logbook complet dans la console si verbose = TRUE, sinon que les erreurs

display_console <- function (
  verbose = FALSE,
  logbook_file = NULL
){

  if (is.null(logbook_file)){
    stop("logbook_file not filled in ")
  }
  logbook<-(readLines(logbook_file))
  logbook<-unname(sapply(logbook,
                         function(l){
                           substring(l, gregexpr(":[0-9]{2} : ", l)[[1]][1][1]+6)
                         }))

  logbook_df<-data.frame(logbook)

  start_batch<-(grep("Start of batch procedure", x=logbook, value=FALSE))

  dernier_lancement<-tail(start_batch, n=1)

  dernier_logbook<-logbook_df[dernier_lancement:nrow(logbook_df),]


  if (verbose){
    writeLines(dernier_logbook)
  }else{

    dernier_logbook_df<-data.frame(dernier_logbook)

    debut_error<-(grep("Error ", x=dernier_logbook, value=FALSE))
    debut_error<- data.frame(debut_error)

    if (!nrow(debut_error)==0){
      error <- dernier_logbook_df[debut_error[1,]:nrow(dernier_logbook_df),]
      error_df<-data.frame(error)
      debut_error2<-(grep("Error ", x=error, value=FALSE))
      debut_error2<- data.frame(debut_error2)

      fin_possible1<-(grep("End of TauArgus run", x=error, value=FALSE))
      fin_possible2<-(grep("<", x=error, value=FALSE))
      fin_error<-min(fin_possible1, fin_possible2)

      error_message<-error_df[debut_error2[1,]:fin_error-1,]
      error_message<-error_df[debut_error2[1,]:fin_error-1,]

      writeLines(error_message)
    }
  }
}

# Autres fonctions utiles -------------------------------------------------

rm_cmd <- function(cmd) sub("^ *<.+> *", "", cmd)

unquote <- function(s) sub("^[\"'](.+)[\"']$", "\\1", s)

#' Runs a Tau-Argus batch
#'
#' Executes the instructions contained in an .arb file for Tau-Argus. \cr
#' (Exécute les instructions contenues dans un fichier .arb pour Tau-Argus.)
#'
#' Only the argument \code{arb_filename} is required, because all
#' necessary information is present in this file.
#'
#' This is the only function in the package that runs Tau-Argus. It
#' therefore requires the software to be accessible from the workstation.
#'
#' The location of the TauArgus.exe program is defined globally when the
#' loading the package. In fact, the argument \code{tauargus_exe} will not
#' normally not have to be specified (except to override the global option the
#' time of the execution of the function).
#'
#' Checks are made before the actual launching of Tau-Argus:
#' existence of the software on the computer, of the asc and rda files, of the folders where
#' write the results, the variables to be used (crossings, response variable) in the
#' response variable) in the metadata (rda file). \cr
#'
#' (Seul l’argument \code{arb_filename} est obligatoire, car toutes les
#' informations nécessaires sont présentes dans ce fichier.
#' Il s'agit de la seule fonction du package qui exécute Tau-Argus. Elle
#' nécessite donc que le logiciel soit accessible depuis le poste de travail.
#'
#' L'emplacement du programme TauArgus.exe est défini de manière globale au
#' chargement du package. De fait, l'argument \code{tauargus_exe} n'aura
#' normalement pas à être spécifié (sauf pour surcharger l'option globale le
#' temps de l'exécution de la fonction).
#'
#' Des vérifications sont effectuées avant le lancement effectif de Tau-Argus :
#' existence du logiciel sur le poste, des fichiers asc et rda, des dossiers où
#' écrire les résultats, des variables à utiliser (croisements, variable de
#' réponse) dans les métadonnées (fichier rda).)
#'
#' @param arb_filename name of the batch file to execute. \cr
#' (nom du fichier batch à exécuter.)
#' @param is_tabular boolean, if the data is already tabulated or not \cr
#' (booléen, si les données sont déja tabulées ou non)
#' @param missing_dir what to do if the folders where the results will be written
#' results do not exist ("stop" to trigger an error, "create" to
#' create the missing folders). \cr
#' (action si les dossiers où seront écrits les
#'   résultats n'existent pas ("stop" pour déclencher une erreur, "create" pour
#'   créer les dossiers manquants).)
#' @param tauargus_exe directory and name of the Tau-Argus software. \cr
#' (répertoire et nom du logiciel Tau-Argus.)
#' @param logbook name of the file where the error log is saved.
#' If NULL, a "logbook.txt" file will be saved in the working directory). \cr
#' (nom du fichier où est enregistré le journal d'erreurs.
#' Si NULL, le fichier "logbook.txt" sera suavegardé sur le répertoire de travail.)
#' @param show_batch_console to display the batch progress in the
#' console. \cr
#' (pour afficher le déroulement du batch dans la
#'   console.)
#' @param verbose boolean, to display the batch execution (if TRUE) or
#' only error messages if any (if FALSE) \cr
#' (booléen, pour afficher l'exécution du batch (si TRUE) ou
#' uniquement les messages d'erreurs, s'il y en a (si FALSE))
#' @param import to import in R the files produced, \code{TRUE} by
#' default. \cr
#' (pour importer dans R les fichiers produits, \code{TRUE} par
#'   défaut.)
#' @param ... additional parameters for \code{system()}. \cr
#' (paramètres supplémentaires pour \code{system()}.)
#'
#' @return \itemize{
#' \item{a list of data.frame containing the results if
#' \code{import = TRUE} (via the \code{link{import}} function)} ;
#' \item{\code{NULL} otherwise}.
#' }
#'
#' @inheritSection micro_asc_rda See also
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
                    is_tabular = getOption("rtauargus.is_tabular"),
                    missing_dir = getOption("rtauargus.missing_dir"),
                    tauargus_exe = getOption("rtauargus.tauargus_exe"),
                    logbook = NULL,
                    show_batch_console = getOption("rtauargus.show_batch_console"),
                    verbose = TRUE,
                    import = getOption("rtauargus.import"),

                    ...) {

  # valeur par défaut du package si option vide ................

  if (is.null(missing_dir)) missing_dir <- op.rtauargus$rtauargus.missing_dir
  if (is.null(tauargus_exe)) tauargus_exe <- op.rtauargus$rtauargus.tauargus_exe
  if(is.null(logbook)) logbook <- "logbook.txt"
  if (is.null(show_batch_console)) {
    show_batch_console <- op.rtauargus$rtauargus.show_batch_console
  }
  if (is.null(import)) import <- op.rtauargus$rtauargus.import

  # présence TauArgus.exe ......................................

  if (!file.exists(tauargus_exe)) {
    stop(
      "Tau-Argus cannot be found (", tauargus_exe, ")\n  ",
      "please specify the parammeter tauargus_exe or the option rtauargus.tauargus_exe"
    )
  }

  # lecture contenu fichier arb pour vérifications .............

  if (!file.exists(arb_filename)) {
    stop(
      "Batch file cannot be found : ", arb_filename
    )
  }
  infos_arb <- arb_contents(arb_filename)

  # présence des fichiers (input) ..............................
  if (is_tabular!= TRUE){
    if (!file.exists(infos_arb$openmicrodata)) {
      stop(
        "Microdata file cannot be found (.asc): ", infos_arb$openmicrodata
      )
    }
  }
  if (!file.exists(infos_arb$openmetadata)) {
    stop(
      "Metadata file cannot be found (.rda) : ", infos_arb$openmetadata, "\n",
      "(utiliser `micro_asc_rda` pour creer un fichier rda)"
    )
  }
  if (!is.null(infos_arb$apriori)) {
    hst <- infos_arb$apriori$file
    if (any(manq <- !file.exists(hst))) {
      stop(
        "Apriori file cannot be found (.hst): ",
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
        "\nDirectory cannot be found :\n  ",
        paste(missd, collapse = "\n  "),
        "\n(use missing_dir = \"create\" ?)"
      )
    } else if (missing_dir == "create") {
      warning(
        "Directory created :\n    ",
        paste(missd, collapse = "\n    ")
      )
      purrr::walk(missd, dir.create, recursive = TRUE)
    } else {
      stop("'missing_dir' incorrect. allowed values : \"stop\", \"create\".")
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
      "Variable(s) specified(s) missing(s) in the metadata file (rda) :\n    ",
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
  logbook_file<-paste0( normPath2(logbook))


  # appel .....................................................

  system(
    commande,
    show.output.on.console = show_batch_console,
    ...
  )
  display_console(verbose, logbook_file)

  if (import) import(arb_filename) else invisible(NULL)

}
