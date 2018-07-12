#' Spécifie l'emplacement de Tau Argus
#'
#' Modifie l'option \code{rtauargus_exe}. Si le paramètre n'est pas renseigné,
#' revient à l'emplacement par défaut (version portable sur AUS).
#'
#' La fonction ne vérifie pas si le chemin existe réellement ou si l'exécutable
#' est bien présent.
#'
#' @param path_exe chemin complet et nom du programme Tau Argus.
#' @param quiet \code{TRUE} pour opérer de manière silencieuse.
#'
#' @export

set_tauargus_exe <- function(path_exe = "Y:/Logiciels/Tau/TauArgus.exe",
                             quiet = FALSE) {

  if (!grepl("\\.exe", path_exe)) {
    stop("Inclure le nom du programme avec l'extension .exe")
  }

  options(rtauargus_exe = path_exe)

  if (!quiet) message('Tau Argus : "', getOption("rtauargus_exe"), '"')

}

#' Informations sur Tau Argus
#'
#' Recherche des informations sur la version de Tau Argus utilisée. Cette
#' fonction est expérimentale.
#'
#' La fonction lit le contenu d'un éventuel fichier TauNews.html (historique des
#' changements) et récupère les informations dans l'en-tête du dernier
#' paragraphe rédigé. Si le fichier n'est pas présent ou change de structure
#' d'une version sur l'autre, la fonction ne donnera pas le résultat attendu.
#'
#' @return Un vecteur caractère contenant trois composantes : path, version et
#'   date.
#'
#' @export

tauargus_info <- function() {

  ta_path <- dirname(getOption("rtauargus_exe"))

  ta_news <- file.path(ta_path, "TauNews.html")
  if (!file.exists(ta_news)) {
    warning("fichier ", ta_news, " introuvable")
    return(NULL)
  }

  news <- readLines(ta_news)
  i_version <- grep("ARGUS version \\d", news)[1]

  ta_version <- news[i_version]
  ta_date <- news[i_version + 1]

  c(
    path = ta_path,
    version = sub(".+(\\d+\\.\\d+\\.\\d+).+", "\\1", ta_version),
    date = sub(".+>([0-9]{1,2} +[A-Za-z]+ +[0-9]{4})<.+", "\\1", ta_date)
  )

}
