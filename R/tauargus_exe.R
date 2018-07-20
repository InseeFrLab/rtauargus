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

  ta_path <- dirname(getOption("rtauargus.tauargus_exe"))

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
