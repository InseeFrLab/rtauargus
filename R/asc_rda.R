#' @importFrom dplyr %>%

write_rda <- function(info_var, chemin = getwd()) {

  # info_var est une liste contenant les infos pour chaque variable

  sapply(
    info_var,
    function(x) {
      paste(
        sep = "\n",
        paste(x$colname, x$position, x$width),
        paste0("  <", x$type_var, ">"),
        #<TOTCODE>
        if (!is.na(x$hierleadstring) | !is.na(x$hierlevels))
          "  <HIERARCHICAL>",
        if (!is.na(x$hierleadstring))
          paste0("  <HIERLEADSTRING> \"", x$hierleadstring, "\""),
        if (!is.na(x$hiercodelist))
          paste0("  <HIERCODELIST> \"", chemin, "/", x$hiercodelist, "\""),
        if (!is.na(x$hierlevels))
          paste0("  <HIERLEVELS> ", x$hierlevels),
        if (x$type_var %in% c("NUMERIC", "WEIGHT"))
          paste0("  <DECIMALS> ", x$digits)
      )
    }
  ) %>%
  gsub("(\n)+", "\n", .) %>% # plusieurs sauts de lignes par un seul
  sub("\n$", "", .) %>% # supprime dernier saut de ligne
  gsub("/", "\\\\", .) # remplace / par \, facultatif ?

}

#' Crée les fichiers asc et rda à partir de microdonnées
#'
#' Crée un fichier texte de longueur fixe (asc) et un fichier de métadonnées
#' (rda) à partir de microdonnées et d'informations additionnelles.
#'
#' Le paramètre \code{decimals} indique le nombre minimal de décimales à faire
#' figurer dans le fichier en sortie (quel que soit le nombre de décimales
#' effectivement présent dans \code{microdata}). Il s'applique à toutes les
#' variables réelles (double) mais pas aux variables entières (integer). Par
#' exemple, pour ajouter des zéros à une variable entière, la convertir avec
#' \code{as.double} au préalable.
#'
#' Les chiffres après la virgule peuvent être incorrects si le nombre total de
#' chiffres (avant ou après le séparateur décimal) est supérieur à 15. Voir
#' \code{\link[gdata]{write.fwf}} (fonction utilisée pour écrire le fichier asc)
#' pour plus de détails.
#'
#' @param microdata [\strong{obligatoire}] data.frame contenant les microdonnées.
#' @param asc_filename nom du fichier asc (avec extension). Si non renseigné, un
#'   fichier temporaire.
#' @param rda_filename nom du fichier rda (avec extension). Si non renseigné,
#'   \code{asc_filename} avec l'extension "rda" à la place de "asc").
#' @param weight nom de la variable de poids.
#' @param holding nom de la variable de holding.
#' @param hrc informations des variables hiérarchiques. Une liste dont les noms
#'   sont les variables concernées et les éléments sont les informations.
#'   Exemple :
#'   \code{list(}
#'   \code{VAR1 = c(hierleadstring = "@", hiercodelist = "hrc/var1.hrc"),}
#'   \code{VAR3 = c(hierlevels = "3 1 0")}
#'   \code{)}
#' @param decimals nombre de décimales minimum à afficher, par défaut 0 (cf.
#'   section 'Details').
#' @param totcode (pas encore implémenté)
#' @param missing (pas encore implémenté)
#' @param codelist (pas encore implémenté)
#' @param request (pas encore implémenté)
#' @param request_code (pas encore implémenté)
#'
#' @return Renvoie les noms des fichiers asc et rda sous forme de liste (de
#'   manière invisible).
#'
#' @examples
#' # donnees fictives temporaires
#' micro_df <- data.frame(
#'   ARRONDISSEMENT = c("443", "541", "543"),
#'               CA = c(100, 0, 7),
#'            POIDS = c(1, 2.71, 4.2)
#' )
#'
#' res <- micro_asc_rda(
#'   micro_df,
#'   weight = "POIDS",
#'   decimals = 1,
#'   hrc = list(ARRONDISSEMENT = c(hierlevels = "2 1"))
#' )
#'
#' # visualisation des fichiers produits
#' res # (noms des fichiers)
#' file.show(res$asc_filename, pager = "internal", title = "asc")
#' file.show(res$rda_filename, pager = "internal", title = "rda")
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom purrr transpose
#'
#' @export

micro_asc_rda <- function(microdata,
                          asc_filename = NULL,
                          rda_filename = NULL,
                          weight = NULL,
                          holding = NULL,
                          hrc = NULL,
                          decimals = 0,
                          totcode = "Total",
                          missing = NULL,
                          codelist = NULL,
                          request = NULL,
                          request_code = NULL) {

  microdata <- as.data.frame(microdata) # (probleme avec tibble notamment)

  # parametres non renseignés
  if (is.null(asc_filename)) asc_filename <- tempfile(fileext = ".asc")
  if (is.null(rda_filename)) rda_filename <- sub("asc$", "rda", asc_filename)

  # genere fichier longueur fixe et infos associees
  fwf_info <-
    gdata::write.fwf(
      microdata,
      asc_filename,
      formatInfo = TRUE,
      colnames = FALSE,
      justify = "right", # pour les variables caractères uniquement
      digits = 15, # max ? voir aide de format
      nsmall = decimals,
      scientific = FALSE
    )

  num <- vapply(microdata, is.numeric, logical(1))

  fwf_info <-
    fwf_info %>%
    mutate(
      type_var = ifelse(num, "NUMERIC", "RECODEABLE"),
      ordre_init = seq(nrow(.))
    )

  # modifier fwf_info en ajoutant donnees contenues dans weight, holding et hrc

  if (!is.null(weight)) {
    fwf_info$type_var[fwf_info$colname == weight] <- "WEIGHT"
  }

  if (!is.null(holding)) {
    fwf_info$type_var[fwf_info$colname == holding] <- "HOLDING"
  }

  if (!is.null(hrc)) {
    hrc_df <- df_param(hrc)
    fwf_info <-
      merge(
        fwf_info,
        hrc_df,
        by = "colname",
        all.x = TRUE
      )
  }

  # ajoute variables concernant la hiéarchie manquantes
  for (hier_var in c("hierleadstring", "hiercodelist", "hierlevels")) {
    if (is.null(fwf_info[[hier_var]])) fwf_info[[hier_var]] <- NA_character_
  }

  # reordonne car merge a fait un tri
  # et reorganise par variable (transpose)
  fwf_info <-
    fwf_info %>%
    arrange(ordre_init) %>%
    dplyr::select(-ordre_init, -nlevels, -exp) %>%
    transpose()

  # appeler write_rda sur ce fichier modifie
  res <- write_rda(fwf_info)
  writeLines(res, rda_filename)

  # renvoie les noms des fichiers asc et rda de manière invisible
  invisible(
    list(
      asc_filename = normPath2(asc_filename),
      rda_filename = normPath2(rda_filename)
    )
  )

}
