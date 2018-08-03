write_rda_1var <- function(info_var) {

  # écrit partie du .rda à partir des infos (une liste) pour une seule variable

  with(
    info_var,
    paste(
      sep = "\n",
      paste(
        colname,
        position,
        width,
        if (!is.na(missing) & missing != "") missing
      ),
      paste0("  <", type_var, ">"),
      if (!is.na(totcode))
        paste0("  <TOTCODE> \"", totcode, "\""),
      if (!is.na(codelist))
        paste0("  <CODELIST> \"", codelist, "\""),
      if (!is.na(hierleadstring) | !is.na(hierlevels))
        "  <HIERARCHICAL>",
      if (!is.na(hierleadstring))
        paste0("  <HIERLEADSTRING> \"", hierleadstring, "\""),
      if (!is.na(hiercodelist))
        paste0("  <HIERCODELIST> \"", hiercodelist, "\""),
      if (!is.na(hierlevels))
        paste0("  <HIERLEVELS> ", hierlevels),
      if (type_var %in% c("NUMERIC", "WEIGHT"))
        paste0("  <DECIMALS> ", digits)
    )
  )

}

#' @importFrom dplyr %>%

write_rda <- function(info_vars) {

  # écrit les infos format .rda pour toutes les variables
  # (info_vars est une liste contenant les infos pour chaque variable)

  chemin_complet <- function(x) {
    if (!is.na(x$hiercodelist)) x$hiercodelist <- normPath2(x$hiercodelist)
    if (!is.na(x$codelist)) x$codelist <- normPath2(x$codelist)
    return(x)
  }
  info_vars <- lapply(info_vars, chemin_complet)

  vapply(info_vars, write_rda_1var, character(1)) %>%
    gsub("(\n)+", "\n", .) %>% # plusieurs sauts de lignes par un seul
    sub("\n$", "", .) # supprime dernier saut de ligne

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
#' Les colonnes vides (\code{NA} ou chaînes de caractère vide) ne seront pas
#' exportées dans le fichier asc. Un message d'avertissement listera les
#' colonnes concernées.
#'
#' @param microdata [\strong{obligatoire}] data.frame contenant les
#'   microdonnées.
#' @param asc_filename nom du fichier asc (avec extension). Si non renseigné, un
#'   fichier temporaire.
#' @param rda_filename nom du fichier rda (avec extension). Si non renseigné,
#'   \code{asc_filename} avec l'extension "rda" à la place de "asc").
#' @param weight_var nom de la variable de poids.
#' @param holding_var nom de la variable de holding.
#' @param hrc informations des variables hiérarchiques. Une liste dont les noms
#'   sont les variables concernées et les éléments sont les informations.
#'   Exemple :
#'   \code{list(}
#'   \code{VAR1 = c(hierleadstring = "@", hiercodelist = "hrc/var1.hrc"),}
#'   \code{VAR3 = c(hierlevels = "3 1 0")}
#'   \code{)}
#' @param decimals nombre minimum de décimales à afficher, par défaut 0 (cf.
#'   section 'Details').
#' @param totcode (pas encore implémenté)
#' @param missing (pas encore implémenté)
#' @param codelist (pas encore implémenté)
#' @param request_var (pas encore implémenté)
#' @param request_code (pas encore implémenté)
#'
#' @return Renvoie les noms des fichiers asc et rda sous forme de liste (de
#'   manière invisible).
#'
#' @examples
#' # donnees fictives temporaires
#' micro_df <-
#'   data.frame(
#'     GEO   = c("443", "541", "543"),
#'     CA    = c(100, 0, 7),
#'     POIDS = c(1, 2.71, 4.2)
#'   )
#'
#' res <- micro_asc_rda(
#'   microdata  = micro_df,
#'   weight_var = "POIDS",
#'   decimals   = 1,
#'   hrc        = list(GEO = c(hierlevels = "2 1"))
#' )
#'
#' # visualisation des fichiers produits
#' res # (noms des fichiers)
#' file.show(res$asc_filename, pager = "internal", title = "fichier asc")
#' file.show(res$rda_filename, pager = "internal", title = "fichier rda")
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom purrr transpose
#'
#' @export

micro_asc_rda <- function(microdata,
                          asc_filename = NULL,
                          rda_filename = NULL,
                          weight_var   = NULL,
                          holding_var  = NULL,
                          hrc          = NULL,
                          decimals     = getOption("rtauargus.decimals"),
                          totcode      = getOption("rtauargus.totcode"),
                          missing      = getOption("rtauargus.missing"),
                          codelist     = NULL,
                          request_var  = NULL,
                          request_code = NULL) {

  microdata <- as.data.frame(microdata) # (probleme avec tibble notamment)

  # ignore colonnes de longueurs nulles
  colvides <- sapply(microdata, function(x) all(is.na(x)) | all(x == ""))
  if (any(colvides)) {
    warning(
      "Colonnes vides non exportees en asc : ",
      paste(names(microdata)[colvides], collapse = ", ")
    )
    microdata <- microdata[!colvides]
  }

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

  if (!is.null(weight_var)) {
    fwf_info$type_var[fwf_info$colname == weight_var] <- "WEIGHT"
  }

  if (!is.null(holding_var)) {
    fwf_info$type_var[fwf_info$colname == holding_var] <- "HOLDING"
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

  # ajoute missing, totcode, codelist
  var_quanti <- names(microdata)[!num]
  missing_df  <- df_param_defaut(names(microdata), "missing", missing)
  codelist_df <- df_param_defaut(var_quanti, "codelist", codelist)
  totcode_df  <-
    df_param_defaut(var_quanti, "totcode", totcode) %>%
    mutate(totcode = dplyr::coalesce(totcode, getOption("rtauargus.totcode")))

  fwf_info <-
    purrr::reduce(
      list(fwf_info, missing_df, totcode_df, codelist_df),
      merge,
      by = "colname",
      all.x = TRUE
    )

  # reordonne car merge a fait un tri
  fwf_info <-
    fwf_info %>%
    arrange(ordre_init) %>%
    dplyr::select(-ordre_init, -nlevels, -exp)

  # reorganise en une liste de variables
  fwf_info <- transpose(fwf_info)

  # genere vecteur format .rda
  res <- write_rda(fwf_info)

  # écrit fichier texte
  writeLines(res, rda_filename)

  # renvoie les noms des fichiers asc et rda de manière invisible
  invisible(
    list(
      asc_filename = normPath2(asc_filename),
      rda_filename = normPath2(rda_filename)
    )
  )

}
