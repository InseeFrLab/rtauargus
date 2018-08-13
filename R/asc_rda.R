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
      if (!is.na(hierarchical))
        "  <HIERARCHICAL>",
      if (!is.na(hierarchical) && grepl("\\.hrc$", hierarchical))
        paste0("  <HIERCODELIST> \"", hierarchical, "\""),
      if (!is.na(hierarchical) && grepl("\\.hrc$", hierarchical))
        paste0("  <HIERLEADSTRING> \"", hierleadstring, "\""),
      if (!is.na(hierarchical) && grepl("^(\\d+ +)+\\d+$", hierarchical))
        paste0("  <HIERLEVELS> ", hierarchical),
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
#' @param microdata [\strong{obligatoire}] data.frame contenant les
#'   microdonnées.
#' @param asc_filename nom du fichier asc (avec extension). Si non renseigné, un
#'   fichier temporaire.
#' @param rda_filename nom du fichier rda (avec extension). Si non renseigné,
#'   \code{asc_filename} avec l'extension "rda" à la place de "asc".
#' @param weight_var nom de la variable de poids.
#' @param holding_var nom de la variable de holding.
#' @param decimals nombre minimal de décimales à afficher (voir section 'Nombre
#'   de décimales').
#' @param hrc informations sur les variables hiérarchiques (voir section
#'   'Variables hiérarchiques').
#' @param hierleadstring caractère qui, répété n fois, indique que la valeur est
#'   à n niveaux de profondeur dans la hiérarchie.
#' @param totcode code(s) pour le total d'une variable catégorielle (voir
#'   section 'Paramètres spécifiques' pour la syntaxe de ce paramètre). Les
#'   variables non spécifiées (ni par défaut, ni explicitement) se verront
#'   attribuer la valeur de \code{rtauargus.totcode}.
#' @param missing code(s) pour une valeur manquante (voir section
#'   'Paramètres spécifiques' pour la syntaxe de ce paramètre).
#' @param codelist fichier(s) contenant les libellés des variables catégorielles
#'   (voir section 'Paramètres spécifiques' pour la syntaxe de ce paramètre).
#' @param request (pas encore implémenté)
#'
#' @return Renvoie les noms des fichiers asc et rda sous forme de liste (de
#'   manière invisible). Les colonnes vides (remplies de \code{NA} ou de chaînes
#'   de caractères vides) ne seront pas exportées dans le fichier asc. Un
#'   message d'avertissement listera les colonnes concernées.
#'
#' @section Paramètres spécifiques:
#'
#' Les paramètres \code{hrc}, \code{totcode}, \code{missing} et \code{codelist}
#' sont à renseigner sous la forme d'un vecteur indiquant la valeur à prendre
#' pour chaque variable.
#'
#' Les noms des éléments du vecteur donnent la variable concernée, les éléments
#' du vecteur donnent la valeur du paramètre pour Tau-Argus. Un élément non
#' nommé constituera la valeur par défaut, qui sera attribuée à toutes les
#' variables pouvant prendre ce paramètre.
#'
#' Par exemple :
#' \itemize{
#'   \item{\code{totcode = "Ensemble"} : écrit \code{<TOTCODE> "Ensemble"} pour
#'     toutes les variables catégorielles}
#'   \item{\code{totcode = c("Ensemble", GEO = "France")} : idem, sauf pour la
#'     variable \code{GEO}}
#' }
#'
#' @section Variables hiérarchiques:
#'
#' Le paramètre \code{hrc} obéit aux mêmes règles de syntaxe que \code{totcode},
#' \code{missing} et \code{codelist} (éventuelle valeur commune, valeurs
#' spécifiques à chaque variable). Il présente de plus la particularité
#' d'accepter plusieurs façons de spécifier les valeurs associées aux variables
#' hiérarchiques.
#'
#' Pour définir une hiérarchie basée sur les positions des caractères
#' (\strong{hierlevels}), passer une suite de nombre entiers séparés par des
#' espaces.
#'
#' \emph{Exemple :} \code{c(CODECOM = "2 3 0 0 0")}
#'
#' Si la hiérarchie est définie dans un fichier hrc à part
#' (\strong{hiercodelist}), la fonction attend l'emplacement de ce fichier (et
#' un éventuel \code{hierleadstring} s'il diffère de l'option par défaut du
#' package). Dans ce cas, on peut écrire explicitement le chemin vers un fichier
#' existant (\code{c(A38 = "a38.hrc")}), mais aussi aussi passer un appel à
#' \code{\link{write_hrc}} qui génèrera un fichier hrc à partir de microdonnées.
#'
#' \emph{Exemple :} \code{c(A38 = write_hrc(microdata, c("A38", "A21", "A10")))}
#'
#' Un raccourci pour cet appel est d'écrire les variables consituant la
#' hiérarchie séparées par des ">". Dans ce cas, les microdonnées et
#' hierleadstring qu'utilise \code{write_hrc} sont ceux déclarés dans
#' \code{micro_asc_rda}.
#'
#' \emph{Exemple :} \code{c(A38 = "A38 > A21 > A10")} \emph{(nombre d'espaces
#' quelconque avant et après les ">")}
#'
#' Les deux dernières méthodes passent par la création d'un fichier temporaire.
#' Pour un fichier hrc réutilisable, il est nécessaire de le créer au préalable
#' à l'aide de \code{write_hrc}.
#'
#' @section Nombre de décimales:
#'
#' Le paramètre \code{decimals} indique le nombre minimal de décimales à faire
#' figurer dans le fichier en sortie (quel que soit le nombre de décimales
#' effectivement présent dans \code{microdata}). Il s'applique à toutes les
#' variables réelles (double) mais pas aux variables entières (integer). Pour
#' ajouter des zéros à une variable entière, la convertir avec \code{as.double}
#' au préalable.
#'
#' Les chiffres après la virgule peuvent être incorrects dans le fichie asc si
#' le nombre total de chiffres (avant ou après le séparateur décimal) est
#' supérieur à 15. Voir \code{\link[gdata]{write.fwf}} (fonction utilisée pour
#' écrire le fichier asc) pour plus de détails.
#'
#' @examples
#' # donnees fictives
#' micro_df <-
#'   data.frame(
#'     GEO   = c("443", "541", "543"),
#'     A10   = c( "AZ",  "BE",  "BE"),
#'     A21   = c(  "A",   "B",   "C"),
#'     DIPL  = c( "01",  "??",  "02"),
#'     CA    = c(  100,     0,     7),
#'     POIDS = c(    1,  2.71,   4.2)
#'   )
#'
#' # creation asc + rda
#' res <-
#'   micro_asc_rda(
#'     microdata  = micro_df,
#'     weight_var = "POIDS",
#'     decimals   = 1,
#'     hrc        = c(A21 = "A21>A10", GEO = "2 1 0", DIPL = "dipl.hrc"),
#'     totcode    = c(GEO = "France"),
#'     missing    = c(DIPL = "??")
#'   )
#'
#' # visualisation des fichiers produits
#' file.show(
#'   res$asc_filename, res$rda_filename,
#'   header = unlist(res),
#'   pager = "internal"
#' )
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom purrr transpose
#'
#' @export

micro_asc_rda <- function(microdata,
                          asc_filename   = NULL,
                          rda_filename   = NULL,
                          weight_var     = NULL,
                          holding_var    = NULL,
                          decimals       = getOption("rtauargus.decimals"),
                          hrc            = NULL,
                          hierleadstring = getOption("rtauargus.hierleadstring"),
                          totcode        = getOption("rtauargus.totcode"),
                          missing        = getOption("rtauargus.missing"),
                          codelist       = NULL,
                          request        = NULL) {

  microdata <- as.data.frame(microdata) # (probleme avec tibble notamment)

  # ignore colonnes de longueurs nulles  ..................................

  colvides <- sapply(microdata, function(x) all(is.na(x)) | all(x == ""))
  if (any(colvides)) {
    warning(
      "Colonnes vides non exportees en asc : ",
      paste(names(microdata)[colvides], collapse = ", ")
    )
    microdata <- microdata[!colvides]
  }

  # parametres non renseignés  ...........................................

  if (is.null(asc_filename)) asc_filename <- tempfile("RTA_", fileext = ".asc")
  if (is.null(rda_filename)) rda_filename <- sub("asc$", "rda", asc_filename)

  # genere fichier longueur fixe et infos associees  .....................

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

  # weight_var et holding_var ..........................................

  if (!is.null(weight_var)) {
    fwf_info$type_var[fwf_info$colname == weight_var] <- "WEIGHT"
  }

  if (!is.null(holding_var)) {
    fwf_info$type_var[fwf_info$colname == holding_var] <- "HOLDING"
  }

  # missing, totcode, codelist  ........................................

  var_quanti <- names(microdata)[!num]

  missing_df  <- df_param_defaut(names(microdata), "missing", missing)
  codelist_df <- df_param_defaut(var_quanti, "codelist", codelist)
  totcode_df  <-
    df_param_defaut(var_quanti, "totcode", totcode) %>%
    mutate(totcode = dplyr::coalesce(totcode, getOption("rtauargus.totcode")))

  # hierachical  ......................................................

  norm_hrc <-
    normalise_hrc(
      hrc,
      microdata = microdata,
      hierleadstring = hierleadstring
    )

  hrc_df <- df_param_defaut(var_quanti, "hierarchical", norm_hrc)
  hrc_df$hierleadstring <- NA_character_
  need_leadstring <- grepl("\\.hrc$", hrc_df$hierarchical)
  hrc_df$hierleadstring[need_leadstring] <- hierleadstring

  fwf_info <-
    purrr::reduce(
      list(fwf_info, missing_df, totcode_df, codelist_df, hrc_df),
      merge,
      by = "colname",
      all.x = TRUE
    )

  # reordonne (car tri par merge)  ...................................

  fwf_info <-
    fwf_info %>%
    arrange(ordre_init) %>%
    dplyr::select(-ordre_init, -nlevels, -exp)

  # reorganise en une liste de variables .............................
  fwf_info <- transpose(fwf_info)

  # genere vecteur format .rda .......................................
  res <- write_rda(fwf_info)

  # écrit fichier texte ..............................................
  writeLines(res, rda_filename)

  # renvoie noms des fichiers asc et rda de manière invisible .......
  invisible(
    list(
      asc_filename = normPath2(asc_filename),
      rda_filename = normPath2(rda_filename)
    )
  )

}
