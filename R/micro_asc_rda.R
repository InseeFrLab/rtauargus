write_rda_1var <- function(info_var) {

  # écrit partie du .rda à partir des infos (une liste) pour une seule variable

  ligne1 <- with(info_var,
    paste(
      colname,
      position,
      width,
      if (!is.na(missing) & missing != "") missing
    )
  )

  with(
    info_var,
    paste(
      sep = "\n",
      trimws(ligne1),
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

#' Creates asc and rda files from microdata
#'
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' Development on `micro_asc_rda()` is complete, and for new code we recommend
#' switching to the tabular-wise protection provided by `tab_rtauargus()`
#' or `tab_multi_manager()`, which offer a lot more features for your
#' protection problems.
#'
#' See more details in `vignette("rtauargus")` or in
#' `vignette("protect_multi_tables)`.
#'
#' Creates a fixed length text file (asc) and a metadata file
#' (rda) from microdata and additional information. \cr
#' (Crée un fichier texte de longueur fixe (asc) et un fichier de métadonnées
#' (rda) à partir de microdonnées et d'informations additionnelles.)
#'
#' @param microdata  data.frame containing the microdata. \cr
#' ( data.frame contenant les microdonnées.)
#' @param asc_filename name of the asc file (with extension). If not filled in,
#' a temporary file. \cr
#' (nom du fichier asc (avec extension). Si non renseigné,
#' un fichier temporaire.)
#' @param rda_filename name of the rda file (with extension). If not filled in,
#' `asc_filename` with the extension "rda" instead of "asc". \cr
#' (nom du fichier rda (avec extension). Si non renseigné,
#' `asc_filename` avec l'extension "rda" à la place de "asc".)
#' @param weight_var name of the weight variable. \cr
#' (nom de la variable de poids.)
#' @param holding_var nom de la variable de holding.
#' (name of the holding variable.) \cr
#' @param decimals minimum number of decimals to display (see section 'Number of
#' of decimals').
#' (nombre minimal de décimales à afficher (voir section 'Number of
#' of decimals').)
#' @param hrc information about the hierarchical variables (see section
#' section 'Hierarchical variables'). \cr
#' (informations sur les variables hiérarchiques (voir section
#' 'Hierarchical variables').)
#' @param hierleadstring character which, repeated n times, indicates that the value is
#' at n levels deep in the hierarchy. \cr
#' (caractère qui, répété n fois, indique que la valeur est
#' à n niveaux de profondeur dans la hiérarchie.)
#' @param totcode code(s) for the total of a categorical variable (see
#' section 'Specific parameters' for the syntax of this parameter). The
#' variables not specified (neither by default nor explicitly) will be
#' assigned the value of `rtauargus.totcode`. \cr
#' (code(s) pour le total d'une variable catégorielle (voir
#' section 'Specific parameters' pour la syntaxe de ce paramètre). Les
#' variables non spécifiées (ni par défaut, ni explicitement) se verront
#' attribuer la valeur de `rtauargus.totcode`.)
#' @param missing code(s) for a missing value (see section
#' 'Specific parameters' for the syntax of this parameter). \cr
#' (code(s) pour une valeur manquante (voir section
#'   'Specific parameters' pour la syntaxe de ce paramètre).)
#' @param codelist file(s) containing labels for categorical variables
#' (see section 'Specific parameters' for the syntax of this parameter). \cr
#' (fichier(s) contenant les libellés des variables catégorielles
#'   (voir section 'Specific parameters' pour la syntaxe de ce paramètre).)
#' @param request (not yet implemented - pas encore implémenté)
#'
#' @return
#' Returns the names of the asc and rda files as a list (invisibly).
#' invisibly). Empty columns (filled with `NA` or empty strings) are not
#' strings) will not be exported to the asc file. A
#' warning message will list the affected columns. \cr
#' (Renvoie les noms des fichiers asc et rda sous forme de liste (de
#'   manière invisible). Les colonnes vides (remplies de `NA` ou de chaînes
#'   de caractères vides) ne seront pas exportées dans le fichier asc. Un
#'   message d'avertissement listera les colonnes concernées.)
#'
#' @section Specific parameters:
#'
#' The parameters `totcode`, `missing` and `codelist`
#' are to be filled in as a vector indicating the value to take
#' for each variable.
#'
#' The names of the elements of the vector give the variable concerned, the elements
#' of the vector give the value of the parameter for Tau-Argus. An unnamed element
#' element will be the default value, which will be assigned to all
#' variables that can take this parameter.
#'
#' For example :
#' \itemize{
#'   \item{`totcode = "All"` : écrit `<TOTCODE> "All"` for all
#'   categorical variables}
#'   \item{`totcode = c("All", GEO = "France")` : idem, except for the
#'     `GEO` variable }
#' }
#'
#' (Les paramètres `totcode`, `missing` et `codelist`
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
#'   \item{`totcode = "Ensemble"` : écrit `<TOTCODE> "Ensemble"` pour
#'     toutes les variables catégorielles}
#'   \item{`totcode = c("Ensemble", GEO = "France")` : idem, sauf pour la
#'     variable `GEO`}
#' })
#'
#' @section Hierarchical variables:
#'
#' The parameter `hrc` follows the same syntax rules as `totcode`,
#' `missing` and `codelist` (named vector containing as many elements
#' as there are variables to describe). It also has the particularity
#' to accept several ways of specifying the values associated with the
#' hierarchical variables.
#'
#' To define a hierarchy based on the positions of characters
#' (**hierierlevels**), pass a sequence of integers separated by
#' spaces.
#'
#' *Example :* `c(CODECOM = "2 3 0 0 0")`
#'
#' If the hierarchy is defined in a separate hrc file
#' (**hiercodelist**), the function expects the location of this file (and
#' a possible `hiercodelist` if it differs from the default option of the
#' package). In this case, you can write explicitly the path to an existing file
#' file (`c(A38 = "a38.hrc")`), but also make a call to
#' [write_hrc()] which will generate an hrc file from microdata.
#'
#' *Example :* `c(A38 = write_hrc(microdata, c("A38", "A21", "A10")))`
#'
#' A shortcut for this call is to write the variables constituting the
#' hierarchy separated by ">". In this case, the microdata and
#' hierleadstring that `write_hrc` uses are those declared in
#' `micro_asc_rda`.
#'
#' *Example :* `c(A38 = "A38 > A21 > A10")` *(number of spaces
#' any before and after the ">")*
#'
#' The last two methods require the creation of a temporary file.
#' For a reusable hrc file, it is necessary to create it beforehand
#' using `write_hrc`.
#'
#' The three methods require that the elements of the vector in parameter
#' be named (with the name of the variable), even if there is only one
#' element.
#'
#' (Le paramètre `hrc` obéit aux mêmes règles de syntaxe que `totcode`,
#' `missing` et `codelist` (vecteur nommé contenant autant d'éléments
#' que de variables à décrire). Il présente de plus la particularité
#' d'accepter plusieurs façons de spécifier les valeurs associées aux variables
#' hiérarchiques.
#'
#' Pour définir une hiérarchie basée sur les positions des caractères
#' (**hierlevels**), passer une suite de nombre entiers séparés par des
#' espaces.
#'
#' Si la hiérarchie est définie dans un fichier hrc à part
#' (**hiercodelist**), la fonction attend l'emplacement de ce fichier (et
#' un éventuel `hierleadstring` s'il diffère de l'option par défaut du
#' package). Dans ce cas, on peut écrire explicitement le chemin vers un fichier
#' existant (`c(A38 = "a38.hrc")`), mais aussi passer un appel à
#' [write_hrc()] qui génèrera un fichier hrc à partir de microdonnées.
#'
#' Un raccourci pour cet appel est d'écrire les variables constituant la
#' hiérarchie séparées par des ">". Dans ce cas, les microdonnées et
#' hierleadstring qu'utilise `write_hrc` sont ceux déclarés dans
#' `micro_asc_rda`.
#'
#' *Exemple :* `c(A38 = "A38 > A21 > A10")` *(nombre d'espaces
#' quelconque avant et après les ">")*
#'
#' Les deux dernières méthodes passent par la création d'un fichier temporaire.
#' Pour un fichier hrc réutilisable, il est nécessaire de le créer au préalable
#' à l'aide de `write_hrc`.
#'
#' Les trois méthodes nécessitent que les éléments du vecteur en paramètre
#' soient nommés (avec le nom de la variable), même s'il n'y a qu'un seul
#' élément.)
#'
#' @section Number of decimals:
#'
#' The parameter `decimals` indicates the minimum number of decimals to be
#' appear in the output file (whatever the number of decimals
#' actually present in `microdata`). It applies to all
#' real variables (double) but not to integer variables (integer). For
#' add zeros to an integer variable, convert it with `as.double`
#' beforehand.
#'
#' The digits after the decimal point may be incorrect in the asc file if
#' the total number of digits (before or after the decimal separator) is
#' greater than 15. See [gdata::write.fwf()] (function used to
#' writing the asc file) for more details. \cr
#'
#' (Le paramètre `decimals` indique le nombre minimal de décimales à faire
#' figurer dans le fichier en sortie (quel que soit le nombre de décimales
#' effectivement présent dans `microdata`). Il s'applique à toutes les
#' variables réelles (double) mais pas aux variables entières (integer). Pour
#' ajouter des zéros à une variable entière, la convertir avec `as.double`
#' au préalable.
#'
#' Les chiffres après la virgule peuvent être incorrects dans le fichier asc si
#' le nombre total de chiffres (avant ou après le séparateur décimal) est
#' supérieur à 15. Voir [gdata::write.fwf()] (fonction utilisée pour
#' écrire le fichier asc) pour plus de détails.)
#'
#' @section See also:
#'
#' The function [micro_rtauargus()], which uses this
#' function and inherits its parameters. \cr
#' (La fonction [micro_rtauargus()], qui utilise cette
#' fonction et hérite de ses paramètres.)
#'
#' @examples
#' # dummy data/donnees fictives
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
#' # Content of output files/visualisation des fichiers produits
#' file.show(
#'   res$asc_filename, res$rda_filename,
#'   header = unlist(res),
#'   pager = "internal"
#' )
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom purrr transpose
#' @importFrom rlang .data
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

  # valeur par défaut du package si option vide ...........................

  if (is.null(decimals)) decimals <- op.rtauargus$rtauargus.decimals
  if (is.null(hierleadstring)) {
    hierleadstring <- op.rtauargus$rtauargus.hierleadstring
  }
  if (is.null(totcode)) totcode <- op.rtauargus$rtauargus.totcode
  if (is.null(missing)) missing <- op.rtauargus$rtauargus.missing

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
      ordre_init = dplyr::row_number()
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

  if (!is.null(hrc) & (is.null(names(hrc)) | any(names(hrc) == ""))) {
    stop("noms manquants pour hrc. Exemple : hrc = c(VAR = \"var.hrc\")")
  }

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
    arrange(.data$ordre_init) %>%
    dplyr::select(-dplyr::all_of(c("ordre_init", "nlevels", "exp")))

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
