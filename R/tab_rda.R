
write_rda_1var_tab <- function(info_var) {

  # écrit partie du .rda à partir des infos (une liste) pour une seule variable

  ligne1 <- with(info_var,
                 paste(
                   colname,
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
      if (type_var %in% c("NUMERIC", "WEIGHT","MAXSCORE"))
        paste0("  <DECIMALS> ", digits)
    )
  )

}

#' @importFrom dplyr %>%
write_rda_tab <- function(info_vars) {


  # écrit les infos format .rda pour toutes les variables
  # (info_vars est une liste contenant les infos pour chaque variable)

  chemin_complet <- function(x) {
    if (!is.na(x$codelist)) x$codelist <- normPath2(x$codelist)
    return(x)
  }
  info_vars <- lapply(info_vars, chemin_complet)

  vapply(info_vars, write_rda_1var_tab, character(1)) %>%
    gsub("(\n)+", "\n", .) %>% # plusieurs sauts de lignes par un seul
    sub("\n$", "", .) # supprime dernier saut de ligne

}

#' Crée les fichiers rda à partir de données tabulées
#'
#' Crée un fichier tabular (tab) et de métadonnées
#' (rda) à partir de données tabulées et d'informations additionnelles.
#'
#' @param tabular [\strong{obligatoire}] data.frame contenant les
#'   données tabulées
#' @param tab_filename nom du fichier tab (avec extension)
#' @param rda_filename nom du fichier rda (avec extension).
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
#' @param value nom de la colonne contenant la valeur des cellules
#' @param freq  nom de la colonne contenant les effectifs pour une cellule
#' @param maxscore nom de la colonne contenant la valeur du plus gros contributeur
#' d'une cellule
#' @param separator charactere utilisé en tant que separateur dans le fichier .tab
#' @return Renvoie le nom du fichier rda sous forme de liste (de
#'   manière invisible).
#'
#' @section Paramètres spécifiques:
#'
#' Les paramètres \code{totcode}, \code{missing} et \code{codelist}
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
#' \code{missing} et \code{codelist} (vecteur nommé contenant autant d'éléments
#' que de variables à décrire). Il présente de plus la particularité
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
#' existant (\code{c(A38 = "a38.hrc")}), mais aussi passer un appel à
#' \code{\link{write_hrc}} qui génèrera un fichier hrc à partir de données tabulées.
#'
#' \emph{Exemple :} \code{c(A38 = write_hrc(tabular, c("A38", "A21", "A10")))}
#'
#'
#' Les deux dernières méthodes passent par la création d'un fichier temporaire.
#' Pour un fichier hrc réutilisable, il est nécessaire de le créer au préalable
#' à l'aide de \code{write_hrc}.
#'
#' Les trois méthodes nécessitent que les éléments du vecteur en paramètre
#' soient nommés (avec le nom de la variable), même s'il n'y a qu'un seul
#' élément.
#'
#' @section Nombre de décimales:
#'
#' Le paramètre \code{decimals} indique le nombre minimal de décimales à faire
#' figurer dans le fichier en sortie (quel que soit le nombre de décimales
#' effectivement présent dans \code{tabular}). Il s'applique à toutes les
#' variables réelles (double) mais pas aux variables entières (integer). Pour
#' ajouter des zéros à une variable entière, la convertir avec \code{as.double}
#' au préalable.
#'
#'
#' @section Voir aussi: La fonction \code{\link{rtauargus}}, qui utilise cette
#' fonction et hérite de ses paramètres.
#'
#' @examples
#' \dontrun{
#' # donnees fictives
#' tab_df <-
#'   data.frame(
#'     GEO      = c("443", "541", "543"),
#'     A10      = c( "AZ",  "BE",  "BE"),
#'     A21      = c(  "A",   "B",   "C"),
#'     DIPL     = c( "01",  "??",  "02"),
#'     CA       = c(  100,     0,     7),
#'     effectif = c(    3,     6,     8),
#'     max_CA   = c(   57,     0,     1),
#'   )
#'
#' # creation rda
#' res <-
#'   tab_rda(
#'     tabular  = tab_df,
#'     decimals   = 1,
#'     hrc        = c(A21 = "A21>A10", GEO = "2 1 0", DIPL = "dipl.hrc"),
#'     totcode    = c(GEO = "France"),
#'     missing    = c(DIPL = "??"),
#'     value = "CA",
#'     freq = "effectif",
#'     maxscore = "max_CA",
#'     separator = ","
#'   )
#'
#' # visualisation des fichiers produits
#' file.show(
#'  res$rda_filename,
#'   header = unlist(res),
#'   pager = "internal"
#' )
#' }
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom purrr transpose
#' @importFrom rlang .data
#'
#' @export

tab_rda <- function(
	tabular,
	tab_filename   = NULL,
	rda_filename   = NULL,
	decimals       = getOption("rtauargus.decimals"),
	hrc            = NULL,
	hierleadstring = getOption("rtauargus.hierleadstring"),
	totcode        = getOption("rtauargus.totcode"),
	missing        = getOption("rtauargus.missing"),
	codelist       = NULL,
	value          = NULL,
	freq           = NULL,
	maxscore       = NULL,
	separator      = ","
) {

  tabular <- as.data.frame(tabular) # (probleme avec tibble notamment)

  # valeur par défaut du package si option vide ...........................

  if (is.null(decimals)) decimals <- op.rtauargus$rtauargus.decimals
  if (is.null(hierleadstring)) {
    hierleadstring <- op.rtauargus$rtauargus.hierleadstring
  }
  if (is.null(totcode)) totcode <- op.rtauargus$rtauargus.totcode
  if (is.null(missing)) missing <- op.rtauargus$rtauargus.missing

  # ignore colonnes de longueurs nulles  ..................................

  colvides <- sapply(tabular, function(x) all(is.na(x)) | all(x == ""))
  if (any(colvides)) {
    warning(
      "Colonnes vides : ",
      paste(names(tabular)[colvides], collapse = ", ")
    )
    tabular <- tabular[!colvides]
  }

  # parametres non renseignés  ...........................................

  if (is.null(rda_filename)) rda_filename <- tempfile("RTA_", fileext = ".rda")

  # genere fichier longueur fixe et infos associees  .....................

  fwf_info_tabular <-
    gdata::write.fwf(
      tabular,
	  file = tab_filename,
      formatInfo = TRUE,
      colnames = FALSE,
      justify = "right", # pour les variables caractères uniquement
      digits = 15, # max ? voir aide de format
      nsmall = decimals,
      scientific = FALSE,
	    sep=separator
    )

  num <- vapply(tabular, is.numeric, logical(1))

  fwf_info_tabular <-
    fwf_info_tabular %>%
    mutate(
      type_var = ifelse(num, "NUMERIC", "RECODEABLE"),
      ordre_init = dplyr::row_number()
    )

  # weight_var et holding_var ..........................................

  if (!is.null(freq)) {
    fwf_info_tabular$type_var[fwf_info_tabular$colname == freq] <- "FREQUENCY"
  }

  if (!is.null(maxscore)) {
    fwf_info_tabular$type_var[fwf_info_tabular$colname == maxscore] <- "MAXSCORE"
  }

  # missing, totcode, codelist  ........................................

  var_quanti <- names(tabular)[!num]

  missing_df  <- df_param_defaut(names(tabular), "missing", missing)
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
      tabular,
      hierleadstring = hierleadstring
    )

  hrc_df <- df_param_defaut(var_quanti, "hierarchical", norm_hrc)
  hrc_df$hierleadstring <- NA_character_
  need_leadstring <- grepl("\\.hrc$", hrc_df$hierarchical)
  hrc_df$hierleadstring[need_leadstring] <- hierleadstring

  fwf_info_tabular <-
    purrr::reduce(
      list(fwf_info_tabular, missing_df, totcode_df, codelist_df, hrc_df),
      merge,
      by = "colname",
      all.x = TRUE
    )

  # reordonne (car tri par merge)  ...................................

  fwf_info_tabular <-
    fwf_info_tabular %>%
    arrange(.data$ordre_init) %>%
    dplyr::select(
      -dplyr::all_of(c("ordre_init", "nlevels", "exp","position","width"))
    )

  # reorganise en une liste de variables .............................
  fwf_info_tabular <-transpose(fwf_info_tabular)

  # genere vecteur format .rda .......................................
  res <- character(0)
  # instructions
  res[1] <- sprintf('   <SEPARATOR> "%s"',separator)
  res[2] <- sprintf('   <SAFE> "s"')
  res[3] <- sprintf('   <UNSAFE> "u"')
  res[4] <- sprintf('   <PROTECT> "p"')

  res <- c(res,write_rda_tab(fwf_info_tabular))

  # écrit fichier texte ..............................................
  writeLines(res, rda_filename)

  # renvoie noms des fichiers tab et rda de manière invisible .......
  invisible(
    list(
		tab_filename = normPath2(tab_filename),
		rda_filename = normPath2(rda_filename)
	)
  )


}

