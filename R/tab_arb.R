specify_tables_one_tab <- function(expl, resp, safety_rules) {
  specif_table <- paste0(
    paste(cite(expl), collapse = ""), '|',
    cite(resp), '||'
  )
  specif_table <- paste("<SPECIFYTABLE>", specif_table)
  safety_rules <- paste("<SAFETYRULE>", safety_rules)
  paste(specif_table,safety_rules, sep = "\n")
}


# Genere partie SUPPRESS et WRITETABLE
suppr_writetable_tab <- function(suppress,
                                 output_names,
                                 output_type,
                                 output_options) {

  # chemin complet sorties
  output_names <- normPath2(output_names)

  # commandes batch
  suppr_cmd <- paste("<SUPPRESS>", suppress)
  write_cmd <-
    paste0(
      '<WRITETABLE> (',
      1, ',',
      output_type, ',',
      output_options, ',"',
      output_names, '")'
    )

  paste (suppr_cmd, write_cmd, sep = "\n")



}

# Paramètre a priori ------------------------------------------------------

# Normalise parametre "apriori" pour le faire passer dans la
# fonction tab_apriori_batch

norm_apriori_params_tab <- function(params) {

  if (is.character(params)) {

    # forme simple : vecteur des noms de fichiers
    list(params, sep = ',', ignore_err = 0 , exp_triv = 0)

  } else if (is.list(params)) {

    # options supplémentaires (valeurs par défaut si absents)
    if (is.null(params$sep))        params$sep        <- ","
    if (is.null(params$ignore_err)) params$ignore_err <- 0
    if (is.null(params$exp_triv))   params$exp_triv   <- 0

    params

  }

}

# Génère <APRIORI>s

tab_apriori_batch <- function(hst_names, sep, ignore_err = 0 , exp_triv = 0) {

  paste0(
    '<APRIORI> "',
    normPath2(hst_names), '",',
    1, ',"',
    sep, '",',
    ignore_err, ',',
    exp_triv
  )

}


# Exportée ----------------------------------------------------------------

#' Creates a batch file (.arb) for tabular data in order to run Tau Argus
#'
#' Function doesn't check if the asc and rda files exists\cr
#' (Crée un fichier batch (.arb) pour données tabulées exécutable par
#' Tau-Argus en ligne de commande. La fonction ne vérifie pas
#' si les fichiers asc et rda existent.)
#'
#' @section Syntaxe:
#'
#' For tabular data, this function secures only one table at a time#'
#' Unless specific details, use syntax from Tau-Argus manual \cr
#' Sauf mention contraire, utiliser la syntaxe mentionnée dans la documentation
#' de Tau-Argus.
#'
#' Syntax pour `suppress` :
#' First parameter is the table number, in this case it will always be 1 \cr
#'
#' (Pour les données tabulées la fonction traite un tableau à la fois
#' Syntaxe spéciale pour `suppress` :
#' le premier paramètre dans la syntaxe Tau-Argus est le numéro du tableau.
#' Dans le cas de cette fonction ce sera toujours 1)
#'
#'
#' @section Informations *hst_filename*:
#' It's possible to add an apriori file (.hst) for a tabular, it can be generated
#' by the table_rda() function.
#'
#' Other options are not mandatory. To modify default value use a vector with the
#' .hst file path as first element and then complete with those parameters :
#' `sep` for the separator,
#' `ignore_err` for IgnoreError and `exp_triv` for ExpandTrivial. \cr
#'
#' (Il est possible de fournir un fichier apriori (.hst) pour un tableau,
#' il peut être fourni par la fonction table_rda()
#' Exemple : hst_filename = "mon_chemin/apriori.hst" \cr
#'
#' Les options supplémentaires sont facultatives. Pour modifier les valeurs par
#' défaut, passer une liste ayant comme premier élément le(s) fichier(s) hst et
#' compléter avec les éléments portant les noms `sep` pour le séparateur,
#' `ignore_err` pour IgnoreError et `exp_triv` pour ExpandTrivial.
#' Comme pour les noms de fichiers, spécifier une seule valeur par paramètre ou
#' autant de valeurs que de tabulations.)
#'
#' @param arb_filename path of the arb filename, if unspecified, creates a temporary
#' file \cr
#' (nom du fichier arb généré (avec extension). Si non renseigné, un fichier temporaire.)
#' @param tab_filename  path of the tab_filename.\cr
#' (nom du fichier .tab (avec extension).)
#' @inheritParams tab_rda
#' @param hst_filename Apriori file name, syntax detailed below,
#' Example : hst_filename = "path_to_file/apriori.hst"\cr
#' (fichier(s) d'informations *a priori*. Voir ci-dessous
#' pour la syntaxe. Exemple : hst_filename = "path_to_file/apriori.hst")
#' @param explanatory_vars
#' Explanatory vars in a vector
#' Example : `c("CJ", "A21")` for the tabular `CJ` x `A21` \cr
#' ( variables catégorielles, sous forme de vecteur.
#' Exemple : `c("CJ", "A21")` pour le premier
#' Pour un tableau croisant `CJ` x `A21`)
#' @param value Colname for response variable in the tabular
#' `"<freq>"`. For frequency table \cr
#' (Nom de la variable de réponse dans le tableau
#' `"<freq>"`. Permet de tariter les tableaux de fréquence)
#' @param safety_rules
#' Rules for primary suppression with Argus syntax, if the primary suppression
#' has been dealt with an apriori file specify manual safety range :"MAN(10)"
#' for example.\cr
#' ( Règle(s) de secret primaire.
#' Chaîne de caractères en syntaxe batch Tau-Argus. Si le secret primaire
#' a été traité dans un fichier d'apriori : utiliser "MAN(10)")
#' @param suppress
#' Algortihm for secondary suppression (Tau-Argus batch syntax), and the
#' parameters for it.\cr
#' ( Algorithme de gestion du secret secondaire
#' (syntaxe batch de Tau-Argus), ainsi que les potentiels paramètres associés)
#' @param output_names output file name\cr
#' (nom du fichier en sortie.)
#' @param output_type Type of the output file (Argus codification)
#' By default `"2"` (csv for pivot-table).
#' For SBS files use `"4"`\cr
#' (Format des fichiers en sortie (codification Tau-Argus).
#' Valeur par défaut du package : `"2"` (csv for pivot-table).
#' Pour le format SBS utiliser `"4"`)
#' @param output_options Additionnal parameter for the output,
#' by default : code{"AS+"} (print Status). To specify no options : `""`.\cr
#' (Options supplémentaires des fichiers en sortie. Valeur
#'   par défaut du package : `"AS+"` (affichage du statut). Pour ne
#'   spécifier aucune option, `""`.)
#' @param gointeractive Boolean, if TRUE will open a Tau-Argus window and launch
#' the batch in it (`FALSE` by default). \cr
#' (Possibilité de lancer le batch depuis le menu de Tau-Argus
#'  (`FALSE` par défaut).)
#'
#' @return A list containing two elements :
#'  the arb file name and the output name (usefull if the name is generated randomly) \cr
#'  (Une liste de deux éléments : le nom du fichier arb, le nom
#'   fichiers en sortie (utile pour récupérer les noms générés aléatoirement).)
#'
#'
#' @examples
#' \dontrun{
#' # creating arb file
#' infos_arb <- tab_arb(
#'   arb_filename = "path/tab1.arb",
#'   tab_filename = "path/tab1.tab",
#'   rda_filename = "path/tab1.rda",
#'   explanatory_vars = c("REGION", "CJ"),
#'   value = "CA",
#'   safety_rules = "NK(1,85)|FREQ(3,10)",
#'   suppress = "GH(1,100)",
#'   output_names = "path/tab1.csv",
#'   output_options = "AS+SE+",
#'   output_type = "4"
#' )
#' # show the content of the file in console
#' # visualisation du contenu du fichier dans la console
#' file.show(infos_arb$arb_filename, pager = "console")
#' }
#' @export

tab_arb <- function(
    arb_filename     = NULL,
    tab_filename,
    rda_filename     = NULL,
    hst_filename     = NULL,
    explanatory_vars,
    value     = getOption("rtauargus.response_var"),
    safety_rules,
    suppress,
    separator      = getOption("rtauargus.separator"),
    output_names     = NULL,
    output_type      = getOption("rtauargus.output_type"),
    output_options   = getOption("rtauargus.output_options"),
    gointeractive    = FALSE
){

  # valeur par défaut du package si option vide
  if (is.null(value)) value <- op.rtauargus$rtauargus.value
  if (is.null(output_type)) output_type <- op.rtauargus$rtauargus.output_type
  if (is.null(output_options)) {
    output_options <- op.rtauargus$rtauargus.output_options
  }

  # parametres non renseignés
  if (is.null(arb_filename)) arb_filename <- tempfile("RTA_", fileext = ".arb")
  if (is.null(rda_filename)) rda_filename <- sub("tab$", "rda", tab_filename)
  if (is.null(output_names)) {
    if (length(output_type) == 1) {
      ext <- output_extensions[output_type]
    } else {
      stopifnot(length(output_type) == 1)
      ext <- output_extensions[output_type]
    }
    output_names <- tempfile("RTA_", fileext = ext)
  }
  if (is.null(output_options)) output_options <- ""


  # interdit 'WGT' dans safety_rules
  if (length(grep("WGT", safety_rules, ignore.case = TRUE))) {
    stop(
      "Weight need to be used when the table is created', "
    )
  }

  # output_names doivent comporter une extension de fichier
  # (sinon Tau-Argus plante)
  if (!all(grepl("\\.", basename(output_names)))) {
    stop("output_names needs to specify the type of file expected")
  }

  # chemins absolus
  tab_full <- normPath2(tab_filename)
  rda_full <- normPath2(rda_filename)

  res <- character(0)

  # commentaire
  res[1] <- "// Batch generated by package *rtauargus*"
  res[2] <- paste0("// (", format(Sys.time(), "%Y-%m-%d %X %Z)"))

  # open...
  res[3] <- sprintf('<OPENTABLEDATA> "%s"', tab_full)
  res[4] <- sprintf('<OPENMETADATA> "%s"', rda_full)

  # tabulations + secret primaire
  tab_sp <-
    specify_tables_one_tab(
      expl = explanatory_vars,
      resp = value,
      safety_rules = safety_rules
    )
  res <- c(res, tab_sp)

  # read...
  res <- c(res, "<READTABLE> 1")

  # apriori (doit figurer avant suppress)
  if (!is.null(hst_filename)) {

    std_apriori <- norm_apriori_params_tab(hst_filename)
    ap_batch <-
      tab_apriori_batch(
        hst_names = std_apriori[[1]],
        sep = std_apriori$sep,
        ignore_err = std_apriori$ignore_err,
        exp_triv = std_apriori$exp_triv
      )

    res <- c(res, ap_batch)

  }

  # suppress + writetable
  sw <-
    suppr_writetable_tab(
      suppress,
      output_names,
      output_type,
      output_options
    )
  res <- c(res, sw)

  # gointeractive
  if (gointeractive) res <- c(res, "<GOINTERACTIVE>")

  # ligne vide finale
  res <- c(res, "")

  # ecriture
  writeLines(res, arb_filename)

  # valeurs en sortie
  invisible(
    list(
      arb_filename = normPath2(arb_filename),
      output_names = normPath2(output_names)
    )
  )

}
