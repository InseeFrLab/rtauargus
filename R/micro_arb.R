specify_tables_one <- function(expl, resp, shad, cost) {
  paste0(
    paste(cite(expl), collapse = ""), '|',
    cite(resp), '|',
    cite(shad), '|',
    cite(cost)
  )
}

# Genere partie SPECIFYTABLE et SAFETYRULE
specif_safety <- function(explanatory_vars,
                          response_var,
                          shadow_var,
                          cost_var,
                          safety_rules,
                          weighted) {

  specify_tables <-
    mapply(
      specify_tables_one,
      explanatory_vars,
      response_var,
      shadow_var,
      cost_var
    )

  specify_tables <- paste("<SPECIFYTABLE>", specify_tables)

  if (!is.null(names(explanatory_vars))) {
    table_ids <- sprintf('// <TABLE_ID> "%s"', names(explanatory_vars))
    specify_tables <- paste(table_ids, specify_tables, sep = "\n")
  }

  safety_rules <- paste("<SAFETYRULE>", safety_rules)
  sr_weight <- mapply(
    function(s, w) paste0(s, if (w) "|Wgt(1)"),
    safety_rules,
    weighted
  )

  mapply(paste, specify_tables, sr_weight, sep = "\n", USE.NAMES = FALSE)

}

# Genere partie SUPPRESS et WRITETABLE
suppr_writetable <- function(suppress,
                             linked,
                             output_names,
                             output_type,
                             output_options) {

  # numero table
  num_table <- seq_along(output_names)

  if (linked & length(suppress) > 1) {
    stop("un seul suppress permis quand linked = TRUE")
  }

  # methode suppr
  if (length(suppress) == 1) {
    if (linked) suppr_n <- 0 else suppr_n <- num_table
    suppress <- gsub(" ", "", suppress) # supprime tout espace
    suppress_fmt <- sub("\\([^),]+(,*.*)\\)", "(%i\\1)", suppress)
    suppress <- sprintf(suppress_fmt, suppr_n)
  }

  # chemin complet sorties
  output_names <- normPath2(output_names)

  # commandes batch
  suppr_cmd <- paste("<SUPPRESS>", suppress)
  write_cmd <-
    paste0(
      '<WRITETABLE> (',
      num_table, ',',
      output_type, ',',
      output_options, ',"',
      output_names, '")'
    )

  if (linked) {
    c(
      suppr_cmd,
      mapply(paste, write_cmd, sep = "\n", USE.NAMES = FALSE)
    )
  } else {
    mapply(paste, suppr_cmd, write_cmd, sep = "\n", USE.NAMES = FALSE)
  }


}


# Paramètre a priori ------------------------------------------------------

# Normalise parametre "apriori" pour le faire passer dans la
# fonction apriori_batch

norm_apriori_params <- function(params) {

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

apriori_batch <- function(ntab, hst_names, sep = ',', ignore_err = 0 , exp_triv = 0) {

  # verif pour éviter recyclage qd longueurs args > 1 ne correspondent pas
  n <- lengths(list(hst_names, sep, ignore_err, exp_triv))
  if (any(ntab > 1 & n > 1 & n != ntab)) stop("longueur arguments")
  if (any(ntab == 1 & n != 1)) stop("longueur arguments")

  paste0(
    '<APRIORI> "',
    normPath2(hst_names), '",',
    seq(ntab), ',"',
    sep, '",',
    ignore_err, ',',
    exp_triv
  )

}


# Exportée ----------------------------------------------------------------

#' Creates a batch file (.arb) for microdata
#'
#' Creates a batch file for microdata, executable by Tau-Argus in
#' command line. \cr
#' (Crée un fichier batch pour microdonnées, exécutable par Tau-Argus en ligne
#' de commande.)
#'
#' The function does not check if asc and rda files exist. \cr
#' (La fonction ne vérifie pas si les fichiers asc et rda existent.)
#'
#' @section Syntax:
#' Tau-Argus can handle multiple tabs for the same set of
#' microdata. Passing a single value for an option will apply the same
#' processing to each tab. For differentiated options, passing a
#' vector containing as many values as there are tabs. If the lengths do not
#' match (not recommended), a recycling is performed.
#'
#' Unless otherwise stated, use the syntax mentioned in the documentation
#' of Tau-Argus.
#'
#' Special syntax for `suppress` : the first parameter in the
#' Tau-Argus syntax is the tab number. If the method is identical
#' for all tabs, this first parameter will be ignored and the numbers
#' are automatically recalculated for the batch. In the writing
#' `suppress = "GH(n,100)"`, n will thus be transformed into 1 for the
#' first tab, into 2 for the second tab, etc.
#'
#' (Tau-Argus peut traiter plusieurs tabulations pour un même jeu de
#' microdonnées. Passer une seule valeur pour une option appliquera le même
#' traitement à chaque tabulation. Pour des options différenciées, passer un
#' vecteur contenant autant de valeurs que de tabulations. Si les longueurs ne
#' correspondent pas (déconseillé), un recyclage est effectué.
#'
#' Sauf mention contraire, utiliser la syntaxe mentionnée dans la documentation
#' de Tau-Argus.
#'
#' Syntaxe spéciale pour `suppress` : le premier paramètre dans la
#' syntaxe Tau-Argus est le numéro de la tabulation. Si la méthode est identique
#' pour toutes les tabulations, ce premier paramètre sera ignoré et les numéros
#' recalculés automatiquement pour le batch. Dans l'écriture
#' `suppress = "GH(n,100)"`, n sera ainsi transformé en 1 pour la première
#' tabulation, en 2 pour la deuxième tabulation, etc.)
#'
#' @section Table identifiers:
#' If the list `explanatory_vars` has names, these will be
#' used in the batch to give an identifier to the table, in the form of
#' of a comment line (`// <TABLE_ID> "..."`). They will be
#' reused by the `import` function to name the R format arrays
#' tables in output.
#'
#' (Si la liste `explanatory_vars` comporte des noms, ceux-ci seront
#' utilisés dans le batch pour donner un identifiant au tableau, sous la forme
#' d'une ligne de commentaire (`// <TABLE_ID> "..."`). Ils seront
#' réutilisés par la fonction `import` pour nommer les tableaux formats R
#' en sortie.)
#'
#' @section Use an apriori file:
#' It is possible to provide an apriori file (.hst) for each tabulation.
#'
#' The easiest way is to pass a vector containing as many
#' hst files as there are tabs. If the file is the same for all
#' tabs, specify the name of this file (it will be used for all
#' tabs).
#'
#' The additional options are optional. To change the default values,
#' pass a list with the hst file(s) as the first item and
#' complete with the elements having the names `sep` for the separator,
#' `ignore_err` for IgnoreError and `exp_triv` for ExpandTrivial.
#' As for filenames, specify only one value per parameter or
#' as many values as there are tabs.

#' (Il est possible de fournir un fichier apriori (.hst) pour chaque tabulation.
#'
#' La manière la plus simple est de passer un vecteur contenant autant de noms
#' de fichiers hst que de tabulations. Si le fichier est le même pour toutes les
#' tabulations, spécifier le nom de ce fichier (il sera utilisé pour toutes les
#' tabulations).
#'
#' Les options supplémentaires sont facultatives. Pour modifier les valeurs par
#' défaut, passer une liste ayant comme premier élément le(s) fichier(s) hst et
#' compléter avec les éléments portant les noms `sep` pour le séparateur,
#' `ignore_err` pour IgnoreError et `exp_triv` pour ExpandTrivial.
#' Comme pour les noms de fichiers, spécifier une seule valeur par paramètre ou
#' autant de valeurs que de tabulations.)
#'
#' @param arb_filename name of the generated arb file (with
#' extension). If not specified, a temporary file. \cr
#' (nom du fichier arb généré (avec extension). Si non renseigné, un fichier
#' temporaire.)
#' @param asc_filename  name of the asc file
#' (with extension). \cr
#' ( nom du fichier asc (avec extension).)
#' @inheritParams micro_asc_rda
#' @param explanatory_vars  categorical variables, in
#' form of a list of vectors. Each element of the list is a vector of
#' variable names forming a tab.
#' Example: `list(c("CJ", "A21"), c("SEX", "REGION"))` for the first
#' table crossing `CJ` x `A21` and the second table crossing
#' `SEXE` x `REGION`.
#' If a single tabulation, a simple vector of the variables to be crossed is
#' accepted (no need for `list(...)`). \cr
#' ( variables catégorielles, sous
#' forme de liste de vecteurs. Chaque élément de la liste est un vecteur des
#' noms des variables formant une tabulation.
#' Exemple: `list(c("CJ", "A21"), c("SEX", "REGION"))` pour la première
#' table croisant `CJ` x `A21` et la seconde croisant
#' `SEXE` x `REGION`
#' Si une seule tabulation, un simple vecteur des variables à croiser est
#' accepté (pas besoin de `list(...)`).)
#' @param response_var response variable to be summed, or counted if
#' `"<freq>"`. A single value or as many values as there are tabs. \cr
#' (variable de réponse à sommer, ou comptage si `"<freq>"`.
#' Une seule valeur ou autant de valeurs que de tabulations.)
#' @param shadow_var variable(s) for applying the primary secret. If not
#' filled in, `response_var` will be used by Tau-Argus. \cr
#' (variable(s) pour l'application du secret primaire. Si non
#' renseigné, `response_var` sera utilisé par Tau-Argus.)
#' @param cost_var cost variable(s) for the secondary secret. \cr
#' (variable(s) de coût pour le secret secondaire.)
#' @param safety_rules  primary secret rule(s).
#' String in Tau-Argus batch syntax. The weighting is treated
#' in a separate parameter (do not specify WGT here, use the
#' `weighted`). \cr
#' ( règle(s) de secret primaire.
#' Chaîne de caractères en syntaxe batch Tau-Argus. La pondération est traitée
#' dans un paramètre à part (ne pas spécifier WGT ici, utiliser le paramètre
#' `weighted`).)
#' @param weighted indicator(s) (boolean). \cr
#' (indicatrice(s) de pondération (booléen).)
#' @param suppress  secret management method(s)
#' secondary (Tau-Argus batch syntax). If the method is the same for
#' each tabulation, the first parameter (table number) will be ignored and
#' renumbered automatically (see section 'Syntax'). \cr
#' ( méthode(s) de gestion du secret
#' secondaire (syntaxe batch de Tau-Argus). Si la méthode est la même pour
#' chaque tabulation, le premier paramètre (numéro du tableau) sera ignoré et
#' renuméroté automatiquement (voir la section 'Syntax').)
#' @param linked to process the secondary secret jointly on all
#' tabs. Only one delete command is allowed in this case (applied to all
#' all tables). \cr
#' (pour traiter le secret secondaire conjointement sur toutes les
#' tabulations. Une seule commande suppress autorisée dans ce cas (appliquée à
#' tous les tableaux).)
#' @param output_names names of output files. If filled in,
#' the number of file names must be the same as the number of tabs. If left
#' empty, as many temporary file names as there are tabs will be
#' generated. \cr
#' (noms des fichiers en sortie. Si renseigné, obligatoirement autant de noms
#' de fichiers que de tabulations. Si laissé
#' vide, autant de noms de fichiers temporaires que de tabulations seront
#' générés.)
#' @param output_type format of output files (Tau-Argus codification).
#' Default value of the package: `"2"` (csv for pivot-table). \cr
#' (format des fichiers en sortie (codification Tau-Argus).
#' Valeur par défaut du package : `"2"` (csv for pivot-table).)
#' @param output_options additional options for output files. default value of
#' the package: `"AS+"` (status display). To specify no option, `""`. \cr
#' (options supplémentaires des fichiers en sortie. Valeur
#' par défaut du package : `"AS+"` (affichage du statut). Pour ne
#' spécifier aucune option, `""`.)
#' @param apriori information file(s) *a priori*. See below
#' for the syntax. \cr
#' (fichier(s) d'informations *a priori*. Voir ci-dessous
#' pour la syntaxe.)
#' @param gointeractive to have the possibility to launch the batch from the
#' menu of Tau-Argus. \cr
#' (pour avoir la possibilité de lancer le batch depuis le
#' menu de Tau-Argus (`FALSE` par défaut).)
#'
#' @return A list of two elements: arb filename and names of output files
#' (useful to get back the randomly generated names) \cr
#' (Une liste de deux éléments : le nom du fichier arb, les noms des
#' fichiers en sortie (utile pour récupérer les noms générés aléatoirement).)
#'
#' @inheritSection micro_asc_rda See also
#'
#' @examples
#' # creation of the .arb file
#' infos_arb <- micro_arb(
#'   asc_filename = "donnees.asc",
#'   explanatory_vars = list(c("REGION", "CJ"), c("REGION")),
#'   response_var = c("CA", "<freq>"),
#'   safety_rules = c("NK(1,85)|FREQ(3,10)", "FREQ(3,10)"),
#'   suppress = "GH(.,100)",
#'   output_names = c("tab1.csv", "~/tab2.csv"),
#'   output_options = c("AS+SE+", "SE+"),
#'   output_type = "2"
#' )
#'
#' # Content of the created file visible in the R console
#' # (Visualisation du contenu du fichier dans la console)
#' file.show(infos_arb$arb_filename, pager = "console")
#' @export

micro_arb <- function(arb_filename     = NULL,
                      asc_filename,
                      rda_filename     = NULL,
                      explanatory_vars,
                      response_var     = getOption("rtauargus.response_var"),
                      shadow_var       = NULL,
                      cost_var         = NULL,
                      safety_rules,
                      weighted         = getOption("rtauargus.weighted"),
                      suppress,
                      linked           = getOption("rtauargus.linked"),
                      output_names     = NULL,
                      output_type      = getOption("rtauargus.output_type"),
                      output_options   = getOption("rtauargus.output_options"),
                      apriori          = NULL,
                      gointeractive    = FALSE) {

  # valeur par défaut du package si option vide
  if (is.null(response_var)) response_var <- op.rtauargus$rtauargus.response_var
  if (is.null(weighted)) weighted <- op.rtauargus$rtauargus.weighted
  if (is.null(linked)) linked <- op.rtauargus$rtauargus.linked
  if (is.null(output_type)) output_type <- op.rtauargus$rtauargus.output_type
  if (is.null(output_options)) {
    output_options <- op.rtauargus$rtauargus.output_options
  }

  # si une seule tabulation, vecteur autorisé
  if (is.atomic(explanatory_vars)) explanatory_vars <- list(explanatory_vars)
  nb_tabul <- length(explanatory_vars)

  # parametres non renseignés
  if (is.null(arb_filename)) arb_filename <- tempfile("RTA_", fileext = ".arb")
  if (is.null(rda_filename)) rda_filename <- sub("asc$", "rda", asc_filename)
  if (is.null(shadow_var)) shadow_var <- ""
  if (is.null(cost_var)) cost_var <- ""
  if (is.null(output_names)) {
    if (length(output_type) == 1) {
      ext <- rep(output_extensions[output_type], nb_tabul)
    } else {
      stopifnot(length(output_type) == nb_tabul)
      ext <- output_extensions[output_type]
    }
    output_names <- tempfile("RTA_", fileext = ext)
  }
  if (is.null(output_options)) output_options <- ""

  # correspondance nombre tab et nombre fichiers sortie
  if (length(explanatory_vars) != length(output_names)) {
    stop("renseigner autant de noms de fichiers que de tabulations")
  }

  # interdit 'WGT' dans safety_rules
  if (length(grep("WGT", safety_rules, ignore.case = TRUE))) {
    stop(
      "ne pas renseigner WGT dans 'safety_rules', ",
      "utiliser le parametre 'weighted' pour la ponderation"
    )
  }

  # output_names doivent comporter une extension de fichier
  # (sinon Tau-Argus plante)
  if (!all(grepl("\\.", basename(output_names)))) {
    stop("output_names doivent comporter une extension de fichier")
  }

  # chemins absolus
  asc_full <- normPath2(asc_filename)
  rda_full <- normPath2(rda_filename)

  res <- character(0)

  # commentaire
  res[1] <- "// Batch generated by package *rtauargus*"
  res[2] <- paste0("// (", format(Sys.time(), "%Y-%m-%d %X %Z)"))

  # open...
  res[3] <- sprintf('<OPENMICRODATA> "%s"', asc_full)
  res[4] <- sprintf('<OPENMETADATA> "%s"', rda_full)

  # tabulations + secret primaire
  tab_sp <-
    specif_safety(
      explanatory_vars = explanatory_vars,
      response_var = response_var,
      shadow_var = shadow_var,
      cost_var = cost_var,
      safety_rules = safety_rules,
      weighted = weighted
    )
  res <- c(res, tab_sp)

  # read...
  res <- c(res, "<READMICRODATA>")

  # apriori (doit figurer avant suppress)
  if (!is.null(apriori)) {

    std_apriori <- norm_apriori_params(apriori)
    ap_batch <-
      apriori_batch(
        ntab = length(explanatory_vars),
        hst_names = std_apriori[[1]],
        sep = std_apriori$sep,
        ignore_err = std_apriori$ignore_err,
        exp_triv = std_apriori$exp_triv
      )

    res <- c(res, ap_batch)

  }

  # suppress + writetable
  sw <-
    suppr_writetable(
      suppress,
      linked,
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
