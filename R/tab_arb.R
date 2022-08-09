specify_tables_one_tab <- function(expl, resp, shad, cost) {
  paste0(
    paste(cite(expl), collapse = ""), '|',
    cite(resp), '|',
    cite(shad), '|',
    cite(cost)
  )
}

# Genere partie SPECIFYTABLE et SAFETYRULE
specif_safety_tab <- function(explanatory_vars,
                              response_var,
                              shadow_var,
                              cost_var,
                              safety_rules
) {

  specify_tables <-
    mapply(
      specify_tables_one_tab,
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

  mapply(paste, specify_tables,safety_rules, sep = "\n", USE.NAMES = FALSE)

}

# Genere partie SUPPRESS et WRITETABLE
suppr_writetable_tab <- function(suppress,
                                 output_names,
                                 output_type,
                                 output_options) {

  # numero table
  num_table <- seq_along(output_names)

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

  mapply(paste, suppr_cmd, write_cmd, sep = "\n", USE.NAMES = FALSE)



}


# Paramètre a priori ------------------------------------------------------

# Normalise parametre "apriori" pour le faire passer dans la
# fonction apriori_batch

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

#' Create batch file (.arb) for tabular data in order to run Tau Argus
#' Crée un fichier batch (.arb) pour données tabulées exécutable par
#' Tau-Argus en ligne de commande.
#'
#' Function doesn't check if the asc and rda files exists
#' La fonction ne vérifie pas si les fichiers asc et rda existent.
#'
#' @section Syntaxe:
#'
#' For tabular data, the function secure one table
#' Pour les données tabulées la fonction traite un tableau à la fois
#'
#' Unless specific details, use syntaxe from Tau-Argus manual
#' Sauf mention contraire, utiliser la syntaxe mentionnée dans la documentation
#' de Tau-Argus.
#'
#' Syntaxe spéciale pour \code{suppress} :
#' First parameter is the table number, in this case it will always be 1
#' le premier paramètre dans la syntaxe Tau-Argus est le numéro de la tabulation.
#' Dans le cas de cette fonction ce sera toujours 1
#'
#'
#' @section Informations \emph{apriori}:
#' It's possible to add an apriori file (.hst) for a tabular, it can be generated
#' by the table_rda() function
#' Il est possible de fournir un fichier apriori (.hst) pour chaque tabulation,
#' il peut être fourni par la fonction table_rda()
#'
#' Other options are not mandatory. To modify default value use a vector with the
#' .hst file path as first element and then complete with those parameters :
#' \code{sep} for the separator,
#' \code{ignore_err} for IgnoreError and \code{exp_triv} for ExpandTrivial.
#'
#' Les options supplémentaires sont facultatives. Pour modifier les valeurs par
#' défaut, passer une liste ayant comme premier élément le(s) fichier(s) hst et
#' compléter avec les éléments portant les noms \code{sep} pour le séparateur,
#' \code{ignore_err} pour IgnoreError et \code{exp_triv} pour ExpandTrivial.
#' Comme pour les noms de fichiers, spécifier une seule valeur par paramètre ou
#' autant de valeurs que de tabulations.
#'
#' @param arb_filename path of the arb filename, if unspecified, creates a temporary
#' file
#' nom du fichier arb généré (avec extension). Si non renseigné, un fichier temporaire.
#' @param tab_filename [\strong{obligatoire}] path of the tab_filename, mandatory
#' nom du fichier .tab (avec extension).
#' @inheritParams tab_rda
#' @param explanatory_vars [\strong{obligatoire}] explanatory vars in a vector
#'  variables catégorielles, sous forme de vecteur.
#'   Example : \code{c("CJ", "A21")} for the tabular \code{CJ} x \code{A21}
#'   Exemple : \code{c("CJ", "A21")} pour le premier
#'   tableau croisant \code{CJ} x \code{A21}
#' @param response_var response variable name in the tabular
#'   \code{"<freq>"}. Une seule valeur.
#' @param shadow_var variable for primary secret. If unspecified, \code{response_var}
#' will be used by Tau Argus
#'   variable(s) pour l'application du secret primaire. Si non
#'   renseigné, \code{response_var} sera utilisé par Tau-Argus.
#' @param cost_var variable(s) de coût pour le secret secondaire.
#' @param safety_rules [\strong{obligatoire}] règle(s) de secret primaire.
#'   Chaîne de caractères en syntaxe batch Tau-Argus. La pondération est traitée
#'   dans un paramètre à part (ne pas spécifier WGT ici, utiliser le paramètre
#'   \code{weighted}).
#' @param suppress [\strong{obligatoire}] méthode(s) de gestion du secret
#'   secondaire (syntaxe batch de Tau-Argus). Si la méthode est la même pour
#'   chaque tabulation, le premier paramètre (numéro du tableau) sera ignoré et
#'   renuméroté automatiquement (voir la section 'Syntaxe').
#' @param output_names noms des fichiers en sortie. Si renseigné,
#'   obligatoirement autant de noms de fichiers que de tabulations. Si laissé
#'   vide, autant de noms de fichiers temporaires que de tabulations seront
#'   générés.
#' @param output_type format des fichiers en sortie (codification Tau-Argus).
#'   Valeur par défaut du package : \code{"2"} (csv for pivot-table).
#' @param output_options options supplémentaires des fichiers en sortie. Valeur
#'   par défaut du package : \code{"AS+"} (affichage du statut). Pour ne
#'   spécifier aucune option, \code{""}.
#' @param apriori fichier(s) d'informations \emph{a priori}. Voir ci-dessous
#'   pour la syntaxe.
#' @param gointeractive pour avoir la possibilité de lancer le batch depuis le
#'   menu de Tau-Argus (\code{FALSE} par défaut).
#'
#'
#' @return Une liste de deux éléments : le nom du fichier arb, les noms des
#'   fichiers en sortie (utile pour récupérer les noms générés aléatoirement).
#'
#'
#' @inheritSection tab_rda See also
#'
#'
#' @examples
#' \dontrun{
#' # creating arb file
#' infos_arb <- tab_arb(
#'   tab_filename = "donnees.tab",
#'   explanatory_vars = c("REGION", "CJ"),
#'   response_var = c("CA", "<freq>"),
#'   safety_rules = c("NK(1,85)|FREQ(3,10)"),
#'   suppress = "GH(1,100)",
#'   output_names = c("tab1.csv"),
#'   output_options = "AS+SE+",
#'   output_type = "2"
#' )
#' # show the content of the file in console
#' # visualisation du contenu du fichier dans la console
#' file.show(infos_arb$arb_filename, pager = "console")
#' }
#' @export

tab_arb <- function(arb_filename     = NULL,
                    tab_filename,
                    rda_filename     = NULL,
                    explanatory_vars,
                    response_var     = getOption("rtauargus.response_var"),
                    shadow_var       = NULL,
                    cost_var         = NULL,
                    safety_rules,
                    suppress,
                    output_names     = NULL,
                    output_type      = getOption("rtauargus.output_type"),
                    output_options   = getOption("rtauargus.output_options"),
                    apriori          = NULL,
                    gointeractive    = FALSE) {

  # valeur par défaut du package si option vide
  if (is.null(response_var)) response_var <- op.rtauargus$rtauargus.response_var
  if (is.null(output_type)) output_type <- op.rtauargus$rtauargus.output_type
  if (is.null(output_options)) {
    output_options <- op.rtauargus$rtauargus.output_options
  }

  # si une seule tabulation, vecteur autorisé
  if (is.atomic(explanatory_vars)) explanatory_vars <- list(explanatory_vars)
  nb_tabul <- length(explanatory_vars)

  # parametres non renseignés
  if (is.null(arb_filename)) arb_filename <- tempfile("RTA_", fileext = ".arb")
  if (is.null(rda_filename)) rda_filename <- sub("tab$", "rda", tab_filename)
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
      "Le poids est applique a la creation de la  table', "
    )
  }

  # output_names doivent comporter une extension de fichier
  # (sinon Tau-Argus plante)
  if (!all(grepl("\\.", basename(output_names)))) {
    stop("output_names doivent comporter une extension de fichier")
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
    specif_safety_tab(
      explanatory_vars = explanatory_vars,
      response_var = response_var,
      shadow_var = shadow_var,
      cost_var = cost_var,
      safety_rules = safety_rules
    )
  res <- c(res, tab_sp)

  # read...
  res <- c(res, "<READTABLE> 1")

  # apriori (doit figurer avant suppress)
  if (!is.null(apriori)) {

    std_apriori <- norm_apriori_params_tab(apriori)
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
