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

#' Crée un fichier batch (.arb) pour microdonnées
#'
#' Crée un fichier batch pour microdonnées, exécutable par Tau-Argus en ligne de
#' commande.
#'
#' Tau-Argus peut traiter jusqu'à 10 tabulations pour un même jeu de
#' microdonnées. Passer une seule valeur pour une option appliquera le même
#' traitement à chaque tabulation. Pour des options différenciées, passer un
#' vecteur contenant autant de valeurs que de tabulations. Si les longueurs ne
#' correspondent pas (déconseillé), un recyclage est effectué.
#'
#' Sauf mention contraire, utiliser la syntaxe mentionnée dans la documentation
#' de Tau-Argus.
#'
#' Syntaxe spéciale pour \code{suppress} : le premier paramètre dans la
#' syntaxe Tau-Argus est le numéro de la tabulation. Si la méthode est identique
#' pour toutes les tabulations, ce premier paramètre sera ignoré et les numéros
#' recalculés automatiquement pour le batch. Dans l'écriture
#' \code{suppress = "GH(n,100)"}, n sera ainsi transformé en 1 pour la première
#' tabulation, en 2 pour la deuxième tabulation, etc.
#'
#' La fonction ne vérifie pas si les fichiers asc et rda existent.
#'
#' @section Identifiants des tableaux:
#' Si la liste \code{explanatory_vars} comporte des noms, ceux-ci seront
#' utilisés dans le batch pour donner un identifiant au tableau, sous la forme
#' d'une ligne de commentaire (\code{// <TABLE_ID> "..."}). Ils seront
#' réutilisés par la fonction \code{import} pour nommer les tableaux formats R
#' en sortie.
#'
#' @section Informations \emph{a priori}:
#' Il est possible de fournir un fichier a priori (.hst) pour chaque tabulation.
#'
#' La manière la plus simple est de passer un vecteur contenant autant de noms de
#' fichiers hst que de tabulations. Si le fichier est le même pour toutes les
#' tabulations, spécifier le nom de ce fichier (il sera utilisé pour toutes les
#' tabulations).
#'
#' Les options supplémentaires sont facultatives. Pour modifier les valeurs par
#' défaut, passer une liste ayant comme premier élément le(s) fichier(s) hst et
#' compléter avec les éléments portant les noms \code{sep} pour le séparateur,
#' \code{ignore_err} pour IgnoreError et \code{exp_triv} pour ExpandTrivial.
#' Comme pour les noms de fichiers, spécifier une seule valeur par paramètre ou
#' autant de valeurs que de tabulations.
#'
#' @param arb_filename nom du fichier arb généré (avec
#'   extension). Si non renseigné, un fichier temporaire.
#' @param asc_filename [\strong{obligatoire}] nom du fichier asc (avec extension).
#' @inheritParams micro_asc_rda
#' @param explanatory_vars [\strong{obligatoire}] variables catégorielles, sous
#'   forme de liste de vecteurs. Chaque élément de la liste est un vecteur des
#'   noms des variables formant une tabulation.
#'   Exemple : \code{list(c("CJ", "A21"), c("SEXE", "REGION"))} pour le premier
#'   tableau croisant \code{CJ} x \code{A21} et le deuxième tableau croisant
#'   \code{SEXE} x \code{REGION}.
#'   Si une seule tabulation, un simple vecteur des variables à croiser est
#'   accepté (pas besoin de \code{list(...)}).
#' @param response_var variable de réponse à sommer, ou comptage si
#'   \code{"<freq>"}. Une seule valeur ou autant de valeurs que de tabulations.
#' @param shadow_var variable(s) pour l'application du secret primaire. Si non
#'   renseigné, \code{response_var} sera utilisé par Tau-Argus.
#' @param cost_var variable(s) de coût pour le secret secondaire.
#' @param safety_rules [\strong{obligatoire}] règle(s) de secret primaire (hors
#'   pondération). Chaîne de caractères en syntaxe batch Tau-Argus.
#' @param weighted indicatrice(s) de pondération (booléen).
#' @param suppress [\strong{obligatoire}] méthode(s) de gestion du secret
#'   secondaire (syntaxe batch de Tau-Argus). Si la méthode est la même pour
#'   chaque tabulation, le premier paramètre (numéro du tableau) sera ignoré et
#'   renuméroté automatiquement.
#' @param linked pour traiter le secret secondaire conjointement sur toutes les
#'   tabulations. Une seule commande suppress autorisée dans ce cas (appliquée à
#'   tous les tableaux).
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
#' @return Une liste de deux éléments : le nom du fichier arb, les noms des
#'   fichiers en sortie (utile pour récupérer les noms générés aléatoirement).
#'
#' @examples
#' # donnees fictives
#' micro_df <- data.frame(
#'   REGION = c("44", "11", "11"),
#'       CJ = c("1" , "2" , "1" ),
#'       CA = c( 100,  0  ,  7  )
#'   )
#' # cree les inputs asc et rda (dans dossier temporaire)
#' tmp <- micro_asc_rda(micro_df)
#'
#' # cree fichier arb
#' infos_arb <- micro_arb(
#'   asc_filename = tmp$asc_filename,
#'   explanatory_vars = list(c("REGION", "CJ"), c("REGION")),
#'   response_var = c("CA", "<freq>"),
#'   safety_rules = c("NK(1,85)|FREQ(3,10)", "FREQ(3,10)"),
#'   suppress = "GH(.,100)",
#'   output_names = c("tab1.csv", "~/tab2.csv"),
#'   output_options = c("AS+SE+", "SE+"),
#'   output_type = "2"
#' )
#'
#' # Visualisation du fichier batch dans la console
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
