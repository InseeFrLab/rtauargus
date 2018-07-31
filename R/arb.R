cite <- function(x, guillemet = "\"") {

  # met entre guillemet les elements d'un vecteur (sauf si chaîne vide)
  ifelse(
    x == "",
    x,
    paste0(guillemet, x, guillemet)
  )

}

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
                             output_names,
                             output_type,
                             output_options) {

  # numero table
  num_table <- seq_along(output_names)

  # methode suppr
  if (length(suppress) == 1) {
    suppress_fmt <- sub("\\((.+),", "\\(%i,", suppress)
    suppress <- sprintf(suppress_fmt, num_table)
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

  mapply(paste, suppr_cmd, write_cmd, sep = "\n", USE.NAMES = FALSE)

}

#' Crée un fichier batch (.arb) pour microdonnées
#'
#' Crée un fichier batch pour microdonnées, exécutable par Tau Argus en ligne de
#' commande.
#'
#' Tau Argus peut traiter jusqu'à 10 tabulations pour un même jeu de
#' microdonnées. Passer une seule valeur pour une option appliquera le même
#' traitement à chaque tabulation. Pour des options différenciées, passer un
#' vecteur contenant autant de valeurs que de tabulations. Si les longueurs ne
#' correspondent pas (déconseillé), un recyclage est effectué.
#'
#' Sauf mention contraire, utiliser la syntaxe mentionnée dans la documentation
#' de Tau Argus.
#'
#' La fonction ne vérifie pas si les fichiers asc et rda existent.
#'
#' @param arb_filename nom du fichier arb généré (avec
#'   extension). Si non renseigné, un fichier temporaire.
#' @param asc_filename [\strong{obligatoire}] nom du fichier asc (avec extension).
#' @inheritParams micro_asc_rda
#' @param explanatory_vars [\strong{obligatoire}] variables catégorielles, sous
#'   forme de liste de vecteurs. Chaque élément de la liste est un vecteur des
#'   noms de variables indiquant une tabulation.
#'   Exemple : \code{list(c("CJ", "A21"), c("SEXE", "REGION"))} pour le premier
#'   tableau croisant \code{CJ} par \code{A21} et le deuxième tableau croisant
#'   \code{SEXE} par \code{REGION}.
#'   Si une seule tabulation, un vecteur des variables à croiser est accepté.
#' @param response_var variable de réponse, par défaut \code{"<freq>"}
#'   (comptage).
#' @param shadow_var variable(s) pour l'application du
#'   secret primaire. Si non renseigné, \code{response_var} sera utilisé par Tau
#'   Argus.
#' @param cost_var variable(s) de coût pour le secret
#'   secondaire.
#' @param safety_rules [\strong{obligatoire}] règle(s) de secret primaire (hors
#'   pondération).
#' @param weighted indicatrice(s) de pondération
#'   (\code{FALSE} par défaut).
#' @param suppress [\strong{obligatoire}] méthode(s) de gestion du secret
#'   secondaire (syntaxe batch de Tau Argus). Si la méthode est la même pour
#'   chaque tabulation, le premier paramètre (numéro du tableau) sera ignoré et
#'   rempli automatiquement.
#' @param linked [pas encore implémenté]
#' @param output_names noms des fichiers en sortie. Si renseigné,
#'   obligatoirement autant de noms de fichiers que de tabulations. Si laissé
#'   vide, autant de fichiers temporaires que de tabulations.
#' @param output_type format des fichiers en sortie (codification Tau Argus).
#'   Par défaut, \code{2}.
#' @param output_options options supplémentaires des fichiers en sortie, par
#'   exemple \code{"AS+SE+"} (vide par défaut).
#' @param gointeractive pour avoir la possibilité de lancer le batch depuis le
#'   menu de Tau Argus (\code{FALSE} par défaut).
#'
#' @return Une liste de deux éléments : le nom du fichier arb, les noms des
#'   fichiers en sortie.
#'
#' @examples
#' # donnees fictives
#' micro_df <- data.frame(
#'   REGION = c("44", "11", "11"),
#'       CJ = c("1", "2", "1"),
#'       CA = c(100, 0, 7)
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
#'   output_type = 2
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
                      gointeractive    = FALSE) {

  # si une seule tabulation, vecteur autorisé
  if (is.atomic(explanatory_vars)) explanatory_vars <- list(explanatory_vars)
  nb_tabul <- length(explanatory_vars)

  # parametres non renseignés
  if (is.null(arb_filename)) arb_filename <- tempfile("rtauargus_", fileext = ".arb")
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
    output_names <- tempfile("rtauargus_", fileext = ext)
  }
  if (is.null(output_options)) output_options <- ""

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

  # suppress + writetable
  sw <-
    suppr_writetable(
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
