# Fonctions utiles ------------------------------------------------------------

read_fixheader <- function(file,
                           fixed_header = NULL,
                           read_function = utils::read.csv,
                           ...) {

  ## lit un fichier avec potentiellement une colonne vide de trop
  ## dans l'en-tete (bug de Tau-Argus 4.1.4 notamment)
  ## si en-tete corrigé non fourni, le déduit de la première ligne du fichier

  if (is.null(fixed_header)) {
    # lit l'en-tete seulement
    header <-
      scan(
        text = readLines(file, n = 1),
        what = "character",
        sep = ",",
        na.strings = "",
        quiet = TRUE
      )
    fixed_header <- stats::na.omit(header)
  }

  # lit sauf ligne 1 en specifiant en-tete corrigé
  read_function(
    file,
    skip = 1,
    header = FALSE,
    col.names = fixed_header,
    ...
  )

}

#' @importFrom purrr map
#' @importFrom purrr map_at
#' @importFrom purrr transpose
#' @importFrom dplyr %>%

specif_decompose <- function(specif) {

  ## extrait les infos contenus dans instructions <SPECIFYTABLE> d'un batch
  ## pour par exemple les utiliser dans reimport des données
  ## param specif : un vecteur de commandes <SPECIFYTABLE>

  # menage debut fin
  specif <- sub("^<SPECIFYTABLE> +", "", trimws(specif))

  # variable comptage
  specif <- sub("<freq>", "Freq", specif)

  # separe differentes parties
  specif <- paste0(specif, "|") # pour dernière valeur vide (cf. ?strsplit)
  specif_split <- strsplit(specif, "|", fixed = TRUE)

  # reclasse par type de variable
  type_var <- c("explanatory", "response", "shadow", "cost")
  res <- transpose(specif_split, type_var)

  # supprime guillements et espaces (sauf dans explanatory)
  res <-
    res %>%
    map_at(
      c("response", "shadow", "cost"),
      ~ gsub("(\"| )", "", .x)
    )

  # separe variables dans explanatory
  res %>%
    map_at(
      "explanatory",
      ~ .x %>%
        stringr::str_match_all('[^" ]+') %>%
        map(~ .[ , 1])
    )

}

# fonction read.csv avec parametres predefinis pour lire output TA
my_readcsv <-
  purrr::partial(
    utils::read.csv,
    stringsAsFactors = FALSE,
    na.strings = "-"
  )

read_ta_output <- function(file, type, expl_resp) {

  ## lit une sortie de Tau-Argus en fonction du type et des options
  ## renseignées

  if (type == "2") {

    res <- read_fixheader(file, read_function = my_readcsv)

  } else if (type == "4") {

    res <- my_readcsv(
      file,
      header = FALSE,
      col.names = c(expl_resp, "N", "Status", "Dom")
    )

  } else {

    warning(
      "impossible d'importer autre chose que ",
      "type \"2\" (csv file for pivot-table), ",
      "type \"4\" (SBS output-format).",
      call. = FALSE
    )
    res <- data.frame()

  }

  if (!is.null(res$Status)) res$Status <- as.character(res$Status)

  res

}

meta_import <- function(data,
                        explanatory,
                        response,
                        shadow,
                        cost,
                        apriori,
                        safetyrule,
                        suppress,
                        output_type,
                        output_options) {

  ## ajoute les meta donnees du batch (atrributs) à un tableau exporté

  # liés ?
  num_tab <- stringr::str_match(suppress, "\\((\\d+)")[ , 2]
  linked <- all(num_tab == 0)

  # remplace num tabl par point
  suppress <- sub("\\(\\d+,", "\\(.,", suppress)

  structure(
    data,
    explanatory_vars = explanatory,
    response_var     = response,
    shadow_var       = if (shadow != "") shadow,
    cost_var         = if (cost != "") cost,
    apriori          = if (!is.na(apriori)) apriori,
    safetyrule       = safetyrule,
    suppress         = suppress,
    linked           = linked,
    output_type      = output_type,
    output_options   = if (output_options != "") output_options
  )

}


# Fonction exportée -----------------------------------------------------------

#' Importe les résultats de Tau-Argus
#'
#' Importe dans R les résultats générés par Tau-Argus à partir des informations
#' contenues dans un fichier arb.
#'
#' Nécessite que le batch ait été exécuté et se soit terminé sans erreur. Afin
#' d'importer immédiatement après exécution du batch, cette fonction sera ainsi
#' le plus souvent appelée via \code{\link{run_arb}} (en paramétrant
#' \code{import = TRUE}).
#'
#' Il n'est possible (pour l'instant) que d'importer les résultats de type "2"
#' (csv for pivot-table) et "4" (sbs). En cas d'impossibilité de l'import pour
#' une tabulation donnée, un data.frame vide est retourné (avec un message
#' d'avertissement).
#'
#' @param arb_filename nom du fichier arb (avec extension) contenant les
#'   informations nécessaires à l'import.
#'
#' @return Une liste d'un ou plusieurs data.frames. Chaque data.frame correspond
#'   au résultat d'une tabulation. Les noms des tableaux renseignés dans les
#'   lignes du batch de la forme \code{// <TABLE_ID> "..."} sont récupérés.
#'
#' @section Attributs:
#'
#' À chaque data.frame est associé un ensemble d'attributs (métadonnées)
#' permettant de conserver une trace des spécifications passées à Tau-Argus.
#'
#' Attributs systématiquement présents :
#' \code{explanatory_vars}, \code{response_var}, \code{safetyrule},
#' \code{suppress}, \code{linked}, \code{output_type}.
#'
#' Attributs présents uniquement si l'option correspondante a été renseignée par
#' l'utilisateur : \code{shadow_var}, \code{cost_var}, \code{output_options}.
#'
#' @inheritSection micro_asc_rda Voir aussi
#'
#' @importFrom dplyr %>%
#' @importFrom purrr map_at
#' @importFrom purrr transpose
#'
#' @export

import <- function(arb_filename) {

  # lignes du fichier dans un vecteur
  lignes <- readLines(arb_filename)

  # extraction commandes par type
  type_commande <- c("specifytable", "safetyrule", "suppress", "writetable")
  commandes <-
    lapply(
      purrr::set_names(type_commande),
      function(t) grep(sprintf("<%s>", toupper(t)), lignes, value = TRUE)
    )

  infos_writetable <-
    stringr::str_match(
      commandes$writetable,
      "\\((\\d+),([1-6]+),(.*),\"(.+)\"\\)"
    )
  output_types   <- infos_writetable[ , 3]
  output_options <- infos_writetable[ , 4]
  output_names   <- infos_writetable[ , 5]

  infos_specifytable <- specif_decompose(commandes$specifytable)

  expl_resp <-
    infos_specifytable[c("explanatory", "response")] %>%
    transpose() %>%
    lapply(unlist, use.names = FALSE)

  commandes <-
    commandes %>%
    map_at(
      c("safetyrule", "suppress"),
      ~ gsub("<.+> +", "", trimws(.x))
    )

  res <- mapply(
    output_names,
    output_types,
    expl_resp,
    FUN = read_ta_output,
    USE.NAMES = FALSE,
    SIMPLIFY = FALSE
  )

  # id des tableaux
  motif_tid <- "^// +<TABLE_ID> +\"(.*)\"$"
  lignes_tid <- grep(motif_tid, lignes, value = TRUE)
  if (length(lignes_tid)) {
    t_id <- stringr::str_match(lignes_tid, motif_tid)[ , 2]
    names(res) <- t_id
  }

  # apriori : detection fichiers et numéros de tabulation associés
  apriori <- rep(NA, length(res)) # init vecteur vide
  motif_apriori <- '^ *<APRIORI> *"(.+?)", *(\\d{1,2}),.+$'
  lignes_apriori <- grep(motif_apriori, lignes, value = TRUE)
  if (length(lignes_apriori)) {
    infos_apriori <- stringr::str_match(lignes_apriori, motif_apriori)
    num_tab <- as.integer(infos_apriori[ , 3])
    apriori[num_tab] <- infos_apriori[ , 2]
  }

  # ajout metadonnees batch
  # (specif table, safetyrule, suppress, output_type, output_options)
  mapply(
    res,
    infos_specifytable$explanatory,
    infos_specifytable$response,
    infos_specifytable$shadow,
    infos_specifytable$cost,
    apriori,
    commandes$safetyrule,
    commandes$suppress,
    output_types,
    output_options,
    FUN = meta_import,
    SIMPLIFY = FALSE
  )

}
