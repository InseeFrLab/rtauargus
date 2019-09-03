# Fonction extrayant les informations contenues dans un fichier batch (arb)
# Structure de la liste en sortie (tous les items pas forcément présents)
    # $ openmicrodata     [chr]
    # $ openmetadata      [chr]
    # $ specifytable      [list]
    #  ..$ explanatory    [list]
    #  ..$ response       [chr]
    #  ..$ shadow         [chr]
    #  ..$ cost           [chr]
    # $ apriori           [list]
    #  ..$ file           [chr]
    #  ..$ sep            [chr]
    #  ..$ ignore_err     [chr]
    #  ..$ exp_triv       [chr]
    # $ safetyrule        [chr]
    # $ suppress          [chr]
    # $ writetable        [list]
    #  ..$ output_types   [chr]
    #  ..$ output_options [chr]
    #  ..$ output_names   [chr]
    # $ gointeractive     [logi]
    # $ tab_id            [chr]

#' @importFrom purrr map_at
#' @importFrom dplyr %>%

arb_contents <- function(arb_filename) {

  lignes <- readLines(arb_filename)

  # extraction commandes

  type_commande <- c(
    "openmicrodata", "openmetadata",
    "specifytable", "apriori", "safetyrule", "suppress",
    "writetable", "gointeractive"
  )
  res <-
    lapply(
      purrr::set_names(type_commande),
      function(t) grep(sprintf("<%s>", toupper(t)), lignes, value = TRUE)
    )

  # decompose commandes complexes

  res$specifytable <- specif_decompose(res$specifytable)
  res$writetable <- write_decompose(res$writetable)
  res$apriori <- apriori_decompose(res$apriori)

  # id tableau (en commentaire)

  motif_tid <- "^// +<TABLE_ID> +\"(.*)\"$"
  lignes_tid <- grep(motif_tid, lignes, value = TRUE)
  if (length(lignes_tid)) {
    res$tab_id <- stringr::str_match(lignes_tid, motif_tid)[ , 2]
  }

  # gointeractive

  res$gointeractive <- as.logical(length(res$gointeractive))

  # met en forme certains items

  res %>%
    map_at(
      c("openmicrodata", "openmetadata", "safetyrule", "suppress"),
      purrr::compose(unquote, rm_cmd)
    )

}


# Décompose commandes avec plusieurs arguments ----------------------------

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

write_decompose <- function(write_cmd) {

  ## extrait les infos contenus dans instructions <WRITETABLE> d'un batch
  ## pour par exemple les utiliser dans reimport des données
  ## param write_cmd : un vecteur de commandes <WRITETABLE>

  res <- NULL

  infos_writetable <-
    stringr::str_match(
      write_cmd,
      "\\((\\d+),([1-6]+),(.*),\"(.+)\"\\)"
    )

  res$output_types   <- infos_writetable[ , 3]
  res$output_options <- infos_writetable[ , 4]
  res$output_names   <- infos_writetable[ , 5]

  res

}

apriori_decompose <- function(apriori_cmd) {

  res <- NULL

  infos_apriori <-
    stringr::str_match(
      apriori_cmd,
      "(.+),\\d{1,2},\"(.+)\",(\\d),(\\d)"
    )

  if (!length(infos_apriori)) return(NULL)

  res$file       <- infos_apriori[ , 2] %>% rm_cmd() %>% unquote()
  res$sep        <- infos_apriori[ , 3]
  res$ignore_err <- infos_apriori[ , 4]
  res$exp_triv   <- infos_apriori[ , 5]

  res

}


# Autres fonctions utiles -------------------------------------------------

rm_cmd <- function(cmd) sub("^ *<.+> *", "", cmd)

unquote <- function(s) sub("^[\"'](.+)[\"']$", "\\1", s)
