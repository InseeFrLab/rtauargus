cite <- function(x, guillemet = "\"", ignore_vide = TRUE) {

  # met entre guillemet les elements d'un vecteur (sauf si chaîne vide)
  ifelse(
    x == "" & ignore_vide,
    x,
    paste0(guillemet, x, guillemet)
  )

}

df_param_defaut <- function(varnames, param_name, valvar) {

  # fonction créant un data.frame de deux colonnes :
  # la premiere donne le nom des variables categorielles des micro-donnees
  #   (`varnames`)
  # la deuxieme porte le nom `param_name`
  #   elle prend les valeurs trouvees dans `valvar`
  #   (un element non nommé de valvar constitute une valeur par défaut
  #   s'appliquant à toute la colonne, les exceptions sont renseignées dans les
  #   éléments nommés)

  val_nm <- names(valvar)
  named <- if (is.null(val_nm)) rep(FALSE, length(valvar)) else val_nm != ""
  specifique <- valvar[named]
  defaut <- valvar[!named]

  if (length(defaut) == 0) { # (y c. NULL)
    defaut <- NA
  } else if (length(defaut) > 1) {
    defaut <- defaut[1]
    warning(
      'plusieurs valeurs par defaut pour "', param_name, '", ',
      'premiere valeur prise en compte ("', defaut, '")'
    )
  }

  res <-
    data.frame(
      colname = varnames,
      .v = defaut,
      stringsAsFactors = FALSE
    )
  row.names(res) <- varnames
  names(res)[2] <- param_name

  res[names(specifique), 2] <- specifique

  res

}

following_dup <- function(x) {

  # doublons consécutifs

  if (anyNA(x)) stop("impossible : valeur(s) manquante(s)")
  c(FALSE, x[-1] == x[-length(x)])

}


# fonction normalizePath sans warning
normPath2 <- purrr::partial(normalizePath, mustWork = FALSE)

# lien parametre extension
output_extensions <- c(
  "1" = ".csv",
  "2" = ".csv",
  "3" = ".txt",
  "4" = ".sbs",
  "5" = ".tab",
  "6" = ".jj"
)

output_description <- c(
  "1" = "(1) csv file ",
  "2" = "(2) csv file for pivot-table",
  "3" = "(3) code-value file",
  "4" = "(4) SBS output-format",
  "5" = "(5) intermediate file",
  "6" = "(6) JJ format file"
)

# vecteur des noms de variables décrites dans un fichier rda pour microdonnées

vars_micro_rda <- function(rda_file) {

  stopifnot(file.exists(rda_file))
  lignes <- readLines(rda_file, warn = FALSE)

  # mot commençant par lettre, précédé éventuellement d'espaces (améliorable ?)
  motif <- "^ *([A-Za-z][^ ]*)"
  lignes_var <- stringr::str_match(lignes, motif)[ , 2]
  lignes_var[!is.na(lignes_var)]

}
