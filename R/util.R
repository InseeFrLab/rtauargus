cite <- function(x, guillemet = "\"", ignore_vide = TRUE) {

  # met entre guillemet les elements d'un vecteur (sauf si chaîne vide)
  ifelse(
    x == "" & ignore_vide,
    x,
    paste0(guillemet, x, guillemet)
  )

}

# fonction transformant une liste de parametres associes a des
# variables en un data.frame (pour hrc notamment)

df_param <- function(list_param) {

  res <-
    data.frame(
      colname = names(list_param),
      stringsAsFactors = FALSE,
      row.names = names(list_param)
    )

  for (i in seq_along(list_param)) {
    curvar <- names(list_param)[i]
    curobj <- list_param[[i]]
    for (j in seq_along(curobj)) {
      curpar <- names(list_param[[i]])[j]
      res[curvar, curpar] <- curobj[j]
    }
  }

  res

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

  if (!length(defaut)) defaut <- NA

  if (length(defaut) > 1) {
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
