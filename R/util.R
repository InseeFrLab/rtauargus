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

  res[names(specifique), 2] <- unlist(specifique)

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

# renvoie les noms des paramètres acceptés par f présents dans list_param

param_function <- function(f, list_param) {
  f_param_names <-
    intersect(
      names(formals(f)),
      names(list_param)
    )
  list_param[f_param_names]
}

# vecteur des variables nécessaires pour micro_rtauargus()

used_var <- function(explanatory_vars,
                     weight_var   = NULL,
                     holding_var  = NULL,
                     response_var = getOption("rtauargus.response_var"),
                     shadow_var   = NULL,
                     cost_var     = NULL) {

  res <- unlist(
    c(explanatory_vars,
      weight_var,
      holding_var,
      response_var,
      shadow_var,
      cost_var
    )
  )

  unique(setdiff(res, "<freq>"))

}


# Fonction qui ajuste la taille d'une chaîne de caractères en ajoutant
# un même caractère devant pour atteindre une taille souhaitée
# @param char string chaîne de caractères à modifier
# @param cible_char integer - longueur de la chaîne souhaitée
# @param add_char caractère à ajouter
# @return chaîne de caractère
trans_var_pour_tau_argus <- function(char='monchar', cible_char=12, add_char='*'){
  diff <- cible_char - nchar(char)
  if(diff > 0)
    char = paste0(paste0(rep(add_char,diff), collapse=''),char)
  return(char)
}

# Fonction vectorisée de la précédente. Cette fonction est à privilégier pour
# une utilisation sur un vecteur, une colonne d'un dataframe par exemple.
#
# @param char character vector
# @param cible_char integer - longueur de la chaîne souhaitée
# @param add_char caractère à ajouter
#
# @return character vector
v_trans_var_pour_tau_argus <- Vectorize(trans_var_pour_tau_argus, vectorize.args = 'char')

# Fonction vectorisée de la précédente. Cette fonction est à privilégier pour
# une utilisation sur un vecteur, une colonne d'un dataframe par exemple.
#
# @param char character vector - vecteur à modifier
#
# @return character vector
rev_var_pour_tau_argus <- function(char='**monchar', del_char='*'){
  if(del_char %in% c('*','+','_',' ')){
    gsub(paste0("[",del_char,"]"),'',char)
  }else gsub(del_char, '', char)
}

# Uniformize labels before running tau-argus
# in hrc file

get_max_char_hrc_file <- function(hrc_file, totcode){
  df <- utils::read.table(hrc_file)

  v_gsub <- Vectorize(gsub, vectorize.args = c("pattern", "x"))
  df$aro <- unlist(regmatches(df$V1, gregexpr("^@*", df$V1)))
  df$lab <- v_gsub(df$aro, "", df$V1)

  df <- df[df$lab != totcode,]
  max_char = max(nchar(df$lab))
  return(list(hrc_df = df, hrc_name = hrc_file, max_char = max_char))
}


uniformize_labels_hrc_file <- function(hrc_df, hrc_name, max_char){

  hrc_df$lab <- v_trans_var_pour_tau_argus(hrc_df$lab, max_char)
  hrc_df$unif <- paste0(hrc_df$aro,hrc_df$lab)

  name <- gsub(".hrc$", "_unif.hrc", hrc_name)

  utils::write.table(
    x = as.data.frame(hrc_df$unif),
    file = name,
    quote = FALSE,
    row.names = FALSE,
    col.names = FALSE,
    sep = "",
    eol = "\n"
  )
  #print(paste0(name, " has been created"))
  return(list(hrc_unif = name, max_char = max_char))
}

# in the data and hrc file
uniformize_labels <- function(data, expl_vars, hrc_files, list_totcode){

  vars_expl_hrc <- names(hrc_files)
  hrc_unif_files <- list()

  for(var_hrc in vars_expl_hrc){
    totcode <- if(is.list(list_totcode)) purrr::flatten(list_totcode)[[var_hrc]] else list_totcode[[var_hrc]]
    hrc_file <- hrc_files[[var_hrc]]
    res <- get_max_char_hrc_file(hrc_file, totcode)
    res_unif_hrc <- uniformize_labels_hrc_file(res$hrc_df, res$hrc_name, res$max_char)
    hrc_unif_files[[var_hrc]] <- res_unif_hrc$hrc_unif
    data[[var_hrc]] <- ifelse(
      data[[var_hrc]] == totcode,
      data[[var_hrc]],
      v_trans_var_pour_tau_argus(data[[var_hrc]], cible_char = res_unif_hrc$max_char)
    )
  }

  for(expl in setdiff(expl_vars, vars_expl_hrc)){
    totcode <- if(is.list(list_totcode)) purrr::flatten(list_totcode)[[expl]] else list_totcode[[expl]]
    max_char = max(nchar(setdiff(data[[expl]], totcode)))
    data[[expl]] <- ifelse(
      data[[expl]] == totcode,
      data[[expl]],
      v_trans_var_pour_tau_argus(data[[expl]], cible_char = max_char)
    )
  }

  return(list(hrc_unif = hrc_unif_files, data = data))
}
