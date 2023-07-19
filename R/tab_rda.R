creer_hst <- function(tabular,
                      explanatory_vars,
                      value,
                      secret_var,
                      secret_no_pl,
                      cost_var,
                      ip,
                      separator) {

  if(!is.null(secret_var)){
    if(is.null(secret_no_pl)){
      tabular$secret_no_pl <- FALSE
    }
  }

  if(!is.null(secret_var)) {

    tabular$label_apriori <-ifelse(tabular[[secret_var]],"u","s")
    tab_hst_secret = tabular[
      tabular[[secret_var]],
      c(explanatory_vars[(explanatory_vars %in% colnames(tabular))],"label_apriori")
    ]
  } else {tab_hst_secret <- data.frame()}

  #Genere le fichier hst lié au coût

  if (!is.null(cost_var)){

    tabular$label_apriori <-paste0("c",separator,tabular[[cost_var]])

  if(!is.null(secret_var)){
    tab_hst_cost = tabular[
      !tabular[[secret_var]],
      c(explanatory_vars[(explanatory_vars %in% colnames(tabular))],"label_apriori")
    ]
      } else {
        tab_hst_cost = tabular[,
          c(explanatory_vars[(explanatory_vars %in% colnames(tabular))],"label_apriori")
        ]
    }

  } else {tab_hst_cost <- data.frame()}

  if ((!is.null(ip)) & (is.numeric(ip)) & !is.null(secret_var)) {
    tabular$val_ip <- ifelse(tabular[[secret_no_pl]] & (tabular[[value]] != 0),
                             format(0.00001,scientific = F),
                             round((ip/100)*tabular[[value]],1))

    tabular$label_apriori <- paste0("pl",separator,tabular[["val_ip"]],separator,tabular[["val_ip"]])

    tab_hst_pl = tabular[
      tabular[[secret_var]],
      c(explanatory_vars[(explanatory_vars %in% colnames(tabular))],"label_apriori")
    ]
  } else {tab_hst_pl <- data.frame()}

  apriori <- rbind(tab_hst_secret,tab_hst_cost)
  apriori <- rbind(apriori,tab_hst_pl)

}

write_rda_1var_tab <- function(info_var) {

  # écrit partie du .rda à partir des infos (une liste) pour une seule variable

  ligne1 <- with(info_var,
                 paste(
                   colname
                 )
  )

  with(
    info_var,
    paste(
      sep = "\n",
      trimws(ligne1),
      paste0("  <", type_var, ">"),
      if (!is.na(totcode))
        paste0("  <TOTCODE> \"", totcode, "\""),
      if (!is.na(codelist))
        paste0("  <CODELIST> \"", codelist, "\""),
      if (!is.na(hierarchical))
        "  <HIERARCHICAL>",
      if (!is.na(hierarchical) && grepl("\\.hrc$", hierarchical))
        paste0("  <HIERCODELIST> \"", hierarchical, "\""),
      if (!is.na(hierarchical) && grepl("\\.hrc$", hierarchical))
        paste0("  <HIERLEADSTRING> \"", hierleadstring, "\""),
      if (!is.na(hierarchical) && grepl("^(\\d+ +)+\\d+$", hierarchical))
        paste0("  <HIERLEVELS> ", hierarchical),
      if (type_var %in% c("NUMERIC","MAXSCORE"))
        paste0("  <DECIMALS> ", digits)
    )
  )

}

#' @importFrom dplyr %>%
write_rda_tab <- function(info_vars) {


  # écrit les infos format .rda pour toutes les variables
  # (info_vars est une liste contenant les infos pour chaque variable)

  chemin_complet <- function(x) {
    if (!is.na(x$codelist)) x$codelist <- normPath2(x$codelist)
    return(x)
  }
  info_vars <- lapply(info_vars, chemin_complet)

  vapply(info_vars, write_rda_1var_tab, character(1)) %>%
    gsub("(\n)+", "\n", .) %>% # plusieurs sauts de lignes par un seul
    sub("\n$", "", .) # supprime dernier saut de ligne

}

#' Creates rda files from tabular data
#'
#' Creates an apriori file for the primary secret,
#' a tabular file (tab) and a metadata file (rda)
#' from tabulated data and additional information.\cr
#'
#' Crée un fichier d'apriori pour le secret primaire,
#' un fichier tabular (tab) et un fichier de métadonnées
#' (rda) à partir de données tabulées et d'informations additionnelles.
#'
#' @param tabular [\strong{mandatory}]
#' data.frame which contains the tabulated data and
#' an additional boolean variable that indicates the primary secret of type boolean \cr
#' ([\strong{obligatoire}] data.frame contenant les données tabulées et
#' une variable supplémentaire indiquant le secret primaire de type booléen.)
#' @param tab_filename tab file name (with .tab extension) \cr
#' nom du fichier tab (avec extension .tab)
#' @param rda_filename rda file name (with .rda extension) \cr
#' nom du fichier rda (avec extension)
#' @param hst_filename hst file name (with .hst extension) \cr
#' nom du fichier hst (avec extension)
#' @param explanatory_vars [\strong{mandatory}] Vector of explanatory variables \cr
#' [\strong{obligatoire}] Variables catégorielles, sous forme  de vecteurs \cr
#' Example : \code{c("A21", "TREFF", "REG")} for a table crossing
#' \code{A21} x \code{TREFF} x \code{REG}
#' (Variable indiquant le secret primaire de type booléen:
#' prend la valeur "TRUE" quand les cellules du tableau doivent être masquées
#' par le secret primaire, "FALSE" sinon. Permet de créer un fichier d'apriori)
#' @param secret_var Nae of the boolean variable which specifies the secret, primary or not :
#'  equal to "TRUE" if a cell is concerned by the secret,"FALSE" otherwise.
#' will  be exported in the apriori file. \cr
#' (Variable indiquant le secret  de type booléen:
#' prend la valeur "TRUE" quand les cellules du tableau doivent être masquées
#'"FALSE" sinon. Permet de créer un fichier d'apriori)
#' @param cost_var Numeric variable allow to change the cost suppression of a cell
#' for secondary suppression, it's the value of the cell by default, can be
#' specified for each cell, fill with NA if the cost doesn't need to be changed
#' for all cells \cr
#' (Variable numeric qui permet de changer la coût de suppression d'une cellule,
#' pris en compte dans les algorithmes de secret secondaire.Par défaut le coût
#' correspond à la valeur de la cellule.  peut être spécifié pour chacune des cellules,
#' peut contenir des NA pour les coûts que l'on ne souhaite pas modifier.)
#' (nombre minimal de décimales à afficher (voir section 'Number of decimals').)
#' @param hrc Informations of hierarchical variables (see section
#' 'Hierarchical variables'). \cr
#' (Informations sur les variables hiérarchiques (voir section
#' 'Hierarchical variables').)
#' (Caractère qui, répété n fois, indique que la valeur est
#' à n niveaux de profondeur dans la hiérarchie.)
#' @param totcode Code(s) which represent the total of a categorical variable
#' (see section 'Specific parameters' for this parameter's syntax).
#' If unspecified for a variable(neither by default nor explicitly)
#' it will be set to  \code{rtauargus.totcode}. \cr
#' (Code(s) pour le total d'une variable catégorielle (voir
#' section 'Specific parameters' pour la syntaxe de ce paramètre). Les
#' variables non spécifiées (ni par défaut, ni explicitement) se verront
#' attribuer la valeur de \code{rtauargus.totcode}.)
#' @param value Name of the column containing the value of the cells. \cr
#' (Nom de la colonne contenant la valeur des cellules)
#' @param freq Name of the column containing the cell frequency. \cr
#' (Nom de la colonne contenant les effectifs pour une cellule)
#' @param ip Value of the safety margin in \% (must be an integer).
#' (Valeur pour les intervalles de protection en \%, doit être entier )
#' @param maxscore Name of the column containing, the value of the largest
#' contributor of a cell. \cr
#' (Nom de la colonne contenant la valeur du plus gros contributeur
#' d'une cellule)
#' @param maxscore_2 Name of the column containing, the value of the second largest
#' contributor to a cell. \cr
#' (Nom de la colonne contenant la valeur du deuxième plus gros contributeur
#' d'une cellule)
#' @param maxscore_3 Name of the column containing, the value of the third largest
#' contributor to a cell. \cr
#' (Nom de la colonne contenant la valeur du troisième plus gros contributeur
#' d'une cellule)
#' @param decimals Minimum number of decimals to display
#' (see section 'Number of decimals') \cr
#' @param separator Character used as separator in the .tab file. \cr
#' (Caractère utilisé en tant que separateur dans le fichier .tab)
#' @param hierleadstring  The character that is used to indicate the depth of a
#' code in the hierarchy. \cr
#' @param codelist file(s) containing labels of a categorical variables
#' (see section 'Specific parameters' for the syntax of this parameter). \cr
#' (Fichier(s) contenant les libellés des variables catégorielles
#' (voir section 'Specific parameters' pour la syntaxe de ce paramètre).)
#' @param secret_no_pl name of boolean variable which indicates if a cell should be
#' concerned by the protection levels "TRUE" if a cell is not concerned by the
#' protection levels,"FALSE" otherwise. will  be exported in the apriori file \cr
#'
#' @return Return the rda file name as a list (invisible).\cr
#' (Renvoie le nom du fichier rda sous forme de liste (de
#' manière invisible).)
#'
#'
#' @section Apriori file :
#'
#' The apriori file (.hst) summarizes for each value of the table
#' if they are concerned by the primary secret or not.
#' With this file tau-argus will not need to set the primary secret itself.
#' The parameter \code{secret_var} indicates the name of the primary secret variable.
#' If there is the additional boolean variable which indicates the primary secret
#' in the table (of tabulated data), the function tab_rda will create
#' an apriori file in a format conforming to tauargus. \cr
#'
#'
#' Le fichier d'apriori (.hst) récapitule pour chaque valeurs
#' du tableau si elles sont concernées par le secret primaire ou non.
#' Avec ce fichier tau-argus n'aura plus besoin de poser le secret primaire lui même,
#' il se basera sur le fichier d'apriori pour le faire.
#' Le paramètre \code{secret_var} indique le nom de la variable du secret primaire.
#' Si l'on rajoute cette variable supplémentaire indiquant
#' le secret primaire (de type booléen) au tableau de données tabulées, la fonction
#' tab_rda permet de créer un fichier d'apriori au format conforme pour tauargus.
#'
#'
#' @section Specific parameters:
#'
#' The parameters \code{totcode}, and \code{codelist}
#' must be given in the form of a vector indicating the value to take for each variable.
#' The names of the elements of the vector give the variable concerned and
#' the elements of the vector give the value of the parameter for Tau-Argus.
#' An unnamed element will set the default value for each variable. \cr
#'
#' (Les paramètres \code{totcode},  et \code{codelist}
#' sont à renseigner sous la forme d'un vecteur indiquant la valeur à prendre
#' pour chaque variable.
#'
#' Les noms des éléments du vecteur donnent la variable concernée, les éléments
#' du vecteur donnent la valeur du paramètre pour Tau-Argus. Un élément non
#' nommé constituera la valeur par défaut, qui sera attribuée à toutes les
#' variables pouvant prendre ce paramètre.)
#'
#' For example :
#' \itemize{
#'   \item{\code{totcode = "global"} : writes \code{<TOTCODE> "global"} for each
#'   explanatory vars}
#'   \item{\code{totcode = c("global", size="total", income="total")} :
#'   \code{<TOTCODE> "global"} for each variable except for \code{size} and
#'   \code{income}} assigned with \code{<TOTCODE> "total"}
#'   by default : {<TOTCODE> "Total"}
#'   \item{\code{totcode = "global"} : écrit \code{<TOTCODE> "global"} pour
#'     toutes les variables catégorielles}
#'   \item{\code{totcode = c("global", size="total", income="total")} :
#'   \code{<TOTCODE> "global"} pour toutes les variables catégorielles
#'   sauf  \code{size} and \code{income}} qui se verront affecter
#'   le total : \code{<TOTCODE> "total"}
#'   Par defaut : {<TOTCODE> "Total"}
#' }
#'
#'
#'
#' @section Hierarchical variables:
#'
#' Parameter \code{hrc} has the same syntax as \code{totcode} and
#' \code{codelist} (named vector containing as many elements as variables to describe).
#' Hierarchy is defined in an separate hrc file (\strong{hiercodelist}).
#' which can be written with the function \code{link{write_hrc2}}.
#' The function expects the location of this file (and a possible \code{hierleadstring}
#' if it differs from the default option of the package : @.
#' The path to the existing file is explicitly given.
#' The elements of the vector in parameter must be named (with the name of the variable),
#' even if there is only one element.
#'
#' emph{Example :}\code{c(category="category.hrc")} \cr
#'
#' (Le paramètre \code{hrc} obéit aux mêmes règles de syntaxe que \code{totcode}
#'  et \code{codelist} (vecteur nommé contenant autant d'éléments
#' que de variables à décrire).
#'
#' La hiérarchie est définie dans un fichier hrc à part (\strong{hiercodelist})
#' qui peut être écrit à l'aide de la fonction \code{\link{write_hrc2}}.
#'
#' La fonction attend l'emplacement de ce fichier (et un éventuel \code{hierleadstring}
#' s'il diffère de l'option par défaut du package).
#' Le chemin vers le fichier existant est explicitement donné.
#' Les éléments du vecteur en paramètre doivent nommés (avec le nom de la variable),
#' même s'il n'y a qu'un seul élément.
#'
#'\emph{Exemple :}\code{c(category="category.hrc")})
#'
#'
#' @section Number of decimals:
#' Parameter \code{decimals} indicates the minimum number of decimal places to
#' include in the output file
#' (whatever the number of decimals actually present in \code{tabular}).
#' It applies to all real variables (double) but not to integer variables.
#' To add zeros to an integer variable, convert it with \code{as.double} beforehand.\cr
#'
#' (Le paramètre \code{decimals} indique le nombre minimal de décimales à faire
#' figurer dans le fichier en sortie (quel que soit le nombre de décimales
#' effectivement présent dans \code{tabular}). Il s'applique à toutes les
#' variables réelles (double) mais pas aux variables entières (integer). Pour
#' ajouter des zéros à une variable entière, la convertir avec \code{as.double}
#' au préalable.)
#'
#' @examples
#' \dontrun{
#' # donnees fictives
#'
#' tab <-data.frame(
#'  category       = c( "A" ,   "B",   "C",   "D",   "E",   "F"),
#'  size           = c("tr1", "tr3", "tr2", "tr1", "tr1", "tr2"),
#'  area           = c( "07",  "01",  "04",  "06",  "02",  "06"),
#'  income         = c(  100,     4,     7,    14,    42,    85),
#'  freq           = c(    2,     6,     8,    45,   100,     1),
#'  max            = c(   54,     2,     1,    13,    19,    85),
#'  primary_secret = c( TRUE, FALSE, FALSE,  TRUE, FALSE,  TRUE),
#'  cost           = c(   NA,    NA,    NA,     1,     5,    NA)
#' )
#'
#' # rda creation
#'
#' files_names <-
#'  tab_rda(
#'   tabular          = tab,
#'   tab_filename     = "tauargus_files/file.tab",
#'   rda_filename     = "tauargus_files/file.rda",
#'   hst_filename     = "tauargus_files/file.hst",
#'   hrc              = c(category = "category.hrc"),
#'   explanatory_vars = c("category" , "size", "area"),
#'   secret_var       = "primary_secret",
#'   cost_var         = "cost"
#'   totcode          = c(
#'     category = "global",
#'     size     = "total",
#'     area     = "global"
#'   ),
#'   value            = "income",
#'   freq             = "freq"
#' )
#'
#' # Viewing product files
#' file.show(
#'  res$rda_filename,
#'   header = unlist(res),
#'   pager = "internal"
#' )
#' }
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom purrr transpose
#' @importFrom rlang .data
#'
#' @export

tab_rda <- function(
    tabular,
    tab_filename   = NULL,
    rda_filename   = NULL,
    hst_filename   = NULL,
    explanatory_vars = NULL,
    secret_var     = NULL,
    cost_var       = NULL,
    hrc            = NULL,
    totcode        = getOption("rtauargus.totcode"),
    value          = NULL,
    freq           = NULL,
    maxscore       = NULL,
    ip             = NULL,
    maxscore_2     = NULL,
    maxscore_3     = NULL,
    decimals       = getOption("rtauargus.decimals"),
    hierleadstring = getOption("rtauargus.hierleadstring"),
    codelist       = NULL,
    separator      = getOption("rtauargus.separator"),
    secret_no_pl    = NULL
){


  tabular <- as.data.frame(tabular) # (probleme avec tibble notamment)


  # valeur par défaut du package si option vide ...........................

  if (is.null(decimals)) decimals <- op.rtauargus$rtauargus.decimals
  if (is.null(hierleadstring)) {
    hierleadstring <- op.rtauargus$rtauargus.hierleadstring
  }
  if (is.null(totcode)) totcode <- op.rtauargus$rtauargus.totcode


  if (!is.null(secret_var)){
    colvides <- sapply(tabular[,!names(tabular)==secret_var], function(x) all(is.na(x)) | all(x == ""))
  } else {
    colvides <- sapply(tabular, function(x) all(is.na(x)) | all(x == ""))
  }

  if (any(colvides) ) {
    name_colvides<-paste(names(tabular)[colvides], collapse = ", ")
    warning(
      "empty columns : ",
      name_colvides
    )
    tabular <- tabular[!colvides]
  }

  # parametres non renseignés  ...........................................
  if (is.null(rda_filename)) rda_filename <- tempfile("RTA_", fileext = ".rda")
  if (is.null(tab_filename)) tab_filename <- "tabular.tab"
  if ((is.null(hst_filename)) & ((!is.null(secret_var))|!is.null(cost_var))) {
    hst_filename <- "apriori.hst"} else if ((is.null(hst_filename)) & (is.null(secret_var)) & (is.null(cost_var))){
      hst_filename <- NULL
    }

  #Gestion du chemin des fichiers
  name_rda <- basename(rda_filename)
  directory_rda <- stringr::str_replace(rda_filename, pattern = name_rda, replacement="")
  if(!(dir.exists(directory_rda))) dir.create(directory_rda, recursive = TRUE, showWarnings = FALSE)

  name_tab <- basename(tab_filename)
  directory_tab <- stringr::str_replace(tab_filename, pattern = name_tab, replacement="")
  if(!(dir.exists(directory_tab))) dir.create(directory_tab, recursive = TRUE, showWarnings = FALSE)

  if((!is.null(hst_filename)) | (!is.null(secret_var))|(!is.null(cost_var))){
    name_hst <- basename(hst_filename)
    directory_hst <- stringr::str_replace(hst_filename, pattern = name_hst, replacement="")
    if(!(dir.exists(directory_hst))) dir.create(directory_hst, recursive = TRUE, showWarnings = FALSE)
  }

  # Controle sur le nombre de colonnes

  col_tabular <- c(
    explanatory_vars,
    secret_var,
    secret_no_pl,
    cost_var,
    value,
    freq,
    maxscore,
    maxscore_2,
    maxscore_3
  )

  # if (length(tabular[1,]) != length(col_tabular))
  # {warning("unspecified columns in table")}
  # Controle hrc
  if(!all(names(hrc) %in% explanatory_vars)){
    stop(" error with label of the hierarchichal variable")
  }

  # Controle sur frequency

  if (any(tabular[[freq]] != round(tabular[[freq]],0))){
    stop("decimals are not allowed for frequency")
  }

  #Controles sur secret_var
  if ((!is.null(secret_var)) && (!secret_var %in% colnames(tabular))){
    stop("secret_var does not exist in tabular")
  }

  if((!is.null(secret_var)) && (any(!is.na(tabular[[secret_var]]))) && (!is.logical(tabular[[secret_var]]))){
    stop("unexpected type : secret_var must be a  boolean variable")
  }

  if((!is.null(secret_var)) && any(is.na(tabular[[secret_var]]))){
    stop("NAs in secret_var are not allowed")
  }
  if(is.null(secret_var) && !is.null(secret_no_pl)){
    stop("protection levels needs to be applied to primary secret, specify
         secret_var")
  }
  #Controles sur secret_no_pl, identiques à secret_var
  if ((!is.null(secret_no_pl)) && (!secret_no_pl %in% colnames(tabular))){
    stop("secret_no_pl does not exist in tabular")
  }

  if((!is.null(secret_no_pl)) && (!is.logical(tabular[[secret_no_pl]]))){
    stop("unexpected type : secret_no_pl must be a  boolean variable")
  }

  if((!is.null(secret_no_pl)) && any(is.na(tabular[[secret_no_pl]]))){
    stop("NAs in secret_no_pl are not allowed")
  }

  # Controles sur cost_var

  if ((!is.null(cost_var)) && (!cost_var %in%  colnames(tabular))){
    stop("cost_var does not exist in tabular")
  }

  if((!is.null(cost_var)) && (!is.numeric(tabular[[cost_var]]))){
    stop("unexpected type : cost_var must be a  numeric variable")
  }

  # Controles sur ip

  if((!is.null(ip)) && (!is.numeric(ip))){
    stop("unexpected type : ip must be a  numeric variable")
  }

  if((!is.null(ip)) && (ip < 0)){
    stop("unexpected value : ip must be a positive integer")
  }

  if((!is.null(ip)) && (ip > 100)){
    stop("unexpected value : ip is over 100, it is too high")
  }



  #Genere le fichier hst lié au secret primaire
  if(any(!is.null(c(ip,secret_var,cost_var)))){
    hst <- creer_hst (tabular,
                      explanatory_vars,
                      value,
                      secret_var,
                      secret_no_pl,
                      cost_var,
                      ip,
                      separator)


    if( !is.null(secret_var) | !is.null(cost_var)| !is.null(secret_no_pl)) {
      if (nrow(hst)==0) message("no cells are unsafe : hst file is empty")

      utils::write.table(
        hst,
        hst_filename,
        row.names=FALSE,
        col.names = FALSE,
        sep= separator,
        quote=FALSE
      )
    }
  }

  # genere fichier longueur fixe (le fichier .tab) dans le dossier indiqué et infos associees  .....................

  if (!is.null(secret_var)) tabular<-tabular[,!names(tabular)==secret_var]
  if (!is.null(secret_no_pl)) tabular<-tabular[,!names(tabular)==secret_no_pl]
  if (!is.null(cost_var)) tabular<-tabular[,!names(tabular)==cost_var]

  tabular <- tabular[,c(explanatory_vars,value,freq,maxscore,maxscore_2,maxscore_3)]
  fwf_info_tabular <-
    gdata::write.fwf(
      tabular,
      file= tab_filename,
      formatInfo = TRUE,
      colnames = FALSE,
      justify = "right", # pour les variables caractères uniquement
      digits = 15,
      nsmall = decimals,
      scientific = FALSE,
      sep=separator
    )

  num <- vapply(tabular, is.numeric, logical(1))



  fwf_info_tabular <-
    fwf_info_tabular %>%
    mutate(
      type_var = ifelse(num, "NUMERIC", "RECODEABLE"),
      ordre_init = dplyr::row_number()
    )

  # freq et maxscore ..........................................

  if (!is.null(freq)) {
    fwf_info_tabular$type_var[fwf_info_tabular$colname == freq] <- "FREQUENCY"
  }

  if (!is.null(maxscore)) {
    fwf_info_tabular$type_var[fwf_info_tabular$colname == maxscore] <- "MAXSCORE"
  }

  if (!is.null(maxscore_2)) {
    fwf_info_tabular$type_var[fwf_info_tabular$colname == maxscore_2] <- "MAXSCORE"
  }

  if (!is.null(maxscore_3)) {
    fwf_info_tabular$type_var[fwf_info_tabular$colname == maxscore_3] <- "MAXSCORE"
  }

  #  totcode, codelist  ........................................

  var_quali <- names(tabular)[!num]

  codelist_df <- df_param_defaut(var_quali, "codelist", codelist)
  totcode_df  <-
    df_param_defaut(var_quali, "totcode", totcode) %>%
    mutate(totcode = dplyr::coalesce(totcode, getOption("rtauargus.totcode")))



  # hierarchical  ......................................................

  if (!is.null(hrc) & (is.null(names(hrc)) | any(names(hrc) == ""))) {
    stop("missing name for hrc. Example : hrc = c(VAR = \"var.hrc\")")
  }

  norm_hrc <- purrr::map(hrc, normalizePath)
  # normalise_hrc(
  #   hrc[[1]],
  #   tabular,
  #   hierleadstring = hierleadstring
  # )

  hrc_df <- df_param_defaut(var_quali, "hierarchical", norm_hrc)
  hrc_df$hierleadstring <- NA_character_
  need_leadstring <- grepl("\\.hrc$", hrc_df$hierarchical)

  hrc_df$hierleadstring[need_leadstring] <- hierleadstring

  fwf_info_tabular <-
    purrr::reduce(
      list(fwf_info_tabular, totcode_df, codelist_df, hrc_df),
      merge,
      by = "colname",
      all.x = TRUE
    )

  # reordonne (car tri par merge)  ...................................

  fwf_info_tabular <-
    fwf_info_tabular %>%
    arrange(.data$ordre_init) %>%
    dplyr::select(
      -dplyr::all_of(c("ordre_init", "nlevels", "exp","position","width"))
    )

  # reorganise en une liste de variables .............................
  fwf_info_tabular <-purrr::transpose(fwf_info_tabular)

  # genere vecteur format .rda .......................................
  res <- character(0)
  # instructions
  res[1] <- sprintf('   <SEPARATOR> "%s"',separator)
  res[2] <- sprintf('   <SAFE> "s"')
  res[3] <- sprintf('   <UNSAFE> "u"')
  res[4] <- sprintf('   <PROTECT> "p"')

  res <- c(res,write_rda_tab(fwf_info_tabular))

  # écrit fichier texte ..............................................
  writeLines(res, rda_filename)

  # renvoie noms des fichiers hst, tab et rda de manière invisible .......
  invisible(
    list(
      tab_filename = normPath2(tab_filename),
      rda_filename = normPath2(rda_filename),
      hst_filename = if(!is.null(hst_filename)){
        normPath2(hst_filename)} else{NULL}
    )
  )



}

