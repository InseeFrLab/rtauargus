ajouter_feuille_unique <- function(table_passage,racine){

  df_long <- data.frame()

  for(i in 1 : (ncol(table_passage)-1)){
    table_prov <- unique(table_passage[,i:(i+1)])
    colnames(table_prov) <- c("parent","enfant")
    table_prov$niveau <- i
    filtre <- table_prov$parent != table_prov$enfant
    table_prov <-  table_prov[filtre,]
    df_long <- rbind(df_long,table_prov)
  }
  compte <- table(df_long$parent)
  compte <- as.data.frame(compte)
  colnames(compte) <- c("parent","nb_occur")
  feuille_unique <- compte[compte$nb_occur == 1,]
  table_unique <- df_long[df_long$parent %in% feuille_unique$parent,]

  for (i in 1:nrow(table_unique)){
    niveau <- table_unique$niveau[[i]]
    enfant <- table_unique$enfant[[i]]
    parent <- table_unique$parent[[i]]
    ligne_a_recup <- which((table_passage[,niveau]==parent) & table_passage[,(niveau+1)]==enfant)[[1]]
    sup_df <- table_passage[1:ligne_a_recup,]
    inf_df <- table_passage[ligne_a_recup:nrow(table_passage),]
    sup_df[ligne_a_recup,][(niveau+1):length(sup_df[ligne_a_recup,])] <- paste0(racine,sup_df[ligne_a_recup,][(niveau+1)])
    res <- rbind(sup_df,inf_df)
    table_passage <- res
  }
  return(res)
}

arobase <- function(string, number, hier_lead_string){
  if(is.na(number)){
    return(NA)
  }else{
    return(
      paste0(
        paste0(rep(hier_lead_string, number), collapse = "")
        , string, "\n", collapse = "")
    )
  }
}
vect_aro <- Vectorize(arobase, vectorize.args = c("string", "number"))

# vect_aro(string = c("ab", "abb"), number =  1:2, hier_lead_string = "!")

#' .hrc writing
#'
#' Creates a .hrc hierarchy from a correspondence table. \cr
#' Ecrit une hiérarchie .hrc à partir d'une table de correspondance.
#'
#' @param corr_table Data frame. Correspondence table, from most aggregated to most detailed
#' \cr
#' Table de correspondance, du plus agrégé au plus fin
#' @param output_name character string. Name for the output file (with no
#' extension) ; default is set to the same name as the correspondence table
#' \cr
#' Nom du fichier en sortie (sans extension) ; par défaut,
#' identique au nom de la table de correspondance
#' @param dir_name character string. Directory name for the hrc file
#' \cr
#' Nom du répertoire dans lequel écrire le fichier hrc
#' @param sort_table boolean. If TRUE, table will be sorted beforehand.
#' (default to FALSE)\cr
#' Si TRUE, la table sera triée avant traitement. (défaut à FALSE)
#' @param rev boolean. If TRUE, column order is reversed.\cr
#' Si TRUE, inverse l'ordre des colonnes.
#' @param hier_lead_string character. (Single) character indicating the
#' hierarchy depth in the .hrc file
#' \cr
#' Caractère unique repérant le niveau de profondeur dans le .hrc
#' @param adjust_unique_roots boolean. If TRUE will add fictional roots to the
#' correspondence table, by doing so there will be no unique roots in the hrc file.
#' With tabular function, unique roots are not handled by Tau-Argus. \cr
#' Si TRUE la fonction va ajouter des feuilles fictives au fichier .hrc afin
#' qu'il n'y ait plus de feuilles uniques. Les feuilles uniques peuvent générer
#' des problèmes dans l'exécution de Tau-Argus
#' @param add_char character If adjust_unique_roots is TRUE add_char is the string that will
#' be used to create fictional roots, be sure that this string does not create
#' duplicates.The string will be paste at the beginning of a unique root
#'  default = "ZZZ" \cr
#' character Si adjust_unique_roots est TRUE add_char est l'élément qui sera
#' utilisé afin de créer des feuilles fictives, il faut être sur que cela
#' ne crée pas de doublons dans la hiérarchie.La chaine de caractère sera
#' ajouté au début d'une feuille unique. Par defaut :"ZZZ"
#' @details Creates a .hrc hierarchy file adapted to tau-Argus from a
#' correspondence table fully describing it. By default, lines are sorted
#' alphabetically so as to regroup identical levels.
#'
#' Ecrit un fichier de hiérarchie .hrc lisible par tau-Argus à
#' partir d'une table de corrrespondance la décrivant complètement. Par défaut,
#' les lignes du tableau seront triées afin de regrouper les niveaux de
#' hiérarchie identiques.
#'
#' @section Details about correspondence table & .hrc:
#' Hierarchy files read by tau-Argus are expected to follow a strict pattern.
#' This function mimicks some of its rigidities.
#' \cr
#'
#' 1 \strong{Ideal case}
#'
#' Here is how a correspondence table is assumed to look like:
#'
#' | (**type**)   | (**details**)   |
#' |--------|------------|
#'   | planet | telluric   |
#'   | planet | gasgiant   |
#'   | star   | bluestar   |
#'   | star   | whitedwarf |
#'   | star   | browndwarf |
#'   | other  | blackhole  |
#'   | other  | pulsar     |
#'
#'
#' Columns must be ordered from most aggregated to most detailed.
#' If they are in reverse order, you may want to use rev = TRUE. In any other
#' case, please reorder columns by hand.\cr
#'
#' Hierarchy must be well-nested : fine levels must systematically be nested
#' into unique higher levels. If this is not compatible with your situation,
#' you will have to split it in different hierarchies and insure common cells
#' are correctly protected (seek further documentation or help if needed).
#' \cr
#'
#' 2 \strong{Dealing with NAs}
#'
#' The write_hrc2 function has to be preferably used without any NAs in your
#' correspondence table. In presence of NAs, the \strong{sort} argument
#' has to be to FALSE. Indeed, NAs would be sorted together and, thus,
#' be separated from their expected place in the hierarchy.
#'
#' Below, we introduce two common cases where correspondence tables could have
#' NAs. The first one is supported by the function, the second one is not.
#'
#' Please be careful when dealing with NAs and check thoroughly the
#' resulting .hrc file, or consider filling in NAs beforehand.
#'
#' 2.1 \emph{Sparse hierarchies} \cr
#' Hierarchy is sparse when NAs are inserted instead of repeating under a given
#' level.
#'
#' | (**type**)   | (**details**)   |
#' |--------|------------|
#' | planet | telluric   |
#' |        | gasgiant   |
#' | star   | bluestar   |
#' |        | whitedwarf |
#' |        | reddwarf   |
#' | other  | blackhole  |
#' |        | pulsar     |
#'
#' Such cases still issue a warning for the presence of NAs, but do not pose
#' any problem, if \strong{sort=FALSE} is set.
#'
#' 2.2 \emph{Non-uniform hierarchies}\cr
#' Hierarchies with non-uniform depth happen when some levels are not detailed
#' to the  lowest detail, creating NAs.
#'
#' | (**type**)   | (**details**)   |
#' |--------|------------|
#'   | planet | telluric   |
#'   | planet | gasgiant   |
#'   | star   |            |
#'   | other  | blackhole  |
#'   | other  | pulsar     |
#'
#' Processing such a file will generate an error with the following messages:
#' \emph{Missing values on the last column of the correspondence table is not allowed.
#' If relevant, you could fill in with the value of the previous column}
#'
#' @section Détails sur les tables de correspondance et le .hrc:
#' Tau-Argus attend des fichiers écrits avec précision. Certaines de ses
#' rigidités sont reproduites par cette fonction.
#' \cr
#'
#' 1 \strong{Cas idéal}
#'
#' Voici l'aspect général que devrait avoir une table de correspondance :
#'
#' | (**type**)   | (**details**)   |
#' |--------|------------|
#'   | planet | telluric   |
#'   | planet | gasgiant   |
#'   | star   | bluestar   |
#'   | star   | whitedwarf |
#'   | star   | browndwarf |
#'   | other  | blackhole  |
#'   | other  | pulsar     |
#'
#' Les colonnes doivent être ordonnées du niveau le plus agrégé au plus fin.
#' Si elles sont en sens inverse, l'option rev = TRUE permet de les mettre en
#' ordre. Dans toute autre situation, vous devrez d'abord les ordonner à la
#' main.
#'\cr
#'
#' La hiérarchie doit être bien emboîtée : un niveau fin doit systématiquement
#' correspondre à un unique niveau agrégé. Si cette exigence n'est pas remplie,
#' il faudra créer plusieurs hiérarchies et faire en sorte que les cellules
#' communes soient correctement protégées (au besoin, consultez la documentation
#' ou chercher de l'aide).
#' \cr
#'
#' 2 \strong{Valeurs manquantes}
#'
#' La fonction write_hrc2 doit être utilisée de préférence sans aucun NA dans votre
#' table de correspondance. En présence de NAs, l'argument \strong{sort}
#' doit être à FALSE. En effet, les NAs seraient triés ensemble et, donc,
#' être séparées de leur place attendue dans la hiérarchie.
#'
#' Ci-dessous, nous présentons deux cas courants où les tables de correspondance
#' pourraient avoir NAs. Le premier cas est pris en charge par la fonction,
#' le second ne l'est pas.
#'
#' Soyez prudent lorsque vous manipulez des NA et vérifiez soigneusement
#' le fichier .hrc résultant ou envisagez de remplir les NAs à l'avance.
#'
#' 2.1 \emph{Hiérarchies creuses} \cr
#' Une hiérarchie est creuse si des NAs sont insérées au lieu de répéter un
#' niveau donné verticalement.
#'
#' | (**type**)   | (**details**)   |
#' |--------|------------|
#' | planet | telluric   |
#' |        | gasgiant   |
#' | star   | bluestar   |
#' |        | whitedwarf |
#' |        | reddwarf   |
#' | other  | blackhole  |
#' |        | pulsar     |
#'
#' De tels cas émettent toujours un avertissement du fait de la présence de NA,
#' mais ne posent aucun problème, si on utilise \strong{sort=FALSE}.
#'
#' 2.2 \emph{Hiérarchies non-uniformes}\cr
#' Les hiérarchies à profondeur non-uniforme correspondent aux cas où certains
#' niveaux ne sont pas détaillés jusqu'au bout, la fin de certaines lignes étant
#' manquante.
#'
#' | (**type**)   | (**details**)   |
#' |--------|------------|
#'   | planet | telluric   |
#'   | planet | gasgiant   |
#'   | star   |            |
#'   | other  | blackhole  |
#'   | other  | pulsar     |
#'
#' Le traitement d'un tel fichier générera une erreur avec les messages suivants :
#' \emph{Missing values on the last column of the correspondence table is not allowed.
#' If relevant, you could fill in with the value of the previous column}
#'
#' @return Invisible. Path to the written .hrc file.
#' \cr
#' Chemin vers le fichier .hrc.
#'
#' @examples
#' # 1. Standard example. Table will be written on your working directory.
#' # Exemple standard. La table sera écrite dans votre répertoire de travail.
#' astral <- data.frame(
#'   type      = c("planet", "planet", "star", "star", "star", "other", "other"),
#'   details   = c("telluric", "gasgiant", "bluestar", "whitedwarf", "reddwarf", "blackhole", "pulsar")
#' )
#' path <- write_hrc2(astral, hier_lead_string = "@")
#' read.table(path)
#' # Note that line order was changed ('other' comes before 'planet'), to no
#' # consequence whatsoever for Tau-Argus.
#' # Remarque : l'ordre des lignes a été modifié ('other' arrive avant 'planet'),
#' # ce qui n'a aucune conséquence pour Tau-Argus.
#'
#' # Wrong column order:
#' # Mauvais ordonnancement des colonnes :
#' astral_inv <- data.frame(
#'   details   = c("telluric", "gasgiant", "bluestar", "whitedwarf", "reddwarf", "blackhole", "pulsar"),
#'   type      = c("planet", "planet", "star", "star", "star", "other", "other")
#' )
#' path <- write_hrc2(astral_inv, hier_lead_string = "@")
#' read.table(path)
#' # Because of the inverted order, everything is written backwards : planet is a
#' # subtype of gasgiant, etc.
#' # À cause de l'inversion des colonnes, tout est écrit à l'envers : planet est
#' # devenu une sous-catégorie de gasgiant, par exemple.
#'
#' # Correction :
#' path <- write_hrc2(astral_inv, rev = TRUE, hier_lead_string = "@")
#' read.table(path)
#'
#' # 2.1 Sparse case
#' # Cas creux
#' astral_sparse <- data.frame(
#'   type      = c("planet", NA, "star", NA, NA, "other", NA),
#'   details   = c("telluric", "gasgiant", "bluestar", "whitedwarf", "reddwarf", "blackhole", "pulsar")
#' )
#' # NAs in general are risky, but, in this case, the function works well.
#' # Les valeurs manquantes causent un risque, mais, dans ce genre de cas, la fonction
#' a le comportement attendu.
#' path <- write_hrc2(astral_sparse, hier_lead_string = "@")
#' read.table(path)
#'
#' # 2.2 Non-uniform depth
#' # Hiérarchie non-uniforme
#' astral_nu <- data.frame(
#'   type      = c("planet", "planet", "star", "other", "other"),
#'   details  = c("telluric", "gasgiant", NA, "blackhole", "pulsar")
#' )
#' # The following code will generate an error
#' # (see section Details about correspondence table & .hrc)
#' path <- write_hrc2(astral_nu, hier_lead_string = "@")
#' To fix the issue, you have to fill in the NAs beforehand.
#'
#' astral_nu_fill <- data.frame(
#'   type      = c("planet", "planet", "star", "other", "other"),
#'   details  = c("telluric", "gasgiant", "star", "blackhole", "pulsar")
#' )
#' # The following code will work
#' path <- write_hrc2(astral_nu_fill, hier_lead_string = "@")
#' read.table(path)
#'
#' @importFrom zoo na.locf
#' @export

write_hrc2 <- function(corr_table,
                       output_name = NULL,
                       dir_name = NULL,
                       sort_table = FALSE,
                       rev = FALSE,
                       hier_lead_string = getOption("rtauargus.hierleadstring"),
                       adjust_unique_roots = FALSE,
                       add_char = "ZZZ"
){

  if(! any(class(corr_table) %in% c("data.frame","matrix"))){
    class_corr <- class(corr_table)
    stop(paste0("corr_table has to be a data frame or a matrix, not ", class_corr))
  }
  # Set default filename / directory
  if (is.null(output_name)) {
    givenfilename <- deparse(substitute(corr_table))
    output_name <- givenfilename
  }

  if(is.null(dir_name)){
    dir_name <- getwd()
  }else if(dir_name == ""){
    dir_name <- getwd()
  } else if(! dir.exists(dir_name)){
    stop(paste0("directory ", dir_name, " doesn't exist."))
  }

  d = dim.data.frame(corr_table)

  #### Basic verifications & formatting

  # Reverse column order if asked
  if (rev) corr_table <- rev(corr_table)

  # Make corr_table a data frame, or raise error
  corr_table <- tryCatch(
    {
      as.data.frame(corr_table)
    },
    error = function(msg){
      stop("Cannot coerce corr_table to a data frame")
      print(msg)
    }
  )

  # Check hier_lead_string
  if(nchar(hier_lead_string) != 1){
    stop("hier_lead_string should be 1 single character")
  }


  # Error if presence of NAs on the last column
  if(sum(is.na(corr_table[[d[2]]]))>0){
    stop(
      "Missing values on the last column of the correspondence table is not allowed. If relevant, you could fill in with the value of the previous column"
    )
  }

  # Warn about presence of NAs elsewhere
  if(sum(is.na(corr_table))>0){

    warning("Missing values in correspondence table will be filled in (see documentation).
            If unintended, this can cause errors when using the .hrc file with tau-Argus.")
    corr_table <- zoo::na.locf(corr_table)
  }

  if(adjust_unique_roots==TRUE){
    warning(paste0("If there is unique roots in the table, the function will create
fictional roots to adjust the hrc file for Tau-Argus, they will be created
by copying the unique roots and adding ",add_char," at the beginning
of the root character, if this creates duplicates, change the add_char
parameter"))
    corr_table <- ajouter_feuille_unique(corr_table,add_char)
  }
  # (Todo : lister cas de NA non gênantes et bloquer les autres)

  # Try to detect a problem with detailed column
  if (sum(duplicated(corr_table[,d[2]]))>0) {
    warning("There are duplicates in the expectedly most detailed level
    (last column). Please be sure columns are rightfully ordered.")
  }

  # Check if all columns are character
  # suspects <- NULL
  # for (col in 1:d[2]){
  #   if (!is.character(corr_table[,col])) {
  #     suspects <- c(suspects, col)
  #   }
  # }
  suspects <- names(corr_table[,!sapply(corr_table, is.character)])
  if(length(suspects) > 0)  message("Note : the following columns are not of character type : ", colnames(corr_table)[suspects], ". There may be an issue reading the table.")

  #### Creating the hrc file

  # 0. Sort the correspondence table
  if (sort_table){
    for (j in 1:d[2]){
      corr_table <- corr_table[
        order(corr_table[,d[2]-j+1])
        ,]
      # CORR JJ à vérifier
      # sort the table is not efficient if there are NA values !
      # corr_table <- corr_table[
      #   order(corr_table[,1])
      #   ,]
    }
  }

  # 1. Compare cell values in order to erase duplicates (vertically / horizontally)

  corr_table_decale <- rbind(
    rep("line1"),
    corr_table[1:(d[1]-1),]
  )
  corr_table_dec_left <- cbind(
    w = rep("col1"),
    corr_table[,1:d[2]-1]
  )

  compare <- corr_table == corr_table_decale #<-- cells identical to their upper
  # neighbour
  compare_left <- corr_table == corr_table_dec_left
  missing <- is.na(corr_table)

  # 2. Add a fitting number of hier_lead_string to all

  depth_table <- as.data.frame(
    matrix(0:(d[2]-1),nrow = d[1], ncol = d[2], byrow = TRUE)
  )

  # the numeric values (from 0 to d2 -1) correspond to the depth in the
  # hierarchy, which will govern how many hier_lead_string are added when
  # writing the hrc.
  # One adjustment has to be done for cases when a same level is repeated
  # in a line :

  compare_col <- t(apply(
    compare_left,
    MARGIN = 1,
    cumsum
  ))
  depth_table <- depth_table - compare_col

  for(col in 1:d[2]){
    corr_table[,col] <- vect_aro(
      string = corr_table[,col],
      number = depth_table[,col],
      hier_lead_string
    )
  }


  # for (colonne in 1:d[2]) {
  #   corr_table[,colonne] <- paste0(
  #     paste0(rep(hier_lead_string, colonne-1), collapse = ""),
  #     corr_table[,colonne],
  #     "\n")
  # }

  corr_table[compare] <- ""
  corr_table[compare_left] <- ""
  corr_table[missing] <- ""

  # 3. Write corresponding table
  # Note that columns & cells are not separated by anything, but cells that have
  # not been erased still hold a line break ("\n") so that there will be line
  # breaks only after non-void characters.

  loc_file <- paste0(c(dir_name, "/", output_name, ".hrc"), collapse = "")

  write.table(
    x = corr_table,
    file = loc_file,
    quote = FALSE,
    row.names = FALSE,
    col.names = FALSE,
    sep = "",
    eol = ""
  )

  invisible(loc_file)
}


