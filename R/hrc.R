# Fonction exportée -------------------------------------------------------

#' Crée un fichier hrc à partir de microdonnées
#'
#' Crée un fichier hrc (hiérarachie) à partir de plusieurs variables d'un jeu de
#' microdonnées.
#'
#' La fonction reconstitue la hiérarchie des variables à partir des niveaux
#' présents dans les données. Les variables dans \code{vars_hrc} doivent être
#' \strong{classées de la plus fine à la plus agrégée}.
#'
#' La relation entre chaque niveau hiérarchique doit être une application (au
#' sens mathématique du terme), c'est-à-dire que chaque niveau fin doit avoir un
#' seul et unique niveau agrégé correspondant. La création de la hiérarchie est
#' impossible si cette condition n'est pas remplie.
#'
#' Si le nom du fichier en sortie n'est pas spécifié, un fichier temporaire est
#' créé. Il sera effacé en fin de session. Le chemin vers ce fichier peut être
#' récupéré dans la valeur de retour de la fonction.
#'
#' Les valeurs manquantes présentes dans les variables hiérarchiques seront
#' préalablement imputées à l'aide d'une autre variable hiérarchique (paramètre
#' \code{fill_na}). En stratégie ascendante (\code{"up"}), les variables sont
#' parcourues de la plus agrégée à la plus fine, et inversement en stratégie
#' descendante (\code{"down"}).
#'
#' Le paramètre \code{compact} permet de créer des hiérarchies à profondeurs
#' variables. L'idée est de couper les branches constituées d'une seule valeur
#' répétée jusqu'à la profondeur maximale (voir exemples).
#'
#' @inheritParams micro_asc_rda
#' @param vars_hrc \strong{[obligatoire]} vecteur des noms des variables
#'   constituant la hiérarchie, du niveau le plus fin au niveau le plus agrégé.
#' @param hrc_filename nom et emplacement du fichier hrc produit. Si non
#'   renseigné, un fichier temporaire.
#' @param fill_na remplissage d'éventuelles valeurs manquantes, à l'aide d'une
#'   autre variable :\itemize{
#'     \item{\code{"up"} (défaut) : variable hiérarchique de niveau
#'        immédiatement supérieur}
#'     \item{\code{"down"} : variable hiérarchique de niveau immédiatement
#'        inférieur}
#'    }
#' @param compact pour élaguer les branches répétant une unique valeur jusqu'au
#'   plus bas niveau de profondeur (\code{TRUE} par défaut).
#' @param hierlevels si une seule variable est spécifiée dans \code{vars_hrc},
#'   permet de générer la hiérarchie selon la position des caractères dans la
#'   chaîne. Par exemple, \code{hierlevels = "2 3"} pour construire une
#'   hiérarchie département-commune à partir d'un code commune.
#'
#' @return Le nom du fichier hrc (utile dans le cas d'un fichier temporaire au
#'   nom aléatoire).
#' @examples
#' # Hierarchie "complete" ............................
#'
#' df_naf <- data.frame(
#'   A10 = c("AZ", "BE", "BE", "BE", "BE", "BE", "BE"),
#'   A21 = c("A" , "C" , "B" , "C" , "C" , "C" , "B" ),
#'   A88 = c("01", "10", "06", "10", "12", "11", "07")
#' )
#'
#' tmp_file <- write_hrc(df_naf, c("A88", "A21", "A10"))
#' file.show(tmp_file, pager = "console")
#'
#' tmp_file <- write_hrc(df_naf, c("A88", "A10"), hierleadstring = ":")
#' file.show(tmp_file, pager = "console")
#'
#' # Hierarchie de profondeur variable  ...............
#'
#' df <- data.frame(
#'   niv1 = c("A"  , "A"  , "A"  , "B"  , "C"  , "C" ),
#'   niv2 = c("A1" , "A1" , "A2" ,  NA  , "C1" , "C2"),
#'   niv3 = c("A1x", "A1y",  NA  ,  NA  , "C1" ,  NA )
#' )
#'
#' tmp_file <- write_hrc(df, c("niv3", "niv2", "niv1"))
#' file.show(tmp_file, pager = "console")
#'
#' tmp_file <- write_hrc(df, c("niv3", "niv2", "niv1"), compact = FALSE)
#' file.show(tmp_file, pager = "console")
#'
#' @export

write_hrc <- function(microdata,
                      vars_hrc,
                      hierleadstring = getOption("rtauargus.hierleadstring"),
                      hrc_filename = NULL,
                      fill_na = c("up", "down"),
                      compact = TRUE,
                      hierlevels = NULL) {

  if (is.null(hrc_filename)) hrc_filename <- tempfile("RTA_", fileext = ".hrc")

  # hierlevels

  if (!is.null(hierlevels)) {
    if (length(vars_hrc) != 1) {
      stop("avec hierlevels, une seule variable hierarchique a specifier")
    }
    microdata <- df_hierlevels(microdata[[vars_hrc]], hierlevels)
    vars_hrc <- names(microdata)
  }

  # verifs

  stopifnot(length(vars_hrc) > 0)

  absents <- !vars_hrc %in% names(microdata)
  if (any(absents)) {
    stop(
      "colonne(s) introuvable(s) : ",
      paste(vars_hrc[absents], collapse = ", ")
    )
  }

  if (length(vars_hrc) == 1) {
    res <- as.character(microdata[[vars_hrc]])
    writeLines(sort(unique(res)), hrc_filename)
    warning("hierarchie d'un seul niveau")
    return(invisible(hrc_filename))
  }

  # remplit NA
  if (anyNA(microdata[vars_hrc])) {
    stopifnot(fill_na[1] %in% c("up", "down"))
    vars_fill <- if (fill_na[1] == "up") rev(vars_hrc) else vars_hrc
    microdata <- fill_na_hrc(microdata, vars_fill)
    warning("valeurs manquantes imputees pour construire la hierarchie")
  }

  # construit hiérarchie sous forme de liste
  list_hrc <- hrc_list(microdata, vars_hrc)

  val_prof <- prof_list(list_hrc)

  # coupe branches avec unique valeur depuis une profondeur donnée
  if (compact) val_prof <- val_prof[!following_dup(names(val_prof))]

  # génère vecteur format Tau-Argus
  res <-
    purrr::imap_chr(
      val_prof,
      ~ paste0(strrep(hierleadstring, .x), .y)
    )

  # vérif validité arbre
  if (!check_seq_prof(val_prof)) {
    vars_str <- paste(vars_hrc, collapse = " > ")
    res_str <- paste(res, collapse = "\n")
    stop(
      "Niveaux de hierarchie incoherents '", vars_str, "'\n",
      "   (essayer avec compact = FALSE ?)\n",
      res_str
    )
  }

  # écrit
  writeLines(res, hrc_filename)
  invisible(hrc_filename)

}


# Fonction préparant les données ------------------------------------------

fill_na_hrc <- function(microdata, vars) {

  # remplit les valeurs manquants en "coalescant" les variables 2 à 2
  # (de gauche à droite)

  res <- microdata[vars]
  res[] <- lapply(res, as.character)

  n <- length(vars)

  for (i in seq(n - 1)) {

   var_ref <- vars[i]
   var_cible <- vars[i + 1]

   res[[var_cible]] <- dplyr::coalesce(res[[var_cible]], res[[var_ref]])

  }

  res

}


# Fonctions créant une liste hiérarchique ---------------------------------

#' @importFrom dplyr %>%

sublevels <- function(fin, agr) {

  # crée une liste à deux niveaux à partir de microdonnées (vecteurs)
  # écarte les croisements inexploitables (NA dans fin ou dans agr)

  compt <- table(fin, agr, useNA = "no")

  if (sum(compt) == 0) {
    stop("aucun croisement exploitable (valeurs manquantes ?)")
  }

  if (!is_hrc(compt)) {
    stop(
      "variables non hierarchiques ",
      "(meme niveau fin dans plusieurs niveaux agreges differents)"
    )
  }

  compt <-
    compt %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    subset(Freq > 0)

  tapply(
    compt$fin,
    INDEX = compt$agr,
    function(x) {
      names(x) <- x
      res <- lapply(x, function(x) NULL)
      list(res) # pour empecher simplification
    }
  )

}

imbrique <- function(fin, agr) {

  purrr::map(agr, ~ fin[names(.)])

}

hrc_list <- function(df, vars_hrc) {

  # calculs passage levels 2 à 2 ...............

  n <- length(vars_hrc)
  levs <- vector("list", n - 1)

  for (i in seq(n - 1)) {

    nom_fin <- vars_hrc[i]
    nom_agr <- vars_hrc[i + 1]
    levs[[i]] <- sublevels(df[[nom_fin]], df[[nom_agr]])

  }

  # imbrication ................................

  Reduce(imbrique, levs)

}


# Fonction parcourant la liste hiérarchique -------------------------------

prof_list <- function(z, level = 0,result = NULL) {

  if (length(z) > 0) {

    for (i in length(z):1) {
      names(level) <- names(z)[i]
      result <- c(level, prof_list(z[[i]], level + 1, result))
    }

  }

  result

}


# Fonctions de validation -------------------------------------------------

is_hrc <- function(crois_fin_agr) {

  # prend en argument un comptage (S3:table, matrice) et renvoie vrai
  # ssi chaque niveau fin correspond à un seul niveau agrégé
  # (i.e. le relation fin -> agr est une application au sens mathématique)
  # les lignes remplies de 0 sont aussi acceptées (causées par des NA)

  if (!length(crois_fin_agr)) return(FALSE)

  fin_unique_agr <-
    apply(
      crois_fin_agr,
      1,
      function(x) sum(as.logical(x)) %in% 0:1 # 0 aussi autorisé
    )

  all(fin_unique_agr)

}

check_seq_prof <- function(x) {

  # vérifie un vecteur composé des niveaux de profondeurs dans l'ordre
  # d'écriture du fichier hrc, pour voir s'il n'y  pas de profondeurs
  # manquantes

  if (!length(x)) return(FALSE)
  if (x[1] != 0) return(FALSE)
  if (any(x < 0)) return(FALSE)
  if (any(floor(x) != x)) return(FALSE)

  x1 <- x[-1]
  ecart <- x1 - x[-length(x)]

  ok <- (x1 > 0 & ecart <= 1) | (x1 == 0 & ecart <= 0)
  all(ok)

}


# Autre -------------------------------------------------------------------

normalise_hrc <- function(params_hrc,
                          microdata = NULL,
                          hierleadstring = NULL) {

  # normalise un vecteur de paramètres sur les hiérarchies (transmis à
  # micro_as_rda par exemple)
  # - pour les syntaxes raccourcies 'v1 > v2 > ...', génère le fichier
  #     temporaire et renvoie le fichier
  # - normalizePath des fichiers hrc déjà existants
  # - laisse le reste en l'état (hierlevels)

  # type param

  validrname <- "[\\.]?[[:alpha:]][\\._[:alnum:]]*"
  vars <- grep(sprintf("^(%s *> *)+%s$", validrname, validrname), params_hrc)
  fich <- grep(".+\\.hrc$", params_hrc)
  lvls <- grep("^(\\d+ +)+\\d+$", params_hrc)

  params_err <- setdiff(seq_along(params_hrc), c(vars, fich, lvls))

  if (length(params_err)) {
    stop(
      "Parametres hrc incorrects :\n   ",
      paste(unname(params_hrc[params_err]), collapse = "\n   ")
    )
  }

  # transformations

  if (length(vars)) {

    if (is.null(microdata)) {
      stop("specifier microdata pour construire une hierarchie 'v1 > v2 > ...'")
    }

    if (is.null(hierleadstring)) stop("specifier hierleadstring")

    list_vars <- strsplit(params_hrc[vars], " *> *")

    hrc_files <-
      purrr::map_chr(
        .f = write_hrc,
        list_vars,
        microdata = microdata,
        hierleadstring = hierleadstring
      )

    params_hrc[vars] <- hrc_files

  }

  if (length(fich)) {

    params_hrc[fich] <- normPath2(params_hrc[fich])

  }

  params_hrc

}

#' @importFrom dplyr %>%

df_hierlevels <- function(var_hrc, hierlevels) {

  # construit un data.frame pouvant être lu par hrc_list, à partir d'un vecteur
  # et de niveaux hiérarchiques par position dans la chaîne

  hierlevels <- trimws(hierlevels)
  if (!grepl("^(\\d+ +)+\\d+$", hierlevels)) {
    stop("hierlevels doit contenir plusieurs chiffres separes par des espaces")
  }

  var_hrc <- var_hrc %>% unique() %>% as.character()
  n1 <- nchar(var_hrc[1])
  if (any(nchar(var_hrc) != n1)) {
    stop("le nombre de caracteres doit etre identique pour tous les elements")
  }

  lev <- strsplit(hierlevels, " +")[[1]]
  lev <- as.integer(lev) %>% `[`(. != 0)
  if (sum(lev) != n1) {
    stop("la somme de hierlevels doit etre egale au nombre de caracteres")
  }

  lev <- lev %>% cumsum() %>% rev() %>% `[`(-1)

  res <- data.frame(var_hrc, stringsAsFactors = FALSE)
  for (i in seq_along(lev)) res[[i + 1]] <- substr(var_hrc, 1, lev[i])
  res

}
