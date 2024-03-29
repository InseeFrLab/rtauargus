#' Mass protection
#'
#' Optimization of the function \code{link{rtauargus}} for a large number of
#' crossovers (all having the same parameters). \cr
#' (Optimisation de la fonction [micro_rtauargus()] pour un grand nombre de
#' croisements (ayant tous les mêmes paramètres).)
#'
#' In interactive mode, Tau-Argus can process up to 10 tabs
#' simultaneously for the same microdata set. In batch mode, a larger number of
#' of crossings can be specified. However, doing so
#' can be particularly time consuming, as Tau-Argus takes a lot of time
#' to read large text files of microdata.
#'
#' `rtauargus_plus` helps to improve the speed of execution. The function
#' splits the list of tabs into groups of size `grp_size` and
#' makes a call to `micro_rtauargus` for each group. It writes an
#' asc file restricted to the only variables actually used within a
#' of a group.
#'
#' The results are then aggregated into a single list, as if Tau-Argus
#' had been called only once.
#'
#' Modifying `grp_size` will not change the result, only the time
#' execution time. A value around 5 (default) seems to be a good compromise
#' between reading too large asc files and calling Tau-Argus too often.
#' Tau-Argus too much. It can be adjusted according to the number of
#' common variables within each tab group.
#
#' (En mode interactif, Tau-Argus peut traiter jusqu'à 10 tabulations
#' simultanément pour un même jeu de microdonnées. En mode batch, un nombre plus
#' important de croisements peut être spécifié. Cependant, procéder de la sorte
#' peut être particulièrement long, car Tau-Argus prend beaucoup de temps
#' pour lire des fichiers texte volumineux de microdonnées.
#'
#' `rtauargus_plus` permet d'améliorer la vitesse d'exécution. La fonction
#' découpe la liste des tabulations en groupes de taille `grp_size` et
#' effectue un appel à `micro_rtauargus` pour chaque groupe. Elle écrit un
#' fichier asc restreint aux seules variables effectivement utilisées au sein
#' d'un groupe.
#'
#' Les résultats sont ensuite agrégés en une unique liste, comme si Tau-Argus
#' n'avait été appelé qu'une seule fois.
#'
#' Modifier `grp_size` ne changera pas le résultat, seulement le temps
#' d'exécution. Une valeur autour de 5 (défaut) semble être un bon compromis
#' entre une lecture de fichiers asc trop volumineux et un nombre d'appels à
#' Tau-Argus trop important. Elle peut être ajustée en fonction du nombre de
#' variables communes à l'intérieur de chaque groupe de tabulations.)
#'
#'
#' @section Limits in relation to the function `micro_rtauargus`:
#'
#' In return for the speed of execution, the crossings must have the
#' same characteristics (same primary secret rules, same secondary secret method,
#' same secondary secret, same weighting variable, etc.). The parameters
#' `safety_rules`, `supress`, ..., must therefore contain a
#' unique value.
#'
#' Moreover, it is not possible to specify `asc_filename`,
#' `rda_filename`, `arb_filename` or `output_names` to
#' retrieve the intermediate files. These files will be written to a
#' temporary folder (and overwritten with each new group). Therefore,
#' specifying `import = FALSE` is irrelevant and will be ignored.
#'
#' The data must be a data.frame (asc and rda files not allowed).
#'
#' If the `linked` option is used, the link will only be effective at
#' within each group of tabs. \cr
#'
#' (En contrepartie de la vitesse d'exécution, les croisements doivent avoir les
#' mêmes caractéristiques (mêmes règles de secret primaire, même méthode de
#' secret secondaire, même variable de pondération, etc.). Les paramètres
#' `safety_rules`, `supress`, ..., doivent donc contenir une valeur
#' unique.
#'
#' De plus, il n'est pas possible de spécifier `asc_filename`,
#' `rda_filename`, `arb_filename` ou `output_names` pour
#' récupérer les fichiers intermédiaires. Ces fichiers seront écrits dans un
#' dossier temporaire (et écrasés à chaque nouveau groupe). Par conséquent,
#' spécifier `import = FALSE` est sans objet et sera ignoré.
#'
#' Les données doivent obligatoirement être un data.frame (fichiers asc et rda
#' pas autorisés).
#'
#' Si l'option `linked` est utilisée, la liaison ne sera effective qu'à
#' l'intérieur de chaque groupe de tabulations.)
#'
#' @param grp_size number of tables per Tau-Argus call (an integer between
#' between 1 and 10). \cr
#' (nombre de tableaux par appel de Tau-Argus (un entier compris
#'   entre 1 et 10).)
#' @inheritParams micro_asc_rda
#' @inheritParams micro_arb
#' @inheritParams micro_rtauargus
#' @param suppress  secondary secret management method
#' (Tau-Argus batch syntax). Only one method allowed for
#' all tables. Example : `"GH(.,100)"` (the dot playing the role of the
#' tabulation number). \cr
#'  méthode de gestion du secret
#'   secondaire (syntaxe batch de Tau-Argus). Une seule méthode autorisée pour
#'   tous les tableaux. Exemple `"GH(.,100)"` (le point jouant le rôle du
#'   numéro de tabulation).
#'
#' @return A list of data.frames (secret arrays). \cr
#' (Une liste de data.frames (tableaux secrétisés).)
#'
#' @seealso [micro_rtauargus()], a function called repeatedly by
#' `rtauargus_plus`. \cr
#' fonction appelée de manière répétée par
#'   `rtauargus_plus`.
#'
#' @examples
#' \dontrun{
#' # example of ?rtauargus, with an (artificial) repetition of the crossings
#' rtauargus_plus(
#'   grp_size         = 6, # (pour lancer les 12 croisements en 2 appels)
#'   microdata        = data.frame(V1 = c("A", "A", "B", "C", "A"), V2 = "Z"),
#'   explanatory_vars = rep(list("V1", "V2", c("V1", "V2")), times = 4),
#'   safety_rules     = "FREQ(3,10)",
#'   suppress         = "GH(.,100)"
#' )}
#' @export

rtauargus_plus <- function(grp_size = 5,
                           microdata,
                           explanatory_vars,
                           safety_rules,
                           suppress,
                           ...) {

  # verif préalables

  if(!is.data.frame(microdata)) {
    stop("`microdata` doit etre un data.frame")
  }

  if (length(safety_rules) > 1 | length(suppress) > 1) {
    stop(
      "impossible, ",
      "regles des secretisation doivent etre les memes pour tous les tableaux"
    )
  }

  if (grp_size < 1 | grp_size > 10) { # limitation Tau-Argus pour un groupe
    stop("`grp_size` doit etre compris entre 1 et 10")
  }

  # retravaille les paramètres dans (...)

  .dots <- list(...)
  if (!is.null(.dots$asc_filename) |
      !is.null(.dots$rda_filename) |
      !is.null(.dots$arb_filename) |
      !is.null(.dots$output_names)) {
    warning("parametres `_names` ignores, utilisation de fichiers temporaires")
    .dots <- .dots[!grepl("names?$", names(.dots))]
  }
  .dots$import <- TRUE # import obligatoire pour récupérer les data.frames
  if (!is.null(.dots$linked)) {
    message("`linked` s'appliquera uniquement entre tableaux d'un meme groupe")
  }
  params_used_var <- param_function(used_var, .dots)

  # appels rtauargus

  ntab <- length(explanatory_vars)
  grps <- (seq(ntab) - 1) %/% grp_size

  res_list <-
    lapply(
      split(seq(ntab), grps),
      function(ntabs) {
        # variables strictement nécessaires au groupe en cours
        params_used_var$explanatory_vars <- explanatory_vars[ntabs]
        vars <- do.call(used_var, params_used_var)
        # appel TA
        params_rtauargus <- c(
          list(
            microdata        = microdata[vars],
            explanatory_vars = explanatory_vars[ntabs],
            safety_rules     = safety_rules,
            suppress         = suppress
          ),
          .dots
        )
        do.call(micro_rtauargus, params_rtauargus)
      }
    )

  # aplatit liste de listes de data.frames en une liste de data.frames

  res_unnamed <-
    unlist(
      res_list,
      recursive = FALSE,
      use.names = FALSE
    )

  # remet les noms éventuels

  stats::setNames(
    res_unnamed,
    names(explanatory_vars)
  )

}
