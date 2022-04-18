#' Secrétisation en masse
#'
#' Optimisation de la fonction \code{\link{rtauargus}} pour un grand nombre de
#' croisements (ayant tous les mêmes paramètres).
#'
#' En mode interactif, Tau-Argus peut traiter jusqu'à 10 tabulations
#' simultanément pour un même jeu de microdonnées. En mode batch, un nombre plus
#' important de croisements peut être spécifié. Cependant, procéder de la sorte
#' peut être particulièrement long, car Tau-Argus prend beaucoup de temps
#' pour lire des fichiers texte volumineux de microdonnées.
#'
#' \code{rtauargus_plus} permet d'améliorer la vitesse d'exécution. La fonction
#' découpe la liste des tabulations en groupes de taille \code{grp_size} et
#' effectue un appel à \code{rtauargus} pour chaque groupe. Elle écrit un
#' fichier asc restreint aux seules variables effectivement utilisées au sein
#' d'un groupe.
#'
#' Les résultats sont ensuite agrégés en une unique liste, comme si Tau-Argus
#' n'avait été appelé qu'une seule fois.
#'
#' Modifier \code{grp_size} ne changera pas le résultat, seulement le temps
#' d'exécution. Une valeur autour de 5 (défaut) semble être un bon compromis
#' entre une lecture de fichiers asc trop volumineux et un nombre d'appels à
#' Tau-Argus trop important. Elle peut être ajustée en fonction du nombre de
#' variables communes à l'intérieur de chaque groupe de tabulations.
#'
#' @section Limites par rapport à la fonction \code{rtauargus}:
#'
#' En contrepartie de la vitesse d'exécution, les croisements doivent avoir les
#' mêmes caractéristiques (mêmes règles de secret primaire, même méthode de
#' secret secondaire, même variable de pondération, etc.). Les paramètres
#' \code{safety_rules}, \code{supress}, ..., doivent donc contenir une valeur
#' unique.
#'
#' De plus, il n'est pas possible de spécifier \code{asc_filename},
#' \code{rda_filename}, \code{arb_filename} ou \code{output_names} pour
#' récupérer les fichiers intermédiaires. Ces fichiers seront écrits dans un
#' dossier temporaire (et écrasés à chaque nouveau groupe). Par conséquent,
#' spécifier \code{import = FALSE} est sans objet et sera ignoré.
#'
#' Les données doivent obligatoirement être un data.frame (fichiers asc et rda
#' pas autorisés).
#'
#' Si l'option \code{linked} est utilisée, la liaison ne sera effective qu'à
#' l'intérieur de chaque groupe de tabulations.
#'
#' @param grp_size nombre de tableaux par appel de Tau-Argus (un entier compris
#'   entre 1 et 10).
#' @inheritParams micro_asc_rda
#' @inheritParams micro_arb
#' @inheritParams rtauargus
#' @param suppress [\strong{obligatoire}] méthode de gestion du secret
#'   secondaire (syntaxe batch de Tau-Argus). Une seule méthode autorisée pour
#'   tous les tableaux. Exemple \code{"GH(.,100)"} (le point jouant le rôle du
#'   numéro de tabulation).
#'
#' @return Une liste de data.frames (tableaux secrétisés).
#'
#' @seealso \code{\link{rtauargus}}, fonction appelée de manière répétée par
#'   \code{rtauargus_plus}.
#'
#' @examples
#' \dontrun{
#' # exemple de ?rtauargus, avec une repetition (artificielle) des croisements
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
        do.call(rtauargus, params_rtauargus)
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
