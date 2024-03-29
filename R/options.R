# Valeurs par défaut (utilisées ici et dans .onLoad)
# Endroit unique où modifier ces valeurs internes aux package
# (y compris pour mise à jour tableau rubrique aide)

op.rtauargus <- list(
  # asc-rda ----------------------------------------------------- #
  rtauargus.decimals           = 0L,
  rtauargus.totcode            = "Total",
  rtauargus.missing            = "",
  rtauargus.hierleadstring     = "@",
  # tab_rda --------------------------------------------------------- #
  rtauargus.separator          = ",",
  # arb --------------------------------------------------------- #
  rtauargus.response_var       = "<freq>",
  rtauargus.weighted           = FALSE,
  rtauargus.linked             = FALSE,
  rtauargus.output_type        = "4",
  rtauargus.output_options     = "",
  # run_arb ----------------------------------------------------- #
  rtauargus.missing_dir        = "stop",
  rtauargus.tauargus_exe       = "Y:/Logiciels/TauArgus/TauArgus.exe",
  rtauargus.show_batch_console = FALSE,
  rtauargus.import             = FALSE,
  rtauargus.is_tabular         = TRUE
)

df_op.rtauargus <- function(html = FALSE) {

  # convertit la liste en un data.frame (pour la documentation)

  op <- op.rtauargus

  val <- data.frame(
    option = names(op),
    val = unlist(op),
    ordre = seq_along(op),
    type = vapply(op, typeof, character(1)),
    stringsAsFactors = FALSE
  )

  fun_names <- c("micro_asc_rda", "tab_rda", "micro_arb", "tab_arb", "run_arb")

  param_names <- lapply(fun_names, function(x) names(formals(get(x))))
  func <- data.frame(
    option = paste0("rtauargus.", unlist(param_names)),
    func = rep(fun_names, vapply(param_names, length, 1L))
  )

  res <- merge(val, func, by = "option")
  res <- res[order(res$ordre), ]
  res$ordre <- NULL
  row.names(res) <- NULL

  res$val <-
    ifelse(
      res$type == "character",
      cite(res$val, ignore_vide = FALSE),
      res$val
    )

  if (html) {
    res$val <- gsub("<", "&lt;", res$val)
    res$val <- gsub(">", "&gt;", res$val)
  }

  res

}


#' Manages options of rtauargus package
#'
#' Manages (displays, modifies, resets) the options of rtauargus package. \cr
#' Gère les options du package (affiche, modifie, réinitialise).
#'
#' @usage
#' rtauargus_options()
#'
#' # options(rtauargus.<opt> = <val>)
#'
#' @details
#'
#' The options of the package define the default behaviour of the functions.
#'
#' These options are used if a mandatory argument of a function is not set
#' by the user. They let not to systematically repeat the same parameter
#' for each call of a function. The name of the option is the same as the
#' name of the function prefixed by `rtauargus.` :
#'
#' *For example, `rtauargus.decimals` will be used if the argument
#' `decimals` in the `micro_asc_rda` function is not set by the
#' user.*
#'
#' On loading the package, all the rtauargus options, that are not already
#' been set by the user, are set with their default values (see table below).
#' The already defined options keep the values set by the user.
#'
#' The options can be set during a session with the following instruction
#' `options(rtauargus.`...` = `...`)`, or with a configuration
#' file where the user have written its own options with such instructions,
#' but this latter is not a proper way if reproducibility is sought.
#' Les options du package définissent les comportements par défaut des
#' fonctions.
#'
#' If the user inadvertently removes some options, the functions will use
#' the default values of the package. \cr
#'
#' (Ces options sont utilisées si un argument obligatoire d’une fonction n’est
#' pas renseigné. Elles permettent de ne pas répéter systématiquement le même
#' paramètre à chaque appel d'une fonction. Le nom de l’option est le nom de
#' l’argument d’une fonction précédé de `rtauargus.` :
#'
#' *Par exemple, `rtauargus.decimals` sera la valeur utilisée si l’argument
#' `decimals` de la fonction `micro_asc_rda` n’est pas renseigné par
#' l’utilisateur.*
#'
#' Au chargement, le package attribue une valeur par défaut à toutes les options
#' de rtauargus qui ne sont pas encore déclarées (cf. tableau ci-dessous). Les
#' options déjà définies par l'utilisateur gardent leurs valeurs.
#'
#' Elles peuvent être redéfinies pour une session par une instruction
#' `options(rtauargus.`...` = `...`)`, ou de manière globale si
#' de telles instructions sont placées dans un fichier de configuration propre à
#' l'utilisateur (fortement déconseillé si le programme a vocation à être
#' reproductible).
#'
#' En cas d'effacement accidentel d'une option par l'utilisateur, les fonctions
#' utiliseront les valeurs par défaut du package.)
#'
#' @param ... names of the options to reset, separated by commas. If no name is
#' specified, all the options will be reset. The prefix `"rtauargus."`
#' is not required. \cr
#' noms des options à réinitialiser, séparés par des virgules. Si
#'   aucun nom n'est spécifié, toutes les options du package seront
#'   réinitialisées. Le préfixe `"rtauargus."` est facultatif.
#'
#' @section List of options:
#' \tabular{lll}{
#'  **Option**                  \tab  **Default Value**                                    \tab **Function**   \cr
#'  `------------------------`  \tab  `---------------------------------`                      \tab `-------------`\cr
#'  rtauargus.decimals               \tab  \Sexpr{rtauargus:::op.rtauargus$rtauargus.decimals}           \tab [micro_asc_rda]\cr
#'  rtauargus.totcode                \tab "\Sexpr{rtauargus:::op.rtauargus$rtauargus.totcode}"           \tab                     \cr
#'  rtauargus.missing                \tab "\Sexpr{rtauargus:::op.rtauargus$rtauargus.missing}"           \tab                     \cr
#'  rtauargus.hierleadstring         \tab "\Sexpr{rtauargus:::op.rtauargus$rtauargus.hierleadstring}"    \tab                     \cr
#'  `------------------------`  \tab  `---------------------------------`                      \tab `-------------`\cr
#'  rtauargus.response_var           \tab "\Sexpr{rtauargus:::op.rtauargus$rtauargus.response_var}"      \tab [micro_arb]    \cr
#'  rtauargus.weighted               \tab  \Sexpr{rtauargus:::op.rtauargus$rtauargus.weighted}           \tab                     \cr
#'  rtauargus.linked                 \tab  \Sexpr{rtauargus:::op.rtauargus$rtauargus.linked}             \tab                     \cr
#'  rtauargus.output_type            \tab "\Sexpr{rtauargus:::op.rtauargus$rtauargus.output_type}"       \tab                     \cr
#'  rtauargus.output_options         \tab "\Sexpr{rtauargus:::op.rtauargus$rtauargus.output_options}"    \tab                     \cr
#'  `------------------------`  \tab  `---------------------------------`                      \tab `-------------`\cr
#'  rtauargus.missing_dir            \tab "\Sexpr{rtauargus:::op.rtauargus$rtauargus.missing_dir}"       \tab [run_arb]      \cr
#'  rtauargus.tauargus_exe           \tab "\Sexpr{rtauargus:::op.rtauargus$rtauargus.tauargus_exe}"      \tab                     \cr
#'  rtauargus.show_batch_console     \tab  \Sexpr{rtauargus:::op.rtauargus$rtauargus.show_batch_console} \tab                     \cr
#'  rtauargus.import                 \tab  \Sexpr{rtauargus:::op.rtauargus$rtauargus.import}             \tab
#' }
#'
#' @examples
#' rtauargus_options()
#'
#' # modifies some options
#' options(
#'   rtauargus.tauargus_exe = "Z:/tmp/TauArgus.exe",
#'   rtauargus.output_type = "4",
#'   rtauargus.weighted = TRUE
#' )
#' str(rtauargus_options())
#'
#' # resets some options (prefix "rtauargus." facultatif)
#' reset_rtauargus_options("output_type", "rtauargus.tauargus_exe")
#' str(rtauargus_options())
#'
#' # resets everything
#' reset_rtauargus_options()
#' str(rtauargus_options())
#' @seealso [options], R options system \cr
#' le système d'options de R dans lequel s'insèrent les options de ce package.
#' @export
#' @rdname rtauargus_options

rtauargus_options <- function() {

  # affiche les options du package
  opt <- options()
  rtauargus_options <- grep("^rtauargus\\.", names(opt))
  opt[rtauargus_options]

}

#' @export
#' @rdname rtauargus_options

reset_rtauargus_options <- function(...) {

  # reinitialise certaines options ou toutes

  modif <- c(...)

  if (length(modif) == 0) {

    modif <- names(op.rtauargus)

  } else {

    # autorise nom sans prefixe rtauargus.
    modif <-
      ifelse(
        grepl("^rtauargus\\.", modif),
        modif,
        paste0("rtauargus.", modif)
      )

    # warning si options inconnues
    inconnu <- modif[!modif %in% names(op.rtauargus)]
    if (length(inconnu)) {
      warning(
        "impossible de reinitisaliser : ",
        paste(inconnu, collapse = ", ")
      )
    }

  }

  options(op.rtauargus[modif])

  invisible(rtauargus_options())

}
