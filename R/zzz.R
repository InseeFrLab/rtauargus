# internal state of the package (e.g. how the Tau-Argus location was found)
rtauargus_state <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {

  # location de l'executable tauargus (version open source sur AUS)
  # initialisé au chargement du package seulement
  #   si n'existe pas déjà dans les options de R
   # op.rtauargus défini dans package_options.R

  # tauargus_exe: TAUARGUS_EXE environment variable, .Renviron or automatic
  # search (see locate_tauargus_exe())

  op <- options()
  defaults <- op.rtauargus
  toset <- !(names(defaults) %in% names(op))

  if ("rtauargus.tauargus_exe" %in% names(defaults)[toset]) {
    loc <- locate_tauargus_exe()
    rtauargus_state$exe_source <- attr(loc, "source")
    defaults[["rtauargus.tauargus_exe"]] <- as.character(loc)
  } else {
    rtauargus_state$exe_source <- "option"
  }

  if (any(toset)) options(defaults[toset])

  invisible()

}

.onAttach <- function(libname, pkgname) {

  ta_exe <- getOption("rtauargus.tauargus_exe")
  if (is.null(ta_exe)) ta_exe <- ""

  status <-
    if (!nzchar(ta_exe)) {
      paste0(
        'Tau-Argus : not found\n',
        '  (you will be asked to select TauArgus.exe when it is first needed)\n'
      )
    } else if (!file.exists(ta_exe)) {
      paste0('Tau-Argus : "', ta_exe, '"\n  (note: file not found)\n')
    } else if (identical(rtauargus_state$exe_source, "detected")) {
      paste0('Tau-Argus : "', ta_exe, '"\n  (auto-detected)\n')
    } else {
      paste0('Tau-Argus : "', ta_exe, '"\n')
    }

  packageStartupMessage(
    '\n',
    status,
    '\n',
    '  To set the location permanently (saved in .Renviron) :\n',
    '    set_tauargus_exe("path/to/TauArgus.exe", install = TRUE)\n\n',
    '  To set it for the current session only :\n',
    '    set_tauargus_exe("path/to/TauArgus.exe")\n\n',
    '  To display all the options of the package :\n',
    '    rtauargus_options()',
    '\n'
  )

}

.onUnload <- function(libpath) {

  options(purrr::map(op.rtauargus, ~ NULL))
  packageStartupMessage('rtauargus : package options disabled')

}
