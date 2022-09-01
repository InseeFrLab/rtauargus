.onLoad <- function(libname, pkgname) {

  # location de l'executable tauargus (version open source sur AUS)
  # initialisé au chargement du package seulement
  #   si n'existe pas déjà dans les options de R
   # op.rtauargus défini dans package_options.R

  op <- options()
  toset <- !(names(op.rtauargus) %in% names(op))
  if (any(toset)) options(op.rtauargus[toset])

  invisible()

}

.onAttach <- function(libname, pkgname) {

  ta_exe <- getOption("rtauargus.tauargus_exe")

  packageStartupMessage(
    '\n',
    'Tau-Argus : "', ta_exe, '"\n',
    if (!file.exists(ta_exe)) '  (note: unknown location)\n',
    '\n',
    '  To change this directory :\n',
    '    options(rtauargus.tauargus_exe = "chemin/vers/TauArgus.exe")\n\n',
    '  To return to the default location,\n',
    '    reset_rtauargus_options("tauargus_exe")\n\n',
    '  To display all the options of the package :\n',
    '    rtauargus_options()',
    '\n'
  )

}

.onUnload <- function(libpath) {

  options(purrr::map(op.rtauargus, ~ NULL))
  packageStartupMessage('rtauargus : package options disabled')

}
