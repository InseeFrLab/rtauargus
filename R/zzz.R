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

  packageStartupMessage(
    '\n',
    'Tau Argus : "', getOption("rtauargus.tauargus_exe"), '"\n\n',
    '  Pour changer ce repertoire :\n',
    '    options(rtauargus.tauargus_exe = "chemin/vers/TauArgus.exe")\n\n',
    '  Pour revenir a l\'emplacement par defaut :\n',
    '    reset_rtauargus_options("tauargus_exe")\n\n',
    '  Pour afficher l\'ensemble des options du package :\n',
    '    rtauargus_options()',
    '\n'
  )

}
