.onLoad <- function(libname, pkgname) {

  # location de l'executable tauargus (version open source sur AUS)
  # initialisé au chargement du package seulement
  #   si n'existe pas déjà dans les options de R

  op <- options()
  op.rtauargus <- list(
    rtauargus_exe = "Y:/Logiciels/Tau/TauArgus.exe"
  )
  toset <- !(names(op.rtauargus) %in% names(op))
  if (any(toset)) options(op.rtauargus[toset])

  invisible()

}

.onAttach <- function(libname, pkgname) {

  packageStartupMessage(
    '\n',
    'Tau Argus : "', getOption("rtauargus_exe"), '"\n\n',
    '  Pour changer le repertoire :\n',
    '    set_tauargus_exe("dossier/ou/trouver/TauArgus.exe")\n',
    '  Pour revenir a la valeur par defaut (version portable AUS) :\n',
    '    set_tauargus_exe()',
    '\n'
  )

}
