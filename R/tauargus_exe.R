#' Set or get the location of Tau-Argus
#'
#' `set_tauargus_exe()` stores the path to TauArgus.exe in the `TAUARGUS_EXE`
#' environment variable, and optionally writes it to your `.Renviron` file so
#' that it is available in every future R session. `get_tauargus_exe()`
#' retrieves this location. \cr
#' (`set_tauargus_exe()` enregistre le chemin de TauArgus.exe dans la variable
#' d'environnement `TAUARGUS_EXE`, et éventuellement dans le fichier
#' `.Renviron` pour qu'il soit disponible dans toutes les sessions R futures.
#' `get_tauargus_exe()` récupère cet emplacement.)
#'
#' `get_tauargus_exe()` looks for Tau-Argus in this order:
#' 1. the `TAUARGUS_EXE` environment variable of the current session;
#' 2. a `TAUARGUS_EXE` entry in the user `.Renviron` file (the file pointed to
#'    by `R_ENVIRON_USER` if defined, `~/.Renviron` otherwise);
#' 3. if `detect = TRUE`, an automatic search: `TauArgus` on the `PATH`, then
#'    folders whose name contains "Argus" (up to three levels deep) in the usual
#'    install locations (Program Files, `%LOCALAPPDATA%`, the user folder and
#'    its Downloads folder, the root of the system drive). If several versions
#'    are found, the most recent one is used.
#'
#' If Tau-Argus still cannot be found when [run_arb()] is called in an
#' interactive session, a file chooser opens so you can select TauArgus.exe,
#' and you are asked whether to save it in `.Renviron`. \cr
#' (`get_tauargus_exe()` cherche Tau-Argus dans la variable d'environnement
#' `TAUARGUS_EXE`, puis dans le fichier `.Renviron`, puis (si `detect = TRUE`)
#' par une recherche automatique dans les emplacements d'installation usuels.
#' Si Tau-Argus reste introuvable au lancement de [run_arb()] en session
#' interactive, une fenêtre permet de sélectionner TauArgus.exe.)
#'
#' @param detect if `TRUE` (default), searches for Tau-Argus in the usual
#' install locations when it is not set. \cr
#' (si `TRUE` (défaut), recherche Tau-Argus dans les emplacements
#' d'installation usuels s'il n'est pas défini.)
#' @param path path to the TauArgus.exe file. \cr
#' (chemin vers le fichier TauArgus.exe.)
#' @param install if `TRUE`, the path is written to your `.Renviron` file
#' so it is used in future sessions. If `FALSE` (default), it is only set for
#' the current session. \cr
#' (si `TRUE`, le chemin est écrit dans le fichier `.Renviron` pour être
#' utilisé lors des sessions futures. Si `FALSE` (défaut), il n'est défini
#' que pour la session en cours.)
#' @param overwrite if `TRUE`, replaces an existing `TAUARGUS_EXE` entry in
#' `.Renviron`. \cr
#' (si `TRUE`, remplace une entrée `TAUARGUS_EXE` déjà présente dans
#' `.Renviron`.)
#'
#' @return `set_tauargus_exe()` invisibly returns the normalized path.
#' `get_tauargus_exe()` returns the path, or `""` if none is found.
#'
#' @examples
#' \dontrun{
#' # current session only
#' set_tauargus_exe("C:/TauArgus/TauArgus4.2.3/TauArgus.exe")
#'
#' # saved in .Renviron for future sessions
#' set_tauargus_exe("C:/TauArgus/TauArgus4.2.3/TauArgus.exe", install = TRUE)
#'
#' get_tauargus_exe()
#' }
#'
#' @export
#' @rdname tauargus_exe

set_tauargus_exe <- function(path, install = FALSE, overwrite = FALSE) {

  if (!is.character(path) || length(path) != 1 || is.na(path) || !nzchar(path)) {
    stop("`path` must be a single, non-empty character string.")
  }

  path <- gsub("\\\\", "/", path.expand(path))

  if (!file.exists(path)) {
    warning("The file '", path, "' does not exist (yet). It is stored anyway.")
  }

  if (install) {

    renv <- renviron_path()

    if (file.exists(renv)) {
      file.copy(renv, paste0(renv, ".bak"), overwrite = TRUE)
      lines <- readLines(renv, warn = FALSE)
    } else {
      file.create(renv)
      lines <- character(0)
    }

    existing <- grepl("^\\s*TAUARGUS_EXE\\s*=", lines)

    if (any(existing)) {
      if (!overwrite) {
        stop(
          "A TAUARGUS_EXE entry already exists in ", renv, ".\n  ",
          "Use `overwrite = TRUE` to replace it."
        )
      }
      lines <- lines[!existing]
    }

    lines <- c(lines, paste0('TAUARGUS_EXE="', path, '"'))
    writeLines(lines, renv)

    message(
      "The location of Tau-Argus has been stored in ", renv, ".\n",
      "It will be used automatically in future R sessions."
    )

  }

  Sys.setenv(TAUARGUS_EXE = path)
  options(rtauargus.tauargus_exe = path)

  invisible(path)

}

#' @export
#' @rdname tauargus_exe

get_tauargus_exe <- function(detect = TRUE) {
  as.character(locate_tauargus_exe(detect = detect))
}

# Same as get_tauargus_exe(), but the result carries a "source" attribute:
# "env", "renviron", "detected" or "none"
locate_tauargus_exe <- function(detect = TRUE) {

  found <- function(path, source) structure(path, source = source)

  exe <- Sys.getenv("TAUARGUS_EXE", unset = "")
  if (nzchar(exe)) return(found(exe, "env"))

  exe <- read_renviron_tauargus_exe()
  if (nzchar(exe)) return(found(exe, "renviron"))

  if (detect) {
    exe <- detect_tauargus_exe()
    if (nzchar(exe)) return(found(exe, "detected"))
  }

  found("", "none")

}

# TAUARGUS_EXE entry of the user .Renviron file ("" if none)
read_renviron_tauargus_exe <- function() {

  renv <- renviron_path()
  if (!file.exists(renv)) return("")

  lines <- readLines(renv, warn = FALSE)
  entry <- grep("^\\s*TAUARGUS_EXE\\s*=", lines, value = TRUE)
  if (!length(entry)) return("")

  # last definition wins, as in readRenviron()
  val <- sub("^\\s*TAUARGUS_EXE\\s*=\\s*", "", entry[length(entry)])
  val <- trimws(val)
  val <- gsub("^[\"']|[\"']$", "", val)

  val

}

# Usual locations where Tau-Argus may be installed or unzipped
tauargus_search_roots <- function() {

  env <- function(x) Sys.getenv(x, unset = "")
  local_app <- env("LOCALAPPDATA")
  home <- env("USERPROFILE")
  if (!nzchar(home)) home <- path.expand("~")
  sys_drive <- env("SystemDrive")

  roots <- c(
    env("ProgramFiles"),
    env("ProgramFiles(x86)"),
    env("ProgramW6432"),
    if (nzchar(local_app)) file.path(local_app, "Programs"),
    local_app,
    home,
    file.path(home, "Downloads"),
    if (nzchar(sys_drive)) paste0(sys_drive, "/")
  )

  roots <- gsub("\\\\", "/", roots[nzchar(roots)])
  unique(roots[dir.exists(roots)])

}

# Searches for TauArgus.exe on the PATH and in the usual locations.
# Returns the most recent version found, or "" if none.
detect_tauargus_exe <- function(roots = tauargus_search_roots()) {

  on_path <- Sys.which("TauArgus")
  if (nzchar(on_path)) return(gsub("\\\\", "/", unname(on_path)))

  if (!length(roots)) return("")

  # folders whose name contains "Argus", with TauArgus.exe up to 3 levels deep
  # e.g. <root>/TauArgus/TauArgus4.2.3/TauArgus.exe
  patterns <- c(
    file.path(roots, "*Argus*", "TauArgus.exe"),
    file.path(roots, "*Argus*", "*", "TauArgus.exe"),
    file.path(roots, "*Argus*", "*", "*", "TauArgus.exe")
  )
  candidates <- unique(Sys.glob(patterns))
  candidates <- candidates[file.exists(candidates) & !dir.exists(candidates)]

  if (!length(candidates)) return("")

  pick_latest_tauargus(candidates)

}

# Among several TauArgus.exe, chooses the highest version number found in the
# path (e.g. "TauArgus4.2.3"), then the most recently modified file.
pick_latest_tauargus <- function(candidates) {

  version_of <- function(p) {
    v <- regmatches(p, gregexpr("[0-9]+(\\.[0-9]+)+", p))[[1]]
    if (length(v)) v[length(v)] else "0"
  }
  versions <- numeric_version(vapply(candidates, version_of, character(1)))
  mtimes <- file.mtime(candidates)

  # order() does not handle numeric_version: rank via sorted unique values
  v_rank <- match(versions, sort(unique(versions)))
  best <- order(-v_rank, -as.numeric(mtimes))[1]

  candidates[best]

}

# Interactive selection of TauArgus.exe, with an offer to save it in .Renviron.
# Returns the chosen path, or "" if cancelled / not interactive.
choose_tauargus_exe <- function() {

  if (!is_interactive()) return("")

  message(
    "Tau-Argus could not be found. ",
    "Please select the TauArgus.exe file in the window that opens."
  )

  caption <- "Select TauArgus.exe"
  path <- tryCatch(
    {
      if (requireNamespace("rstudioapi", quietly = TRUE) &&
          rstudioapi::isAvailable()) {
        rstudioapi::selectFile(caption = caption, filter = "TauArgus (*.exe)")
      } else if (.Platform$OS.type == "windows") {
        utils::choose.files(
          caption = caption,
          multi = FALSE,
          filters = rbind(c("TauArgus (*.exe)", "*.exe"))
        )
      } else {
        file.choose()
      }
    },
    error = function(e) NULL # file.choose() raises an error when cancelled
  )

  if (!length(path) || is.na(path[1]) || !nzchar(path[1])) {
    message("No file selected.")
    return("")
  }
  path <- path[1]

  save <- utils::askYesNo(
    paste0(
      "Save this location in ", renviron_path(),
      " so that it is used in future R sessions?"
    ),
    default = TRUE
  )

  set_tauargus_exe(path, install = isTRUE(save), overwrite = TRUE)

}

# wrapper so that tests can mock interactivity
is_interactive <- function() interactive()

# location of the user .Renviron file
renviron_path <- function() {
  user <- Sys.getenv("R_ENVIRON_USER", unset = "")
  if (nzchar(user)) return(path.expand(user))
  file.path(path.expand("~"), ".Renviron")
}
