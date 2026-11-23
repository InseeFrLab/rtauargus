# Set or get the location of Tau-Argus

`set_tauargus_exe()` stores the path to TauArgus.exe in the
`TAUARGUS_EXE` environment variable, and optionally writes it to your
`.Renviron` file so that it is available in every future R session.
`get_tauargus_exe()` retrieves this location.  
(`set_tauargus_exe()` enregistre le chemin de TauArgus.exe dans la
variable d'environnement `TAUARGUS_EXE`, et éventuellement dans le
fichier `.Renviron` pour qu'il soit disponible dans toutes les sessions
R futures. `get_tauargus_exe()` récupère cet emplacement.)

## Usage

``` r
set_tauargus_exe(path, install = FALSE, overwrite = FALSE, check = TRUE)

get_tauargus_exe(detect = TRUE)
```

## Arguments

- path:

  path to the TauArgus.exe file.  
  (chemin vers le fichier TauArgus.exe.)

- install:

  if `TRUE`, the path is written to your `.Renviron` file so it is used
  in future sessions. If `FALSE` (default), it is only set for the
  current session.  
  (si `TRUE`, le chemin est écrit dans le fichier `.Renviron` pour être
  utilisé lors des sessions futures. Si `FALSE` (défaut), il n'est
  défini que pour la session en cours.)

- overwrite:

  if `TRUE`, replaces an existing `TAUARGUS_EXE` entry in `.Renviron`.  
  (si `TRUE`, remplace une entrée `TAUARGUS_EXE` déjà présente dans
  `.Renviron`.)

- check:

  if `TRUE` (default), stops when `path` is not an existing file (for
  instance a missing file or a folder). Use `check = FALSE` to store a
  path that is not reachable yet (e.g. a network drive that is not
  connected).  
  (si `TRUE` (défaut), erreur si `path` n'est pas un fichier existant
  (fichier absent ou dossier). `check = FALSE` permet d'enregistrer un
  chemin non accessible pour le moment, par exemple un lecteur réseau
  non connecté.)

- detect:

  if `TRUE` (default), searches for Tau-Argus in the usual install
  locations when it is not set.  
  (si `TRUE` (défaut), recherche Tau-Argus dans les emplacements
  d'installation usuels s'il n'est pas défini.)

## Value

`set_tauargus_exe()` invisibly returns the normalized path.
`get_tauargus_exe()` returns the path, or `""` if none is found.

If the stored location (`TAUARGUS_EXE`) points to a file that no longer
exists, `get_tauargus_exe()` falls back to the automatic search and says
so. If nothing is found, the stored (invalid) path is returned.

## Details

`get_tauargus_exe()` looks for Tau-Argus in this order:

1.  the `TAUARGUS_EXE` environment variable of the current session;

2.  a `TAUARGUS_EXE` entry in the user `.Renviron` file (the file
    pointed to by `R_ENVIRON_USER` if defined, `~/.Renviron` otherwise);

3.  if `detect = TRUE`, an automatic search: `TauArgus` on the `PATH`,
    then folders whose name contains "Argus" (up to three levels deep)
    in the usual install locations (Program Files, `%LOCALAPPDATA%`, the
    user folder and its Downloads folder, the root of the system drive).
    If several versions are found, the most recent one is used.

If Tau-Argus still cannot be found when
[`run_arb()`](https://inseefrlab.github.io/rtauargus/reference/run_arb.md)
is called in an interactive session, a file chooser opens so you can
select TauArgus.exe, and you are asked whether to save it in
`.Renviron`.  
(`get_tauargus_exe()` cherche Tau-Argus dans la variable d'environnement
`TAUARGUS_EXE`, puis dans le fichier `.Renviron`, puis (si
`detect = TRUE`) par une recherche automatique dans les emplacements
d'installation usuels. Si Tau-Argus reste introuvable au lancement de
[`run_arb()`](https://inseefrlab.github.io/rtauargus/reference/run_arb.md)
en session interactive, une fenêtre permet de sélectionner
TauArgus.exe.)

## Examples

``` r
if (FALSE) { # \dontrun{
# current session only
set_tauargus_exe("C:/TauArgus/TauArgus4.2.3/TauArgus.exe")

# saved in .Renviron for future sessions
set_tauargus_exe("C:/TauArgus/TauArgus4.2.3/TauArgus.exe", install = TRUE)

get_tauargus_exe()
} # }
```
