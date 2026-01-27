# Runs a Tau-Argus batch

Executes the instructions contained in an .arb file for Tau-Argus.  
(Exécute les instructions contenues dans un fichier .arb pour
Tau-Argus.)

## Utilisation

``` r
run_arb(
  arb_filename,
  is_tabular = getOption("rtauargus.is_tabular"),
  missing_dir = getOption("rtauargus.missing_dir"),
  tauargus_exe = getOption("rtauargus.tauargus_exe"),
  logbook = NULL,
  show_batch_console = getOption("rtauargus.show_batch_console"),
  verbose = TRUE,
  import = getOption("rtauargus.import"),
  ...
)
```

## Arguments

- arb_filename:

  name of the batch file to execute.  
  (nom du fichier batch à exécuter.)

- is_tabular:

  boolean, if the data is already tabulated or not  
  (booléen, si les données sont déja tabulées ou non)

- missing_dir:

  what to do if the folders where the results will be written results do
  not exist ("stop" to trigger an error, "create" to create the missing
  folders).  
  (action si les dossiers où seront écrits les résultats n'existent pas
  ("stop" pour déclencher une erreur, "create" pour créer les dossiers
  manquants).)

- tauargus_exe:

  directory and name of the Tau-Argus software.  
  (répertoire et nom du logiciel Tau-Argus.)

- logbook:

  name of the file where the error log is saved. If NULL, a
  "logbook.txt" file will be saved in the working directory).  
  (nom du fichier où est enregistré le journal d'erreurs. Si NULL, le
  fichier "logbook.txt" sera suavegardé sur le répertoire de travail.)

- show_batch_console:

  to display the batch progress in the console.  
  (pour afficher le déroulement du batch dans la console.)

- verbose:

  boolean, to display the batch execution (if TRUE) or only error
  messages if any (if FALSE)  
  (booléen, pour afficher l'exécution du batch (si TRUE) ou uniquement
  les messages d'erreurs, s'il y en a (si FALSE))

- import:

  to import in R the files produced, `TRUE` by default.  
  (pour importer dans R les fichiers produits, `TRUE` par défaut.)

- ...:

  additional parameters for
  [`system()`](https://rdrr.io/r/base/system.html).  
  (paramètres supplémentaires pour
  [`system()`](https://rdrr.io/r/base/system.html).)

## Valeur de retour

- a list of data.frame containing the results if `import = TRUE` (via
  the `link{import}` function) ;

- `NULL` otherwise.

## Détails

Only the argument `arb_filename` is required, because all necessary
information is present in this file.

This is the only function in the package that runs Tau-Argus. It
therefore requires the software to be accessible from the workstation.

The location of the TauArgus.exe program is defined globally when the
loading the package. In fact, the argument `tauargus_exe` will not
normally not have to be specified (except to override the global option
the time of the execution of the function).

Checks are made before the actual launching of Tau-Argus: existence of
the software on the computer, of the asc and rda files, of the folders
where write the results, the variables to be used (crossings, response
variable) in the response variable) in the metadata (rda file).  

(Seul l’argument `arb_filename` est obligatoire, car toutes les
informations nécessaires sont présentes dans ce fichier. Il s'agit de la
seule fonction du package qui exécute Tau-Argus. Elle nécessite donc que
le logiciel soit accessible depuis le poste de travail.

L'emplacement du programme TauArgus.exe est défini de manière globale au
chargement du package. De fait, l'argument `tauargus_exe` n'aura
normalement pas à être spécifié (sauf pour surcharger l'option globale
le temps de l'exécution de la fonction).

Des vérifications sont effectuées avant le lancement effectif de
Tau-Argus : existence du logiciel sur le poste, des fichiers asc et rda,
des dossiers où écrire les résultats, des variables à utiliser
(croisements, variable de réponse) dans les métadonnées (fichier rda).)

## See also

The function
[`micro_rtauargus()`](https://inseefrlab.github.io/rtauargus/reference/micro_rtauargus.md),
which uses this function and inherits its parameters.  
(La fonction
[`micro_rtauargus()`](https://inseefrlab.github.io/rtauargus/reference/micro_rtauargus.md),
qui utilise cette fonction et hérite de ses paramètres.)

## Exemples

``` r
if (FALSE) { # \dontrun{

micro_arb("my_batch.arb")
} # }
```
