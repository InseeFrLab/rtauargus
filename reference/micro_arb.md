# Creates a batch file (.arb) for microdata

**\[superseded\]**

Development on `micro_arb()` is complete, and for new code we recommend
switching to the tabular-wise protection provided by
[`tab_rtauargus()`](https://inseefrlab.github.io/rtauargus/reference/tab_rtauargus.md)
or
[`tab_multi_manager()`](https://inseefrlab.github.io/rtauargus/reference/tab_multi_manager.md),
which offer a lot more features for your protection problems.

See more details in
[`vignette("rtauargus")`](https://inseefrlab.github.io/rtauargus/articles/rtauargus.md)
or in `vignette("protect_multi_tables)`.

Creates a batch file for microdata, executable by Tau-Argus in command
line.  
(Crée un fichier batch pour microdonnées, exécutable par Tau-Argus en
ligne de commande.)

The function does not check if asc and rda files exist.  
(La fonction ne vérifie pas si les fichiers asc et rda existent.)

## Utilisation

``` r
micro_arb(
  arb_filename = NULL,
  asc_filename,
  rda_filename = NULL,
  explanatory_vars,
  response_var = getOption("rtauargus.response_var"),
  shadow_var = NULL,
  cost_var = NULL,
  safety_rules,
  weighted = getOption("rtauargus.weighted"),
  suppress,
  linked = getOption("rtauargus.linked"),
  output_names = NULL,
  output_type = getOption("rtauargus.output_type"),
  output_options = getOption("rtauargus.output_options"),
  apriori = NULL,
  gointeractive = FALSE
)
```

## Arguments

- arb_filename:

  name of the generated arb file (with extension). If not specified, a
  temporary file.  
  (nom du fichier arb généré (avec extension). Si non renseigné, un
  fichier temporaire.)

- asc_filename:

  name of the asc file (with extension).  
  ( nom du fichier asc (avec extension).)

- rda_filename:

  name of the rda file (with extension). If not filled in,
  `asc_filename` with the extension "rda" instead of "asc".  
  (nom du fichier rda (avec extension). Si non renseigné, `asc_filename`
  avec l'extension "rda" à la place de "asc".)

- explanatory_vars:

  categorical variables, in form of a list of vectors. Each element of
  the list is a vector of variable names forming a tab. Example:
  `list(c("CJ", "A21"), c("SEX", "REGION"))` for the first table
  crossing `CJ` x `A21` and the second table crossing `SEXE` x `REGION`.
  If a single tabulation, a simple vector of the variables to be crossed
  is accepted (no need for `list(...)`).  
  ( variables catégorielles, sous forme de liste de vecteurs. Chaque
  élément de la liste est un vecteur des noms des variables formant une
  tabulation. Exemple: `list(c("CJ", "A21"), c("SEX", "REGION"))` pour
  la première table croisant `CJ` x `A21` et la seconde croisant `SEXE`
  x `REGION` Si une seule tabulation, un simple vecteur des variables à
  croiser est accepté (pas besoin de `list(...)`).)

- response_var:

  response variable to be summed, or counted if `"<freq>"`. A single
  value or as many values as there are tabs.  
  (variable de réponse à sommer, ou comptage si `"<freq>"`. Une seule
  valeur ou autant de valeurs que de tabulations.)

- shadow_var:

  variable(s) for applying the primary secret. If not filled in,
  `response_var` will be used by Tau-Argus.  
  (variable(s) pour l'application du secret primaire. Si non renseigné,
  `response_var` sera utilisé par Tau-Argus.)

- cost_var:

  cost variable(s) for the secondary secret.  
  (variable(s) de coût pour le secret secondaire.)

- safety_rules:

  primary secret rule(s). String in Tau-Argus batch syntax. The
  weighting is treated in a separate parameter (do not specify WGT here,
  use the `weighted`).  
  ( règle(s) de secret primaire. Chaîne de caractères en syntaxe batch
  Tau-Argus. La pondération est traitée dans un paramètre à part (ne pas
  spécifier WGT ici, utiliser le paramètre `weighted`).)

- weighted:

  indicator(s) (boolean).  
  (indicatrice(s) de pondération (booléen).)

- suppress:

  secret management method(s) secondary (Tau-Argus batch syntax). If the
  method is the same for each tabulation, the first parameter (table
  number) will be ignored and renumbered automatically (see section
  'Syntax').  
  ( méthode(s) de gestion du secret secondaire (syntaxe batch de
  Tau-Argus). Si la méthode est la même pour chaque tabulation, le
  premier paramètre (numéro du tableau) sera ignoré et renuméroté
  automatiquement (voir la section 'Syntax').)

- linked:

  to process the secondary secret jointly on all tabs. Only one delete
  command is allowed in this case (applied to all all tables).  
  (pour traiter le secret secondaire conjointement sur toutes les
  tabulations. Une seule commande suppress autorisée dans ce cas
  (appliquée à tous les tableaux).)

- output_names:

  names of output files. If filled in, the number of file names must be
  the same as the number of tabs. If left empty, as many temporary file
  names as there are tabs will be generated.  
  (noms des fichiers en sortie. Si renseigné, obligatoirement autant de
  noms de fichiers que de tabulations. Si laissé vide, autant de noms de
  fichiers temporaires que de tabulations seront générés.)

- output_type:

  format of output files (Tau-Argus codification). Default value of the
  package: `"2"` (csv for pivot-table).  
  (format des fichiers en sortie (codification Tau-Argus). Valeur par
  défaut du package : `"2"` (csv for pivot-table).)

- output_options:

  additional options for output files. default value of the package:
  `"AS+"` (status display). To specify no option, `""`.  
  (options supplémentaires des fichiers en sortie. Valeur par défaut du
  package : `"AS+"` (affichage du statut). Pour ne spécifier aucune
  option, `""`.)

- apriori:

  information file(s) *a priori*. See below for the syntax.  
  (fichier(s) d'informations *a priori*. Voir ci-dessous pour la
  syntaxe.)

- gointeractive:

  to have the possibility to launch the batch from the menu of
  Tau-Argus.  
  (pour avoir la possibilité de lancer le batch depuis le menu de
  Tau-Argus (`FALSE` par défaut).)

## Valeur de retour

A list of two elements: arb filename and names of output files (useful
to get back the randomly generated names)  
(Une liste de deux éléments : le nom du fichier arb, les noms des
fichiers en sortie (utile pour récupérer les noms générés
aléatoirement).)

## Syntax

Tau-Argus can handle multiple tabs for the same set of microdata.
Passing a single value for an option will apply the same processing to
each tab. For differentiated options, passing a vector containing as
many values as there are tabs. If the lengths do not match (not
recommended), a recycling is performed.

Unless otherwise stated, use the syntax mentioned in the documentation
of Tau-Argus.

Special syntax for `suppress` : the first parameter in the Tau-Argus
syntax is the tab number. If the method is identical for all tabs, this
first parameter will be ignored and the numbers are automatically
recalculated for the batch. In the writing `suppress = "GH(n,100)"`, n
will thus be transformed into 1 for the first tab, into 2 for the second
tab, etc.

(Tau-Argus peut traiter plusieurs tabulations pour un même jeu de
microdonnées. Passer une seule valeur pour une option appliquera le même
traitement à chaque tabulation. Pour des options différenciées, passer
un vecteur contenant autant de valeurs que de tabulations. Si les
longueurs ne correspondent pas (déconseillé), un recyclage est effectué.

Sauf mention contraire, utiliser la syntaxe mentionnée dans la
documentation de Tau-Argus.

Syntaxe spéciale pour `suppress` : le premier paramètre dans la syntaxe
Tau-Argus est le numéro de la tabulation. Si la méthode est identique
pour toutes les tabulations, ce premier paramètre sera ignoré et les
numéros recalculés automatiquement pour le batch. Dans l'écriture
`suppress = "GH(n,100)"`, n sera ainsi transformé en 1 pour la première
tabulation, en 2 pour la deuxième tabulation, etc.)

## Table identifiers

If the list `explanatory_vars` has names, these will be used in the
batch to give an identifier to the table, in the form of of a comment
line (`// <TABLE_ID> "..."`). They will be reused by the `import`
function to name the R format arrays tables in output.

(Si la liste `explanatory_vars` comporte des noms, ceux-ci seront
utilisés dans le batch pour donner un identifiant au tableau, sous la
forme d'une ligne de commentaire (`// <TABLE_ID> "..."`). Ils seront
réutilisés par la fonction `import` pour nommer les tableaux formats R
en sortie.)

## Use an apriori file

It is possible to provide an apriori file (.hst) for each tabulation.

The easiest way is to pass a vector containing as many hst files as
there are tabs. If the file is the same for all tabs, specify the name
of this file (it will be used for all tabs).

The additional options are optional. To change the default values, pass
a list with the hst file(s) as the first item and complete with the
elements having the names `sep` for the separator, `ignore_err` for
IgnoreError and `exp_triv` for ExpandTrivial. As for filenames, specify
only one value per parameter or as many values as there are tabs. (Il
est possible de fournir un fichier apriori (.hst) pour chaque
tabulation.

La manière la plus simple est de passer un vecteur contenant autant de
noms de fichiers hst que de tabulations. Si le fichier est le même pour
toutes les tabulations, spécifier le nom de ce fichier (il sera utilisé
pour toutes les tabulations).

Les options supplémentaires sont facultatives. Pour modifier les valeurs
par défaut, passer une liste ayant comme premier élément le(s)
fichier(s) hst et compléter avec les éléments portant les noms `sep`
pour le séparateur, `ignore_err` pour IgnoreError et `exp_triv` pour
ExpandTrivial. Comme pour les noms de fichiers, spécifier une seule
valeur par paramètre ou autant de valeurs que de tabulations.)

## See also

The function
[`micro_rtauargus()`](https://inseefrlab.github.io/rtauargus/reference/micro_rtauargus.md),
which uses this function and inherits its parameters.  
(La fonction
[`micro_rtauargus()`](https://inseefrlab.github.io/rtauargus/reference/micro_rtauargus.md),
qui utilise cette fonction et hérite de ses paramètres.)

## Exemples

``` r
# creation of the .arb file
infos_arb <- micro_arb(
  asc_filename = "donnees.asc",
  explanatory_vars = list(c("REGION", "CJ"), c("REGION")),
  response_var = c("CA", "<freq>"),
  safety_rules = c("NK(1,85)|FREQ(3,10)", "FREQ(3,10)"),
  suppress = "GH(.,100)",
  output_names = c("tab1.csv", "~/tab2.csv"),
  output_options = c("AS+SE+", "SE+"),
  output_type = "2"
)

# Content of the created file visible in the R console
# (Visualisation du contenu du fichier dans la console)
file.show(infos_arb$arb_filename, pager = "console")
```
