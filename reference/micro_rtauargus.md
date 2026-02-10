# Protects tables from microdata

**\[superseded\]**

Development on `micro_rtauargus()` is complete, and for new code we
recommend switching to the tabular-wise protection provided by
[`tab_rtauargus()`](https://inseefrlab.github.io/rtauargus/reference/tab_rtauargus.md)
or
[`tab_multi_manager()`](https://inseefrlab.github.io/rtauargus/reference/tab_multi_manager.md),
which offer a lot more features for your protection problems.

See more details in
[`vignette("rtauargus")`](https://inseefrlab.github.io/rtauargus/articles/rtauargus.md)
or in `vignette("protect_multi_tables)`.

Protects tables built from microdata and specifications of the
crossings. The function allows to perform the complete process, namely
the creation of the asc and rda files, the construction of the arb file,
the effective launching of Tau-Argus and the eventual recovery of the
results in R.  
(Secrétise des tableaux construits à partir de microdonnées et des
spécifications des croisements. La fonction permet d'effectuer le
processus complet, à savoir la création des fichiers asc et rda, la
construction du fichier arb, le lancement effectif de Tau-Argus et la
récupération éventuelle des résultats dans R.)

The function executes sequentially the functions:

- [`micro_asc_rda()`](https://inseefrlab.github.io/rtauargus/reference/micro_asc_rda.md)
  `->`
  [`micro_arb()`](https://inseefrlab.github.io/rtauargus/reference/micro_arb.md)
  `->`
  [`run_arb()`](https://inseefrlab.github.io/rtauargus/reference/run_arb.md)

Intermediate files without a name entered (`asc_filename`...) will be
created in a temporary folder, with randomly generated names. This
mechanism allows the user to abstract from the preparation of
preparation of the data and to maintain the entire chain of processing
in R.  

(La fonction exécute séquentiellement les fonctions :

- [`micro_asc_rda()`](https://inseefrlab.github.io/rtauargus/reference/micro_asc_rda.md)
  `->`
  [`micro_arb()`](https://inseefrlab.github.io/rtauargus/reference/micro_arb.md)
  `->`
  [`run_arb()`](https://inseefrlab.github.io/rtauargus/reference/run_arb.md)

Les fichiers intermédiaires sans nom renseigné (`asc_filename`...)
seront créés dans un dossier temporaire, avec des noms générés
aléatoirement. Ce mécanisme permet à l'utilisateur de s'abstraire de la
préparation des données propre à Tau-Argus et de maintenir l'intégralité
de la chaîne de traitements dans R.)

## Usage

``` r
micro_rtauargus(microdata, explanatory_vars, safety_rules, suppress, ...)
```

## Arguments

- microdata:

  data.frame containing the microdata (or path to text files already
  present: see section *Microdata already as text files*).  
  ( data.frame contenant les microdonnées (ou chemin vers des fichiers
  texte déjà présents : voir section *Microdata already as text
  files*).)

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

- safety_rules:

  primary secret rule(s). String in Tau-Argus batch syntax. The
  weighting is treated in a separate parameter (do not specify WGT here,
  use the `weighted`).  
  ( règle(s) de secret primaire. Chaîne de caractères en syntaxe batch
  Tau-Argus. La pondération est traitée dans un paramètre à part (ne pas
  spécifier WGT ici, utiliser le paramètre `weighted`).)

- suppress:

  secret management method(s) secondary (Tau-Argus batch syntax). If the
  method is the same for each tabulation, the first parameter (table
  number) will be ignored and renumbered automatically (see section
  'Syntax').  
  ( méthode(s) de gestion du secret secondaire (syntaxe batch de
  Tau-Argus). Si la méthode est la même pour chaque tabulation, le
  premier paramètre (numéro du tableau) sera ignoré et renuméroté
  automatiquement (voir la section 'Syntax').)

- ...:

  optional parameters for
  [`micro_asc_rda()`](https://inseefrlab.github.io/rtauargus/reference/micro_asc_rda.md),
  [`micro_arb()`](https://inseefrlab.github.io/rtauargus/reference/micro_arb.md)
  and
  [`run_arb()`](https://inseefrlab.github.io/rtauargus/reference/run_arb.md).
  See the help for these functions.  
  (paramètres optionnels pour
  [`micro_asc_rda()`](https://inseefrlab.github.io/rtauargus/reference/micro_asc_rda.md),
  [`micro_arb()`](https://inseefrlab.github.io/rtauargus/reference/micro_arb.md)
  et
  [`run_arb()`](https://inseefrlab.github.io/rtauargus/reference/run_arb.md).
  Voir l'aide de ces fonctions.)

## Value

If `import = TRUE`, a list of data.frames (protected tables), `NULL`
otherwise.  

(Si `import = TRUE`, une liste de data.frames (tableaux secrétisés),
`NULL` sinon.)

## Microdata already as text files

To use existing asc and rda files already existing, it is possible to
provide, instead of the data.frame, a character vector indicating the
path of these files. The first element of this vector is the asc file,
the second element the rda file. The rda file can be omitted if it has
the same name as the asc file (except for the extension). Use this
option to start the whole process without the generation of the text
data. Do not specify `asc_filename` or `rda_filename` (used to name the
text files to be created, which is irrelevant here).  

Pour utiliser des fichiers asc et rda existant déjà, il est possible de
fournir à la place du data.frame un vecteur caractère indiquant le
chemin de ces fichiers. Le premier élément de ce vecteur est le fichier
asc, le deuxième élément le fichier rda. Le fichier rda peut être omis
s'il porte le même nom que le fichier asc (à l'extension près).

Utiliser cette option pour lancer le processus complet sans la
génération des données en texte. Ne pas spécifier `asc_filename` ou
`rda_filename` (sert à nommer les fichiers texte à créer, ce qui est
sans objet ici).

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

## See also

[`rtauargus_plus()`](https://inseefrlab.github.io/rtauargus/reference/rtauargus_plus.md),
a version optimized for a large number of tables (at the cost of some
usage restrictions).  
([`rtauargus_plus()`](https://inseefrlab.github.io/rtauargus/reference/rtauargus_plus.md),
une version optimisée pour un grand nombre de tableaux (au prix de
quelques restrictions d'usage).)

## Examples

``` r
if (FALSE) { # \dontrun{
rtauargus(
  microdata = data.frame(V1 = c("A", "A", "B", "C", "A"), V2 = "Z"),
  explanatory_vars = c("V1", "V2"),
  safety_rules = "FREQ(3,10)",
  suppress = "GH(.,100)",
  output_options = "AS+" # (example of optional parameter for micro_arb)
)} # }
```
