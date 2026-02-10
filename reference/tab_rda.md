# Creates rda files from tabular data

Creates an apriori file for the primary secret, a tabular file (tab) and
a metadata file (rda) from tabulated data and additional information.  

## Usage

``` r
tab_rda(
  tabular,
  tab_filename = NULL,
  rda_filename = NULL,
  hst_filename = NULL,
  explanatory_vars = NULL,
  secret_var = NULL,
  cost_var = NULL,
  hrc = NULL,
  totcode = getOption("rtauargus.totcode"),
  value = NULL,
  freq = NULL,
  maxscore = NULL,
  ip = NULL,
  maxscore_2 = NULL,
  maxscore_3 = NULL,
  decimals = getOption("rtauargus.decimals"),
  hierleadstring = getOption("rtauargus.hierleadstring"),
  codelist = NULL,
  separator = getOption("rtauargus.separator"),
  secret_no_pl = NULL
)
```

## Arguments

- tabular:

  data.frame which contains the tabulated data and an additional boolean
  variable that indicates the primary secret of type boolean  
  ( data.frame contenant les données tabulées et une variable
  supplémentaire indiquant le secret primaire de type booléen.)

- tab_filename:

  tab file name (with .tab extension)  
  nom du fichier tab (avec extension .tab)

- rda_filename:

  rda file name (with .rda extension)  
  nom du fichier rda (avec extension)

- hst_filename:

  hst file name (with .hst extension)  
  nom du fichier hst (avec extension)

- explanatory_vars:

  Vector of explanatory variables  
  Variables catégorielles, sous forme de vecteurs  
  Example : `c("A21", "TREFF", "REG")` for a table crossing `A21` x
  `TREFF` x `REG` (Variable indiquant le secret primaire de type
  booléen: prend la valeur "TRUE" quand les cellules du tableau doivent
  être masquées par le secret primaire, "FALSE" sinon. Permet de créer
  un fichier d'apriori)

- secret_var:

  Nae of the boolean variable which specifies the secret, primary or not
  : equal to "TRUE" if a cell is concerned by the secret,"FALSE"
  otherwise. will be exported in the apriori file.  
  (Variable indiquant le secret de type booléen: prend la valeur "TRUE"
  quand les cellules du tableau doivent être masquées "FALSE" sinon.
  Permet de créer un fichier d'apriori)

- cost_var:

  Numeric variable allow to change the cost suppression of a cell for
  secondary suppression, it's the value of the cell by default, can be
  specified for each cell, fill with NA if the cost doesn't need to be
  changed for all cells  
  (Variable numeric qui permet de changer la coût de suppression d'une
  cellule, pris en compte dans les algorithmes de secret secondaire.Par
  défaut le coût correspond à la valeur de la cellule. peut être
  spécifié pour chacune des cellules, peut contenir des NA pour les
  coûts que l'on ne souhaite pas modifier.) (nombre minimal de décimales
  à afficher (voir section 'Number of decimals').)

- hrc:

  Informations of hierarchical variables (see section 'Hierarchical
  variables').  
  (Informations sur les variables hiérarchiques (voir section
  'Hierarchical variables').) (Caractère qui, répété n fois, indique que
  la valeur est à n niveaux de profondeur dans la hiérarchie.)

- totcode:

  Code(s) which represent the total of a categorical variable (see
  section 'Specific parameters' for this parameter's syntax). If
  unspecified for a variable(neither by default nor explicitly) it will
  be set to `rtauargus.totcode`.  
  (Code(s) pour le total d'une variable catégorielle (voir section
  'Specific parameters' pour la syntaxe de ce paramètre). Les variables
  non spécifiées (ni par défaut, ni explicitement) se verront attribuer
  la valeur de `rtauargus.totcode`.)

- value:

  Name of the column containing the value of the cells.  
  (Nom de la colonne contenant la valeur des cellules)

- freq:

  Name of the column containing the cell frequency.  
  (Nom de la colonne contenant les effectifs pour une cellule)

- maxscore:

  Name of the column containing, the value of the largest contributor of
  a cell.  
  (Nom de la colonne contenant la valeur du plus gros contributeur d'une
  cellule)

- ip:

  Value of the safety margin in % (must be an integer). (Valeur pour les
  intervalles de protection en %, doit être entier )

- maxscore_2:

  Name of the column containing, the value of the second largest
  contributor to a cell.  
  (Nom de la colonne contenant la valeur du deuxième plus gros
  contributeur d'une cellule)

- maxscore_3:

  Name of the column containing, the value of the third largest
  contributor to a cell.  
  (Nom de la colonne contenant la valeur du troisième plus gros
  contributeur d'une cellule)

- decimals:

  Minimum number of decimals to display (see section 'Number of
  decimals')  

- hierleadstring:

  The character that is used to indicate the depth of a code in the
  hierarchy.  

- codelist:

  file(s) containing labels of a categorical variables (see section
  'Specific parameters' for the syntax of this parameter).  
  (Fichier(s) contenant les libellés des variables catégorielles (voir
  section 'Specific parameters' pour la syntaxe de ce paramètre).)

- separator:

  Character used as separator in the .tab file.  
  (Caractère utilisé en tant que separateur dans le fichier .tab)

- secret_no_pl:

  name of a boolean variable which indicates the cells on which the
  protection levels won't be applied. If `secret_no_pl = NULL`
  (default), the protection levels are applied on each cell which gets a
  `TRUE` status for the `secret_var`.  

## Value

Return the rda file name as a list (invisible).  
(Renvoie le nom du fichier rda sous forme de liste (de manière
invisible).)

## Details

Crée un fichier d'apriori pour le secret primaire, un fichier tabular
(tab) et un fichier de métadonnées (rda) à partir de données tabulées et
d'informations additionnelles.

## Apriori file

The apriori file (.hst) summarizes for each value of the table if they
are concerned by the primary secret or not. With this file tau-argus
will not need to set the primary secret itself. The parameter
`secret_var` indicates the name of the primary secret variable. If there
is the additional boolean variable which indicates the primary secret in
the table (of tabulated data), the function tab_rda will create an
apriori file in a format conforming to tauargus.  

Le fichier d'apriori (.hst) récapitule pour chaque valeurs du tableau si
elles sont concernées par le secret primaire ou non. Avec ce fichier
tau-argus n'aura plus besoin de poser le secret primaire lui même, il se
basera sur le fichier d'apriori pour le faire. Le paramètre `secret_var`
indique le nom de la variable du secret primaire. Si l'on rajoute cette
variable supplémentaire indiquant le secret primaire (de type booléen)
au tableau de données tabulées, la fonction tab_rda permet de créer un
fichier d'apriori au format conforme pour tauargus.

## Specific parameters

The parameters `totcode`, and `codelist` must be given in the form of a
vector indicating the value to take for each variable. The names of the
elements of the vector give the variable concerned and the elements of
the vector give the value of the parameter for Tau-Argus. An unnamed
element will set the default value for each variable.  

(Les paramètres `totcode`, et `codelist` sont à renseigner sous la forme
d'un vecteur indiquant la valeur à prendre pour chaque variable.

Les noms des éléments du vecteur donnent la variable concernée, les
éléments du vecteur donnent la valeur du paramètre pour Tau-Argus. Un
élément non nommé constituera la valeur par défaut, qui sera attribuée à
toutes les variables pouvant prendre ce paramètre.)

For example :

- `totcode = "global"` : writes `<TOTCODE> "global"` for each
  explanatory vars

- `totcode = c("global", size="total", income="total")` :
  `<TOTCODE> "global"` for each variable except for `size` and `income`
  assigned with `<TOTCODE> "total"` by default : `<TOTCODE> "Total"`

- `totcode = "global"` : écrit `<TOTCODE> "global"` pour toutes les
  variables catégorielles

- `totcode = c("global", size="total", income="total")` :
  `<TOTCODE> "global"` pour toutes les variables catégorielles sauf
  `size` and `income` qui se verront affecter le total :
  `<TOTCODE> "total"` Par defaut : `<TOTCODE> "Total"`

## Hierarchical variables

Parameter `hrc` has the same syntax as `totcode` and `codelist` (named
vector containing as many elements as variables to describe). Hierarchy
is defined in an separate hrc file (**hiercodelist**). which can be
written with the function
[`write_hrc2()`](https://inseefrlab.github.io/rtauargus/reference/write_hrc2.md).
The function expects the location of this file (and a possible
`hierleadstring` if it differs from the default option of the package :
@. The path to the existing file is explicitly given. The elements of
the vector in parameter must be named (with the name of the variable),
even if there is only one element.

*Example :*`c(category="category.hrc")`  

(Le paramètre `hrc` obéit aux mêmes règles de syntaxe que `totcode` et
`codelist` (vecteur nommé contenant autant d'éléments que de variables à
décrire).

La hiérarchie est définie dans un fichier hrc à part (**hiercodelist**)
qui peut être écrit à l'aide de la fonction
[`write_hrc2()`](https://inseefrlab.github.io/rtauargus/reference/write_hrc2.md).

La fonction attend l'emplacement de ce fichier (et un éventuel
`hierleadstring` s'il diffère de l'option par défaut du package). Le
chemin vers le fichier existant est explicitement donné. Les éléments du
vecteur en paramètre doivent nommés (avec le nom de la variable), même
s'il n'y a qu'un seul élément.

*Exemple :*`c(category="category.hrc")`)

## Number of decimals

Parameter `decimals` indicates the minimum number of decimal places to
include in the output file (whatever the number of decimals actually
present in `tabular`). It applies to all real variables (double) but not
to integer variables. To add zeros to an integer variable, convert it
with `as.double` beforehand.  

(Le paramètre `decimals` indique le nombre minimal de décimales à faire
figurer dans le fichier en sortie (quel que soit le nombre de décimales
effectivement présent dans `tabular`). Il s'applique à toutes les
variables réelles (double) mais pas aux variables entières (integer).
Pour ajouter des zéros à une variable entière, la convertir avec
`as.double` au préalable.)

## Examples
