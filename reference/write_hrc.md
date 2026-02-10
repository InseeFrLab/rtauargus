# Creates a hrc file from microdata

Creates an hrc file (hierarchy) from several variables in a set of
microdata set.  
Crée un fichier hrc (hiérarchie) à partir de plusieurs variables d'un
jeu de microdonnées.

## Usage

``` r
write_hrc(
  microdata,
  vars_hrc,
  hierleadstring = getOption("rtauargus.hierleadstring"),
  hrc_filename = NULL,
  fill_na = c("up", "down"),
  compact = TRUE,
  hierlevels = NULL
)
```

## Arguments

- microdata:

  data.frame containing the microdata.  
  ( data.frame contenant les microdonnées.)

- vars_hrc:

  vector of variable names constituting the hierarchy, from the finest
  to the most aggregated level.  
  (vecteur des noms des variables constituant la hiérarchie, du niveau
  le plus fin au niveau le plus agrégé.)

- hierleadstring:

  character which, repeated n times, indicates that the value is at n
  levels deep in the hierarchy.  
  (caractère qui, répété n fois, indique que la valeur est à n niveaux
  de profondeur dans la hiérarchie.)

- hrc_filename:

  name and location of the produced hrc file. If not filled, a temporary
  file.  
  (nom et emplacement du fichier hrc produit. Si non renseigné, un
  fichier temporaire.)

- fill_na:

  fill in any missing values, using an other variable :

  - `"up"` (default) : hierarchical variable of the level level
    immediately above

  - `"down"` : hierarchical variable of the level immediately lower

    
  (remplissage d'éventuelles valeurs manquantes, à l'aide d'une autre
  variable :

  - `"up"` (défaut) : variable hiérarchique de niveau immédiatement
    supérieur

  - `"down"` : variable hiérarchique de niveau immédiatement inférieur

  )

- compact:

  to prune branches repeating a single value to the lowest level of
  depth (`TRUE` by default).  
  (pour élaguer les branches répétant une unique valeur jusqu'au plus
  bas niveau de profondeur (`TRUE` par défaut).)

- hierlevels:

  if only one variable is specified in `vars_hrc`, allows to generate
  the hierarchy according to the position of the characters in the
  string. For example, `hierlevels = "2 3"` to build a hierarchy from a
  common code.  
  (si une seule variable est spécifiée dans `vars_hrc`, permet de
  générer la hiérarchie selon la position des caractères dans la chaîne.
  Par exemple, `hierlevels = "2 3"` pour construire une hiérarchie
  département-commune à partir d'un code commune.)

## Value

The name of the hrc file (useful in the case of a temporary file with
random name).  
(Le nom du fichier hrc (utile dans le cas d'un fichier temporaire au nom
aléatoire).)

## Details

The function reconstructs the variable hierarchy from the levels present
in the data. The variables in `vars_hrc` must be **classified from the
finest to the most aggregated**.

The relationship between each hierarchical level must be an application
(in the mathematical sense of the term), i.e. each fine level must have
a single corresponding aggregated level. The creation of the hierarchy
is impossible if this condition is not met.

If the name of the output file is not specified, a temporary file is
created. It will be deleted at the end of the session. The path to this
file can be retrieved in the return value of the function.

Missing values in the hierarchical variables will be imputed beforehand
using another hierarchical variable (parameter `fill_na`). In ascending
strategy (`"up"`), the variables are from the most aggregated to the
most refined, and vice versa in the downward strategy (`"down"`).

The parameter `compact` allows to create hierarchies with variable
depths. The idea is to cut the branches consisting of a single value
repeated up to the maximum depth (see examples).  

La fonction reconstitue la hiérarchie des variables à partir des niveaux
présents dans les données. Les variables dans `vars_hrc` doivent être
**classées de la plus fine à la plus agrégée**.

La relation entre chaque niveau hiérarchique doit être une application
(au sens mathématique du terme), c'est-à-dire que chaque niveau fin doit
avoir un seul et unique niveau agrégé correspondant. La création de la
hiérarchie est impossible si cette condition n'est pas remplie.

Si le nom du fichier en sortie n'est pas spécifié, un fichier temporaire
est créé. Il sera effacé en fin de session. Le chemin vers ce fichier
peut être récupéré dans la valeur de retour de la fonction.

Les valeurs manquantes présentes dans les variables hiérarchiques seront
préalablement imputées à l'aide d'une autre variable hiérarchique
(paramètre `fill_na`). En stratégie ascendante (`"up"`), les variables
sont parcourues de la plus agrégée à la plus fine, et inversement en
stratégie descendante (`"down"`).

Le paramètre `compact` permet de créer des hiérarchies à profondeurs
variables. L'idée est de couper les branches constituées d'une seule
valeur répétée jusqu'à la profondeur maximale (voir exemples).

## Examples

``` r
# Full Hierarchy ............................

df_naf <- data.frame(
  A10 = c("AZ", "BE", "BE", "BE", "BE", "BE", "BE"),
  A21 = c("A" , "C" , "B" , "C" , "C" , "C" , "B" ),
  A88 = c("01", "10", "06", "10", "12", "11", "07")
)

tmp_file <- write_hrc(df_naf, c("A88", "A21", "A10"))
file.show(tmp_file, pager = "console")

tmp_file <- write_hrc(df_naf, c("A88", "A10"), hierleadstring = ":")
file.show(tmp_file, pager = "console")

# Hierarchy with varying depth  ...............

df <- data.frame(
  niv1 = c("A"  , "A"  , "A"  , "B"  , "C"  , "C" ),
  niv2 = c("A1" , "A1" , "A2" ,  NA  , "C1" , "C2"),
  niv3 = c("A1x", "A1y",  NA  ,  NA  , "C1" ,  NA )
)

tmp_file <- write_hrc(df, c("niv3", "niv2", "niv1"))
#> Warning: valeurs manquantes imputees pour construire la hierarchie
file.show(tmp_file, pager = "console")

tmp_file <- write_hrc(df, c("niv3", "niv2", "niv1"), compact = FALSE)
#> Warning: valeurs manquantes imputees pour construire la hierarchie
file.show(tmp_file, pager = "console")
```
