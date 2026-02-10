# Mass protection

Optimization of the function `rtauargus` for a large number of
crossovers (all having the same parameters).  
(Optimisation de la fonction
[`micro_rtauargus()`](https://inseefrlab.github.io/rtauargus/reference/micro_rtauargus.md)
pour un grand nombre de croisements (ayant tous les mêmes paramètres).)

## Usage

``` r
rtauargus_plus(
  grp_size = 5,
  microdata,
  explanatory_vars,
  safety_rules,
  suppress,
  ...
)
```

## Arguments

- grp_size:

  number of tables per Tau-Argus call (an integer between between 1 and
  10).  
  (nombre de tableaux par appel de Tau-Argus (un entier compris entre 1
  et 10).)

- microdata:

  data.frame containing the microdata.  
  ( data.frame contenant les microdonnées.)

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

  secondary secret management method (Tau-Argus batch syntax). Only one
  method allowed for all tables. Example : `"GH(.,100)"` (the dot
  playing the role of the tabulation number).  
  méthode de gestion du secret secondaire (syntaxe batch de Tau-Argus).
  Une seule méthode autorisée pour tous les tableaux. Exemple
  `"GH(.,100)"` (le point jouant le rôle du numéro de tabulation).

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

A list of data.frames (secret arrays).  
(Une liste de data.frames (tableaux secrétisés).)

## Details

In interactive mode, Tau-Argus can process up to 10 tabs simultaneously
for the same microdata set. In batch mode, a larger number of of
crossings can be specified. However, doing so can be particularly time
consuming, as Tau-Argus takes a lot of time to read large text files of
microdata.

`rtauargus_plus` helps to improve the speed of execution. The function
splits the list of tabs into groups of size `grp_size` and makes a call
to
[`micro_rtauargus()`](https://inseefrlab.github.io/rtauargus/reference/micro_rtauargus.md)
for each group. It writes an asc file restricted to the only variables
actually used within a of a group.

The results are then aggregated into a single list, as if Tau-Argus had
been called only once.

Modifying `grp_size` will not change the result, only the time execution
time. A value around 5 (default) seems to be a good compromise between
reading too large asc files and calling Tau-Argus too often. Tau-Argus
too much. It can be adjusted according to the number of common variables
within each tab group. (En mode interactif, Tau-Argus peut traiter
jusqu'à 10 tabulations simultanément pour un même jeu de microdonnées.
En mode batch, un nombre plus important de croisements peut être
spécifié. Cependant, procéder de la sorte peut être particulièrement
long, car Tau-Argus prend beaucoup de temps pour lire des fichiers texte
volumineux de microdonnées.

`rtauargus_plus` permet d'améliorer la vitesse d'exécution. La fonction
découpe la liste des tabulations en groupes de taille `grp_size` et
effectue un appel à
[`micro_rtauargus()`](https://inseefrlab.github.io/rtauargus/reference/micro_rtauargus.md)
pour chaque groupe. Elle écrit un fichier asc restreint aux seules
variables effectivement utilisées au sein d'un groupe.

Les résultats sont ensuite agrégés en une unique liste, comme si
Tau-Argus n'avait été appelé qu'une seule fois.

Modifier `grp_size` ne changera pas le résultat, seulement le temps
d'exécution. Une valeur autour de 5 (défaut) semble être un bon
compromis entre une lecture de fichiers asc trop volumineux et un nombre
d'appels à Tau-Argus trop important. Elle peut être ajustée en fonction
du nombre de variables communes à l'intérieur de chaque groupe de
tabulations.)

## Limits in relation to the function [`micro_rtauargus()`](https://inseefrlab.github.io/rtauargus/reference/micro_rtauargus.md)

In return for the speed of execution, the crossings must have the same
characteristics (same primary secret rules, same secondary secret
method, same secondary secret, same weighting variable, etc.). The
parameters `safety_rules`, `supress`, ..., must therefore contain a
unique value.

Moreover, it is not possible to specify `asc_filename`, `rda_filename`,
`arb_filename` or `output_names` to retrieve the intermediate files.
These files will be written to a temporary folder (and overwritten with
each new group). Therefore, specifying `import = FALSE` is irrelevant
and will be ignored.

The data must be a data.frame (asc and rda files not allowed).

If the `linked` option is used, the link will only be effective at
within each group of tabs.  

(En contrepartie de la vitesse d'exécution, les croisements doivent
avoir les mêmes caractéristiques (mêmes règles de secret primaire, même
méthode de secret secondaire, même variable de pondération, etc.). Les
paramètres `safety_rules`, `supress`, ..., doivent donc contenir une
valeur unique.

De plus, il n'est pas possible de spécifier `asc_filename`,
`rda_filename`, `arb_filename` ou `output_names` pour récupérer les
fichiers intermédiaires. Ces fichiers seront écrits dans un dossier
temporaire (et écrasés à chaque nouveau groupe). Par conséquent,
spécifier `import = FALSE` est sans objet et sera ignoré.

Les données doivent obligatoirement être un data.frame (fichiers asc et
rda pas autorisés).

Si l'option `linked` est utilisée, la liaison ne sera effective qu'à
l'intérieur de chaque groupe de tabulations.)

## See also

[`micro_rtauargus()`](https://inseefrlab.github.io/rtauargus/reference/micro_rtauargus.md),
a function called repeatedly by `rtauargus_plus`.  
fonction appelée de manière répétée par `rtauargus_plus`.

## Examples

``` r
if (FALSE) { # \dontrun{
# example of ?rtauargus, with an (artificial) repetition of the crossings
rtauargus_plus(
  grp_size         = 6, # (pour lancer les 12 croisements en 2 appels)
  microdata        = data.frame(V1 = c("A", "A", "B", "C", "A"), V2 = "Z"),
  explanatory_vars = rep(list("V1", "V2", c("V1", "V2")), times = 4),
  safety_rules     = "FREQ(3,10)",
  suppress         = "GH(.,100)"
)} # }
```
