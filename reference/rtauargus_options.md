# Manages options of rtauargus package

Manages (displays, modifies, resets) the options of rtauargus package.  
Gère les options du package (affiche, modifie, réinitialise).

## Usage

``` r
rtauargus_options()

# options(rtauargus.<opt> = <val>)

reset_rtauargus_options(...)
```

## Arguments

- ...:

  names of the options to reset, separated by commas. If no name is
  specified, all the options will be reset. The prefix `"rtauargus."` is
  not required.  
  noms des options à réinitialiser, séparés par des virgules. Si aucun
  nom n'est spécifié, toutes les options du package seront
  réinitialisées. Le préfixe `"rtauargus."` est facultatif.

## Details

The options of the package define the default behaviour of the
functions.

These options are used if a mandatory argument of a function is not set
by the user. They let not to systematically repeat the same parameter
for each call of a function. The name of the option is the same as the
name of the function prefixed by `rtauargus.` :

*For example, `rtauargus.decimals` will be used if the argument
`decimals` in the
[`micro_asc_rda()`](https://inseefrlab.github.io/rtauargus/reference/micro_asc_rda.md)
function is not set by the user.*

On loading the package, all the rtauargus options, that are not already
been set by the user, are set with their default values (see table
below). The already defined options keep the values set by the user.

The options can be set during a session with the following instruction
`options(rtauargus.`...`=`...`)`, or with a configuration file where the
user have written its own options with such instructions, but this
latter is not a proper way if reproducibility is sought. Les options du
package définissent les comportements par défaut des fonctions.

If the user inadvertently removes some options, the functions will use
the default values of the package.  

(Ces options sont utilisées si un argument obligatoire d’une fonction
n’est pas renseigné. Elles permettent de ne pas répéter systématiquement
le même paramètre à chaque appel d'une fonction. Le nom de l’option est
le nom de l’argument d’une fonction précédé de `rtauargus.` :

*Par exemple, `rtauargus.decimals` sera la valeur utilisée si l’argument
`decimals` de la fonction
[`micro_asc_rda()`](https://inseefrlab.github.io/rtauargus/reference/micro_asc_rda.md)
n’est pas renseigné par l’utilisateur.*

Au chargement, le package attribue une valeur par défaut à toutes les
options de rtauargus qui ne sont pas encore déclarées (cf. tableau
ci-dessous). Les options déjà définies par l'utilisateur gardent leurs
valeurs.

Elles peuvent être redéfinies pour une session par une instruction
`options(rtauargus.`...`=`...`)`, ou de manière globale si de telles
instructions sont placées dans un fichier de configuration propre à
l'utilisateur (fortement déconseillé si le programme a vocation à être
reproductible).

En cas d'effacement accidentel d'une option par l'utilisateur, les
fonctions utiliseront les valeurs par défaut du package.)

## List of options

|                              |                                       |                                                                                        |
|------------------------------|---------------------------------------|----------------------------------------------------------------------------------------|
| **Option**                   | **Default Value**                     | **Function**                                                                           |
| `------------------------`   | `---------------------------------`   | `-------------`                                                                        |
| rtauargus.decimals           | 0                                     | [`micro_asc_rda()`](https://inseefrlab.github.io/rtauargus/reference/micro_asc_rda.md) |
| rtauargus.totcode            | "Total "                              |                                                                                        |
| rtauargus.missing            | " "                                   |                                                                                        |
| rtauargus.hierleadstring     | "@ "                                  |                                                                                        |
| `------------------------`   | `---------------------------------`   | `-------------`                                                                        |
| rtauargus.response_var       | "\<freq\> "                           | [`micro_arb()`](https://inseefrlab.github.io/rtauargus/reference/micro_arb.md)         |
| rtauargus.weighted           | FALSE                                 |                                                                                        |
| rtauargus.linked             | FALSE                                 |                                                                                        |
| rtauargus.output_type        | "4 "                                  |                                                                                        |
| rtauargus.output_options     | " "                                   |                                                                                        |
| `------------------------`   | `---------------------------------`   | `-------------`                                                                        |
| rtauargus.missing_dir        | "stop "                               | [`run_arb()`](https://inseefrlab.github.io/rtauargus/reference/run_arb.md)             |
| rtauargus.tauargus_exe       | "Y:/Logiciels/TauArgus/TauArgus.exe " |                                                                                        |
| rtauargus.show_batch_console | FALSE                                 |                                                                                        |
| rtauargus.import             | FALSE                                 |                                                                                        |

## See also

[options](https://rdrr.io/r/base/options.html), R options system  
le système d'options de R dans lequel s'insèrent les options de ce
package.

## Examples

``` r
rtauargus_options()
#> $rtauargus.decimals
#> [1] 0
#> 
#> $rtauargus.hierleadstring
#> [1] "@"
#> 
#> $rtauargus.import
#> [1] FALSE
#> 
#> $rtauargus.is_tabular
#> [1] TRUE
#> 
#> $rtauargus.linked
#> [1] FALSE
#> 
#> $rtauargus.missing
#> [1] ""
#> 
#> $rtauargus.missing_dir
#> [1] "stop"
#> 
#> $rtauargus.output_options
#> [1] ""
#> 
#> $rtauargus.output_type
#> [1] "4"
#> 
#> $rtauargus.response_var
#> [1] "<freq>"
#> 
#> $rtauargus.separator
#> [1] ","
#> 
#> $rtauargus.show_batch_console
#> [1] FALSE
#> 
#> $rtauargus.tauargus_exe
#> [1] "Y:/Logiciels/TauArgus/TauArgus.exe"
#> 
#> $rtauargus.totcode
#> [1] "Total"
#> 
#> $rtauargus.weighted
#> [1] FALSE
#> 

# modifies some options
options(
  rtauargus.tauargus_exe = "Z:/tmp/TauArgus.exe",
  rtauargus.output_type = "4",
  rtauargus.weighted = TRUE
)
str(rtauargus_options())
#> List of 15
#>  $ rtauargus.decimals          : int 0
#>  $ rtauargus.hierleadstring    : chr "@"
#>  $ rtauargus.import            : logi FALSE
#>  $ rtauargus.is_tabular        : logi TRUE
#>  $ rtauargus.linked            : logi FALSE
#>  $ rtauargus.missing           : chr ""
#>  $ rtauargus.missing_dir       : chr "stop"
#>  $ rtauargus.output_options    : chr ""
#>  $ rtauargus.output_type       : chr "4"
#>  $ rtauargus.response_var      : chr "<freq>"
#>  $ rtauargus.separator         : chr ","
#>  $ rtauargus.show_batch_console: logi FALSE
#>  $ rtauargus.tauargus_exe      : chr "Z:/tmp/TauArgus.exe"
#>  $ rtauargus.totcode           : chr "Total"
#>  $ rtauargus.weighted          : logi TRUE

# resets some options (prefix "rtauargus." facultatif)
reset_rtauargus_options("output_type", "rtauargus.tauargus_exe")
str(rtauargus_options())
#> List of 15
#>  $ rtauargus.decimals          : int 0
#>  $ rtauargus.hierleadstring    : chr "@"
#>  $ rtauargus.import            : logi FALSE
#>  $ rtauargus.is_tabular        : logi TRUE
#>  $ rtauargus.linked            : logi FALSE
#>  $ rtauargus.missing           : chr ""
#>  $ rtauargus.missing_dir       : chr "stop"
#>  $ rtauargus.output_options    : chr ""
#>  $ rtauargus.output_type       : chr "4"
#>  $ rtauargus.response_var      : chr "<freq>"
#>  $ rtauargus.separator         : chr ","
#>  $ rtauargus.show_batch_console: logi FALSE
#>  $ rtauargus.tauargus_exe      : chr "Y:/Logiciels/TauArgus/TauArgus.exe"
#>  $ rtauargus.totcode           : chr "Total"
#>  $ rtauargus.weighted          : logi TRUE

# resets everything
reset_rtauargus_options()
str(rtauargus_options())
#> List of 15
#>  $ rtauargus.decimals          : int 0
#>  $ rtauargus.hierleadstring    : chr "@"
#>  $ rtauargus.import            : logi FALSE
#>  $ rtauargus.is_tabular        : logi TRUE
#>  $ rtauargus.linked            : logi FALSE
#>  $ rtauargus.missing           : chr ""
#>  $ rtauargus.missing_dir       : chr "stop"
#>  $ rtauargus.output_options    : chr ""
#>  $ rtauargus.output_type       : chr "4"
#>  $ rtauargus.response_var      : chr "<freq>"
#>  $ rtauargus.separator         : chr ","
#>  $ rtauargus.show_batch_console: logi FALSE
#>  $ rtauargus.tauargus_exe      : chr "Y:/Logiciels/TauArgus/TauArgus.exe"
#>  $ rtauargus.totcode           : chr "Total"
#>  $ rtauargus.weighted          : logi FALSE
```
