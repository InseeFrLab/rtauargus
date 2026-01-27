# tabulate grouped data with all margins, handling hierarchical variables

tabulate grouped data with all margins, handling hierarchical variables

## Utilisation

``` r
tabulate_micro_data(
  df,
  cat_vars = NULL,
  hrc_vars = NULL,
  pond_var = NULL,
  resp_var = NULL,
  marge_label = "Total"
)
```

## Arguments

- df:

  data.frame or data.table

- cat_vars:

  vector of categorical variables but not hierarchical

- hrc_vars:

  named list (name = VAR final name, value = VAR current names)

- pond_var:

  weight (NULL if no weight is used)

- resp_var:

  vector of response variables (NULL to only compute frequency table)

- marge_label:

  label of margins (applied to all cat and hrc variables)

## Valeur de retour

a tibble

## Exemples

``` r
library(data.table)
#> 
#> Attachement du package : ‘data.table’
#> Les objets suivants sont masqués depuis ‘package:dplyr’:
#> 
#>     between, first, last

data("indiv_dt")

#Non hierarchical variables
res_all_dtp <- tabulate_micro_data(
  df = indiv_dt,
  #categorical but not hierarchical variables
  cat_vars = c("A10", "SIZE","CJ"),
  #weight var
  pond_var = "WEIGHT",
  #response variable
  resp_var = "TURNOVER",
  # Labels of the margins
  marge_label = "Total"
)
str(res_all_dtp)
#> Classes ‘data.table’ and 'data.frame':   58 obs. of  6 variables:
#>  $ A10         : chr  "FZ" "BE" "GI" "Total" ...
#>  $ SIZE        : chr  "Total" "Total" "Total" "tr1" ...
#>  $ CJ          : chr  "Total" "Total" "Total" "Total" ...
#>  $ nb_obs      : num  11421 4082 22478 36112 1825 ...
#>  $ TURNOVER_tot: num  11087582 3344353 22236029 34765446 1849758 ...
#>  $ TURNOVER_max: num  8016 9373 9400 9400 6510 ...
#>  - attr(*, ".internal.selfref")=<externalptr> 

#With one hierarchical variable
res_all_dtph <- tabulate_micro_data(
  df = indiv_dt,
  #categorical but not hierarchical variables
  cat_vars = c("SIZE","CJ"),
  #categorical nested variables
  hrc_vars = list(ACTIVITY = c("A10","A21")),
  pond_var = "WEIGHT",
  resp_var = c("TURNOVER","PRODUCTION"),
  marge_label = "Total"
)
str(res_all_dtph)
#> Classes ‘data.table’ and 'data.frame':   160 obs. of  8 variables:
#>  $ ACTIVITY      : chr  "FZ" "BE" "GI" "Total" ...
#>  $ SIZE          : chr  "Total" "Total" "Total" "tr1" ...
#>  $ CJ            : chr  "Total" "Total" "Total" "Total" ...
#>  $ nb_obs        : num  11421 4082 22478 36112 1825 ...
#>  $ TURNOVER_tot  : num  11087582 3344353 22236029 34765446 1849758 ...
#>  $ PRODUCTION_tot: num  2209740 670391 4435408 6931152 373839 ...
#>  $ TURNOVER_max  : num  8016 9373 9400 9400 6510 ...
#>  $ PRODUCTION_max: num  1612 1870 1876 1876 1317 ...
#>  - attr(*, ".internal.selfref")=<externalptr> 
```
