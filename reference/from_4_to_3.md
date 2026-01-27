# Function reducing from 4 to 3 categorical variables

Function reducing from 4 to 3 categorical variables

## Utilisation

``` r
from_4_to_3(
  dfs,
  dfs_name,
  totcode,
  hrcfiles = NULL,
  sep_dir = FALSE,
  hrc_dir = "hrc_alt",
  v1 = NULL,
  v2 = NULL,
  sep = "_",
  maximize_nb_tabs = FALSE
)
```

## Arguments

- dfs:

  data.frame with 4 categorical variables (n \>= 2 in the general case)

- dfs_name:

  name of the dataframe

- totcode:

  named vector of totals for categorical variables

- hrcfiles:

  named vector indicating the hrc files of hierarchical variables among
  the categorical variables of dfs

- sep_dir:

  allows forcing the writing of hrc into a separate folder, default is
  FALSE

- hrc_dir:

  folder to write hrc files if writing to a new folder is forced or if
  no folder is specified in hrcfiles

- v1:

  allows forcing the value of the first variable to merge, not specified
  by default (NULL)

- v2:

  allows forcing the value of the second variable to merge, not
  specified by default (NULL)

- sep:

  separator used during concatenation of variables

- maximize_nb_tabs:

  specifies whether to prefer selecting hierarchical variables with the
  most nodes in priority (TRUE), generating more tables but with smaller
  sizes, or non-hierarchical variables with the fewest modalities
  (FALSE) to create fewer tables

## Valeur de retour

A list containing the following components:

- `tabs`: named list of 3-dimensional dataframes (n-1 dimensions in the
  general case) with nested hierarchies

- `hrc`: named list of hrc specific to the variable created through
  merging

- `alt_tot`: named list of totals

- `vars`: named list of vectors representing the merged variables during
  the two stages of dimension reduction

## Exemples

``` r
library(dplyr)
#> 
#> Attachement du package : ‘dplyr’
#> Les objets suivants sont masqués depuis ‘package:stats’:
#> 
#>     filter, lag
#> Les objets suivants sont masqués depuis ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
data <- expand.grid(
  ACT = c("Total", "A", "B", "A1", "A2", "B1", "B2"),
  GEO = c("Total", "G1", "G2"),
  SEX = c("Total", "F", "M"),
  AGE = c("Total", "AGE1", "AGE2"),
  stringsAsFactors = FALSE
) %>%
  as.data.frame()

data <- data %>% mutate(VALUE = 1)

hrc_act <- "hrc_ACT.hrc"

sdcHierarchies::hier_create(root = "Total", nodes = c("A","B")) %>%
  sdcHierarchies::hier_add(root = "A", nodes = c("A1","A2")) %>%
  sdcHierarchies::hier_add(root = "B", nodes = c("B1","B2")) %>%
  sdcHierarchies::hier_convert(as = "argus") %>%
  slice(-1) %>%
  mutate(levels = substring(paste0(level,name),3)) %>%
  select(levels) %>%
  write.table(file = hrc_act, row.names = FALSE, col.names = FALSE, quote = FALSE)

# Results of the function
res1 <- from_4_to_3(
  dfs = data,
  dfs_name = "tab",
  totcode = c(SEX = "Total", AGE = "Total", GEO = "Total", ACT = "Total"),
  hrcfiles = c(ACT = hrc_act),
  sep_dir = TRUE,
  hrc_dir = "output"
)

# Maximize the number of tables
res2 <- from_4_to_3(
  dfs = data,
  dfs_name = "tab",
  totcode = c(SEX = "Total", AGE = "Total", GEO = "Total", ACT = "Total"),
  hrcfiles = c(ACT = hrc_act),
  sep_dir = TRUE,
  hrc_dir = "output",
  maximize_nb_tabs = TRUE
)
```
