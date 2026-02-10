# Transition from 4 to 3 variables via the merging of two hierarchical variables

Transition from 4 to 3 variables via the merging of two hierarchical
variables

## Usage

``` r
from_4_to_3_case_2_hr(
  dfs,
  dfs_name,
  v1,
  v2,
  totcode,
  hrcfiles,
  dir_name,
  sep = "_"
)
```

## Arguments

- dfs:

  data.frame with 4 categorical variables (n \>= 2 in the general case)

- dfs_name:

  name of the data.frame in the list provided by the user

- v1:

  hierarchical categorical variable

- v2:

  hierarchical categorical variable

- totcode:

  named vector of totals for categorical variables

- hrcfiles:

  named vector indicating the hrc files of hierarchical variables among
  the categorical variables of dfs

- dir_name:

  folder where to write the hrc files if no folder is specified in
  hrcfiles

- sep:

  separator used during the concatenation of variables

## Value

A list containing the following components:

- `tabs`: named list of 3-dimensional dataframes (n-1 dimensions in the
  general case) with nested hierarchies

- `hrcs`: named list of hrc specific to the variable created via the
  merge

- `alt_tot`: named list of totals

- `vars`: named list of vectors representing the merged variables during
  the two stages of dimension reduction

## Examples

``` r
library(dplyr)
data <- expand.grid(
  ACT = c("Total", "A", "B", "A1", "A2", "B1", "B2"),
  SEX = c("Total", "F", "M","F1","F2","M1","M2"),
  AGE = c("Total", "AGE1", "AGE2", "AGE11", "AGE12", "AGE21", "AGE22"),
  ECO = c("PIB","Ménages","Entreprises"),
  stringsAsFactors = FALSE,
  KEEP.OUT.ATTRS = FALSE
) %>%
  as.data.frame()

data <- data %>% mutate(VALUE = 1:n())

hrc_act <- "hrc_ACT.hrc"
sdcHierarchies::hier_create(root = "Total", nodes = c("A","B")) %>%
  sdcHierarchies::hier_add(root = "A", nodes = c("A1","A2")) %>%
  sdcHierarchies::hier_convert(as = "argus") %>%
  slice(-1) %>%
  mutate(levels = substring(paste0(level,name),3)) %>%
  select(levels) %>%
  write.table(file = hrc_act, row.names = FALSE, col.names = FALSE, quote = FALSE)

hrc_sex <- "hrc_SEX.hrc"
sdcHierarchies::hier_create(root = "Total", nodes = c("F","M")) %>%
  sdcHierarchies::hier_add(root = "F", nodes = c("F1","F2")) %>%
  sdcHierarchies::hier_add(root = "M", nodes = c("M1","M2")) %>%
  sdcHierarchies::hier_convert(as = "argus") %>%
  slice(-1) %>%
  mutate(levels = substring(paste0(level,name),3)) %>%
  select(levels) %>%
  write.table(file = hrc_sex, row.names = FALSE, col.names = FALSE, quote = FALSE)

res <- from_4_to_3_case_2_hr(dfs = data,
                                dfs_name = "dfs_name",
                                v1 = "ACT",v2 = "SEX",
                                totcode = c(ACT = "Total",SEX = "Total",
                                            AGE = "Total",ECO = "PIB"),
                                hrcfiles = c(ACT = hrc_act, SEX = hrc_sex),
                                dir_name = "output")
```
