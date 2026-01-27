# Change the result of dimension reduction to be directly usable in rtauargus

Change the result of dimension reduction to be directly usable in
rtauargus

## Utilisation

``` r
sp_format(res, dfs_name, sep, totcode, hrcfiles)
```

## Arguments

- res:

  result of variable merging composed of name_non_changed_vars, a list
  of lists of tables, a list of hierarchical files, a list of subtotals
  associated with these files, and a list of vectors of variables or a
  vector of variables depending on the base size of the dataframes

- dfs_name:

  the name of the entered dataframes

- sep:

  character

- totcode:

  character named vector

- hrcfiles:

  character named vector

## Valeur de retour

A list containing:

- `tabs`: named list of 3-dimensional dataframes with nested hierarchies

- `alt_hrc`: named list of hrc specific to the variables created during
  merging to go to dimension 3

- `alt_totcode`: named list of totals specific to the variables created
  during merging to go to dimension 3

- `vars`: categorical variables of the output dataframes

- `sep`: separator used to link the variables

- `totcode`: named vector of totals for all categorical variables

- `hrcfiles`: named vector of hrc for categorical variables (except the
  merged one)

- `fus_vars`: named vector of vectors representing the merged variables
  during dimension reduction

## Exemples

``` r
library(dplyr)
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

res <- sp_format(res1,
        dfs_name = "tab",
        sep = "_",
        totcode = c(SEX="Total",AGE="Total",
                   GEO="Total", ACT="Total"),
       hrcfiles = c(ACT = hrc_act)
       )
```
