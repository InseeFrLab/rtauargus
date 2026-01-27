# General function to choose variables to merge, limiting the number of generated tables while ensuring not to generate tables that are too large.

General function to choose variables to merge, limiting the number of
generated tables while ensuring not to generate tables that are too
large.

## Utilisation

``` r
var_to_merge(
  dfs,
  totcode,
  hrcfiles = NULL,
  nb_var = 4,
  nb_tab_option = "min",
  limit = 150
)
```

## Arguments

- dfs:

  data.frame

- totcode:

  named vector of totals for categorical variables

- hrcfiles:

  named vector of hrc files for categorical variables

- nb_var:

  number of variables to merge

- nb_tab_option:

  strategy to follow for choosing variables automatically:

  - `'min'`: minimize the number of tables;

  - `'max'`: maximize the number of tables;

  - `'smart'`: minimize the number of tables under the constraint of
    their row count.

- limit:

  maximum allowed row count in the 'smart' case

## Valeur de retour

A list of vectors representing the chosen variables to merge

## Exemples

``` r
library(dplyr)
data <- expand.grid(
  ACT = c("Total", "A", "B", "A1", "A2", "B1", "B2"),
  GEO = c("Total", "GA", "GB", "GA1", "GA2"),
  SEX = c("Total", "F", "M"),
  AGE = c("Total", "AGE1", "AGE2"),
  stringsAsFactors = FALSE,
  KEEP.OUT.ATTRS = FALSE
) %>%
  as.data.frame()

data <- data %>% mutate(VALUE = 1:n())

hrc_act <- "hrc_ACT.hrc"
sdcHierarchies::hier_create(root = "Total", nodes = c("A","B")) %>%
  sdcHierarchies::hier_add(root = "A", nodes = c("A1","A2")) %>%
  sdcHierarchies::hier_add(root = "B", nodes = c("B1","B2")) %>%
  sdcHierarchies::hier_convert(as = "argus") %>%
  slice(-1) %>%
  mutate(levels = substring(paste0(level,name),3)) %>%
  select(levels) %>%
  write.table(file = hrc_act, row.names = FALSE, col.names = FALSE, quote = FALSE)

hrc_geo <- "hrc_GEO.hrc"
sdcHierarchies::hier_create(root = "Total", nodes = c("GA","GB")) %>%
  sdcHierarchies::hier_add(root = "GA", nodes = c("GA1","GA2")) %>%
  sdcHierarchies::hier_convert(as = "argus") %>%
  slice(-1) %>%
  mutate(levels = substring(paste0(level,name),3)) %>%
  select(levels) %>%
  write.table(file = hrc_geo, row.names = FALSE, col.names = FALSE, quote = FALSE)

totcode <- c(SEX="Total",AGE="Total", GEO="Total", ACT="Total")

hrcfiles <- c(ACT = hrc_act, GEO = hrc_geo)

# Consistent: choose two hierarchical variables
res1 <- var_to_merge(dfs = data,
                                        totcode = totcode,
                                        hrcfiles = hrcfiles,
                                        nb_var = 2,
                                        nb_tab_option = 'max')
res1
#> $vars
#> [1] "GEO" "ACT"
#> 
#> $max_row
#> [1] 63
#> 
#> $nb_tab
#> [1] 12
#> 
max(unlist(length_tabs(dfs = data,
                       hrcfiles = hrcfiles,
                       totcode = totcode,
                       v1 = res1$vars[1], v2 = res1$vars[2])))
#> [1] 63

# Consistent: choose two non-hierarchical variables
res2 <- var_to_merge(dfs = data,
                                totcode = totcode,
                                hrcfiles = hrcfiles,
                                nb_var = 2,
                                nb_tab_option = 'min')
res2
#> $vars
#> [1] "SEX" "AGE"
#> 
#> $max_row
#> [1] 245
#> 
#> $nb_tab
#> [1] 2
#> 
max(unlist(length_tabs(dfs = data,
                       hrcfiles = hrcfiles,
                       totcode = totcode,
                       v1 = res2$vars[1], v2 = res2$vars[2])))
#> [1] 245

res3 <- var_to_merge(dfs = data,
                                totcode = totcode,
                                hrcfiles = hrcfiles,
                                limit = 200,
                                nb_var = 2,
                                nb_tab_option = 'smart')
res3
#> $vars
#> [1] "SEX" "GEO"
#> 
#> $max_row
#> [1] 147
#> 
#> $nb_tab
#> [1] 4
#> 
max(unlist(length_tabs(dfs = data,
                       hrcfiles = hrcfiles,
                       totcode = totcode,
                       v1 = res3$vars[1], v2 = res3$vars[2])))
#> [1] 147

# Obtains 147, which is well below 200

res4 <- var_to_merge(dfs = data,
                                totcode = totcode,
                                hrcfiles = hrcfiles,
                                limit = 5,
                                nb_var = 2,
                                nb_tab_option = 'smart')
res4
#> $vars
#> [1] "GEO" "ACT"
#> 
#> $max_row
#> [1] 63
#> 
#> $nb_tab
#> [1] 12
#> 
max(unlist(length_tabs(dfs = data,
                       hrcfiles = hrcfiles,
                       totcode = totcode,
                       v1 = res4$vars[1], v2 = res4$vars[2])))
#> [1] 63

# Receives a warning: unable to reach the announced value
# There are 63 rows (equivalent to the max
# -> this is what reduces the table size)
# And the warning announces 63 rows, which is consistent with the output
```
