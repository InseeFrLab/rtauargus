# Split a Data Frame into Clusters of Linked Tables

This function splits a data frame into a list of data frames (or
clusters), where each cluster represents a group of linked tables.
Tables are grouped based on the `field` variable and their corresponding
`indicator` values. The hierarchical relationships specified in the
`hrc_*` columns are also considered.

## Utilisation

``` r
split_in_clusters(list_hrc_identified)
```

## Arguments

- list_hrc_identified:

  A list returned by the `identify_hrc` function. The first element of
  the list must be a data frame containing the variables:

  - `field`: A grouping variable.

  - `hrc_field`: The hierarchical counterpart of `field`.

  - `indicator`: A variable used to link tables.

  - `hrc_indicator`: The hierarchical counterpart of `indicator`.

## Valeur de retour

A named list of data frames. Each element of the list corresponds to a
cluster of linked tables, and the names of the list elements reflect the
hierarchical grouping path.

## Détails

The function handles cases where the `field` variable is constant across
the data frame or when `field` varies, in which case the data frame is
split by `field` and further split by `indicator` and `hrc_indicator`.

## Exemples

``` r
if (FALSE) { # \dontrun{
data(metadata_pizza_lettuce)

metadata_pizza_lettuce_long <- wide_to_long(metadata_pizza_lettuce)
# Identify hierarchical relationships
list_hrc_identified <- identify_hrc(metadata_pizza_lettuce_long)
# Split into clusters
list_split <- split_in_clusters(list_hrc_identified)

# View the structure of the result
str(list_split)
} # }
```
