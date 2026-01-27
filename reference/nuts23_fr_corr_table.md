# data - Correspondence table describing the NUTS hierarchy.

A dataset describing the nesting of NUTS2 and NUTS3 levels for
Metropolitan France, useful when working with the NUTS variables in the
turnover\_ datasets.

## Utilisation

``` r
nuts23_fr_corr_table
```

## Format

A data frame with 92 rows and 3 variables:

- NUTS2:

  NUTS2 levels in France - equivalent of French "Régions"

- NUTS3:

  NUTS3 levels in France - equivalent of French "Départements"

## Détails

Use the `write_hrc2` function to create a .hrc file from this
correspondence table.
