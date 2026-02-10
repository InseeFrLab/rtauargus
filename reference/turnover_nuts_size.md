# data - Turnover broken down by NUTS and size of French companies (fake values).

A tabular dataset containing the turnover broken down by NUTS
geographical localisation and Size of companies. Useful for playing with
tab\_ functions.

## Usage

``` r
turnover_nuts_size
```

## Format

A tibble/data frame with 460 rows and 5 variables:

- NUTS:

  nuts - european denomination of administrative levels. Hierarchical
  variables with two levels (nuts2 and nuts3) described in the
  nuts23_fr_corr_table dataset. The root is noted "Total"

- SIZE:

  size of the companies (Number of employees in three categories and
  overall category "Total")

- N_OBS:

  Frequency, number of companies

- TOT:

  turnover value in euros

- MAX:

  turnover of the company which contributes the most to the cell.

## See also

nuts23_fr_corr_table
