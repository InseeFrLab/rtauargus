# data - Turnover broken down by business sector and size of French companies (fake values).

A tabular dataset containing the turnover broken down by Business sector
and Size of companies. Useful for playing with tab\_ functions.

## Utilisation

``` r
turnover_act_size
```

## Format

A tibble/data frame with 414 rows and 5 variables:

- ACTIVITY:

  business sector, hierarchical variables with three levels described in
  the activity_corr_table dataset. The root is noted "Total"

- SIZE:

  size of the companies (Number of employees in three categories and
  overall category "Total")

- N_OBS:

  Frequency, number of companies

- TOT:

  turnover value in euros

- MAX:

  turnover of the company which contributes the most to the cell.

## Voir également

activity_corr_table
