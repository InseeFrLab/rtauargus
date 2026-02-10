# data - Turnover broken down by business sector and type of companies (fake values).

A tabular dataset containing the turnover broken down by Business sector
and Type of companies. Useful for playing with tab\_ functions.

## Usage

``` r
turnover_act_cj
```

## Format

A tibble/data frame with 406 rows and 5 variables:

- ACTIVITY:

  business sector, hierarchical variables with three levels described in
  the activity_corr_table dataset. The root is noted "Total"

- CJ:

  Type of companies (3 categories + overall category "Total")

- N_OBS:

  Frequency, number of companies

- TOT:

  turnover

- MAX:

  turnover of the company which contributes the most to the cell.

## See also

activity_corr_table
