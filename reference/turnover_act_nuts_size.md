# data - Turnover broken down by business sector, NUTS, and size of French companies (fake values).

A tabular dataset containing the turnover broken down by Business
sector, NUTS (administrative areas) and Size of companies. The data is
restricted to only three NUTS2 of France (codes FR41, FR42 and FR43) and
their corresponding NUTS3 areas. Useful for playing with tab\_
functions.

## Usage

``` r
turnover_act_nuts_size
```

## Format

A tibble/data frame with 3 168 rows and 6 variables:

- ACTIVITY:

  business sector, hierarchical variables with three levels described in
  the activity_corr_table dataset. The root is noted "Total"

- NUTS:

  nuts - european denomination of administrative levels. Hierarchical
  variables with two levels (nuts2 and nuts3) described in the
  nuts23_fr_corr_table dataset. Only "FR41", "FR42" and "FR43" NUTS2
  areas and their corresponding NUTS3 areas are in the data. The root is
  noted "Total_EAST"

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

activity_corr_table nuts23_fr_corr_table
