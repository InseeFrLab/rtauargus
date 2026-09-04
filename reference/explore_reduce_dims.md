# Analytically computes all possible table splits and summarizes their statistics.

Analytically computes all possible table splits and summarizes their
statistics.

## Usage

``` r
explore_reduce_dims(dfs, totcode, hrcfiles = NULL)
```

## Arguments

- dfs:

  data.frame containing 4 or 5 categorical variables

- totcode:

  named vector of totals for categorical variables

- hrcfiles:

  named vector of hrc files (optional)

## Value

A deduplicated data.frame containing the following columns:

- `nb_tab`: number of generated tables

- `nb_hrc`: number of remaining hierarchical variables

- `min_size`: minimum table size (rows)

- `med_size`: median table size (rows, rounded to integer)

- `max_size`: maximum table size (rows)
