# data - Correspondence table describing the business sectors hierarchy.

A dataset describing the nesting of three levels of business sectors,
useful when working with the ACTIVITY variables in the turnover\_
datasets.

## Usage

``` r
activity_corr_table
```

## Format

A data frame with 92 rows and 3 variables:

- A10:

  business sectors in 10 categories

- A21:

  business sectors in 21 categories

- A88:

  business sectors in 88 categories

## Details

Use the `write_hrc2` function to create a .hrc file from this
correspondence table.
