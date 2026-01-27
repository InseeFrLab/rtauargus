# Check for Non-Total Values in a Data Frame

This function checks if any column in a data frame contains values that
are not part of a given set of totals.

## Utilisation

``` r
contains_non_total(data, totals)
```

## Arguments

- data:

  A data frame containing the data to be checked.

- totals:

  A vector of values considered as totals.

## Valeur de retour

A logical value: `TRUE` if at least one non-total value exists,
otherwise `FALSE`.

## Exemples

``` r
if (FALSE) { # \dontrun{
df <- data.frame(
  col1 = c("A", "B", "TOTAL"),
  col2 = c("X", "TOTAL", "TOTAL")
)
contains_non_total(df, c("TOTAL"))
# Returns TRUE
} # }
```
