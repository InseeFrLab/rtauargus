# Generate All Combinations of spanning variables

This function generates all possible combinations of keys from a given
named list or vector.

## Utilisation

``` r
get_combinations(criteria)
```

## Arguments

- criteria:

  A named list or vector, where the names represent the keys.

## Valeur de retour

A list of combinations, where each combination is a character vector of
key names.

## Exemples

``` r
if (FALSE) { # \dontrun{
criteria <- list(key1 = "value1", key2 = "value2", key3 = "value3")
get_combinations(criteria)
# Returns a list of combinations: c("key1"), c("key2"), c("key3"), c("key1", "key2"), ...
} # }
```
