# Convert Metadata from Wide to Long Format

This function transforms a metadata data frame from a wide format to a
long format. The goal is to consolidate spanning variables into a single
column, making the table easier to handle regardless of the number of
spanning variables described.

## Utilisation

``` r
wide_to_long(df_metadata)
```

## Arguments

- df_metadata:

  A data frame in wide format. It must contain columns starting with
  "spanning" and "hrc_spanning", representing spanning variables and
  their hierarchical counterparts.

## Valeur de retour

A data frame in long format. The resulting data frame contains two
columns:

- `spanning`: The consolidated values of the spanning variables.

- Any additional columns retained from the original data frame. Rows
  where the names of "spanning" and "hrc_spanning" do not match, or
  where `spanning` values are NA, are filtered out.

## Exemples

``` r
if (FALSE) { # \dontrun{
data(metadata_pizza_lettuce)

# View the structure of the original data
str(metadata_pizza_lettuce)

# Convert to long format
metadata_pizza_lettuce_long <- wide_to_long(metadata_pizza_lettuce)

# View the structure of the resulting data
str(metadata_pizza_lettuce_long)
} # }
```
