# Rename variables based on their hierarchies and their equations

This function renames variables in a long-format metadata data frame
based on their hierarchical groupings. Spanning variables are renamed
using the name of their hierarchy in uppercase, while response variables
linked by an equation (specified in the `hrc_indicator` column) are
grouped together, and a new grouping variable is created.

## Utilisation

``` r
identify_hrc_with_eq(df_metadata_long, df_eq_indicator)
```

## Arguments

- df_metadata_long:

  A data frame in long format with the following required columns:

  - `table_name`: Identifies the table.

  - `field` : name of the field of the table.

  - `indicator`: name of the indicator of the table.

  - `hrc_indicator`: Specifies linked response variables.

  - `spanning_*`, `hrc_spanning_*`: Spanning variables and their
    hierarchies.

- df_eq_indicator:

  A dataframe containing the equations on indicators with the following
  required columns :

  - `eq_name`: Name of the equation.

  - `eq_indicator`: The equation for example, A = B + C.

  - `unit`: The unit of the indicators in the equation.

## Valeur de retour

`list_hrc_identified`, a list with two elements:

- `df_indicators`: The updated data frame with renamed variables and
  grouped response variables.

- `df_variable_info`: A data frame mapping original variable names
  (`spanning_old`) to their renamed counterparts (`spanning`).

## Exemples

``` r
if (FALSE) { # \dontrun{
data(metadata_pizza_lettuce)

metadata_pizza_lettuce_long <- wide_to_long(metadata_pizza_lettuce)
 df_eq_ex <- data.frame(
 eq_name = c("eq1"),
 eq_indicator = c("ca_salades = ca_batavia + ca_mache"),
 unit = c("EUR"))

list_hrc_identified <- identify_hrc(metadata_pizza_lettuce_long, df_eq_ex)

str(list_hrc_identified)
} # }
```
