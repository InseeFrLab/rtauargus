# Determines the tables described in a template gathering all the published cells

**\[experimental\]**

## Utilisation

``` r
format_template(data, indicator_column, spanning_var_tot, field_columns)
```

## Arguments

- data:

  template gathering all the published cells

- indicator_column:

  name of the column in which the indicators are

- spanning_var_tot:

  a named list of the spanning variables and their totals

- field_columns:

  vector of all the columns that are fields (ex: year of collect)

## Valeur de retour

named list of a dataframe describing the tables (metadata) and a list of
the modalities of each hierarchical variable (modalities)

## Exemples

``` r
data(enterprise_template)

template_formatted <- format_template(
  data = enterprise_template,
  indicator_column = "INDICATOR",
  spanning_var_tot = list(
    ACTIVITY = "BTSXO_S94",
    NUMBER_EMPL = "_T",
    LEGAL_FORM = "_T"),
  field_columns = c("TIME_PERIOD")
)
#> treating the field 2021 
#> treating the field 2022 
```
