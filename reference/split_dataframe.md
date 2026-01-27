# Split a Data Frame Based on a Chosen Variable

This function splits a data frame into a list of smaller data frames
based on a specified variable (`var`) and its associated hierarchical
variable (`hrc_var`). The function handles cases where `hrc_var`
contains character values or is missing, ensuring that independent and
hierarchical groupings are managed appropriately.

## Utilisation

``` r
split_dataframe(tab, var, hrc_var)
```

## Arguments

- tab:

  A data frame to be split.

- var:

  The variable to base the split on.

- hrc_var:

  The hierarchical variable associated with `var`. If `hrc_var` is
  present, the data frame will first be split by its values; otherwise,
  it will be split directly by `var`.

## Valeur de retour

A list of data frames (`list_diff`), where each element corresponds to a
grouping determined by `var` and/or `hrc_var`.

## Exemples

``` r
if (FALSE) { # \dontrun{
# Example data
tab <- data.frame(
  id = 1:6,
  var = c("A", "A", "B", "B", "C", "C"),
  hrc_var = c(NA, "H1", "H1", "H2", NA, NA),
  value = c(10, 20, 30, 40, 50, 60)
)

# Split by 'var' and 'hrc_var'
list_diff <- split_dataframe(tab, "var", "hrc_var")

# View the structure of the result
str(list_diff)
} # }
```
