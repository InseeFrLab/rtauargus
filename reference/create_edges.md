# Create a Data Frame of Table Inclusion Relationships

This function analyzes a list of data frames (clusters of tables) and
identifies inclusion relationships between tables. A table is considered
to include another if all its spanning variables (columns defining the
structure of the table) are contained within the spanning variables of
the other table.

## Utilisation

``` r
create_edges(list_split)
```

## Arguments

- list_split:

  A list of clusters of tables, where each cluster contains nested data
  frames. Typically, this is the output of the `split_in_clusters`
  function.

## Valeur de retour

A list of data frames (`list_desc_links`), where each data frame
describes the inclusion relationships (`from` and `to`) within a cluster
of tables. Each row in a data frame indicates that the table in the
`from` column is fully included in the table in the `to` column.

## Détails

For example, consider two tables to be published:

- **T1**: `company_turnover = {nuts x size}`

- **T2**: `company_turnover = {nuts x size x pollution}`

All the information in **T1** is included in **T2**. By protecting
**T2**, all cells in **T1** will also be protected. This function
identifies such inclusion relationships and outputs a data frame that
describes these links.

## Exemples

``` r
if (FALSE) { # \dontrun{
data(metadata_pizza_lettuce)

# Convert wide metadata to long format
metadata_pizza_lettuce_long <- wide_to_long(metadata_pizza_lettuce)

# Identify hierarchical relationships
list_hrc_identified <- identify_hrc(metadata_pizza_lettuce_long)

# Split tables into clusters
list_split <- split_in_clusters(list_hrc_identified)

# Identify inclusion relationships between tables
list_desc_links <- create_edges(list_split)

# View the structure of the result
str(list_desc_links)
} # }
```
