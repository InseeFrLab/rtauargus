# Group Tables Based on Inclusion Relationships

This function regroups tables that are included in each other into
clusters, keeping only the tables necessary for protection. Based on the
inclusion relationships detected by the
[`create_edges()`](https://inseefrlab.github.io/rtauargus/reference/create_edges.md)
function, the tables are aggregated to minimize redundancy. The output
identifies the final tables that need to be protected and provides a
mapping of original table names to their respective groups.

## Usage

``` r
grp_tab_names(list_split)
```

## Arguments

- list_split:

  A list of data frames, where each data frame describes the inclusion
  relationships (`from` and `to`) between tables in a cluster.
  Typically, this is the output of the
  [`create_edges()`](https://inseefrlab.github.io/rtauargus/reference/create_edges.md)
  function.

## Value

A list of results for each cluster, where each result is a list
containing:

- `tab_finales`: A data frame describing the final relationships between
  grouped tables. It includes only the tables necessary for protection.

- `passage_nom_tab`: A data frame mapping original table names
  (`Original`) to their respective groups (`Group`).

## Examples

``` r
if (FALSE) { # \dontrun{
# Example data
data(metadata_pizza_lettuce)

# Convert wide metadata to long format
metadata_pizza_lettuce_long <- wide_to_long(metadata_pizza_lettuce)

# Identify hierarchical relationships
list_hrc_identified <- identify_hrc(metadata_pizza_lettuce_long)

# Split tables into clusters
list_split <- split_in_clusters(list_hrc_identified)

# Detect inclusion relationships
list_desc_links <- create_edges(list_split)

# Group tables based on inclusion relationships
list_translation_tables <- grp_tab_names(list_desc_links)

# View structure of the results
str(list_translation_tables)
} # }
```
