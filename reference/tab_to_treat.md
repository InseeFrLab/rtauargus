# Unnest Data Frames to Create a Usable Flat Format

This function processes nested data frames representing tables from
hierarchical clusters, and transforms them into a flat, easy-to-read
format. Each column corresponds to a dplyr::distinct variable,
simplifying downstream analysis and use.

## Usage

``` r
tab_to_treat(list_independent_tables)
```

## Arguments

- list_independent_tables:

  A list of nested tibbles, typically the output of
  [`grp_tab_in_cluster()`](https://inseefrlab.github.io/rtauargus/reference/grp_tab_in_cluster.md),
  where each tibble represents independent tables grouped by clusters.

## Value

A list of unnested tibbles, where each tibble contains the following
columns:

- `table_name`: The name of the table.

- `field`: The field name associated with the table.

- `indicator`: Indicators related to the table.

- `spanning_*`: Columns derived from the spanning metadata (expanded
  into multiple columns).

- `hrc_spanning_*`: Columns derived from hierarchical spanning metadata
  (expanded into multiple columns).

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

# Regroup tables within each cluster
list_independent_tables <- grp_tab_in_cluster(list_split, list_translation_tables)

# Flatten the nested data for downstream use
list_tab_to_treat <- tab_to_treat(list_independent_tables)

# View structure of the results
str(list_tab_to_treat)
} # }
```
