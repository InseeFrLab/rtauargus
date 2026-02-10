# Regroup Tables Inside Clusters Based on Inclusion Relationships

This function processes clusters of tables and regroups tables that are
included in each other within each cluster. It leverages the
relationships identified by
[`grp_tab_names()`](https://inseefrlab.github.io/rtauargus/reference/grp_tab_names.md)
and uses the grouped structure of tables from `list_split`.

## Usage

``` r
grp_tab_in_cluster(list_split, list_translation_tables)
```

## Arguments

- list_split:

  A list of clusters of tables, where each cluster contains nested data
  frames. Typically, this is the output of the
  [`split_in_clusters()`](https://inseefrlab.github.io/rtauargus/reference/split_in_clusters.md)
  function.

- list_translation_tables:

  A list of results from
  [`grp_tab_names()`](https://inseefrlab.github.io/rtauargus/reference/grp_tab_names.md)
  containing inclusion relationships and mappings of table names to
  their respective groups.

## Value

A list of tibbles (`big_tibble_eg`) for each cluster, where each tibble
contains:

- `table_name`: The updated table name based on grouping.

- `data`: Nested data corresponding to the original table structure.

- `spanning`: Columns that define the structure of each table.

- `tab_inclus`: A list of original table names included within each
  grouped table.

## Details

The resulting tibble contains updated table names (to align with the
group mapping) and metadata for each independent table in the cluster.
Additionally, it tracks which tables are included within each group.

## Examples

``` r
if (FALSE) { # \dontrun{
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

# View structure of the results
str(list_independent_tables)
} # }
```
