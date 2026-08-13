# Combine List of Dataframes into a Single Dataframe with Cluster Identification

This function consolidates a list of dataframes, each representing a
cluster of tables, into a single dataframe. It includes an additional
column, `cluster`, to identify the cluster each table belongs to.

## Usage

``` r
dataframe_result(list_independent_tables, list_hrc_identified)
```

## Arguments

- list_independent_tables:

  A list of tibbles, typically the output of
  [`grp_tab_in_cluster()`](https://inseefrlab.github.io/rtauargus/reference/grp_tab_in_cluster.md)
  or
  [`tab_to_treat()`](https://inseefrlab.github.io/rtauargus/reference/tab_to_treat.md).
  Each tibble contains metadata for tables grouped within a specific
  cluster.

- list_hrc_identified:

  A list returned by the `identify_hrc` function. The first element of
  the list must be a data frame containing the variables:

  - `field`: A grouping variable.

  - `hrc_field`: The hierarchical counterpart of `field`.

  - `indicator`: A variable used to link tables.

  - `hrc_indicator`: The hierarchical counterpart of `indicator`.

## Value

A single dataframe (`dfMetadata_to_treat`) with the following structure:

- `cluster`: Identifier for the cluster each table belongs to.

- `table_name`: The name of the table.

- `field`: The field name associated with the table.

- `indicator`: Indicators related to the table.

- `spanning_*`: Columns derived from the spanning metadata, ordered by
  numeric suffix.

- `hrc_spanning_*`: Columns derived from hierarchical spanning metadata,
  ordered by numeric suffix.

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

# Create a dataframe with a cluster id
dataframe_cluster_id <- dataframe_result(list_tab_to_treat)

# View the result dataframe
dataframe_cluster_id
} # }
```
