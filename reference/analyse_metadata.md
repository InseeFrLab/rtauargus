# Analyse Metadata of Tables Needing Secondary Tabular Data Protection

**\[experimental\]**

## Utilisation

``` r
analyse_metadata(df_metadata, df_eq_indicator = NULL, verbose = FALSE)
```

## Arguments

- df_metadata:

  A dataframe containing metadata in wide format.

- df_eq_indicator:

  A datadframe containing the indicators equations if needed.

- verbose:

  Logical. If `TRUE`, returns a detailed list of intermediate results
  from each processing step. If `FALSE`, returns only the cluster
  assignments. Defaults to `FALSE`.

## Valeur de retour

A list or dataframe, depending on the value of the `verbose` parameter:

- If `verbose = TRUE`, returns a list with detailed intermediate
  results:

  - `identify_hrc`:

    A data frame with renamed variables and grouped response variables.

  - `info_var`:

    A mapping of original variable names to their renamed counterparts.

  - `split_in_clusters`:

    A list of clusters obtained after splitting the data.

  - `create_edges`:

    A list of edges created for describing relationships.

  - `grp_tab_names`:

    Translation tables generated for renaming and regrouping.

  - `grp_tab_in_clusters`:

    Independent tables grouped by clusters.

  - `tab_to_treat`:

    Cluster assignments for tables to be treated.

  - `df_tab_to_treat`:

    A dataframe summarizing the tables and their clusters.

- If `verbose = FALSE`, returns only the cluster assignments
  (`tab_to_treat`).

## Détails

This function analyzes a metadata dataframe to determine which tables
should be treated together in the same cluster. It also rearranges and
groups the tables based on hierarchical relationships, creating a
structured output for further processing.

The function performs the following steps:

- Converts the metadata from wide format to long format using
  `wide_to_long`.

- Identifies hierarchical relationships and renames variables with
  `identify_hrc` or `identify_hrc_with_eq`.

- Splits hierarchical relationships into clusters using
  `split_in_clusters`.

- Creates edges to describe the relationships via `create_edges`.

- Generates translation tables for regrouping with `grp_tab_names`.

- Regroups tables into independent clusters with `grp_tab_in_cluster`.

- Identifies tables to be treated together using `tab_to_treat`.

- Produces a final dataframe summarizing the cluster assignments using
  `dataframe_result`.

## Exemples

``` r
data(metadata_pizza_lettuce)

# View the structure of the original data
str(metadata_pizza_lettuce)
#> 'data.frame':    12 obs. of  9 variables:
#>  $ table_name    : chr  "T1" "T2" "T3" "T4" ...
#>  $ field         : chr  "france_entreprises_2023" "france_entreprises_2023" "france_entreprises_2023" "france_entreprises_2023" ...
#>  $ hrc_field     : logi  NA NA NA NA NA NA ...
#>  $ indicator     : chr  "to_pizza" "to_pizza" "to_pizza" "to_pizza" ...
#>  $ hrc_indicator : chr  NA NA NA NA ...
#>  $ spanning_1    : chr  "nuts2" "nuts3" "a10" "a10" ...
#>  $ hrc_spanning_1: chr  "hrc_nuts" "hrc_nuts" "hrc_naf" "hrc_naf" ...
#>  $ spanning_2    : chr  "size" "size" "nuts2" "nuts3" ...
#>  $ hrc_spanning_2: chr  NA NA "hrc_nuts" "hrc_nuts" ...

# Run the analysis
detailed_analysis <- analyse_metadata(metadata_pizza_lettuce, verbose = TRUE)

# Simplified output (non-verbose)
cluster_id_dataframe <- analyse_metadata(metadata_pizza_lettuce, verbose = FALSE)
```
