# Package index

## Protect tables from tabular

Functions for protecting magnitude and frequency tables from one tabular
data or from a set of linked tables.

- [`tab_multi_manager()`](https://inseefrlab.github.io/rtauargus/reference/tab_multi_manager.md)
  : Manages the secondary secret of a list of tables

- [`write_hrc2()`](https://inseefrlab.github.io/rtauargus/reference/write_hrc2.md)
  : Creates a hrc file from correspondence table

- [`tab_rtauargus()`](https://inseefrlab.github.io/rtauargus/reference/tab_rtauargus.md)
  : Protect one table by suppressing cells with Tau-Argus

- [`tab_rtauargus2()`](https://inseefrlab.github.io/rtauargus/reference/tab_rtauargus2.md)
  :

  Wrapper of tab_rtauargus adapted for `tab_multi_manager` function.

- [`tab_rtauargus4()`](https://inseefrlab.github.io/rtauargus/reference/tab_rtauargus4.md)
  **\[experimental\]** : Call Tau-Argus to protect a 4 or 5 dimensions
  table by splitting it in several 3 dimensions table.

- [`tabulate_micro_data()`](https://inseefrlab.github.io/rtauargus/reference/tabulate_micro_data.md)
  : tabulate grouped data with all margins, handling hierarchical
  variables

- [`summarize_secret()`](https://inseefrlab.github.io/rtauargus/reference/summarize_secret.md)
  : Provide the summary of the suppression pattern from a rtauargus
  result

## Proceed to an automatic analysis

Functions to help the links analysis between tables from a metadata
file.

- [`analyse_metadata()`](https://inseefrlab.github.io/rtauargus/reference/analyse_metadata.md)
  **\[experimental\]** : Analyse Metadata of Tables Needing Secondary
  Tabular Data Protection
- [`format_template()`](https://inseefrlab.github.io/rtauargus/reference/format_template.md)
  **\[experimental\]** : Determines the tables described in a template
  gathering all the published cells

## Data

Data to run examples

- [`activity_corr_table`](https://inseefrlab.github.io/rtauargus/reference/activity_corr_table.md)
  : data - Correspondence table describing the business sectors
  hierarchy.
- [`nuts23_fr_corr_table`](https://inseefrlab.github.io/rtauargus/reference/nuts23_fr_corr_table.md)
  : data - Correspondence table describing the NUTS hierarchy.
- [`turnover_act_cj`](https://inseefrlab.github.io/rtauargus/reference/turnover_act_cj.md)
  : data - Turnover broken down by business sector and type of companies
  (fake values).
- [`turnover_act_size`](https://inseefrlab.github.io/rtauargus/reference/turnover_act_size.md)
  : data - Turnover broken down by business sector and size of French
  companies (fake values).
- [`turnover_nuts_cj`](https://inseefrlab.github.io/rtauargus/reference/turnover_nuts_cj.md)
  : data - Turnover broken down by NUTS and size of French companies
  (fake values).
- [`turnover_nuts_size`](https://inseefrlab.github.io/rtauargus/reference/turnover_nuts_size.md)
  : data - Turnover broken down by NUTS and size of French companies
  (fake values).
- [`turnover_act_nuts_size`](https://inseefrlab.github.io/rtauargus/reference/turnover_act_nuts_size.md)
  : data - Turnover broken down by business sector, NUTS, and size of
  French companies (fake values).
- [`datatest1`](https://inseefrlab.github.io/rtauargus/reference/datatest1.md)
  : data crossing 4 categorical variables, none are hierarchical.
- [`datatest2`](https://inseefrlab.github.io/rtauargus/reference/datatest2.md)
  : data crossing 5 categorical variables, none are hierarchical.
- [`indiv_dt`](https://inseefrlab.github.io/rtauargus/reference/indiv_dt.md)
  : Companies data at individual level.
- [`metadata_pizza_lettuce`](https://inseefrlab.github.io/rtauargus/reference/metadata_pizza_lettuce.md)
  : Metadata for pizza and lettuce dataset.
- [`enterprise_template`](https://inseefrlab.github.io/rtauargus/reference/enterprise_template.md)
  : Extract of SBS Eurostat template.

## Protect tables from microdata

Functions for protecting magnitude and frequency tables, i.e. from a
microdataset. Original way to proceed but no longer the favored one.

- [`micro_rtauargus()`](https://inseefrlab.github.io/rtauargus/reference/micro_rtauargus.md)
  **\[superseded\]** : Protects tables from microdata
- [`rtauargus_plus()`](https://inseefrlab.github.io/rtauargus/reference/rtauargus_plus.md)
  : Mass protection

## Step by step functions

Mostly internal functions

- [`tab_rda()`](https://inseefrlab.github.io/rtauargus/reference/tab_rda.md)
  : Creates rda files from tabular data
- [`tab_arb()`](https://inseefrlab.github.io/rtauargus/reference/tab_arb.md)
  : Creates a batch file (.arb) for tabular data in order to run Tau
  Argus
- [`micro_asc_rda()`](https://inseefrlab.github.io/rtauargus/reference/micro_asc_rda.md)
  **\[superseded\]** : Creates asc and rda files from microdata
- [`micro_arb()`](https://inseefrlab.github.io/rtauargus/reference/micro_arb.md)
  **\[superseded\]** : Creates a batch file (.arb) for microdata
- [`run_arb()`](https://inseefrlab.github.io/rtauargus/reference/run_arb.md)
  : Runs a Tau-Argus batch
- [`import()`](https://inseefrlab.github.io/rtauargus/reference/import.md)
  : Imports results from Tau-Argus

## Others

- [`rtauargus_options()`](https://inseefrlab.github.io/rtauargus/reference/rtauargus_options.md)
  [`reset_rtauargus_options()`](https://inseefrlab.github.io/rtauargus/reference/rtauargus_options.md)
  : Manages options of rtauargus package
- [`write_hrc()`](https://inseefrlab.github.io/rtauargus/reference/write_hrc.md)
  : Creates a hrc file from microdata
- [`rtauargus`](https://inseefrlab.github.io/rtauargus/reference/rtauargus-package.md)
  [`rtauargus-package`](https://inseefrlab.github.io/rtauargus/reference/rtauargus-package.md)
  : rtauargus: Using Tau-Argus from R
- [`contains_non_total()`](https://inseefrlab.github.io/rtauargus/reference/contains_non_total.md)
  : Check for Non-Total Values in a Data Frame
- [`create_edges()`](https://inseefrlab.github.io/rtauargus/reference/create_edges.md)
  : Create a Data Frame of Table Inclusion Relationships
- [`dataframe_result()`](https://inseefrlab.github.io/rtauargus/reference/dataframe_result.md)
  : Combine List of Dataframes into a Single Dataframe with Cluster
  Identification
- [`enterprise_template`](https://inseefrlab.github.io/rtauargus/reference/enterprise_template.md)
  : Extract of SBS Eurostat template.
- [`filter_on_marginal_of_spanning_var()`](https://inseefrlab.github.io/rtauargus/reference/filter_on_marginal_of_spanning_var.md)
  : Filter a Data Frame Based on Marginal Criteria
- [`get_combinations()`](https://inseefrlab.github.io/rtauargus/reference/get_combinations.md)
  : Generate All Combinations of spanning variables
- [`grp_tab_in_cluster()`](https://inseefrlab.github.io/rtauargus/reference/grp_tab_in_cluster.md)
  : Regroup Tables Inside Clusters Based on Inclusion Relationships
- [`grp_tab_names()`](https://inseefrlab.github.io/rtauargus/reference/grp_tab_names.md)
  : Group Tables Based on Inclusion Relationships
- [`identify_hrc()`](https://inseefrlab.github.io/rtauargus/reference/identify_hrc.md)
  : Rename Variables Based on Their Hierarchies
- [`identify_hrc_with_eq()`](https://inseefrlab.github.io/rtauargus/reference/identify_hrc_with_eq.md)
  : Rename variables based on their hierarchies and their equations
- [`metadata_pizza_lettuce`](https://inseefrlab.github.io/rtauargus/reference/metadata_pizza_lettuce.md)
  : Metadata for pizza and lettuce dataset.
- [`split_dataframe()`](https://inseefrlab.github.io/rtauargus/reference/split_dataframe.md)
  : Split a Data Frame Based on a Chosen Variable
- [`split_in_clusters()`](https://inseefrlab.github.io/rtauargus/reference/split_in_clusters.md)
  : Split a Data Frame into Clusters of Linked Tables
- [`tab_to_treat()`](https://inseefrlab.github.io/rtauargus/reference/tab_to_treat.md)
  : Unnest Data Frames to Create a Usable Flat Format
- [`wide_to_long()`](https://inseefrlab.github.io/rtauargus/reference/wide_to_long.md)
  : Convert Metadata from Wide to Long Format
- [`explore_reduce_dims()`](https://inseefrlab.github.io/rtauargus/reference/explore_reduce_dims.md)
  : Analytically computes all possible table splits and summarizes their
  statistics.
