# Metadata for pizza and lettuce dataset.

A dataset containing metadata information for various tables and
indicators related to enterprises turnover selling pizzas and lettuces
in France.

## Utilisation

``` r
metadata_pizza_lettuce
```

## Format

A data frame with 12 rows and 9 variables:

- table_name:

  Character. Name of the table (e.g., "T1", "T2").

- field:

  Character. Data source or category (e.g., "france_entreprises_2023").

- hrc_field:

  Logical. Indicates if hierarchical classification is applied to the
  field (NA if not applicable).

- indicator:

  Character. The indicator being measured (e.g., "to_pizza").

- hrc_indicator:

  Character. Hierarchical classification for the indicator (NA if not
  applicable).

- spanning_1:

  Character. First spanning variable (e.g., "nuts2", "nuts3", "a10").

- hrc_spanning_1:

  Character. Hierarchical classification for spanning_1 (e.g.,
  "hrc_nuts", "hrc_naf").

- spanning_2:

  Character. Second spanning variable (e.g., "size", "nuts2", "nuts3").

- hrc_spanning_2:

  Character. Hierarchical classification for spanning_2 (NA if not
  applicable).
