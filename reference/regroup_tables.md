# Regroup tables within a group (i.e. equation / group of linked equations) based on spanning combination completeness

For a given group of tables, this function identifies which tables cover
all sides of an equation (total, rhs1, rhs2, ...) for their spanning
combination, and which do not. Tables with complete combinations are
merged into a single row; tables with incomplete combinations are kept
as standalone rows with their original spannings.

## Usage

``` r
regroup_tables(df_group, spanning_combination_group)
```

## Arguments

- df_group:

  A tibble containing the rows of a single group from `df_with_group`.
  Must contain columns: `table_name`, `spanning`, `side`, `var_mapped`,
  `indicator`, `unit`, and `group`.

- spanning_combination_group:

  A tibble produced by the `spanning_combination_group` pipeline,
  containing one row per (group, spanning_key) combination. Must contain
  columns: `group`, `spanning_key`, and `all_combinations` (logical
  indicating whether the spanning combination covers all sides of the
  equation).

## Value

A tibble with one row per (merged or solo) table cluster and spanning,
containing the following columns (among others):

- table_name:

  Dot-separated list of merged table names (e.g. `"T7.T9.T11"`) for
  complete combinations, or the original table name for incomplete ones.

- indicator:

  The unit value shared across the merged tables.

- initial_indicator:

  The `var_mapped` value of the `total` side, used to track the original
  indicator before merging.

## Examples

``` r
if (FALSE) { # \dontrun{
list_groups <- split(df_with_group, df_with_group$group)

df_eq_initial_spannings <- purrr::map(list_groups, function(df_group) {
  regroup_tables(df_group, spanning_combination_group)
}) |>
  purrr::compact() |>
  dplyr::bind_rows()
} # }
```
