#' Rename variables based on their hierarchies and their equations
#'
#' This function renames variables in a long-format metadata data frame based on
#' their hierarchical groupings. Spanning variables are renamed using the name
#' of their hierarchy in uppercase, while response variables linked by an
#' equation (specified in the `hrc_indicator` column) are grouped together, and
#' a new grouping variable is created.
#'
#' @param df_metadata_long A data frame in long format with the following
#'   required columns:
#'   - `table_name`: Identifies the table.
#'   - `field` : name of the field of the table.
#'   - `indicator`: name of the indicator of the table.
#'   - `hrc_indicator`: Specifies linked response variables.
#'   - `spanning_*`, `hrc_spanning_*`: Spanning variables and their hierarchies.
#' @param df_eq_indicator A dataframe containing the equations on indicators with
#'    the following required columns :
#'    - `eq_name`: Name of the equation.
#'    - `eq_indicator`: The equation for example, A = B + C.
#'    - `unit`: The unit of the indicators in the equation.
#'
#' @return `list_hrc_identified`, a list with two elements:
#'   - `df_indicators`: The updated data frame with renamed variables and grouped
#'     response variables.
#'   - `df_variable_info`: A data frame mapping original variable names
#'     (`spanning_old`) to their renamed counterparts (`spanning`).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data(metadata_pizza_lettuce)
#'
#' metadata_pizza_lettuce_long <- wide_to_long(metadata_pizza_lettuce)
#'  df_eq_ex <- data.frame(
#'  eq_name = c("eq1"),
#'  eq_indicator = c("ca_salades = ca_batavia + ca_mache"),
#'  unit = c("EUR"))
#'
#' list_hrc_identified <- identify_hrc(metadata_pizza_lettuce_long, df_eq_ex)
#'
#' str(list_hrc_identified)
#' }
#'
#' @importFrom dplyr where
#'
identify_hrc_with_eq <- function(df_metadata_long,df_eq_indicator){
  # check that the input is in the right format: right column names
  check_column_names <- function(df) {
    fixed_columns <- c("eq_name", "eq_indicator", "unit")
    if (!all(fixed_columns %in% names(df))) {
      stop("Error: The dataframe describing the equations between indicators is
           missing one or more required columns: eq_name, eq_indicator, unit.")
    }}
  check_column_names(df_eq_indicator)

  # 'parsed_equations' is a data frame where each equation from 'df_eq_indicator'
  # is parsed into its left-hand side ('total') and right-hand side terms ('rhs1', 'rhs2', etc.),
  # with each rhs term placed in a separate column.
  parsed_equations <- df_eq_indicator %>%
    tidyr::separate(eq_indicator, into = c("total", "rhs"), sep = "=", extra = "merge") %>%
    tidyr::separate_rows(rhs, sep = "\\+") %>%
    dplyr::mutate(across(c(total, rhs), trimws)) %>%
    dplyr::group_by(dplyr::across(-rhs)) %>%
    dplyr::mutate(term_number = paste0("rhs", dplyr::row_number())) %>%
    tidyr::pivot_wider(names_from = term_number, values_from = rhs) %>%
    dplyr::ungroup() %>%
    dplyr::select(eq_name, unit, total, everything())

  # Identify ambiguous totals
  total_counts <- parsed_equations %>% dplyr::count(total, name = "n_total")

  # Build a total -> total_alt mapping by eq_name
  # For all equations (ambiguous or not), create one row;
  # for non-ambiguous totals, total_alt == total
  alt_map <- parsed_equations %>%
    dplyr::distinct(eq_name, total) %>%
    dplyr::group_by(total) %>%
    dplyr::arrange(eq_name) %>%                 # ordre stable
    dplyr::mutate(alt_idx = dplyr::row_number(),
                  total_alt = dplyr::case_when(
                    dplyr::n() == 1 ~ total,
                    alt_idx == 1 ~ total,
                    TRUE ~ paste0(total, "_alt", alt_idx - 1)
                  )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(eq_name, total, total_alt)

  # Identify chained equations (A = B + C, B = D + E) and group equations together
  # and apply the mapping to the links
  # here we assume links has an eq_name column; otherwise do
  # left_join(links, parsed_equations %>% select(eq_name, total, rhs), ...) first
  links_full <- parsed_equations %>%
    tidyr::pivot_longer(
      cols = starts_with("rhs"),
      names_to = "rhs_term",
      values_to = "rhs"
    ) %>%
    dplyr::filter(!is.na(rhs)) %>%
    dplyr::mutate(
      total = trimws(as.character(total)),
      rhs   = trimws(as.character(rhs))
    ) %>%
    dplyr::distinct() %>%
    left_join(alt_map, by = c("eq_name", "total")) %>%
    mutate(total = dplyr::coalesce(total_alt, total)) %>%
    select(-total_alt) %>%
    left_join(alt_map, by = c("eq_name", "rhs" = "total")) %>%
    mutate(rhs = dplyr::coalesce(total_alt, rhs)) %>%
    select(total, rhs, eq_name) %>%
    dplyr::distinct()

  g_full <- graph_from_data_frame(links_full %>% select(total, rhs), directed = TRUE)
  comp_full <- igraph::components(g_full)$membership
  comp_df <- data.frame(var = names(comp_full), group = as.integer(comp_full), stringsAsFactors = FALSE)

  # reformat parsed_equations in long format in order to join with df_metadata_long
  equations_long <- parsed_equations %>%
    mutate(across(c(total, starts_with("rhs")), trimws)) %>%
    tidyr::pivot_longer(
      cols = c(total, starts_with("rhs")),
      names_to = "side",
      values_to = "var"
    ) %>%
    filter(!is.na(var))

  equations_long_full <- equations_long %>%
    left_join(alt_map, by = c("eq_name", "var" = "total")) %>%
    mutate(var_mapped = dplyr::coalesce(total_alt, var)) %>%
    select(-total_alt) %>%
    left_join(comp_df, by = c("var_mapped" = "var")) %>%
    mutate(group = as.integer(group))

  # 'df_spannings' is a modified version of 'df_metadata_long' where:
  #   - 'spanning' is replaced by its uppercase hierarchical version if available,
  #   - 'indicator' is replaced by its uppercase hierarchical version
  #   (without the 'hrc_' prefix) if available and 'indicator' not part of 'df_eq_indicator'
  indic_not_in_eq <- setdiff(unique(df_metadata_long$indicator),unique(equations_long$var))

  df_variable_info <- df_metadata_long %>%
    mutate(
      spanning_new = ifelse(is.na(hrc_spanning), spanning, toupper(hrc_spanning))
    ) %>%
    distinct(var_start_name = spanning, var_end_name = spanning_new, table_name)

  df_spannings <- df_metadata_long %>%
    mutate(
      spanning = ifelse(is.na(hrc_spanning), spanning, toupper(hrc_spanning)),
      indicator = ifelse(indicator %in% indic_not_in_eq & !is.na(hrc_indicator),
                         toupper(sub("hrc_", "", hrc_indicator)), indicator),
      hrc_indicator = ifelse(indicator %in% unique(equations_long$var), NA, hrc_indicator)
    )

  df_spannings_eq <- df_spannings %>%
    mutate(across(dplyr::where(is.character), ~ gsub("[^[:alnum:]_]", "", .))) %>%
    left_join(equations_long_full, by = c("indicator" = "var"))

  df_with_group <- df_spannings_eq %>% filter(!is.na(group))
  df_without_group <- df_spannings_eq %>% filter(is.na(group))

  if (nrow(df_with_group) == 0) {
    warning(
    "Check the coherence of `df_eq_indicator` and `df_metadata`.
    There is no table description in `df_metadata` with an indicator that is part of one of the equations provided in `df_eq_indicator`.
    `df_eq_indicator` is useless here and will be ignored.")
    if (nrow(df_without_group) > 0) {
      if (all(is.na(df_without_group$hrc_indicator))) {
        return(list(df_without_group, df_variable_info))
      } else {
        df_no_eq_indicators <- build_spanning_based_on_hrc_indicator(df_without_group,df_spannings)
        return(list(df_no_eq_indicators, df_variable_info))
      }
    }
  }

  spanning_combination_group <- df_with_group |>
    group_by(group, table_name) |>
    summarise(
      spanning = list(sort(unique(spanning))),
      side = first(side),
      .groups = "drop"
    ) |>
    group_by(group) |>
    mutate(
      all_sides = list(sort(unique(side))),
      spanning_key = purrr::map_chr(spanning, paste, collapse = "|")
    ) |>
    ungroup()

  spanning_combination_group <- spanning_combination_group |>
    distinct(group, spanning_key, spanning, all_sides) |>
    group_by(group, spanning_key) |>
    summarise(
      spanning = list(spanning[[1]]),
      all_sides = list(all_sides[[1]]),
      .groups = "drop"
    ) |>
    mutate(
      covered_sides = purrr::map2(spanning, group, function(span_set, grp) {
        spanning_combination_group |>
          filter(group == grp) |>
          filter(purrr::map_lgl(spanning, ~ all(span_set %in% .x))) |>
          pull(side) |>
          sort() |>
          unique()
      }),
      sides_manquants = purrr::map2(all_sides, covered_sides, setdiff),
      all_combinations = purrr::map_lgl(sides_manquants, ~ length(.x) == 0)
    ) |>
    unnest_wider(spanning, names_sep = "_")

  list_groups <- split(df_with_group, df_with_group$group)

  df_eq_initial_spannings <- purrr::map(list_groups, function(df_group) {
    regroup_tables(df_group, spanning_combination_group)
  }) |>
    purrr::compact() |>
    dplyr::bind_rows()

  table_group_mapping <- df_eq_initial_spannings %>%
    mutate(table_name_combined = table_name) %>%
    tidyr::separate_rows(table_name, sep = "\\.") %>%
    select(table_name, table_name_combined, group)

  totcode_equation <- df_with_group %>%
    filter(side == "total") %>%
    group_by(group) %>%
    summarise(totcode = first(var_mapped), .groups = "drop")

  df_eq_indicator_spannings <- df_with_group %>%
    filter(!is.na(eq_name)) %>%
    left_join(table_group_mapping, by = c("table_name", "group")) %>%
    left_join(totcode_equation, by = "group") %>%
    group_by(group, table_name_combined) %>%
    summarise(
      table_name = first(table_name_combined),
      field = last(field),
      hrc_field = last(hrc_field),
      spanning = if (length(unique(eq_name)) > 1) {
        paste0(paste0(unique(toupper(eq_name)), collapse = "_"), "^h")
      } else {
        paste0(toupper(last(eq_name)), "^h")
      },
      hrc_spanning  = if (length(unique(eq_name)) > 1) {
        paste0("hrc_", paste0(unique(toupper(eq_name)), collapse = "_"), ".totcode.", first(totcode))
      } else {
        paste0("hrc_", toupper(last(eq_name)), ".totcode.", first(totcode))
      },
      indicator = last(unit),
      hrc_indicator = last(hrc_indicator),
      .groups = "drop"
    ) %>%
    dplyr::distinct(group, table_name, spanning, hrc_spanning, .keep_all = TRUE)

  df_indicators <- bind_rows(df_eq_initial_spannings, df_eq_indicator_spannings) %>%
    select(table_name, field, hrc_field, indicator, hrc_indicator, spanning, hrc_spanning, group) %>%
    mutate(table_name = paste(table_name, "group", group, sep = "_")) %>%
    select(-group) %>%
    unique() %>%
    arrange(table_name)

  df_initial_indicator <- bind_rows(df_eq_initial_spannings, df_eq_indicator_spannings) %>%
    mutate(table_name = paste(table_name, "group", group, sep = "_")) %>%
    group_by(table_name) %>%
    summarise(initial_indicator = first(na.omit(initial_indicator)), .groups = "drop")

  df_indicators <- df_indicators %>%
    left_join(df_initial_indicator, by = "table_name")

  # Tables without group (i.e. withtout group)
  if (nrow(df_without_group) > 0) {
    if (all(is.na(df_without_group$hrc_indicator))) {
      df_indicators <- bind_rows(df_indicators, df_without_group) %>% arrange(table_name)
      return(list(df_indicators, df_variable_info))
    } else {
      df_no_eq_indicators <- build_spanning_based_on_hrc_indicator(df_without_group,df_spannings)
      df_indicators <- bind_rows(df_indicators, df_no_eq_indicators) %>% arrange(table_name)
      return(list(df_indicators, df_variable_info))
    }
  } else {
    return(list(df_indicators, df_variable_info))
  }
}

#' Build a data frame of indicators without equations
#'
#' Internal helper that aggregates rows from \code{df_without_group} that have
#' a non-\code{NA} \code{hrc_indicator}, and appends them to \code{df_spannings}.
#' The result is used to represent response variables that are linked by a
#' hierarchy but not by any equation.
#'
#' @param df_without_group A data frame containing the rows of
#'   \code{df_spannings_eq} that do not belong to any equation group
#'   (\code{group} is \code{NA}). Must contain the following columns:
#'   \code{table_name}, \code{field}, \code{hrc_field}, \code{indicator},
#'   and \code{hrc_indicator}.
#' @param df_spannings A data frame derived from \code{df_metadata_long} with
#'   renamed spanning and indicator variables. It is used as the base to which
#'   the newly built rows are appended via \code{bind_rows}.
#'
#' @return A data frame with one row per \code{table_name} for the non-\code{NA}
#'   \code{hrc_indicator} rows, appended to \code{df_spannings} and sorted by
#'   \code{table_name}. The returned columns are:
#'   \describe{
#'     \item{table_name}{Name of the table.}
#'     \item{field}{Last value of \code{field} within the group.}
#'     \item{hrc_field}{Last value of \code{hrc_field} within the group.}
#'     \item{spanning}{Uppercase \code{hrc_indicator} suffixed with \code{^h}.}
#'     \item{hrc_spanning}{Last value of \code{hrc_indicator} within the group.}
#'     \item{indicator}{Last value of \code{indicator} within the group.}
#'     \item{hrc_indicator}{Last value of \code{hrc_indicator} within the group.}
#'   }
#'
#' @keywords internal
build_spanning_based_on_hrc_indicator <- function(df_without_group, df_spannings) {
  df_without_group %>%
    filter(!is.na(hrc_indicator)) %>%
    dplyr::group_by(table_name) %>%
    summarise(
      field = last(field),
      hrc_field = last(hrc_field),
      spanning = paste0(toupper(last(hrc_indicator)), "^h"),
      hrc_spanning = last(hrc_indicator),
      indicator = last(indicator),
      hrc_indicator = last(hrc_indicator)
    ) %>%
    bind_rows(df_spannings, .) %>%
    arrange(table_name)
}


#' Regroup tables within a group (i.e. equation / group of linked equations)
#' based on spanning combination completeness
#'
#' @description
#' For a given group of tables, this function identifies which tables cover all
#' sides of an equation (total, rhs1, rhs2, ...) for their spanning combination,
#' and which do not. Tables with complete combinations are merged into a single
#' row; tables with incomplete combinations are kept as standalone rows with
#' their original spannings.
#'
#' @param df_group A tibble containing the rows of a single group from
#'   \code{df_with_group}. Must contain columns: \code{table_name},
#'   \code{spanning}, \code{side}, \code{var_mapped}, \code{indicator},
#'   \code{unit}, and \code{group}.
#' @param spanning_combination_group A tibble produced by the
#'   \code{spanning_combination_group} pipeline, containing one row per
#'   (group, spanning_key) combination. Must contain columns: \code{group},
#'   \code{spanning_key}, and \code{all_combinations} (logical indicating
#'   whether the spanning combination covers all sides of the equation).
#'
#' @return A tibble with one row per (merged or solo) table cluster and
#'   spanning, containing the following columns (among others):
#'   \describe{
#'     \item{table_name}{Dot-separated list of merged table names (e.g.
#'       \code{"T7.T9.T11"}) for complete combinations, or the original
#'       table name for incomplete ones.}
#'     \item{indicator}{The unit value shared across the merged tables.}
#'     \item{initial_indicator}{The \code{var_mapped} value of the \code{total}
#'       side, used to track the original indicator before merging.}
#'   }
#'
#' @examples
#' \dontrun{
#' list_groups <- split(df_with_group, df_with_group$group)
#'
#' df_eq_initial_spannings <- purrr::map(list_groups, function(df_group) {
#'   regroup_tables(df_group, spanning_combination_group)
#' }) |>
#'   purrr::compact() |>
#'   dplyr::bind_rows()
#' }
regroup_tables <- function(df_group, spanning_combination_group) {
  current_group <- unique(df_group$group)

  spanning_by_table <- df_group |>
    group_by(table_name) |>
    summarise(spanning_key = paste(sort(unique(spanning)), collapse = "|"), .groups = "drop")

  span_comb <- spanning_combination_group |>
    filter(group == current_group) |>
    select(spanning_key, all_combinations)

  spanning_by_table <- spanning_by_table |> left_join(span_comb, by = "spanning_key")

  tables_complete <- spanning_by_table |> filter(all_combinations)  |> pull(table_name)
  tables_incomplete <- spanning_by_table |> filter(!all_combinations) |> pull(table_name)

  df_merged <- if (length(tables_complete) > 0) {
    df_group |>
      filter(table_name %in% tables_complete) |>
      left_join(spanning_by_table |> select(table_name, spanning_key), by = "table_name") |>
      group_by(across(-c(table_name, side, var_mapped, indicator))) |>
      summarise(
        table_name = paste(sort(unique(table_name)), collapse = "."),
        indicator = last(unit),
        initial_indicator = var_mapped[side == "total"][1],
        .groups = "drop"
      ) |>
      select(-spanning_key)
  }

  df_solo <- if (length(tables_incomplete) > 0) {
    df_group |>
      filter(table_name %in% tables_incomplete) |>
      mutate(
        initial_indicator = var_mapped[side == "total"][1],
        indicator = unit
      ) |>
      select(-c(side, var_mapped))
  }

  bind_rows(df_merged, df_solo) |>
    arrange(table_name, spanning) |>
    select(table_name, field, hrc_field, indicator, everything())
}

