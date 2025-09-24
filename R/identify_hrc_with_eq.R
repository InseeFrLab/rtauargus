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
#'
#' list_hrc_identified <- identify_hrc(metadata_pizza_lettuce_long)
#'
#' str(list_hrc_identified)
#' }
#'
identify_hrc_with_eq <- function(df_metadata_long,df_eq_indicator){
  # check that the input is in the right format: right column names
  check_column_names <- function(df) {
    # Expected fixed column names
    fixed_columns <- c("eq_name", "eq_indicator", "unit")

    # Check that the fixed columns exist
    if (!all(fixed_columns %in% names(df))) {
      stop("Error: The dataframe describing the equations between indicators is
           missing one or more required columns: eq_name, eq_indicator, unit.")
    }}
  check_column_names(df_eq_indicator)

  # browser()
  # parse the equations
  parsed_equations <- df_eq_indicator %>%
    tidyr::separate(eq_indicator, into = c("total", "rhs"), sep = "=", extra = "merge") %>%
    dplyr::mutate(rhs = stringr::str_trim(rhs)) %>%
    tidyr::separate_rows(rhs, sep = "\\+") %>%
    dplyr::mutate(rhs = stringr::str_trim(rhs)) %>%
    dplyr::group_by(dplyr::across(-rhs)) %>%
    dplyr::mutate(term_number = paste0("rhs", dplyr::row_number())) %>%
    tidyr::pivot_wider(names_from = term_number, values_from = rhs) %>%
    dplyr::ungroup() %>%
    dplyr::select(eq_name,unit,total,everything())

  # change to long format in order to join with df_metadata_long
  equations_long <- parsed_equations %>%
    mutate(across(c(total, starts_with("rhs")), trimws)) %>%
    tidyr::pivot_longer(
      cols = c(total, starts_with("rhs")),
      names_to = "side",   # côté équation (total / rhs1 / rhs2...)
      values_to = "var"
    ) %>%
    filter(!is.na(var))

  df_spannings <- df_metadata_long %>%
    mutate(spanning_old = spanning) %>%
    mutate(spanning = ifelse(is.na(hrc_spanning),
                             spanning,
                             toupper(hrc_spanning))) %>%
    mutate(indicator = ifelse(is.na(hrc_indicator),
                              indicator,
                              toupper(sub("hrc_","",hrc_indicator))))
  df_variable_info <- data.frame(
    var_start_name = df_spannings$spanning_old,
    var_end_name = df_spannings$spanning,
    table_name = df_spannings$table_name
  ) %>% unique()
  df_spannings <- df_spannings %>% select(-spanning_old)

  # df_indicators <- df_spannings %>%
  #   # delete all the non-word elements, specifically for the white spaces
  #   mutate(across(where(is.character), ~ gsub("[^[:alnum:]_]", "", .))) %>%
  #   left_join(equations_long, by = c("indicator" = "var")) %>%
  #   filter(!is.na(eq_name)) %>%
  #   dplyr::group_by(table_name) %>%
  #   summarise(
  #     field = last(field),
  #     hrc_field = last(hrc_field),
  #     spanning = paste0(toupper(last(eq_name)),"^h"),
  #     hrc_spanning = paste0("hrc_",last(eq_name)),
  #     indicator = last(unit),
  #     hrc_indicator = last(hrc_indicator)
  #   ) %>%
  #   bind_rows(df_spannings, .) %>%
  #   group_by(table_name) %>%
  #   mutate(indicator = last(indicator)) %>%
  #   ungroup() %>%
  #   arrange(table_name)

  df_spannings_eq <- df_spannings %>%
    # delete all the non-word elements, specifically for the white spaces
    mutate(across(where(is.character), ~ gsub("[^[:alnum:]_]", "", .))) %>%
    left_join(equations_long, by = c("indicator" = "var"))

  df_initial_spannings <- df_spannings_eq %>%
    filter(!is.na(eq_name)) %>%
    group_by(eq_name) %>%
    summarise(
      table_name = paste(table_name, collapse = "."),
      field = last(field),
      hrc_field = last(hrc_field),
      spanning = last(spanning),
      hrc_spanning = last(hrc_spanning),
      indicator = last(unit),
      hrc_indicator = last(hrc_indicator),
      .groups = "drop"
    ) %>%
    select(-eq_name)

  df_indicator_spannings <- df_spannings_eq %>%
    filter(!is.na(eq_name)) %>%
    group_by(eq_name) %>%
    summarise(
      table_name = paste(table_name, collapse = "."),
      field = last(field),
      hrc_field = last(hrc_field),
      spanning = paste0(toupper(last(eq_name)), "^h"),
      hrc_spanning = paste0("hrc_", last(eq_name)),
      indicator = last(unit),
      hrc_indicator = last(hrc_indicator),
      .groups = "drop"
    ) %>%
    select(-eq_name)

  # browser()
  df_indicators <- bind_rows(df_initial_spannings,df_indicator_spannings) %>%
    select(table_name,field,hrc_field,indicator,hrc_indicator,everything()) %>%
    arrange(table_name)

  list_hrc_identified = list(df_indicators,df_variable_info)
  return(list_hrc_identified)
}
