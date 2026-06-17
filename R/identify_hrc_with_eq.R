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
    # Expected fixed column names
    fixed_columns <- c("eq_name", "eq_indicator", "unit")

    # Check that the fixed columns exist
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
    dplyr::mutate(rhs = trimws(rhs)) %>%
    tidyr::separate_rows(rhs, sep = "\\+") %>%
    dplyr::mutate(rhs = trimws(rhs),
                  total = trimws(total)) %>%
    dplyr::group_by(dplyr::across(-rhs)) %>%
    dplyr::mutate(term_number = paste0("rhs", dplyr::row_number())) %>%
    tidyr::pivot_wider(names_from = term_number, values_from = rhs) %>%
    dplyr::ungroup() %>%
    dplyr::select(eq_name, unit, total, everything())

  # Identify chained equations (A = B + C, B = D + E) and group equations together

  # Build dependency links between totals and rhs
  links <- parsed_equations %>%
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
    dplyr::distinct()

  # Identify ambiguous totals
  total_counts <- parsed_equations %>% dplyr::count(total, name = "n_total")
  ambiguous_totals <- total_counts %>% dplyr::filter(n_total > 1) %>% pull(total)

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

  # Apply the mapping to the links
  # 'links' contains total, rhs, eq_name (if not, it must be joined beforehand)
  # here we assume links has an eq_name column; otherwise do
  # left_join(links, parsed_equations %>% select(eq_name, total, rhs), ...) first
  links_full <- links %>%
    # replace total with its equation-specific alternative
    left_join(alt_map, by = c("eq_name", "total")) %>%
    mutate(total = dplyr::coalesce(total_alt, total)) %>%
    select(-total_alt) %>%
    # now replace rhs if it exists as a "total" in alt_map:
    # we must choose the correct total_alt for rhs according to the equation
    # where it plays the role of a total.
    # to do so, join alt_map by mapping rhs -> total, keeping the alt
    # corresponding to the SOURCE row eq_name.
    left_join(alt_map, by = c("eq_name", "rhs" = "total")) %>%
    mutate(rhs = dplyr::coalesce(total_alt, rhs)) %>%
    select(total, rhs, eq_name) %>%
    dplyr::distinct()

  # Build the full graph (including all copies)
  g_full <- graph_from_data_frame(links_full %>% select(total, rhs), directed = TRUE)

  # Compute components on g_full
  comp_full <- igraph::components(g_full)$membership
  comp_df <- data.frame(var = names(comp_full), group = as.integer(comp_full), stringsAsFactors = FALSE)

  ##############################################################################
  # browser() # use this combined with "./rtauargus/dev/graphes_equations_objet_browser.R"
  # to get the graphs showing indicators links based on the equations
  ##############################################################################

  # reformat parsed_equations in long format in order to join with df_metadata_long
  equations_long <- parsed_equations %>%
    mutate(across(c(total, starts_with("rhs")), trimws)) %>%
    tidyr::pivot_longer(
      cols = c(total, starts_with("rhs")),
      names_to = "side",   # côté équation (total / rhs1 / rhs2...)
      values_to = "var"
    ) %>%
    filter(!is.na(var))

  # Update equations_long:
  #       associate the alternative variable (if present) and the corresponding group
  # Notes:
  # - equations_long contains the original variables (var) and eq_name;
  # - we want to recover the "var" or "var_alt" version used in g_full.
  equations_long_full <- equations_long %>%
    # join the correspondence eq_name + var (original total) -> total_alt (if any)
    left_join(alt_map, by = c("eq_name", "var" = "total")) %>%
    mutate(var_mapped = dplyr::coalesce(total_alt, var)) %>%
    select(-total_alt) %>%
    # join the group computed on the full graph
    left_join(comp_df, by = c("var_mapped" = "var")) %>%
    # for var_mapped without a group (isolated), keep NA or assign a single group
    mutate(group = as.integer(group))

  # 'df_spannings' is a modified version of 'df_metadata_long' where:
  #   - 'spanning' is replaced by its uppercase hierarchical version if available,
  #   - 'indicator' is replaced by its uppercase hierarchical version
  #   (without the 'hrc_' prefix) if available and 'indicator' not part of 'df_eq_indicator'
  indic_not_in_eq <- setdiff(unique(df_metadata_long$indicator),unique(equations_long$var))

  df_spannings <- df_metadata_long %>%
    mutate(spanning_old = spanning,
           spanning = ifelse(is.na(hrc_spanning),
                             spanning,
                             toupper(hrc_spanning)),
           indicator = ifelse(indicator %in% indic_not_in_eq & !is.na(hrc_indicator),
                              toupper(sub("hrc_","",hrc_indicator)),
                              indicator),
           hrc_indicator = ifelse(indicator %in% unique(equations_long$var),
                                  NA,
                                  hrc_indicator))

  # 'df_variable_info' is a reference table linking original spanning names ('spanning_old')
  # to their transformed counterparts ('spanning'), along with the corresponding table name.
  df_variable_info <- data.frame(
    var_start_name = df_spannings$spanning_old,
    var_end_name = df_spannings$spanning,
    table_name = df_spannings$table_name
  ) %>% unique()

  # Update 'df_spannings' by removing the temporary 'spanning_old' column.
  df_spannings <- df_spannings %>% select(-spanning_old)

  df_spannings_eq <- df_spannings %>%
    # delete all the non-word elements, specifically for the white spaces
    mutate(across(dplyr::where(is.character), ~ gsub("[^[:alnum:]_]", "", .))) %>%
    left_join(equations_long_full, by = c("indicator" = "var"))

  ##############################################################################
  # Séparer les lignes avec et sans group
  df_with_group <- df_spannings_eq %>% filter(!is.na(group))
  df_without_group <- df_spannings_eq %>% filter(is.na(group))

  # ---- Traitement des lignes AVEC group (nouveau code) ----
  if(nrow(df_with_group) > 0){
    spanning_by_table <- df_with_group %>%
      distinct(table_name, spanning, group)

    common_spannings <- spanning_by_table %>%
      group_by(group, spanning) %>%
      summarise(
        tables_avec_ce_spanning = list(sort(unique(table_name))),
        n_tables = n(),
        .groups = "drop"
      )

    n_tables_par_group <- df_with_group %>%
      group_by(group) %>%
      summarise(n_total = n_distinct(table_name), .groups = "drop")

    common_spannings <- common_spannings %>%
      left_join(n_tables_par_group, by = "group")

    spanning_all <- common_spannings %>%
      filter(n_tables == n_total)

    spanning_extra <- common_spannings %>%
      filter(n_tables < n_total) %>%
      tidyr::unnest(tables_avec_ce_spanning) %>%
      rename(table_name = tables_avec_ce_spanning)

    df_groupe <- df_with_group %>%
      group_by(across(-c(table_name, side, var_mapped, indicator))) %>%
      summarise(
        table_name = paste(sort(unique(table_name)), collapse = "."),
        indicator  = last(unit),
        .groups    = "drop"
      ) %>%
      filter(spanning %in% spanning_all$spanning)

    tables_extra <- spanning_extra %>%
      distinct(table_name, group)

    df_solo <- df_with_group %>%
      semi_join(tables_extra, by = c("table_name", "group")) %>%
      mutate(
        initial_indicator = var_mapped[side == "total"][1],
        indicator = unit
      )

    df_eq_initial_spannings_with_group <- bind_rows(
      df_groupe,
      df_solo %>% select(-c(side, var_mapped))
    ) %>%
      arrange(group, table_name, spanning)

    # ---- Combinaison finale ----
    df_eq_initial_spannings <- bind_rows(
      df_eq_initial_spannings_no_group,
      df_eq_initial_spannings_with_group
    )
  }else{
    df_eq_initial_spannings <- df_eq_initial_spannings_no_group
  }

  ##############################################################################

  # 'df_eq_indicator_spannings' defines the spanning information for equation indicators.
  # Each equation name is transformed into its uppercase form with a "^h" suffix,
  # and its hierarchical version prefixed with "hrc_".
  df_eq_indicator_spannings <- df_spannings_eq %>%
    filter(!is.na(eq_name)) %>%
    group_by(group) %>%
    summarise(
      table_name = paste(unique(table_name), collapse = "."),
      field = last(field),
      hrc_field = last(hrc_field),
      spanning = if(length(unique(eq_name)) > 1) {
        paste0(paste0(unique(toupper(eq_name)), collapse = "_"), "^h")
      } else {
        paste0(toupper(last(eq_name)), "^h")
      },
      hrc_spanning = if(length(unique(eq_name)) > 1) {
        paste0("hrc_", paste0(unique(toupper(eq_name)), collapse = "_"),".totcode.",var_mapped[side == "total"][1])
      } else {
        paste0("hrc_", toupper(last(eq_name)),".totcode.",var_mapped[side == "total"][1])
      },
      indicator = last(unit),
      hrc_indicator = last(hrc_indicator),
      .groups = "drop"
    ) %>%
    dplyr::distinct(group, spanning, hrc_spanning, .keep_all = TRUE)

  # 'df_indicators' combines both initial and indicator spanning information
  # into a single harmonized dataset, keeping key structural columns
  # and sorting rows by table name.
  df_indicators <- bind_rows(df_eq_initial_spannings,df_eq_indicator_spannings) %>%
    select(table_name,field,hrc_field,indicator,hrc_indicator,everything()) %>%
    arrange(table_name)

  # 'df_no_eq_spannings' contains all spanning rows
  # that are not associated with any equation (eq_name is missing).
  df_no_eq_spannings <- df_spannings_eq %>% filter(is.na(eq_name))

  if(nrow(df_no_eq_spannings) > 0){
    if(all(is.na(df_no_eq_spannings$hrc_indicator))){
      df_indicators <- bind_rows(df_indicators,df_no_eq_spannings) %>% arrange(table_name)
      return(list(df_indicators,df_variable_info))
    } else {
      df_no_eq_indicators <- df_no_eq_spannings %>%
        filter(!is.na(hrc_indicator)) %>%
        dplyr::group_by(table_name) %>%
        summarise(
          field = last(field),
          hrc_field = last(hrc_field),
          spanning = paste0(toupper(last(hrc_indicator)),"^h"),
          hrc_spanning = last(hrc_indicator),
          indicator = last(indicator),
          hrc_indicator = last(hrc_indicator)
        ) %>%
        bind_rows(df_spannings, .) %>%
        arrange(table_name)
      df_indicators <- bind_rows(df_indicators,df_no_eq_indicators) %>% arrange(table_name)
      list_hrc_identified = list(df_indicators,df_variable_info)
      return(list_hrc_identified)
    }
  } else {
    list_hrc_identified = list(df_indicators,df_variable_info)
    return(list_hrc_identified)
  }
}
