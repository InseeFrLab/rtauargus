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

  # change to long format in order to join with df_metadata_long
  equations_long <- parsed_equations %>%
    mutate(across(c(total, starts_with("rhs")), trimws)) %>%
    tidyr::pivot_longer(
      cols = c(total, starts_with("rhs")),
      names_to = "side",   # côté équation (total / rhs1 / rhs2...)
      values_to = "var"
    ) %>%
    filter(!is.na(var))

  # Identify chained equations (A = B + C, B = D + E → group both equations together)

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

  # Compter le nombre de fois qu'une variable apparaît à gauche
  total_counts <- parsed_equations %>%
    dplyr::count(total, name = "n_total")

  # Identifier les "totals" ambigus (définis plusieurs fois)
  ambiguous_totals <- total_counts %>%
    dplyr::filter(n_total > 1) %>%
    dplyr::pull(total)

  # Ne garder que les liens dont le total n’est pas ambigu
  links_filtered <- links %>%
    dplyr::filter(!total %in% ambiguous_totals)

  # Créer un graphe uniquement avec les liens non ambigus
  g <- igraph::graph_from_data_frame(links_filtered %>% select(total,rhs), directed = TRUE)

  # Trouver les composantes connexes (chaînes d’équations cohérentes)
  comp <- igraph::components(g)$membership
  comp_df <- data.frame(var = names(comp), group = comp, stringsAsFactors = FALSE)

  # Affecter les groupes aux équations
  equations_long <- equations_long %>%
    dplyr::left_join(comp_df, by = c("var" = "var"))

  # Pour les équations dont le total est ambigu,
  # on leur donne un nouveau groupe unique PAR ÉQUATION
  if (length(ambiguous_totals) > 0) {
    max_group <- ifelse(length(comp_df$group) == 0, 0, max(comp_df$group, na.rm = TRUE))

    # Extraire les équations dont le total est ambigu
    ambiguous_eqs <- equations_long %>%
      dplyr::filter(side == "total", var %in% ambiguous_totals) %>%
      dplyr::distinct(eq_name, var) %>%
      dplyr::mutate(group = seq(max_group + 1, max_group + dplyr::n()))

    # Rejoindre ces nouveaux groupes à toutes les lignes de la même équation
    equations_long <- equations_long %>%
      dplyr::left_join(ambiguous_eqs %>% dplyr::select(eq_name, group),
                       by = "eq_name",
                       suffix = c("", "_ambig")) %>%
      dplyr::mutate(group = dplyr::coalesce(group_ambig, group)) %>%
      dplyr::select(-group_ambig)
  }

  # 'df_spannings' is a modified version of 'df_metadata_long' where:
  #   - 'spanning' is replaced by its uppercase hierarchical version if available,
  #   - 'indicator' is replaced by its uppercase hierarchical version
  #   (without the 'hrc_' prefix) if available.
  df_spannings <- df_metadata_long %>%
    mutate(spanning_old = spanning) %>%
    mutate(spanning = ifelse(is.na(hrc_spanning),
                             spanning,
                             toupper(hrc_spanning))) %>%
    mutate(indicator = ifelse(is.na(hrc_indicator),
                              indicator,
                              toupper(sub("hrc_","",hrc_indicator))))

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
    mutate(across(where(is.character), ~ gsub("[^[:alnum:]_]", "", .))) %>%
    left_join(equations_long, by = c("indicator" = "var"))

  # 'df_eq_initial_spannings' contains the initial spanning information
  # for equations (rows where 'eq_name' is not missing), summarised by equation name.
  # Each equation keeps the last relevant field values, with concatenated table names.
  df_eq_initial_spannings <- df_spannings_eq %>%
    filter(!is.na(eq_name)) %>%
    group_by(group) %>%
    summarise(
      table_name = paste(unique(table_name), collapse = "."),
      field = last(field),
      hrc_field = last(hrc_field),
      spanning = last(spanning),
      hrc_spanning = last(hrc_spanning),
      indicator = last(unit),
      hrc_indicator = last(hrc_indicator),
      .groups = "drop"
    )

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
        paste0("hrc_", paste0(unique(toupper(eq_name)), collapse = "_"))
      } else {
        paste0("hrc_", toupper(last(eq_name)))
      },
      indicator = last(unit),
      hrc_indicator = last(hrc_indicator),
      .groups = "drop"
    )

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
