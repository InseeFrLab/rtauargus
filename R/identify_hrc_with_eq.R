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

  # browser()

  # ---- 1) Identif. totaux ambigus ----
  total_counts <- parsed_equations %>% count(total, name = "n_total")
  ambiguous_totals <- total_counts %>% filter(n_total > 1) %>% pull(total)

  # ---- 2) Construire un mapping total -> total_alt par eq_name ----
  # pour toutes les équations (ambigües ou non) on crée une ligne ;
  # pour les non-ambigües total_alt == total
  alt_map <- parsed_equations %>%
    distinct(eq_name, total) %>%
    group_by(total) %>%
    arrange(eq_name) %>%                 # ordre stable
    mutate(alt_idx = row_number(),
           total_alt = case_when(
             n() == 1 ~ total,
             alt_idx == 1 ~ total,
             TRUE ~ paste0(total, "_alt", alt_idx - 1)
           )
    ) %>%
    ungroup() %>%
    select(eq_name, total, total_alt)

  # ---- 3) Appliquer le mapping aux liens ----
  # 'links' contient total, rhs, eq_name (si tu ne l'as pas, il faut le joindre)
  # ici j'assume links a colonne eq_name ; sinon faire left_join(links, parsed_equations %>% select(eq_name, total, rhs)...) auparavant
  links_full <- links %>%
    # remplacer le total par sa version alt spécifique à l'eq
    left_join(alt_map, by = c("eq_name", "total")) %>%
    mutate(total = coalesce(total_alt, total)) %>%
    select(-total_alt) %>%
    # maintenant, remplacer rhs s'il existe comme "total" dans alt_map :
    # on doit choisir la bonne total_alt pour le rhs selon l'équation où il joue le rôle de total.
    # pour cela on joint alt_map en faisant rhs -> total, et en gardant l'alt correspondant à l'eq_name de la ligne SOURCE.
    left_join(alt_map, by = c("eq_name", "rhs" = "total")) %>%
    mutate(rhs = coalesce(total_alt, rhs)) %>%
    select(total, rhs, eq_name) %>%
    distinct()

  # ---- 4) Construire le graphe complet (avec toutes les copies) ----
  g_full <- graph_from_data_frame(links_full %>% select(total, rhs), directed = TRUE)

  # ---- 5) calculer les composantes sur g_full ----
  comp_full <- components(g_full)$membership
  comp_df <- data.frame(var = names(comp_full), group = as.integer(comp_full), stringsAsFactors = FALSE)

  # ---- 6) Mettre à jour equations_long :
  #       associer le var alt (si present) et le groupe correspondant ----
  # Remarques :
  # - equations_long contient les variables originales (var) et eq_name ;
  # - on veut retrouver la version "var" ou "var_alt" utilisée dans g_full.
  equations_long_full <- equations_long %>%
    # joindre la correspondance eq_name + var(original total) -> total_alt (si existant)
    left_join(alt_map, by = c("eq_name", "var" = "total")) %>%
    mutate(var_mapped = coalesce(total_alt, var)) %>%
    select(-total_alt) %>%
    # joindre le groupe calculé sur le graphe complet
    left_join(comp_df, by = c("var_mapped" = "var")) %>%
    # si pour certains var_mapped il n'y a pas de group (isolés), on peut laisser NA ou donner un groupe unique
    mutate(group = as.integer(group))

  browser()
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
    left_join(equations_long_full, by = c("indicator" = "var"))

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
