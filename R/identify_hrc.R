#' Rename Variables Based on Their Hierarchies
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
identify_hrc <- function(df_metadata_long){
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
  if(all(is.na(df_spannings$hrc_indicator))){ # condition pour les hiérarchies sur les indicateurs
    df_indicators <- df_spannings
    return(list(df_indicators,df_variable_info))
  } else {
    df_indicators <- df_spannings %>%
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
    list_hrc_identified = list(df_indicators,df_variable_info)
    return(list_hrc_identified)
  }
}
