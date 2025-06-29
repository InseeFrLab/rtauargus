#' Unnest Data Frames to Create a Usable Flat Format
#'
#' This function processes nested data frames representing tables from hierarchical clusters,
#' and transforms them into a flat, easy-to-read format. Each column corresponds to a dplyr::distinct variable,
#' simplifying downstream analysis and use.
#'
#' @param list_independent_tables A list of nested tibbles, typically the output of
#'   `grp_tab_in_cluster()`, where each tibble represents independent tables grouped by clusters.
#'
#' @return A list of unnested tibbles, where each tibble contains the following columns:
#'   - `table_name`: The name of the table.
#'   - `field`: The field name associated with the table.
#'   - `indicator`: Indicators related to the table.
#'   - `spanning_*`: Columns derived from the spanning metadata (expanded into multiple columns).
#'   - `hrc_spanning_*`: Columns derived from hierarchical spanning metadata (expanded into multiple columns).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example data
#' data(metadata_pizza_lettuce)
#'
#' # Convert wide metadata to long format
#' metadata_pizza_lettuce_long <- wide_to_long(metadata_pizza_lettuce)
#'
#' # Identify hierarchical relationships
#' list_hrc_identified <- identify_hrc(metadata_pizza_lettuce_long)
#'
#' # Split tables into clusters
#' list_split <- split_in_clusters(list_hrc_identified)
#'
#' # Detect inclusion relationships
#' list_desc_links <- create_edges(list_split)
#'
#' # Group tables based on inclusion relationships
#' list_translation_tables <- grp_tab_names(list_desc_links)
#'
#' # Regroup tables within each cluster
#' list_independent_tables <- grp_tab_in_cluster(list_split, list_translation_tables)
#'
#' # Flatten the nested data for downstream use
#' list_tab_to_treat <- tab_to_treat(list_independent_tables)
#'
#' # View structure of the results
#' str(list_tab_to_treat)
#' }
#'
#' @importFrom tidyr unnest
#' @importFrom tidyr unnest_wider
#' @importFrom dplyr distinct
tab_to_treat <- function(list_independent_tables) {
  # Process each tibble in the list
  list_independent_tables %>% purrr::map(function(big_tibble) {
    # Remove duplicate rows within each nested `data` field
    big_tibble <- big_tibble %>%
      mutate(data = map(data, ~ dplyr::distinct(.x)))

    # Extract key fields and indicators, ensuring consistent values across rows
    big_tibble <- big_tibble %>%
      mutate(
        field = map(data, function(data) { data$field[[1]] }),
        indicator = map(data, function(data) { data$indicator[[1]] }),
        hrc_spanning = map(data, function(data) { data$hrc_spanning })
      )

    # Select and unnest the data into a flat structure
    big_tibble %>%
      select(-c(data, tab_inclus)) %>%
      tidyr::unnest(c(field, indicator)) %>%
      select(table_name, field, indicator, spanning, hrc_spanning) %>%
      tidyr::unnest_wider(spanning, names_sep = "_") %>%
      tidyr::unnest_wider(hrc_spanning, names_sep = "_") %>%
      arrange(table_name)
  })
}


#' Combine List of Dataframes into a Single Dataframe with Cluster Identification
#'
#' This function consolidates a list of dataframes, each representing a cluster of tables,
#' into a single dataframe. It includes an additional column, `cluster`, to identify
#' the cluster each table belongs to.
#'
#' @param list_independent_tables A list of tibbles, typically the output of
#'   `grp_tab_in_cluster()` or `tab_to_treat()`. Each tibble contains metadata
#'   for tables grouped within a specific cluster.
#'
#' @return A single dataframe (`dfMetadata_to_treat`) with the following structure:
#'   - `cluster`: Identifier for the cluster each table belongs to.
#'   - `table_name`: The name of the table.
#'   - `field`: The field name associated with the table.
#'   - `indicator`: Indicators related to the table.
#'   - `spanning_*`: Columns derived from the spanning metadata, ordered by numeric suffix.
#'   - `hrc_spanning_*`: Columns derived from hierarchical spanning metadata, ordered by numeric suffix.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example data
#' data(metadata_pizza_lettuce)
#'
#' # Convert wide metadata to long format
#' metadata_pizza_lettuce_long <- wide_to_long(metadata_pizza_lettuce)
#'
#' # Identify hierarchical relationships
#' list_hrc_identified <- identify_hrc(metadata_pizza_lettuce_long)
#'
#' # Split tables into clusters
#' list_split <- split_in_clusters(list_hrc_identified)
#'
#' # Detect inclusion relationships
#' list_desc_links <- create_edges(list_split)
#'
#' # Group tables based on inclusion relationships
#' list_translation_tables <- grp_tab_names(list_desc_links)
#'
#' # Regroup tables within each cluster
#' list_independent_tables <- grp_tab_in_cluster(list_split, list_translation_tables)
#'
#' # Flatten the nested data for downstream use
#' list_tab_to_treat <- tab_to_treat(list_independent_tables)
#'
#' # Create a dataframe with a cluster id
#' dataframe_cluster_id <- dataframe_result(list_tab_to_treat)
#'
#' # View the result dataframe
#' dataframe_cluster_id
#' }
#'
#' @importFrom purrr imap_dfr
dataframe_result <- function(list_independent_tables) {
  # TODO modifier car il y a une erreur (column field doesn't exist)
  # Combine the list of tibbles into a single dataframe with cluster identifiers
  dataframe_metadata <- purrr::imap_dfr(list_independent_tables, function(tibble, tibble_name) {
    tibble %>% mutate(cluster = tibble_name)
  }) %>%
    select(
      cluster,
      table_name,
      field,
      indicator,
      # Dynamically order columns spanning_xxx by their numeric suffix
      all_of(names(.)[grepl("^spanning_\\d+$", names(.))] %>%
               .[order(as.numeric(sub("spanning_", "", .)))]),
      # Dynamically order columns hrc_spanning_xxx by their numeric suffix
      all_of(names(.)[grepl("^hrc_spanning_\\d+$", names(.))] %>%
               .[order(as.numeric(sub("hrc_spanning_", "", .)))])
    ) %>% as.data.frame()
}


