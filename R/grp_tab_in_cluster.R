#' Regroup Tables Inside Clusters Based on Inclusion Relationships
#'
#' This function processes clusters of tables and regroups tables that are included
#' in each other within each cluster. It leverages the relationships identified by
#' `grp_tab_names()` and uses the grouped structure of tables from `list_split`.
#'
#' The resulting tibble contains updated table names (to align with the group mapping)
#' and metadata for each independent table in the cluster. Additionally, it tracks
#' which tables are included within each group.
#'
#' @param list_split A list of clusters of tables, where each cluster contains
#'   nested data frames. Typically, this is the output of the `split_in_clusters()` function.
#' @param list_translation_tables A list of results from `grp_tab_names()` containing
#'   inclusion relationships and mappings of table names to their respective groups.
#'
#' @return A list of tibbles (`big_tibble_eg`) for each cluster, where each tibble contains:
#'   - `table_name`: The updated table name based on grouping.
#'   - `data`: Nested data corresponding to the original table structure.
#'   - `spanning`: Columns that define the structure of each table.
#'   - `tab_inclus`: A list of original table names included within each grouped table.
#'
#' @export
#'
#' @examples
#' \dontrun{
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
#' # View structure of the results
#' str(list_independent_tables)
#' }
#'
#' @importFrom purrr map map2
#' @importFrom tidyr nest
#'
grp_tab_in_cluster <- function(list_split, list_translation_tables) {
  # Nest each cluster of tables by `table_name`
  nested_crois <- list_split %>%
    purrr::map(function(tab) { tab %>% dplyr::group_by(table_name) %>% tidyr::nest() })

  # Process each cluster using inclusion relationships
  purrr::map2(list_translation_tables, nested_crois, function(tab_to_keep, big_tibble) {
    if (!is.null(tab_to_keep)) {
      # Extract tables to keep
      vec_tab_to_keep <- as.vector(
        tab_to_keep[[1]] %>% select(to.eg) %>% unique() %>% unlist()
      )

      # Process the cluster
      big_tibble_eg <- big_tibble %>%
        mutate(
          spanning = map(data, function(small_tibble) { small_tibble$spanning })
        ) %>%
        dplyr::left_join(tab_to_keep[[2]], by = c("table_name" = "Original")) %>%
        mutate(table_eg = ifelse(is.na(Group), table_name, Group)) %>%
        dplyr::group_by(table_eg) %>% filter(row_number() == 1) %>%
        select(table_eg, data, spanning) %>%
        # Filter to retain only tables in `vec_tab_to_keep`
        filter(table_eg %in% vec_tab_to_keep) %>%
        mutate(
          tab_inclus = map(table_eg, function(table_eg) {
            as.vector(
              tab_to_keep[[1]] %>%
                filter(to.eg == table_eg) %>%
                select(from.eg) %>%
                unique() %>% unlist()
            )
          })
        ) %>% dplyr::rename(table_name = table_eg)

      return(big_tibble_eg)
    } else {
      # Handle clusters with no inclusion relationships
      big_tibble_eg <- big_tibble %>%
        mutate(
          spanning = map(data, function(small_tibble) { small_tibble$spanning }),
          tab_inclus = list(rep("", length(data)))
        )
      return(big_tibble_eg)
    }
  })
}
