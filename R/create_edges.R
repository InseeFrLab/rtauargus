#' Create a Data Frame of Table Inclusion Relationships
#'
#' This function analyzes a list of data frames (clusters of tables) and identifies
#' inclusion relationships between tables. A table is considered to include another
#' if all its spanning variables (columns defining the structure of the table) are
#' contained within the spanning variables of the other table.
#'
#' For example, consider two tables to be published:
#' - **T1**: `company_turnover = {nuts x size}`
#' - **T2**: `company_turnover = {nuts x size x pollution}`
#'
#' All the information in **T1** is included in **T2**. By protecting **T2**, all
#' cells in **T1** will also be protected. This function identifies such inclusion
#' relationships and outputs a data frame that describes these links.
#'
#' @param list_split A list of clusters of tables, where each cluster contains
#'   nested data frames. Typically, this is the output of the `split_in_clusters`
#'   function.
#'
#' @return A list of data frames (`list_desc_links`), where each data frame describes
#'   the inclusion relationships (`from` and `to`) within a cluster of tables. Each
#'   row in a data frame indicates that the table in the `from` column is fully
#'   included in the table in the `to` column.
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
#' # Identify inclusion relationships between tables
#' list_desc_links <- create_edges(list_split)
#'
#' # View the structure of the result
#' str(list_desc_links)
#' }
#'
#' @importFrom tidyr nest
#' @importFrom purrr discard
#' @importFrom dplyr group_by
#'
create_edges <- function(list_split){
  nested_crois <- list_split %>%
    map(function(tab){tab %>% dplyr::group_by(table_name) %>% tidyr::nest()})
  nested_crois <- nested_crois %>% map(function(big_tibble){
    big_tibble %>%
      mutate(spanning = map(data, function(small_tibble){
        small_tibble$spanning}))
  })
  list_desc_links <- nested_crois %>% map(function(big_tibble) {
    # Condition for clusters that only have one table
    if(length(big_tibble$table_name) > 1) {
      spannings_nom_tab <- combn(big_tibble$table_name, 2, FUN = list)
      tab_to_keep <- map(spannings_nom_tab, function(crois) {
        # Extract the first table (Ta) corresponding to the first table of the
        # pair of tables we are studying
        Ta <- big_tibble %>% filter(table_name == crois[[1]])
        # Get the 'spanning' column for Ta
        crois_Ta <- Ta$spanning[[1]]

        # Extract the first table (Tb) corresponding to the second table of the
        # pair of tables we are studying
        Tb <- big_tibble %>% filter(table_name == crois[[2]])
        # Get the 'spanning' column for Tb
        crois_Tb <- Tb$spanning[[1]]

        # Create an empty data frame to store origin → destination relationships
        df_origin_dest <- data.frame(from = character(), to = character(), stringsAsFactors = FALSE)

        # Check if all elements of Ta are included in Tb
        if(all(crois_Ta %in% crois_Tb)) {
          # Add a row indicating that Ta can go to Tb
          df_origin_dest <- rbind(df_origin_dest,
                                  data.frame(from = crois[[1]], to = crois[[2]], stringsAsFactors = FALSE))
        }

        # Check if all elements of Tb are included in Ta
        if(all(crois_Tb %in% crois_Ta)) {
          # Add a row indicating that Tb can go to Ta
          df_origin_dest <- rbind(df_origin_dest,
                                  data.frame(from = crois[[2]], to = crois[[1]], stringsAsFactors = FALSE))
        }

        # Return the data frame containing possible links
        return(df_origin_dest)
      })

      # Filter non empty tables and combine them
      tab_to_keep_compact <- tab_to_keep %>%
        purrr::discard(is.null) %>%
        bind_rows()
      # Returns the compact table or NULL if empty
      if(nrow(tab_to_keep_compact) > 0) {
        return(tab_to_keep_compact)
      } else {
        return(NULL)
      }
    } else {
      return(NULL)
    }
  })
  return(list_desc_links)
}
