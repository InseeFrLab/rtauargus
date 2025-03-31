#' Analyse Metadata of Tables Needing Secondary Tabular Data Protection
#'
#' This function analyzes a metadata dataframe to determine which tables should
#' be treated together in the same cluster.
#' It also rearranges and groups the tables based on hierarchical relationships,
#' creating a structured output for further processing.
#'
#' @param df_metadata A dataframe containing metadata in wide format.
#' @param verbose Logical. If `TRUE`, returns a detailed list of intermediate results
#' from each processing step. If `FALSE`, returns only the cluster assignments. Defaults to `FALSE`.
#'
#' @return A list or dataframe, depending on the value of the `verbose` parameter:
#' \itemize{
#'   \item If `verbose = TRUE`, returns a list with detailed intermediate results:
#'     \describe{
#'       \item{\code{identify_hrc}}{A data frame with renamed variables and grouped response variables.}
#'       \item{\code{info_var}}{A mapping of original variable names to their renamed counterparts.}
#'       \item{\code{split_in_clusters}}{A list of clusters obtained after splitting the data.}
#'       \item{\code{create_edges}}{A list of edges created for describing relationships.}
#'       \item{\code{grp_tab_names}}{Translation tables generated for renaming and regrouping.}
#'       \item{\code{grp_tab_in_clusters}}{Independent tables grouped by clusters.}
#'       \item{\code{tab_to_treat}}{Cluster assignments for tables to be treated.}
#'       \item{\code{df_tab_to_treat}}{A dataframe summarizing the tables and their clusters.}
#'     }
#'   \item If `verbose = FALSE`, returns only the cluster assignments (\code{tab_to_treat}).
#' }
#'
#' @details The function performs the following steps:
#' \itemize{
#'   \item Converts the metadata from wide format to long format using \code{wide_to_long}.
#'   \item Identifies hierarchical relationships and renames variables with \code{identify_hrc}.
#'   \item Splits hierarchical relationships into clusters using \code{split_in_clusters}.
#'   \item Creates edges to describe the relationships via \code{create_edges}.
#'   \item Generates translation tables for regrouping with \code{grp_tab_names}.
#'   \item Regroups tables into independent clusters with \code{grp_tab_in_cluster}.
#'   \item Identifies tables to be treated together using \code{tab_to_treat}.
#'   \item Produces a final dataframe summarizing the cluster assignments using \code{dataframe_result}.
#' }
#'
#' @examples
#' data(metadata_pizza_lettuce)
#'
#' # View the structure of the original data
#' str(metadata_pizza_lettuce)
#'
#' # Run the analysis
#' detailed_analysis <- analyse_metadata(metadata_pizza_lettuce, verbose = TRUE)
#'
#' # Simplified output (non-verbose)
#' cluster_id_dataframe <- analyse_metadata(metadata_pizza_lettuce, verbose = FALSE)
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @export
analyse_metadata <- function(df_metadata,verbose = FALSE){
  # check that the input is in the right format: right column names
  check_column_names <- function(df) {
    # Expected fixed column names
    fixed_columns <- c("table_name", "field", "hrc_field", "indicator", "hrc_indicator")

    # Check that the fixed columns exist
    if (!all(fixed_columns %in% names(df))) {
      stop("Error: The dataframe is missing one or more required columns: table_name, field, hrc_field, indicator, hrc_indicator.")
    }

    # Extract spanning_X and hrc_spanning_X columns
    spanning_columns <- grep("^spanning_[0-9]+$", names(df), value = TRUE)
    hrc_spanning_columns <- grep("^hrc_spanning_[0-9]+$", names(df), value = TRUE)

    # Extract the numbers from the column names (e.g., 1 from spanning_1)
    spanning_numbers <- gsub("^spanning_", "", spanning_columns)
    hrc_spanning_numbers <- gsub("^hrc_spanning_", "", hrc_spanning_columns)

    # Check if each spanning_X has a corresponding hrc_spanning_X
    for (num in spanning_numbers) {
      if (!(paste0("hrc_spanning_", num) %in% hrc_spanning_columns)) {
        stop(paste0("Error: Missing corresponding 'hrc_spanning_", num, "' for 'spanning_", num, "'"))
      }
    }

    # Check if each hrc_spanning_X has a corresponding spanning_X
    for (num in hrc_spanning_numbers) {
      if (!(paste0("spanning_", num) %in% spanning_columns)) {
        stop(paste0("Error: Missing corresponding 'spanning_", num, "' for 'hrc_spanning_", num, "'"))
      }
    }

    # Nothing to return if everything is valid
  }
  check_column_names(df_metadata)

  # check that each table is named
  if(any(is.na(df_metadata$table_name))){
    stop("Each table needs to be named. No NA values for the table_name column.")
  }

  # check that each table has a unique name
  if (anyDuplicated(df_metadata$table_name)) {
    stop("Duplicate values found in 'table_name' column!")
  }

  # start of the actual analysis
  df_metadata_long <- wide_to_long(df_metadata)
  list_hrc_identified <- identify_hrc(df_metadata_long)
  list_split <- split_in_clusters(list_hrc_identified)
  list_desc_links <- create_edges(list_split)
  list_translation_tables <- grp_tab_names(list_desc_links)
  list_independent_tables <- grp_tab_in_cluster(list_split = list_split,
                                                list_translation_tables = list_translation_tables)
  list_cluster_treat <- tab_to_treat(list_independent_tables)
  dataframe_cluster_id <- dataframe_result(list_cluster_treat)
  if(verbose){
    return(list(
      identify_hrc = list_hrc_identified[[1]],
      info_var = list_hrc_identified[[2]],
      split_in_clusters = list_split,
      create_edges = list_desc_links,
      grp_tab_names = list_translation_tables,
      grp_tab_in_clusters = list_independent_tables,
      tab_to_treat = list_cluster_treat,
      df_tab_to_treat = dataframe_cluster_id
    ))
  }else{
    return(dataframe_cluster_id)
  }
}
