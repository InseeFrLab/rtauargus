#' Split a Data Frame Based on a Chosen Variable
#'
#' This function splits a data frame into a list of smaller data frames based on
#' a specified variable (`var`) and its associated hierarchical variable (`hrc_var`).
#' The function handles cases where `hrc_var` contains character values or is missing,
#' ensuring that independent and hierarchical groupings are managed appropriately.
#'
#' @param tab A data frame to be split.
#' @param var The variable to base the split on.
#' @param hrc_var The hierarchical variable associated with `var`. If `hrc_var`
#'   is present, the data frame will first be split by its values; otherwise,
#'   it will be split directly by `var`.
#'
#' @return A list of data frames (`list_diff`), where each element corresponds
#'   to a grouping determined by `var` and/or `hrc_var`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example data
#' tab <- data.frame(
#'   id = 1:6,
#'   var = c("A", "A", "B", "B", "C", "C"),
#'   hrc_var = c(NA, "H1", "H1", "H2", NA, NA),
#'   value = c(10, 20, 30, 40, 50, 60)
#' )
#'
#' # Split by 'var' and 'hrc_var'
#' list_diff <- split_dataframe(tab, "var", "hrc_var")
#'
#' # View the structure of the result
#' str(list_diff)
#' }
split_dataframe <- function(tab,var,hrc_var){
  if(is.character(tab[[hrc_var]])){
    list_linked_tab <- split(tab, f = tab[[hrc_var]])
    tab_hrc_var_na <- tab[is.na(tab[[hrc_var]]),]
    tab_hrc_var_na[[hrc_var]] <- as.logical(tab_hrc_var_na[[hrc_var]])
    list_indep_tab <- split(tab_hrc_var_na,
                            f = tab_hrc_var_na[[var]])
    list_diff <- c(list_linked_tab,list_indep_tab)
  } else {list_diff <- split(tab, f = tab[[var]])}
  return(list_diff)
}


#' Split a Data Frame into Clusters of Linked Tables
#'
#' This function splits a data frame into a list of data frames (or clusters), where
#' each cluster represents a group of linked tables. Tables are grouped based on the
#' `field` variable and their corresponding `indicator` values. The hierarchical
#' relationships specified in the `hrc_*` columns are also considered.
#'
#' The function handles cases where the `field` variable is constant across the data
#' frame or when `field` varies, in which case the data frame is split by `field` and
#' further split by `indicator` and `hrc_indicator`.
#'
#' @param list_hrc_identified A list returned by the `identify_hrc` function. The first
#'   element of the list must be a data frame containing the variables:
#'   - `field`: A grouping variable.
#'   - `hrc_field`: The hierarchical counterpart of `field`.
#'   - `indicator`: A variable used to link tables.
#'   - `hrc_indicator`: The hierarchical counterpart of `indicator`.
#'
#' @return A named list of data frames. Each element of the list corresponds to a
#'   cluster of linked tables, and the names of the list elements reflect the
#'   hierarchical grouping path.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data(metadata_pizza_lettuce)
#'
#' metadata_pizza_lettuce_long <- wide_to_long(metadata_pizza_lettuce)
#' # Identify hierarchical relationships
#' list_hrc_identified <- identify_hrc(metadata_pizza_lettuce_long)
#' # Split into clusters
#' list_split <- split_in_clusters(list_hrc_identified)
#'
#' # View the structure of the result
#' str(list_split)
#' }
#'
split_in_clusters <- function(list_hrc_identified){
  df_hrc_identified <- list_hrc_identified[[1]]
  # Handle cases where the 'field' variable is constant
  if(all(is.na(df_hrc_identified$field))){
    field <- list(df_hrc_identified)
    cat("analyse à field constant\n")
  } else {
    # Otherwise, split by 'field"
    field <- split_dataframe(df_hrc_identified,"field","hrc_field")
  }
  # Function to split nested lists by indicator variables
  split_all_tibbles <- function(nested_list, var, hrc_var) {
    result <- lapply(nested_list, function(inner_list) {
      if (is.list(inner_list)  && !is.data.frame(inner_list)) {
        lapply(inner_list, function(tab) split_dataframe(tab, var, hrc_var))
      } else {
        split_dataframe(inner_list, var, hrc_var)
      }
    })
    return(result)
  }
  # Applying the function to field_zonage
  split_field <- split_all_tibbles(field, "indicator", "hrc_indicator")
  flatten_list <- function(lst, path = "") {
    results <- list()

    if (!is.list(lst)) {
      stop("Input must be a list")
    }

    if(is.null(names(lst))){
      results <- purrr::flatten(lst)
    } else {
      for (name in names(lst)) {
        current_path <- if (path == "") name else paste0(path, ".", name)
        # Check if the current item is a data frame
        if (is.data.frame(lst[[name]])) {
          results[[current_path]] <- lst[[name]]
        } else if (is.list(lst[[name]])) {
          # Recurse into the list if the current item is a list
          results <- c(results, flatten_list(lst[[name]], current_path))
        } else {
          # Handle any other types, if needed
          warning(paste("Ignoring non-data frame element at path:", current_path))
        }
      }
    }
    return(results)
  }
  list_split <- flatten_list(split_field)
  return(list_split)
}
