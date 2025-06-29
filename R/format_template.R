#' Check for Non-Total Values in a Data Frame
#'
#' This function checks if any column in a data frame contains values
#' that are not part of a given set of totals.
#'
#' @param data A data frame containing the data to be checked.
#' @param totals A vector of values considered as totals.
#'
#' @return A logical value: `TRUE` if at least one non-total value exists, otherwise `FALSE`.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   col1 = c("A", "B", "TOTAL"),
#'   col2 = c("X", "TOTAL", "TOTAL")
#' )
#' contains_non_total(df, c("TOTAL"))
#' # Returns TRUE
#' }
#'
#' @importFrom dplyr summarise
#'
contains_non_total <- function(data, totals) {
  any(data %>%
        summarise(across(everything(), ~ any(!(.x %in% totals)))) %>%
        unlist())
}

#' Generate All Combinations of spanning variables
#'
#' This function generates all possible combinations of keys from a given named list or vector.
#'
#' @param criteria A named list or vector, where the names represent the keys.
#'
#' @return A list of combinations, where each combination is a character vector of key names.
#'
#' @examples
#' \dontrun{
#' criteria <- list(key1 = "value1", key2 = "value2", key3 = "value3")
#' get_combinations(criteria)
#' # Returns a list of combinations: c("key1"), c("key2"), c("key3"), c("key1", "key2"), ...
#' }
get_combinations <- function(criteria) {
  keys <- names(criteria)
  unlist(lapply(1:length(keys), function(k) combn(keys, k, simplify = FALSE)), recursive = FALSE)
}

#' Filter a Data Frame Based on Marginal Criteria
#'
#' This function filters rows in a data frame based on marginal criteria.
#' It applies filtering conditions to a subset of keys and their corresponding values,
#' and excludes rows where other keys match the given totals.
#'
#' @param data A data frame to be filtered.
#' @param criteria A named list or vector where the names are column names, and the values
#'        are the totals or values to be used in filtering.
#' @param subset_keys A character vector of keys (column names) to be included in the filter criteria.
#'
#' @return A filtered data frame where rows meet the specified criteria and exclude the subset keys.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   col1 = c("A", "B", "TOTAL"),
#'   col2 = c("X", "TOTAL", "Y"),
#'   col3 = c("Z", "Z", "Z")
#' )
#' criteria <- list(col1 = "TOTAL", col2 = "TOTAL", col3 = "TOTAL")
#' filter_on_marginal_of_spanning_var(df, criteria, subset_keys = c("col1", "col2"))
#' # Filters the data frame based on the criteria and subset_keys.
#' }
filter_on_marginal_of_spanning_var <- function(data, criteria, subset_keys) {
  # Create filter expressions for subset_keys with ==
  filter_expr_in <- purrr::map2(
    subset_keys,
    criteria[subset_keys],
    ~ rlang::expr(!!rlang::sym(.x) == !!.y)
  )
  # Create filter expressions for all other keys with !=
  other_keys <- setdiff(names(criteria), subset_keys)
  filter_expr_not_in <- purrr::map2(
    other_keys,
    criteria[other_keys],
    ~ rlang::expr(!!rlang::sym(.x) != !!.y)
  )
  # Combine the two sets of expressions
  combined_filter_expr <- c(filter_expr_in, filter_expr_not_in)
  # Apply the combined filter
  data %>% filter(!!!combined_filter_expr) %>% select(-!!subset_keys)
}

#' Determines the tables described in a template gathering all the published cells
#'
#' @param data template gathering all the published cells
#' @param indicator_column name of the column in which the indicators are
#' @param spanning_var_tot a named list of the spanning variables and their totals
#' @param field_columns vecotr of all the columns that are fields (ex: year of collect)
#'
#' @return named list of a dataframe describing the tables (metadata) and a list of
#' the modalities of each hierarchical variable (modalities)
#' @export
#'
#' @examples
#' data(enterprise_template)
#'
#' template_formatted <- format_template(
#'   data = enterprise_template,
#'   indicator_column = "INDICATOR",
#'   spanning_var_tot = list(
#'     ACTIVITY = "BTSXO_S94",
#'     NUMBER_EMPL = "_T",
#'     LEGAL_FORM = "_T"),
#'   field_columns = c("TIME_PERIOD")
#' )
#'
#' @importFrom purrr compact
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr everything across all_of pull row_number bind_rows n_distinct
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @export
format_template <- function(data,indicator_column,spanning_var_tot,field_columns) {
  data <- data %>% mutate(field = apply(across(all_of(field_columns)), 1, paste0, collapse = "_"))
  # each modality of field variables can be treated independently
  list_df_field <- split(data,data$field)
  list_metadata <- purrr::imap(list_df_field, function(df_field,field_name){
    cat(paste("treating the field",field_name,"\n"))
    # if there is no link between the indicators, then they can all be treated independently
    list_df_indicator <- split(df_field,df_field %>% select(!!indicator_column))
    list_df_metadata <- purrr::imap(list_df_indicator, function(df_indicator,indicator_name){
      # Step 1: Identify valid columns with more than one unique value
      valid_columns <- df_indicator %>%
        summarise(across(all_of(names(spanning_var_tot)), dplyr::n_distinct)) %>%
        tidyr::pivot_longer(everything(), names_to = "column", values_to = "n_unique") %>%
        filter(n_unique > 1) %>%
        pull(column)
      spanning_totals_valid <- spanning_var_tot[valid_columns]
      df_valid_columns <- df_indicator %>% select(all_of(valid_columns))

      if(length(spanning_totals_valid) == 0){
        warning(paste("In the field",field_name,"the indicator",indicator_name,
                      "is not broken down by any spanning variable. It is removed from the final result.")
                )
        return(NULL)
      }

      # Step 2 : get all the possible combinations of spanning variables
      combinations <- get_combinations(spanning_totals_valid)

      # Step 3: Filter and keep valid combinations, i.e. combinations with something
      # else than just the total
      # If there is only one spanning variable
      if(length(combinations) == 1 & length(combinations[[1]]) == 1) # s'il n'y a qu'une seule variable de croisement
      {
        df_valid_columns_filtered <- df_valid_columns %>%
          filter(!(.[[1]] %in% spanning_totals_valid[[1]]))

        tables_crossing_non_totals <- list(df_valid_columns_filtered)

      } else {
        # More than one spanning variable
        tables_crossing_non_totals <- purrr::map(
          combinations,
          ~ {
            # Apply the filter to get the filtered dataframe
            filtered_df <- filter_on_marginal_of_spanning_var(df_valid_columns, spanning_totals_valid, .x)
            # Check if there are non-total values in the filtered dataframe
            if (contains_non_total(filtered_df, unlist(spanning_totals_valid))) {
              return(filtered_df)  # Keep the dataframe if non-total values remain
            } else {
              return(NULL)  # Discard the dataframe if no non-total values remain
            }
          }
        ) %>% compact()
      }

      # Step 4: Name the list of dataframes
      valid_combinations <- purrr::keep(combinations, ~ {
        filtered_df <- filter_on_marginal_of_spanning_var(df_valid_columns, spanning_totals_valid, .x)
        contains_non_total(filtered_df, unlist(spanning_totals_valid))
      })
      names(tables_crossing_non_totals) <- purrr::map_chr(tables_crossing_non_totals, ~ {
        # Get the column names of the current data frame
        remaining_columns <- names(.x)
        # Construct the name based on these columns
        paste0("crossing.", paste(remaining_columns, collapse = ".and."))
      })
      tables_crossing_variables <- purrr::discard(tables_crossing_non_totals, function(single_var) {
        # Compare the modalities only for the dataframes with one column, i.e. one spanning variable
        if (ncol(single_var) == 1) {
          name_var <- names(single_var)
          # Check if the modalities of this column are all included in at least one of
          # the dataframes crossing multiple spanning variables
          any(purrr::map_lgl(tables_crossing_non_totals, function(multiple_var) {
            ncol(multiple_var) > 1 &&
              name_var %in% names(multiple_var) &&
              setequal(single_var %>% dplyr::pull(!!name_var),
                       multiple_var %>% dplyr::pull(!!name_var) %>% unique())
          }))
        } else {
          FALSE
        }
      })
      spanning_description <- purrr::imap_dfr(
        tables_crossing_variables,
        ~ {
          # Count unique values for each column and rename with prefixes
          formatted_counts <- setNames(
            paste0("hrc_", tolower(names(.x)), "_", sapply(.x, function(col) length(unique(col)))),
            names(.x)
          )
          # Extract variable names and formatted counts
          variable_names <- names(formatted_counts) # Spanning variables
          hrc_values <- formatted_counts            # Corresponding hrc values
          unique_modalities <- setNames(
            sapply(.x, function(col) unique(col), simplify = FALSE),
            formatted_counts
          )
          # Create a dynamic data frame
          data.frame(
            # Dynamically create spanning columns
            setNames(as.list(variable_names), paste0("spanning_", seq_along(variable_names))),
            # Dynamically create hrc spanning columns
            setNames(as.list(hrc_values), paste0("hrc_spanning_", seq_along(hrc_values))),
            stringsAsFactors = FALSE
          ) %>% mutate(indicator = indicator_name, unique_modalities = list(unique_modalities))
        }
      )
      spanning_description <- spanning_description %>%
        mutate(field = field_name) %>%
        mutate(
          table_name = paste(
            "table", field_name, indicator_name, row_number(),
            sep = "_"
          )
        )

      modalities <- spanning_description$unique_modalities
      names(modalities) <- spanning_description$table_name

      spanning_description <- spanning_description %>% select(-unique_modalities)

      return(list(
        metadata = spanning_description,
        modalities = modalities
      ))
    })
    # Combine metadata and modalities
    combined_metadata <- do.call(bind_rows, purrr::map(list_df_metadata, "metadata"))
    combined_modalities <- purrr::reduce(purrr::map(list_df_metadata, "modalities"), c)
    return(list(
      metadata = combined_metadata,
      modalities = combined_modalities
    ))
  })

  # Combine all metadata and modalities across fields
  final_metadata <- do.call(bind_rows, purrr::map(list_metadata, "metadata"))
  final_modalities <- purrr::reduce(purrr::map(list_metadata, "modalities"), c)

  return(list(
    metadata = final_metadata %>% select(table_name,field,indicator,everything()),
    modalities = final_modalities
  ))
}
