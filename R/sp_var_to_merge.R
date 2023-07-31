#' General function to choose variables to merge,
#' limiting the number of generated tables while ensuring not to generate
#' tables that are too large.
#'
#' @param dfs data.frame
#' @param totcode named vector of totals for categorical variables
#' @param hrcfiles named vector of hrc files for categorical variables
#' @param nb_var number of variables to merge
#' @param nb_tab strategy to follow for choosing variables automatically:
#'   - 'min': minimize the number of tables;
#'   - 'max': maximize the number of tables;
#'   - 'smart': minimize the number of tables under the constraint of their row count.
#' @param LIMIT maximum allowed row count in the 'smart' case
#'
#' @return A list of vectors representing the chosen variables to merge
#'
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' data <- expand.grid(
#'   ACT = c("Total", "A", "B", "A1", "A2", "B1", "B2"),
#'   GEO = c("Total", "GA", "GB", "GA1", "GA2"),
#'   SEX = c("Total", "F", "M"),
#'   AGE = c("Total", "AGE1", "AGE2"),
#'   stringsAsFactors = FALSE,
#'   KEEP.OUT.ATTRS = FALSE
#' ) %>%
#'   as.data.frame()
#'
#' data <- data %>% mutate(VALUE = 1:n())
#'
#' hrc_act <- "output/hrc_ACT.hrc"
#' sdcHierarchies::hier_create(root = "Total", nodes = c("A","B")) %>%
#'   sdcHierarchies::hier_add(root = "A", nodes = c("A1","A2")) %>%
#'   sdcHierarchies::hier_add(root = "B", nodes = c("B1","B2")) %>%
#'   sdcHierarchies::hier_convert(as = "argus") %>%
#'   slice(-1) %>%
#'   mutate(levels = substring(paste0(level,name),3)) %>%
#'   select(levels) %>%
#'   write.table(file = hrc_act, row.names = F, col.names = F, quote = F)
#'
#' hrc_geo <- "output/hrc_GEO.hrc"
#' sdcHierarchies::hier_create(root = "Total", nodes = c("GA","GB")) %>%
#'   sdcHierarchies::hier_add(root = "GA", nodes = c("GA1","GA2")) %>%
#'   sdcHierarchies::hier_convert(as = "argus") %>%
#'   slice(-1) %>%
#'   mutate(levels = substring(paste0(level,name),3)) %>%
#'   select(levels) %>%
#'   write.table(file = hrc_geo, row.names = F, col.names = F, quote = F)
#'
#' totcode <- c(SEX="Total",AGE="Total", GEO="Total", ACT="Total")
#'
#' hrcfiles <- c(ACT = hrc_act, GEO = hrc_geo)
#'
#' # Consistent: choose two hierarchical variables
#' res1 <- var_to_merge(dfs = data,
#'                                         totcode = totcode,
#'                                         hrcfiles = hrcfiles,
#'                                         nb_var = 2,
#'                                         nb_tab = 'max')
#' res1
#' max(unlist(length_tabs(dfs = data,
#'                        hrcfiles = hrcfiles,
#'                        totcode = totcode,
#'                        v1 = res1$vars[1], v2 = res1$vars[2])))
#'
#' # Consistent: choose two non-hierarchical variables
#' res2 <- var_to_merge(dfs = data,
#'                                 totcode = totcode,
#'                                 hrcfiles = hrcfiles,
#'                                 nb_var = 2,
#'                                 nb_tab = 'min')
#' res2
#' max(unlist(length_tabs(dfs = data,
#'                        hrcfiles = hrcfiles,
#'                        totcode = totcode,
#'                        v1 = res2$vars[1], v2 = res2$vars[2])))
#'
#' res3 <- var_to_merge(dfs = data,
#'                                 totcode = totcode,
#'                                 hrcfiles = hrcfiles,
#'                                 LIMIT = 200,
#'                                 nb_var = 2,
#'                                 nb_tab = 'smart')
#' res3
#' max(unlist(length_tabs(dfs = data,
#'                        hrcfiles = hrcfiles,
#'                        totcode = totcode,
#'                        v1 = res3$vars[1], v2 = res3$vars[2])))
#'
#' # Obtains 147, which is well below 200
#'
#' res4 <- var_to_merge(dfs = data,
#'                                 totcode = totcode,
#'                                 hrcfiles = hrcfiles,
#'                                 LIMIT = 5,
#'                                 nb_var = 2,
#'                                 nb_tab = 'smart')
#' res4
#' max(unlist(length_tabs(dfs = data,
#'                        hrcfiles = hrcfiles,
#'                        totcode = totcode,
#'                        v1 = res4$vars[1], v2 = res4$vars[2])))
#'
#' # Receives a warning: unable to reach the announced value
#' # There are 63 rows (equivalent to the max
#' # -> this is what reduces the table size)
#' # And the warning announces 63 rows, which is consistent with the output
var_to_merge <- function(
    dfs,
    totcode,
    hrcfiles = NULL,
    nb_var = 4,
    nb_tab = "min",
    LIMIT = 150)
{
  # Case of 2 pairs in dimension 5
  if (nb_var == 4){
    result_comb <- generate_two_pairs(totcode)

    # Case of a triplet in dimension 5
  } else if (nb_var == 3){
    result_comb <- generate_a_triplet(totcode)

    # Case of dimension 4
  } else {
    result_comb <- generate_a_pair(totcode)
  }

  return(var_to_merge_fragment(dfs = dfs,
                                 result_comb = result_comb,
                                 totcode = totcode,
                                 hrcfiles = hrcfiles,
                                 LIMIT = LIMIT,
                                 nb_tab = nb_tab))
}

var_to_merge_fragment <- function(
    dfs,
    result_comb,
    totcode,
    hrcfiles = NULL,
    LIMIT = 150,
    nb_tab = "smart")
{
  # Calculate the number of tables and maximum rows for each combination of variables
  res_func <- lapply(result_comb, function(x) length_tabs(
    dfs = data,
    v1 = x[1],
    v2 = x[2],
    v3 = x[3],
    v4 = x[4],
    totcode = totcode,
    hrcfiles = hrcfiles))

  # Get the maximum rows and number of created tables
  res_max <- sapply(res_func, function(x) max(unlist(x)))
  res_len <- sapply(res_func, function(x) length(unlist(x)))

  # Create a dataframe for better filtering
  df <- data.frame(res_max = res_max, res_len = res_len)

  # Save the row number by adding a column
  df$original_index <- seq(nrow(df))

  # Case: minimize the number of tables
  if (nb_tab == "min"){
    min_nb_tab <-  min(df$res_len)
    filtered_df <- df[df$res_len == min_nb_tab, ]

    # Get the index of the filtered table
    min_index <- which.min(filtered_df$res_max)
    # Print the original index
    i <- filtered_df$original_index[min_index]

    return(list(vars = result_comb[[i]],
                max_row = filtered_df$res_max[min_index],
                nb_tab = filtered_df$res_len[min_index])
           )

    # Case: maximize the number of tables
  } else if (nb_tab == "max"){
    max_nb_tab <-  max(df$res_len)
    filtered_df <- df[df$res_len == max_nb_tab, ]

    # Get the index of the filtered table
    min_index <- which.min(filtered_df$res_max)
    # Print the original index
    i <- filtered_df$original_index[min_index]

    return(list(vars = result_comb[[i]],
                max_row = filtered_df$res_max[min_index],
                nb_tab = filtered_df$res_len[min_index])
    )

    # Case: 'smart' - maximize under the constraint of the size limit
  } else {
    # Filter based on the maximum rows condition
    filtered_df <- df[df$res_max < LIMIT, ]

    # If at least one case satisfies this condition
    if (nrow(filtered_df) > 0){
      # Get the index of the filtered table
      min_index <- which.min(filtered_df$res_len)

      # Print the original index
      i <- filtered_df$original_index[min_index]

      return(list(vars = result_comb[[i]],
                  max_row = filtered_df$res_max[min_index],
                  nb_tab = filtered_df$res_len[min_index])
      )

    } else {
      # Return the result with the fewest tables among those
      # with the shortest tables
      min_res_max <- min(df$res_max)
      warning(c("
      The limit of ",LIMIT," cannot be achieved.
      The largest table has ",min_res_max," rows."))

      filtered_df <- df[df$res_max == min_res_max, ]

      # Get the index of the filtered table
      min_index <- which.min(filtered_df$res_len)

      # Print the original index
      i <- filtered_df$original_index[min_index]

      return(list(vars = result_comb[[i]],
                  max_row = filtered_df$res_max[min_index],
                  nb_tab = filtered_df$res_len[min_index])
      )
    }
  }
}

generate_a_pair <- function(totcode) {
  # Retrieve the categorical variables from the dataframe
  var_cat <- names(totcode)

  # Use combn to get all combinations of two elements
  comb <- combn(var_cat, 2)

  # Transform the results into a list of vectors
  result <- split(t(comb), seq(ncol(comb)))

  return(result)
}

generate_two_pairs <- function(totcode) {
  # Retrieve the categorical variables from the dataframe
  var_cat <- names(totcode)

  # Get all combinations of four elements
  comb <- combn(var_cat, 4)

  # For each combination, obtain two disjoint pairs
  result <- lapply(seq(ncol(comb)), function(i) {
    quad <- comb[, i]
    pair_comb <- t(combn(quad, 2))

    # Create two disjoint pairs for each combination
    pairs <- lapply(seq(nrow(pair_comb)), function(j) {
      pair1 <- pair_comb[j, ]
      pair2 <- setdiff(quad, pair1)

      # Convert the pairs to strings
      pair1_str <- paste(sort(pair1), collapse = ",")
      pair2_str <- paste(sort(pair2), collapse = ",")

      # Create a string representing both pairs
      both_pairs_str <- paste(sort(c(pair1_str, pair2_str)), collapse = ",")
      return(both_pairs_str)
    })
    return(pairs)
  })

  # Flatten the result
  result <- unlist(result, recursive = FALSE)

  # Remove duplicates
  unique_pairs <- unique(result)

  # Convert the strings back to vectors
  result <- lapply(unique_pairs, function(pair_str) {
    pairs <- strsplit(pair_str, ",")[[1]]
    return(pairs)
  })

  return(result)
}

generate_a_triplet <- function(totcode) {
  # Retrieve the categorical variables from the dataframe
  var_cat <- names(totcode)

  # Get all combinations of three elements
  comb <- combn(var_cat, 3)

  # Transform the result into a list of vectors
  result <- split(t(comb), seq(ncol(comb)))

  return(result)
}

#' Calculation of the table sizes generated a priori during the reduction of dimension
#' from 4 or 5 dimensions to 3 dimensions
#'
#' @param dfs a data.frame
#'
#' Variable in the 5->4 or 4->3 step
#' @param v1 the first merged variable
#' @param v2 the second merged variable
#'
#' Variable in the case of 4->3 passage in the 4->3 process
#' do not specify v1_v2 if three variables are merged into one
#' @param v3 the third original variable to be merged
#' @param v4 the fourth original variable to be merged
#'
#' @param hrcfiles named vector of hrc files related to the variables
#'
#' @return a list of the lengths of the tables created during the dimension reduction
#' @export
#'
#' TODO: review the case of a merged trio
#' Verify if the case of 3 variables, with at least one hierarchical variable, is correct
#' It seems correct, but the output is not well "sorted"
#'
#' @examples
#' library(dplyr)
#'
#'
#' # Dimension 4
#' data <- expand.grid(
#'   ACT = c("Total", "A", "B", "A1", "A2","A3", "B1", "B2","B3","B4","C","D","E","F","G","B5"),
#'   GEO = c("Total", "G1", "G2"),
#'   SEX = c("Total", "F", "M"),
#'   AGE = c("Total", "AGE1", "AGE2"),
#'   stringsAsFactors = FALSE
#' ) %>%
#'   as.data.frame()
#'
#' data <- data %>% mutate(VALUE = 1)
#'
#'
#' hrc_act <- "output/hrc_ACT.hrc"
#'
#' sdcHierarchies::hier_create(root = "Total", nodes = c("A","B","C","D","E","F","G")) %>%
#'   sdcHierarchies::hier_add(root = "A", nodes = c("A1","A2","A3")) %>%
#'   sdcHierarchies::hier_add(root = "B", nodes = c("B1","B2","B3","B4","B5")) %>%
#'   sdcHierarchies::hier_convert(as = "argus") %>%
#'   slice(-1) %>%
#'   mutate(levels = substring(paste0(level,name),3)) %>%
#'   select(levels) %>%
#'   write.table(file = hrc_act, row.names = F, col.names = F, quote = F)
#'
#' # Function results
#'
#' res1 <- length_tabs(dfs = data,
#'                     hrcfiles = c(ACT = hrc_act),
#'                     totcode = c(SEX="Total",AGE="Total", GEO="Total", ACT="Total"),
#'                     v1 = "ACT",
#'                     v2 = "GEO")
#'
#' # Dimension 5
#' data <- expand.grid(
#'   ACT = c("Total_A", paste0("A", seq(1,5),"_"),paste0("A1_", seq(1,7)),paste0("A2_", seq(1,9))),
#'   GEO = c("Total_G", "GA", "GB", "GA1", "GA2", "GB1", "GB2","GA3","GB3","GB4"),
#'   SEX = c("Total_S", "F", "M","F1","F2","M1","M2"),
#'   AGE = c("Ensemble", "AGE1", "AGE2", "AGE11", "AGE12", "AGE21", "AGE22"),
#'   ECO = c("PIB","Ménages","Entreprises"),
#'   stringsAsFactors = FALSE,
#'   KEEP.OUT.ATTRS = FALSE
#' ) %>%
#'   as.data.frame()
#'
#' data <- data %>% mutate(VALUE = 1:n())
#'
#' hrc_act <- "output/hrc_ACT.hrc"
#' sdcHierarchies::hier_create(root = "Total_A", nodes = paste0("A", seq(1,5),"_")) %>%
#'   sdcHierarchies::hier_add(root = "A1_", nodes = paste0("A1_", seq(1,7))) %>%
#'   sdcHierarchies::hier_add(root = "A2_", nodes = paste0("A2_", seq(1,9))) %>%
#'   sdcHierarchies::hier_convert(as = "argus") %>%
#'   slice(-1) %>%
#'   mutate(levels = substring(paste0(level,name),3)) %>%
#'   select(levels) %>%
#'   write.table(file = hrc_act, row.names = F, col.names = F, quote = F)
#'
#' hrc_geo <- "output/hrc_GEO.hrc"
#' sdcHierarchies::hier_create(root = "Total_G", nodes = c("GA","GB")) %>%
#'   sdcHierarchies::hier_add(root = "GA", nodes = c("GA1","GA2","GA3")) %>%
#'   sdcHierarchies::hier_add(root = "GB", nodes = c("GB1","GB2","GB3","GB4")) %>%
#'   sdcHierarchies::hier_convert(as = "argus") %>%
#'   slice(-1) %>%
#'   mutate(levels = substring(paste0(level,name),3)) %>%
#'   select(levels) %>%
#'   write.table(file = hrc_geo, row.names = F, col.names = F, quote = F)
#'
#' res2 <- length_tabs(dfs = data,
#'                     hrcfiles = c(ACT = hrc_act, GEO = hrc_geo),
#'                     totcode = c(SEX="Total_S",AGE="Ensemble", GEO="Total_G",
#'                                 ACT="Total_A", ECO = "PIB"),
#'                     v1 = "ACT",v2 = "AGE",
#'                     v3 = "GEO",v4 = "SEX")
#'
#' # Warning : The ouput in case of hierarchical variables
#' # is not in the right order
#' res3 <- length_tabs(dfs = data,
#'                     hrcfiles = c(ACT = hrc_act, GEO = hrc_geo),
#'                     totcode = c(SEX="Total_S",AGE="Ensemble", GEO="Total_G",
#'                                 ACT="Total_A", ECO = "PIB"),
#'                     v1 = "ACT",v2 = "AGE",v3 = "GEO")
length_tabs <- function(
  dfs,
  v1,
  v2,
  v3 = NULL,
  v4 = NULL,
  totcode,
  hrcfiles = NULL)
{
  # To generalize the function to handle NA for an external function
  v3 <- if (!is.null(v3) && is.na(v3)) NULL else v3
  v4 <- if (!is.null(v4) && is.na(v4)) NULL else v4

  # If 4 variables are specified -> 5 dimensions case, 2 couples are created
  if (!is.null(v4)) {
    return(length_tabs_5_4_var(dfs = dfs,
                               hrcfiles = hrcfiles,
                               v1 = v1, v2 = v2,
                               v3 = v3, v4 = v4,
                               totcode = totcode))

    # If 3 variables are specified -> 5 dimensions case, a trio is merged
  } else if (!is.null(v3)) {
    return(length_tabs_5_3_var(dfs = dfs,
                               hrcfiles = hrcfiles,
                               v1 = v1, v2 = v2, v3 = v3,
                               totcode = totcode))

    # If 2 variables are specified -> 4 dimensions case
  } else {
    return(length_tabs_4(dfs = dfs,
                         hrcfiles = hrcfiles,
                         v1 = v1, v2 = v2,
                         totcode = totcode))
  }
}

# case : 4 dimensions
length_tabs_4 <- function(dfs,v1,v2,totcode,hrcfiles=NULL){

  # Retrieval of groupings {nodes + branch}
  # based on whether the variable is hierarchical or not

  # We need to list and then unlist
  # otherwise the ifelse returns the first element of import_hierarchy (big total)
  # instead of returning all the nodes
  level_v1 <- unlist(ifelse(v1 %in% names(hrcfiles),
                            list(import_hierarchy(hrcfiles[[v1]])),
                            list(list(unique(dfs[[v1]])))),
                     recursive = FALSE)

  level_v2 <- unlist(ifelse(v2 %in% names(hrcfiles),
                            list(import_hierarchy(hrcfiles[[v2]])),
                            list(list(unique(dfs[[v2]])))),
                     recursive = FALSE)

  # If case 1 non hrc (not hierarchical) and v2 in hrcfiles, then we need to reorder
  if (!(v2 %in% names(hrcfiles)) & (v1 %in% names(hrcfiles))) {
    tmp <- level_v1
    level_v1 <- level_v2
    level_v2 <- tmp
  }

  # We do all possible combinations between v1 and v2
  # which represents the tables created during the creation of v1_v2 in the 5->4 step

  # For each of these tables, there are two possible hierarchies
  # one with the totals of v1, and the other with the totals of v2
  # thus, for one of the modalities, we do not make any combination with its total
  # hence the -1
  # and finally, we add the grand total, hence the +1
  nb_rows <- lapply(1:length(level_v1), function(i) {
    lapply(1:length(level_v2), function(j) {
      c((length(level_v1[[i]]) - 1) * length(level_v2[[j]]) + 1,
        length(level_v1[[i]]) * (length(level_v2[[j]]) - 1) + 1)
    })
  })

  # Now we need to multiply by the modalities of the non-merged variables

  list_non_merged_vars <- names(totcode[!(names(totcode) %in% c(v1, v2))])

  mod_non_merged_vars <- lapply(list_non_merged_vars,
                                function(x)  length(unique(dfs[[x]])))

  prod_numbers <- prod(unlist(mod_non_merged_vars))

  nb_rows_tot <- lapply(unlist(nb_rows), function(x) x * prod_numbers)

  return(nb_rows_tot)
}

# case : 5 dimensions, two pairs of merged variables
length_tabs_5_4_var <- function(dfs, v1, v2, v3, v4, totcode, hrcfiles = NULL) {

  # Retrieve groupings {nodes + branches} based on whether the variable is hierarchical or not, transitioning from 5 dimensions to 4 dimensions.

  # List and then unlist the results; ifelse returns all nodes instead of just the first one.
  level_v1 <- unlist(ifelse(v1 %in% names(hrcfiles),
                            list(import_hierarchy(hrcfiles[[v1]])),
                            list(list(unique(dfs[[v1]])))),
                     recursive = FALSE)

  level_v2 <- unlist(ifelse(v2 %in% names(hrcfiles),
                            list(import_hierarchy(hrcfiles[[v2]])),
                            list(list(unique(dfs[[v2]])))),
                     recursive = FALSE)

  # Swap level_v1 and level_v2 in case v2 is not hierarchical but v1 is (to maintain order).
  if (!(v2 %in% names(hrcfiles)) & (v1 %in% names(hrcfiles))) {
    tmp <- level_v1
    level_v1 <- level_v2
    level_v2 <- tmp
  }

  level_v3 <- unlist(ifelse(v3 %in% names(hrcfiles),
                            list(import_hierarchy(hrcfiles[[v3]])),
                            list(list(unique(dfs[[v3]])))),
                     recursive = FALSE)

  level_v4 <- unlist(ifelse(v4 %in% names(hrcfiles),
                            list(import_hierarchy(hrcfiles[[v4]])),
                            list(list(unique(dfs[[v4]])))),
                     recursive = FALSE)

  # Swap level_v3 and level_v4 in case v4 is not hierarchical but v3 is (to maintain order).
  if (!(v4 %in% names(hrcfiles)) & (v3 %in% names(hrcfiles))) {
    tmp <- level_v3
    level_v3 <- level_v4
    level_v4 <- tmp

    tmp <- v3
    v3 <- v4
    v4 <- tmp
  }

  # Calculate the length of resulting 4-dimensional datasets for each combination of variables.

  nb_rows <- lapply(1:length(level_v1), function(i) {
    lapply(1:length(level_v2), function(j) {

      c(
        lapply(1:length(level_v3), function(k) {
          lapply(1:length(level_v4), function(l) {

            # A formula to calculate the length of the arrays.
            c( ((length(level_v1[[i]]) - 1) * length(level_v2[[j]]) + 1) *
                 ((length(level_v3[[k]]) - 1) * length(level_v4[[l]]) + 1),

               ((length(level_v1[[i]]) - 1) * length(level_v2[[j]]) + 1) *
                 (length(level_v3[[k]]) * (length(level_v4[[l]]) - 1) + 1)
            )
          })
        }),

        lapply(1:length(level_v3), function(k) {
          lapply(1:length(level_v4), function(l) {

            c( (length(level_v1[[i]]) * (length(level_v2[[j]]) - 1) + 1) *
                 ((length(level_v3[[k]]) - 1) * length(level_v4[[l]]) + 1),

               (length(level_v1[[i]]) * (length(level_v2[[j]]) - 1) + 1) *
                 (length(level_v3[[k]]) * (length(level_v4[[l]]) - 1) + 1)
            )
          })
        })
      )

    })
  })

  # Calculate the total number of rows by multiplying with the unique modalities of non-merged variables.

  list_var_non_fusionnées <- names(totcode[!(names(totcode) %in% c(v1, v2, v3, v4))])

  mod_var_non_fusionnées <- lapply(list_var_non_fusionnées,
                                   function(x)  length(unique(dfs[[x]])))

  prod_numbers <- prod(unlist(mod_var_non_fusionnées))

  nb_rows_tot <- lapply(unlist(nb_rows), function(x) x * prod_numbers)

  return(nb_rows_tot)
}

# case : 5 dimensions, three variables merged into one
length_tabs_5_3_var <- function(dfs, v1, v2, v3, totcode, hrcfiles = NULL) {

  # Case of at least one hierarchical variable
  if (length(setdiff(names(hrcfiles), c(v1, v2, v3))) != length(hrcfiles)) {

    # WARNING
    # This case is a work in progress (WIP)
    # Only the different lengths of modalities are calculated
    # But we do not know specifically the length of table i, for example
    # However, this is not currently critical
    # All modalities appear the correct number of times, but not in the correct order

    # Transition from 5 dimensions to 4 dimensions

    # List and then unlist the results; ifelse returns all nodes instead of just the first one.
    level_v1 <- unlist(ifelse(v1 %in% names(hrcfiles),
                              list(import_hierarchy(hrcfiles[[v1]])),
                              list(list(unique(dfs[[v1]])))),
                       recursive = FALSE)

    level_v2 <- unlist(ifelse(v2 %in% names(hrcfiles),
                              list(import_hierarchy(hrcfiles[[v2]])),
                              list(list(unique(dfs[[v2]])))),
                       recursive = FALSE)

    # Swap level_v1 and level_v2 if v2 is not hierarchical but v1 is (to maintain order).
    if (!(v2 %in% names(hrcfiles)) & (v1 %in% names(hrcfiles))) {
      tmp <- level_v1
      level_v1 <- level_v2
      level_v2 <- tmp
    }

    # Transition from 4 dimensions to 3 dimensions

    # List and then unlist the results; ifelse returns all nodes instead of just the first one.
    level_v3 <- unlist(ifelse(v3 %in% names(hrcfiles),
                              list(import_hierarchy(hrcfiles[[v3]])),
                              list(list(unique(dfs[[v3]])))),
                       recursive = FALSE)


    nb_rows <- lapply(1:length(level_v1), function(i) {

      lapply(1:length(level_v3), function(k) {

        c( (length(level_v1[[i]]) - 1) * length(level_v3[[k]]) + 1,
           length(level_v1[[i]]) * (length(level_v3[[k]]) - 1) + 1
        )
      })

      lapply(1:length(level_v2), function(j) {
        lapply(1:length(level_v3), function(k) {

          c(
            rep(c((length(level_v2[[j]]) - 1) * length(level_v3[[k]]) + 1,
                  length(level_v2[[j]]) * (length(level_v3[[k]]) - 1) + 1
            ),
            times = length(level_v1[[i]])
            ),

            rep(c((length(level_v1[[i]]) - 1) * length(level_v3[[k]]) + 1,
                  length(level_v1[[i]]) * (length(level_v3[[k]]) - 1) + 1
            ),
            times = length(level_v2[[j]])
            )
          )
        })
      })
    })

    # Case of 3 non-hierarchical variables: exact result (the length of table i is known)
  } else {

    n_mod_v1 <- length(unique(dfs[[v1]]))
    n_mod_v2 <- length(unique(dfs[[v2]]))
    n_mod_v3 <- length(unique(dfs[[v3]]))

    nb_rows <- c(
      1 + (n_mod_v3 - 1) * n_mod_v1,
      1 + n_mod_v3 * (n_mod_v1 - 1),

      rep(c(1 + (n_mod_v3 - 1) * n_mod_v2,
            1 + n_mod_v3 * (n_mod_v2 - 1))
          , n_mod_v1),

      rep(c(1 + (n_mod_v3 - 1) * n_mod_v1,
            1 + n_mod_v3 * (n_mod_v1 - 1))
          , n_mod_v2 - 1)
    )
  }

  # Calculate the total number of rows by multiplying with the unique modalities of non-merged variables.

  list_var_non_fusionnées <- names(totcode[!(names(totcode) %in% c(v1, v2, v3))])

  mod_var_non_fusionnées <- lapply(list_var_non_fusionnées,
                                   function(x)  length(unique(dfs[[x]])))

  prod_numbers <- prod(unlist(mod_var_non_fusionnées))

  nb_rows_tot <- lapply(unlist(nb_rows), function(x) x * prod_numbers)

  return(nb_rows_tot)
}

# Function to manage the import of the hierarchy
import_hierarchy <- function(hrcfile) {
  total <- "BIG_Total"
  res_sdc <- sdcHierarchies::hier_import(inp = hrcfile, from = "hrc", root = total) %>%
    sdcHierarchies::hier_convert(as = "sdc")
  # Store all sets of parent + direct child
  levels <- lapply(res_sdc$dims, names)
  return(levels)
}

#' Calculate the number of tables generated when merging 3 variables
#' in the transition from 5 to 3 dimensions
#'
#' @param v1 first variable to be merged
#' @param v2 second variable to be merged
#' @param v3 third variable to be merged (variable that will be merged with v1 and v2 if v4 is not specified)
#' @param v4 fourth variable to be merged (with v3)
#' @param hrcfiles named list of hrc files
#' @param data data.frame (used only in the case where a trio is formed)
#'
#' @return an integer representing the number of tables generated
#' @export
#'
#' TODO: Generalize the case of 3 variables into one?
#'
#' @examples
#' library(dplyr)
#'
#'
#' # Dimension 4
#' data <- expand.grid(
#'   ACT = c("Total", "A", "B", "A1", "A2", "B1", "B2"),
#'   GEO = c("Total", "G1", "G2"),
#'   SEX = c("Total", "F", "M"),
#'   AGE = c("Total", "AGE1", "AGE2"),
#'   stringsAsFactors = FALSE
#' ) %>%
#'   as.data.frame()
#'
#' data <- data %>% mutate(VALUE = 1)
#'
#' hrc_act <- "output/hrc_ACT.hrc"
#'
#' sdcHierarchies::hier_create(root = "Total", nodes = c("A", "B")) %>%
#'   sdcHierarchies::hier_add(root = "A", nodes = c("A1", "A2")) %>%
#'   sdcHierarchies::hier_add(root = "B", nodes = c("B1", "B2")) %>%
#'   sdcHierarchies::hier_convert(as = "argus") %>%
#'   slice(-1) %>%
#'   mutate(levels = substring(paste0(level, name), 3)) %>%
#'   select(levels) %>%
#'   write.table(file = hrc_act, row.names = FALSE, col.names = FALSE, quote = FALSE)
#'
#' # 1 pair created
#' nb_tab_generated(v1 = "ACT", v2 = "GEO",
#'                 hrcfiles = c(ACT = hrc_act))
#'
#' # Dimension 5
#' data <- expand.grid(
#'   ACT = c("Total", "A", "B", "A1", "A2", "B1", "B2"),
#'   GEO = c("Total", "GA", "GB", "GA1", "GA2", "GB1", "GB2"),
#'   SEX = c("Total", "F", "M", "F1", "F2", "M1", "M2"),
#'   AGE = c("Total", "AGE1", "AGE2", "AGE11", "AGE12", "AGE21", "AGE22"),
#'   ECO = c("PIB", "Ménages", "Entreprises"),
#'   stringsAsFactors = FALSE,
#'   KEEP.OUT.ATTRS = FALSE
#' ) %>%
#'   as.data.frame()
#'
#' data <- data %>% mutate(VALUE = 1:n())
#'
#' hrc_act <- "output/hrc_ACT.hrc"
#' sdcHierarchies::hier_create(root = "Total", nodes = c("A", "B")) %>%
#'   sdcHierarchies::hier_add(root = "A", nodes = c("A1", "A2")) %>%
#'   sdcHierarchies::hier_convert(as = "argus") %>%
#'   slice(-1) %>%
#'   mutate(levels = substring(paste0(level, name), 3)) %>%
#'   select(levels) %>%
#'   write.table(file = hrc_act, row.names = FALSE, col.names = FALSE, quote = FALSE)
#'
#' hrc_geo <- "output/hrc_GEO.hrc"
#' sdcHierarchies::hier_create(root = "Total", nodes = c("GA", "GB")) %>%
#'   sdcHierarchies::hier_add(root = "GA", nodes = c("GA1", "GA2")) %>%
#'   sdcHierarchies::hier_add(root = "GB", nodes = c("GB1", "GB2")) %>%
#'   sdcHierarchies::hier_convert(as = "argus") %>%
#'   slice(-1) %>%
#'   mutate(levels = substring(paste0(level, name), 3)) %>%
#'   select(levels) %>%
#'   write.table(file = hrc_geo, row.names = FALSE, col.names = FALSE, quote = FALSE)
#'
#' hrc_sex <- "output/hrc_SEX.hrc"
#' sdcHierarchies::hier_create(root = "Total", nodes = c("F", "M")) %>%
#'   sdcHierarchies::hier_add(root = "F", nodes = c("F1", "F2")) %>%
#'   sdcHierarchies::hier_add(root = "M", nodes = c("M1", "M2")) %>%
#'   sdcHierarchies::hier_convert(as = "argus") %>%
#'   slice(-1) %>%
#'   mutate(levels = substring(paste0(level, name), 3)) %>%
#'   select(levels) %>%
#'   write.table(file = hrc_sex, row.names = FALSE, col.names = FALSE, quote = FALSE)
#'
#' # Trio merged
#' nb_tab_generated(data = data,
#'                 v1 = "ACT", v2 = "GEO", v3 = "SEX",
#'                 hrcfiles = c(ACT = hrc_act, GEO = hrc_geo, SEX = hrc_sex))
#'
#' # 2 pairs created
#' nb_tab_generated(v1 = "ACT", v2 = "GEO",
#'                 v3 = "SEX", v4 = "EXO",
#'                 hrcfiles = c(ACT = hrc_act, GEO = hrc_geo, SEX = hrc_sex))
nb_tab_generated <- function(
  v1,
  v2,
  v3 = NULL,
  v4 = NULL,
  hrcfiles = NULL,
  data = NULL)
{
  # Case dimension 5: 2 couples created
  if (!is.null(v4)) {
    return(4 * nb_nodes(hrcfiles = hrcfiles, v = v1) *
             nb_nodes(hrcfiles = hrcfiles, v = v2) *
             nb_nodes(hrcfiles = hrcfiles, v = v3) *
             nb_nodes(hrcfiles = hrcfiles, v = v4))

    # Case dimension 5: one triplet merged
  } else if (!is.null(v3)) {

    # 2 hierarchical variables merged
    if (!is.null(hrcfiles) & v1 %in% names(hrcfiles) & v2 %in% names(hrcfiles)) {

      # The hierarchy of each variable
      level_v1 <- import_hierarchy(hrcfiles[[v1]])
      level_v2 <- import_hierarchy(hrcfiles[[v2]])

      # Store the sum of nodes of v1_v2 for each table
      # We consider all possible combinations between v1 and v2
      # => represents the tables created during the creation of v1_v2 in the 5->4 step

      # For each of these tables, there are two possible hierarchies
      # one with the totals of v1, and the other with the totals of v2
      # the number of nodes is equal to their number of modalities
      nb_noeuds_var <- sum(sapply(1:length(level_v1), function(i) {
        sum(sapply(1:length(level_v2), function(j) {
          length(level_v1[[i]]) + length(level_v2[[j]])
        }))
      }))

      # 2 non-hierarchical variables merged
    } else if (is.null(hrcfiles) | !(v1 %in% names(hrcfiles)) & !(v2 %in% names(hrcfiles))) {
      # There is only one table in the end
      # which can have two hierarchies
      # totals on v1, or totals on v2
      # the number of nodes is equivalent to the number of modalities
      nb_noeuds_var <- length(unique(data[[v1]])) + length(unique(data[[v2]]))

      # 1 hierarchical variable and 1 non-hierarchical variable merged
    } else {
      var_hier <- ifelse(v1 %in% names(hrcfiles), v1, v2)
      mod_var_non_hier <- ifelse(var_hier == v1,
                                 length(unique(data[[v2]])),
                                 length(unique(data[[v1]])))

      # Analysis of the hierarchy of var_hier
      level_var_hier <- import_hierarchy(hrcfiles[[var_hier]])

      # We consider all possible combinations between v1 and v2
      # => represents the tables created during the creation of v1_v2 in the 5->4 step

      # For each of these tables, there are two possible hierarchies
      # one with the totals of v1, and the other with the totals of v2
      # the number of nodes is equal to their number of modalities
      nb_noeuds_var <- sum(sapply(1:length(level_var_hier), function(i) {
        length(level_var_hier[[i]]) + mod_var_non_hier
      }))
    }

    # nb_nodes corresponds to the number of tables that need to be created
    # to make v1_v2 non-hierarchical
    # for each of these tables, v3 needs to be made non-hierarchical
    # and we create as many tables as its hierarchy has nodes
    # finally, for each created table, two hierarchies are possible
    # totals on v1_v2 and totals on v3
    return(2 * nb_noeuds_var * nb_nodes(hrcfiles, v = v3))

    # Case dimension 4
  } else {
    return(2 * nb_nodes(hrcfiles = hrcfiles, v = v1) *
             nb_nodes(hrcfiles = hrcfiles, v = v2))
  }
}
