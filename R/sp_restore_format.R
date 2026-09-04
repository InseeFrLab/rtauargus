#' Function to reverse the process of dimension reduction
#'
#' @param masq a list of data.frames on which secret/suppression has been applied
#' @param res result of dimension reduction (contains merged variables and separator)
#'
#' @return The original data.frame restored to 4 or 5 dimensions.
#'
#' @keywords internal
#' @noRd
restore_format <- function(masq, res) {

  # ----------------------------------------------------------------------------
  # STRATEGY (Reverse Dimension Reduction):
  # 1. Recombine all processed sub-tables 'masq' into a single dataframe using
  #    bind_rows() and deduplicate identical records via distinct().
  # 2. Check reduction metadata:
  #    - 4D case: Call 'separer4_3()' once to split composite var into (v1, v2).
  #    - 5D case (2 pairs): Call 'separer4_3()' twice (v1_v2 then v3_v4).
  #    - 5D case (1 triplet): Call 'separer5_3()' once to split into (v1, v2, v3).
  # ----------------------------------------------------------------------------

  sep <- res$sep

  # Combine processed sub-tables and keep unique records
  masq_liste_empilee <- dplyr::distinct(dplyr::bind_rows(masq))

  if (is.character(res$fus_vars)) {
    # Case with 4 categorical variables
    # variable

    v1 <- res$fus_vars[1]
    v2 <- res$fus_vars[2]

    v1_v2 <- paste(v1, v2, sep = sep)

    result <- separer4_3(masq_liste_empilee, v1, v2,v1_v2, sep)
    return(result)
  }

  # Case with 5 dimensions
  # variable

  v1<-res$fus_vars$five_to_three[1]
  v2<-res$fus_vars$five_to_three[2]
  v3<-res$fus_vars$four_to_three[1]
  v4<-res$fus_vars$four_to_three[2]
  v1_v2 <- paste(v1, v2, sep = sep)

  if (!(v1_v2 == v3 | v1_v2 == v4)) {
    # Case of fusion between 3 different variables
    v3_v4 <- paste(v3, v4, sep = sep)
    # Split based on 'v1', 'v2', and 'v1_v2' using 'separer4_3' function
    split1 <- separer4_3(masq_liste_empilee, v1, v2, v1_v2, sep)
    # Further split based on 'v3', 'v4', and 'v3_v4'
    result <- separer4_3(split1, v3, v4, v3_v4, sep)

  } else {
    # Case of fusion with an already fused variable
    v3_v4 <- paste(v3, v4, sep = sep)

    if(v1_v2 == v3){
      # Split based on 'v1', 'v2', and 'v4' using 'separer5_3' function
      result<-separer5_3(masq_liste_empilee, v1,v2, v4, v3_v4, sep)
    }else{
      # Split based on 'v1', 'v2', and 'v3' using 'separer5_3' function
      result<-separer5_3(masq_liste_empilee, v1,v2,v3, v3_v4, sep)

    }

  }

  return(result)
}



# Extract 3 original variables (v3, v1, v2) from composite variable 'v3_v4'
separer5_3 <- function(df, v1, v2, v3,v3_v4, sep) {

  # 'fixed = TRUE' bypasses the regex engine for fast C-level string splitting
  splits <- strsplit(df[[v3_v4]], split = sep, fixed = TRUE)

  # Unlist splits directly into a 3-column C matrix (bypasses R sapply loop overhead)
  mat <- matrix(unlist(splits, use.names = FALSE), ncol = 3, byrow = TRUE)

  df[[v3]] <- mat[, 1]
  df[[v1]] <- mat[, 2]
  df[[v2]] <- mat[, 3]
  df[[v3_v4]] <- NULL

  # Reorder columns to place restored variables first
  new_order <- c(v3, v1, v2, setdiff(names(df), c(v3, v1, v2)))
  df <- df[, new_order]

  df
}


# Extract 2 original variables (v1, v2) from composite variable 'v1_v2'
separer4_3 <- function(df, v1, v2, v1_v2, sep) {
  # splits <- strsplit(df[[v1_v2]], split = sep_regex)

  splits <- strsplit(df[[v1_v2]], split = sep, fixed = TRUE)

  # Unlist splits directly into a 2-column C matrix (bypasses R sapply loop overhead)
  mat <- matrix(unlist(splits, use.names = FALSE), ncol = 2, byrow = TRUE)

  df[[v1]] <- mat[, 1]
  df[[v2]] <- mat[, 2]
  df[[v1_v2]] <- NULL

  # Reorder columns to place restored variables first
  new_order <- c(v1, v2, setdiff(names(df), c(v1, v2)))
  df <- df[, new_order]
  df
}
