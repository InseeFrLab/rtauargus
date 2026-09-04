#' Transition from 4 to 3 variables by merging two non-hierarchical variables
#'
#' @param dfs data.frame with 4 categorical variables
#' @param dfs_name name of the data.frame in the user list
#' @param v1 non-hierarchical categorical variable
#' @param v2 non-hierarchical categorical variable
#' @param totcode named vector of totals for categorical variables
#' @param dir_name folder where to write generated hrc files
#' @param sep separator used when concatenating variables
#'
#' @return A list with `tabs`, `hrcs`, `alt_tot` and `vars`.
#'
#' @keywords internal
#' @noRd
from_4_to_3_case_0_hr <- function(
    dfs,
    dfs_name,
    v1,
    v2,
    totcode,
    dir_name,
    sep = "_")
{
  # ----------------------------------------------------------------------------
  # STRATEGY (0 Hierarchical Variables / 2 Flat Variables v1 & v2):
  # Merging two flat variables creates a synthetic 2-level hierarchy for Tau-Argus.
  # We construct two symmetric options:
  #   - Option 1 (tab1): Group by v1 first (Level 1: v1 x Total_v2; Level 2: v1 x v2).
  #   - Option 2 (tab2): Group by v2 first (Level 1: Total_v1 x v2; Level 2: v1 x v2).
  # 'write_hrc2' builds the corresponding .hrc tree files from the 2-level correspondence.
  # ----------------------------------------------------------------------------

  # Totals for each variable
  var1_total <- totcode[v1]
  var2_total <- totcode[v2]

  # the different modalities of the 2 variables
  mods1 <- unique(dfs[[v1]])
  mods2 <- unique(dfs[[v2]])

  var1_mods_except_total <- mods1[mods1 != var1_total]
  var2_mods_except_total <- mods2[mods2 != var2_total]

  # EDGE CASE: If a variable has only 1 non-total modality, 'write_hrc2' fails
  # because a hierarchy node requires at least 2 children.
  # We append a dummy modality ("...ZZZ") to satisfy the 2-children constraint.
  if (length(var1_mods_except_total)==1){
    var1_mods_except_total<-c(var1_mods_except_total,paste(var1_mods_except_total,
                                                           "ZZZ", sep = ""))
  }

  if (length(var2_mods_except_total)==1){
    var2_mods_except_total<-c(var2_mods_except_total,paste(var2_mods_except_total,
                                                           "ZZZ", sep = ""))
  }

  var1_mods_n <- length(var1_mods_except_total)
  var2_mods_n <- length(var2_mods_except_total)

  # Helper to construct merged dataframe and 2-level hierarchy correspondence table
  table_and_hierarchy_creator <- function(var_i_total,
                                   var_j_total,
                                   var_i_mods_except_total,
                                   var_j_mods_except_total,
                                   var_j_mods_n,
                                   vi,vj,i)
    {
    # Introduction of notations:
    # let i = 1, j = 2
    # let i = 2, j = 1

    if (i == 1){
      j <- 2
    } else {
      j <- 1
    }

    # Level 1 (Parent): Grouping by primary variable (vi x Total_vj)
    tabi_lvl1 <- expand.grid(
      v1 = sort(rep(var_i_mods_except_total, var_j_mods_n)),
      v2 = var_j_total,
      stringsAsFactors = FALSE
    ) %>% as.data.frame()

    v_i <- paste("v",i,sep="")
    v_j <- paste("v",j,sep="")

    tabi_lvl1$v3 <- paste(tabi_lvl1[[v_i]], tabi_lvl1[[v_j]], sep = sep)

    # Level 2 (Children): Detailed cross-modalities (vi x vj)
    tabi_lvl2 <- expand.grid(
      v1 = var_i_mods_except_total,
      v2 = var_j_mods_except_total,
      stringsAsFactors = FALSE
    ) %>% as.data.frame()

    tabi_lvl2 <- tabi_lvl2[order(tabi_lvl2$v1, tabi_lvl2$v2), ]

    tabi_lvl2$v3 <- paste(tabi_lvl2[[v_i]], tabi_lvl2[[v_j]], sep = sep)

    # Correspondence table mapping parent (Lvl1) to child (Lvl2)
    tabi_corresp <- data.frame(
      Lvl1 = tabi_lvl1$v3,
      Lvl2 = tabi_lvl2$v3,
      stringsAsFactors = FALSE
    )

    # Filter data and concatenate v1 and v2 into new single column
    tabi <- dfs[(dfs[[vi]] != var_i_total) |
                  (dfs[[vi]] == var_i_total & dfs[[vj]] == var_j_total), ]
    tabi[[paste(v1, v2, sep = sep)]]<- paste(tabi[[v1]],tabi[[v2]],sep = sep)

    tabi[[v1]]<-NULL
    tabi[[v2]]<-NULL

    return(list(tabi,tabi_corresp))
  }

  # Build Option 1 (Group by v1 first)
  res1 <-  table_and_hierarchy_creator(var1_total,
                                var2_total,
                                var1_mods_except_total,
                                var2_mods_except_total,
                                var2_mods_n,
                                v1,v2,1)
  tab1 <- res1[[1]]
  tab1_corresp <- res1[[2]]

  # Build Option 2 (Group by v2 first)
  res2 <- table_and_hierarchy_creator(var2_total,
                               var1_total,
                               var2_mods_except_total,
                               var1_mods_except_total,
                               var1_mods_n,
                               v2,v1,2)
  tab2 <- res2[[1]]
  tab2_corresp <- res2[[2]]

  # Write HRC hierarchy files for both options
  hrc_tab1 <- rtauargus::write_hrc2(tab1_corresp,
                                    file_name = paste(dir_name,"/",
                                                      paste("hrc",dfs_name,
                                                            v1,sep = "_"),
                                                      ".hrc",
                                                      sep=""),
                                    adjust_unique_roots = TRUE
  )

  hrc_tab2 <- rtauargus::write_hrc2(tab2_corresp,
                                    file_name = paste(dir_name,"/",
                                                      paste("hrc",dfs_name,
                                                            v2,sep = "_"),
                                                      ".hrc",
                                                      sep=""),
                                    adjust_unique_roots = TRUE
  )

  tabs <- list(tab1, tab2)

  names(tabs) <- c(paste(dfs_name,v1, sep="_"),
                   paste(dfs_name,v2, sep="_"))

  hrcs <- list(hrc_tab1,
               hrc_tab2)

  names(hrcs) <- names(tabs)

  total_total = paste(totcode[v1],
                      totcode[v2],
                      sep = sep)

  alt_tot=list(total_total,
               total_total)

  names(alt_tot)<- names(tabs)

  return(
    list(
        tabs = tabs,
        hrcs = hrcs,
        alt_tot = alt_tot,
        vars = c(v1, v2))
  )
}
