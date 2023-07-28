#' Transition from 4 to 3 variables by merging two non-hierarchical variables
#'
#' @param dfs data.frame with 4 categorical variables (n >= 2 in the general case)
#' @param nom_dfs name of the data.frame in the list provided by the user
#' @param v1 non-hierarchical categorical variable
#' @param v2 non-hierarchical categorical variable
#' @param totcode named vector of totals for categorical variables
#' @param dir_name folder where to write the hrc files
#' if no folder is specified in hrcfiles
#' @param sep separator used when concatenating variables
#'
#' @return list(tabs, hrcs, alt_tot, vars)
#' tabs : named list of 3-dimensional dataframes (n-1 dimensions in the general case)
#' with nested hierarchies
#' hrc : named list of hrc specific to the variable created via merging
#' alt_tot : named list of totals
#' vars : named list of vectors representing the merged variables
#' during the two steps of dimension reduction
#'
#' @examples
#' library(dplyr)
#'
#' source("R/passage_4_3_cas_2_non_hrc.R",encoding = "UTF-8")
#'
#' data <- expand.grid(
#'   ACT = c("Total", "A", "B", "A1", "A2", "B1", "B2"),
#'   SEX = c("Total", "F", "M","F1","F2","M1","M2"),
#'   AGE = c("Total", "AGE1", "AGE2", "AGE11", "AGE12", "AGE21", "AGE22"),
#'   ECO = c("PIB","Households","Companies"),
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
#'   sdcHierarchies::hier_convert(as = "argus") %>%
#'   slice(-1) %>% 
#'   mutate(levels = substring(paste0(level,name),3)) %>% 
#'   select(levels) %>% 
#'   write.table(file = hrc_act, row.names = F, col.names = F, quote = F)
#'
#' hrc_sex <- "output/hrc_SEX.hrc"
#' sdcHierarchies::hier_create(root = "Total", nodes = c("F","M")) %>% 
#'   sdcHierarchies::hier_add(root = "F", nodes = c("F1","F2")) %>% 
#'   sdcHierarchies::hier_add(root = "M", nodes = c("M1","M2")) %>% 
#'   sdcHierarchies::hier_convert(as = "argus") %>%
#'   slice(-1) %>% 
#'   mutate(levels = substring(paste0(level,name),3)) %>% 
#'   select(levels) %>% 
#'   write.table(file = hrc_sex, row.names = F, col.names = F, quote = F)
#'
#' res1 <- passage_4_3_cas_2_non_hr(dfs = data,
#'                                 nom_dfs = "nom_dfs",
#'                                 v1 = "ECO",v2 = "AGE",
#'                                 totcode = c(ACT = "Total",SEX = "Total",
#'                                             AGE = "Total",ECO = "PIB"),
#'                                 dir_name = "output")
passage_4_3_cas_2_non_hr <- function(
    dfs,
    nom_dfs,
    v1,
    v2,
    totcode,
    dir_name,
    sep = "_")
{
  # the different totals
  var1_total <- totcode[v1]
  var2_total <- totcode[v2]
  
  # the different modalities of the 2 variables
  mods1 <- unique(dfs[[v1]])
  mods2 <- unique(dfs[[v2]])
  
  var1_mods_except_total <- mods1[mods1 != var1_total]
  var2_mods_except_total <- mods2[mods2 != var2_total]
  
  # number of modality for each var
  var1_mods_n <- length(var1_mods_except_total)
  var2_mods_n <- length(var2_mods_except_total)
  
  # generalization creation of the tables with merged variables
  creation_table_3_var <- function(var_i_total,
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
    
    # Construction of the levels for the correspondence table
    tabi_lvl1 <- expand.grid(
      v1 = sort(rep(var_i_mods_except_total, var_j_mods_n)),
      v2 = var_j_total,
      stringsAsFactors = FALSE
    ) %>% as.data.frame()
    
    v_i <- paste("v",i,sep="")
    v_j <- paste("v",j,sep="")
    
    tabi_lvl1$v3 <- paste(tabi_lvl1[[v_i]], tabi_lvl1[[v_j]], sep = sep)
    
    # Creation of the level 2 hierarchy
    tabi_lvl2 <- expand.grid(
      v1 = var_i_mods_except_total,
      v2 = var_j_mods_except_total,
      stringsAsFactors = FALSE
    ) %>% as.data.frame()
    
    tabi_lvl2 <- tabi_lvl2[order(tabi_lvl2$v1, tabi_lvl2$v2), ]
    
    tabi_lvl2$v3 <- paste(tabi_lvl2[[v_i]], tabi_lvl2[[v_j]], sep = sep)
    
    # Creation of the correspondence table
    tabi_corresp <- data.frame(
      Lvl1 = tabi_lvl1$v3,
      Lvl2 = tabi_lvl2$v3,
      stringsAsFactors = FALSE
    )
    
    tabi <- dfs[(dfs[[vi]] != var_i_total) | 
                  (dfs[[vi]] == var_i_total & dfs[[vj]] == var_j_total), ]
    tabi[[paste(v1, v2, sep = sep)]]<- paste(tabi[[v1]],tabi[[v2]],sep = sep)
    
    tabi[[v1]]<-NULL
    tabi[[v2]]<-NULL
    
    return(list(tabi,tabi_corresp))
  }
  
  # We apply the function for "i=1, j=2" then for "i=2,j=1"
  res1 <-  creation_table_3_var(var1_total,
                                var2_total,
                                var1_mods_except_total,
                                var2_mods_except_total,
                                var2_mods_n,
                                v1,v2,1)
  tab1 <- res1[[1]]
  tab1_corresp <- res1[[2]]
  
  res2 <- creation_table_3_var(var2_total,
                               var1_total,
                               var2_mods_except_total,
                               var1_mods_except_total,
                               var1_mods_n,
                               v2,v1,2)
  tab2 <- res2[[1]]
  tab2_corresp <- res2[[2]]
  
  # Construction of hierarchies
  # to do :
  # use file.path()?
  # do not write if the file already exists?
  
  hrc_tab1 <- rtauargus::write_hrc2(tab1_corresp, 
                                    file_name = paste(dir_name,"/",
                                                      paste("hrc",nom_dfs,
                                                            v1,sep = "_"),
                                                      ".hrc",
                                                      sep="")
  )
  
  hrc_tab2 <- rtauargus::write_hrc2(tab2_corresp, 
                                    file_name = paste(dir_name,"/",
                                                      paste("hrc",nom_dfs,
                                                            v2,sep = "_"),
                                                      ".hrc",
                                                      sep="")
  )
  
  tabs <- list(tab1, tab2)
  
  names(tabs) <- c(paste(nom_dfs,v1, sep="_"),
                   paste(nom_dfs,v2, sep="_"))
  
  hrcs <- list(hrc_tab1,
               hrc_tab2)
  
  names(hrcs) <- names(tabs)
  
  total_total = paste(totcode[v1],
                      totcode[v2],
                      sep=sep)
  
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
