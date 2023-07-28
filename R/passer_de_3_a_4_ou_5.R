

#' Function to reverse the process of dimension reduction
#' @param masq a list of data.frames on which the secret has been applied
#' @param res the result of the dimension reduction function (to retrieve
#' the merged variables) and the separator (sep).
#'
#' @return
#' @export
#'
#' @examples
#' library(dplyr)
#' library(stringr)
#' 
#' source("R/reduce_dims.R")
#' source("R/passage_5_3.R",encoding = "UTF-8")
#' source("R/passage_4_3_cas_0_non_hrc.R",encoding = "UTF-8")
#' source("R/passage_4_3_cas_1_non_hrc.R",encoding = "UTF-8")
#' source("R/passage_4_3_cas_2_non_hrc.R",encoding = "UTF-8")
#' source("R/passage_4_3.R",encoding = "UTF-8")
#' source("R/choisir_sep.R",encoding = "UTF-8")
#' source("R/format.R",encoding = "UTF-8")
#' source("R/length_tabs.R",encoding = "UTF-8")
#' source("R/nb_tab.R",encoding = "UTF-8")
#' source("R/chercher_combinaison_variable_a_fusionner.R",encoding = "UTF-8")
#' source("R/passer_de_3_a_4_ou_5.R",encoding = "UTF-8")
#' 
#' # Examples with dimension 4
#' 
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
#' # Result of the function by forcing some variables to be merged
#' res_red_dim <- gen_tabs_5_4_to_3(
#'   dfs = data,
#'   nom_dfs = "tab",
#'   totcode = c(SEX="Total",AGE="Total", GEO="Total", ACT="Total"),
#'   hrcfiles = c(ACT = hrc_act),
#'   sep_dir = TRUE,
#'   hrc_dir = "output",
#'   vars_a_fusionner = c("ACT","GEO")
#' )
#' 
#' res1 <- passer_a_4_ou_5(masq = res_red_dim$tabs, res = res_red_dim)
#' dim(setdiff(res1,data))[1] == 0
#' 
#' # return TRUE 
#' # We have exactly the sames cases in the datatable after splitting and unsplitting data
#' 
#' # Exemple dimension 5
#' 
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
#' # function's result
#' 
#' res_red_dim <- gen_tabs_5_4_to_3(
#'   dfs = data,
#'   nom_dfs = "tab",
#'   totcode = c(SEX="Total_S",AGE="Ensemble", GEO="Total_G", ACT="Total_A", ECO = "PIB"), 
#'   hrcfiles = c(ACT = hrc_act, GEO = hrc_geo),
#'   sep_dir = TRUE,
#'   hrc_dir = "output"
#' )
#'
#' res2 <- passer_a_4_ou_5(masq = res_red_dim$tabs, res = res_red_dim)
passer_a_4_ou_5 <- function(masq, res) {
  
  require(stringr)
  sep <- res$sep
  sep_regex <- gsub("([+])", "\\\\\\1", sep)
  
 
  
  # Unique values from 'masq' (a list) are concatenated into a data frame
  
  masq_liste_empilee <- unique(do.call("rbind",  unname(masq)))
  
  if (class(res$fus_vars) == "character") {
    # Case with 4 categorical variables
    # variable
    
    v1 <- res$fus_vars[1]
    v2 <- res$fus_vars[2]
    
    v1_v2 <- paste(v1, v2, sep = sep)
    
    result <- separer4_3(masq_liste_empilee, v1, v2,v1_v2, sep_regex) 
    return(result)}
  
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
    split1 <- separer4_3(masq_liste_empilee, v1, v2, v1_v2, sep_regex)
    # Further split based on 'v3', 'v4', and 'v3_v4'
    result <- separer4_3(split1, v3, v4, v3_v4, sep_regex)
    
  } else {
    # Case of fusion with an already fused variable
    v3_v4 <- paste(v3, v4, sep = sep)  
    
    if(v1_v2 == v3){
      # Split based on 'v1', 'v2', and 'v4' using 'separer5_3' function
      result<-separer5_3(masq_liste_empilee, v1,v2, v4, v3_v4, sep_regex)
    }else{
      # Split based on 'v1', 'v2', and 'v3' using 'separer5_3' function
      result<-separer5_3(masq_liste_empilee, v1,v2,v3, v3_v4, sep_regex)
      
    }
    
  }
  
  return(result)
}



# Function for splitting the merged variable v1_v2_v3 into v1, v2, and v3
separer5_3 <- function(df, v1, v2, v3,v3_v4, sep_regex) {
  splits <- strsplit(df[[v3_v4]], split = sep_regex)
  df[[v3]] <- sapply(splits, `[`, 1)
  df[[v1]] <- sapply(splits, `[`, 2)
  df[[v2]] <- sapply(splits, `[`, 3)
  df[[v3_v4]] <- NULL
  df
}


# Function for splitting the merged variable v1_v2 into v1 and v2 
separer4_3 <- function(df, v1, v2, v1_v2, sep_regex) {
  splits <- strsplit(df[[v1_v2]], split = sep_regex)  
  df[[v1]] <- sapply(splits, `[`, 1)
  df[[v2]] <- sapply(splits, `[`, 2)
  df[[v1_v2]] <- NULL
  df
}
