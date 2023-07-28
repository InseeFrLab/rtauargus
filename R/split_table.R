#' Title
#'
#' @param res result of splitting by gen_tabs_5_4_to_3 with split = FALSE
#' @param var_fus the fused variables during gen_tabs_5_4_to_3 
#' @param LIMIT the LIMIT of rows of the tables (use a LIMIT for rtauargus )
#'
#' @return list(tabs, hrcs, alt_tot, vars, sep, totcode, hrcfiles, fus_vars)
#' tabs: named list of 3-dimensional dataframes with nested hierarchies
#' hrcs: named list of hrc specific to the variables created during merging to go to dimension 3
#' alt_tot: named list of totals specific to the variables created during merging to go to dimension 3
#' vars: categorical variables of the output dataframes
#' sep: separator used to link the variables
#' totcode: named vector of totals for all categorical variables
#' hrcfiles: named vector of hrc for categorical variables (except the merged one)
#' fus_vars: named vector of vectors representing the merged variables during dimension reduction
#' @export
#'
#' @examples examples in test/test_split_table.R



split_tab <- function(res, var_fus, LIMIT) {
  # table to split because they are too big
  
  res$to_split <- sapply(res$tabs, function(x) nrow(x) > LIMIT)
  table_a_gerer <-names(res$to_split[res$to_split == TRUE]) 
  
  # data to stock
  
  all_tot_stock <- list()
  tabs2 <- list()
  list_vars <- list()
  list_alt_hrcs <- list()
  
  # loop for table to treat
  
  for (t in table_a_gerer) {
    
    # Create of how to split 
    
    hrc <- res$hrcs[[t]][[var_fus]]
    total <- res$alt_tot[[t]][[var_fus]]
    autre_totaux <-res$alt_tot[[t]][names(res$alt_tot[[t]]) != (var_fus)]
    
    res_sdc <-sdcHierarchies::hier_import(inp = hrc, from = "hrc",root = total) %>%
      sdcHierarchies::hier_convert(as = "sdc")
    
    codes_split <- lapply(res_sdc$dims,names)
    n <- length(codes_split)
    
    # Names use for tauargus 
    noms <- lapply(1:n, function(i) paste(t, i, sep = "_"))
    
    # Create tabs by filtering
    tabs <- lapply(codes_split,
                   function(codes) {
                     res <- res$tabs[[t]] %>%
                       filter(res$tabs[[t]][[var_fus]] %in% codes)
                   })
    
    names(tabs) <- noms
    tabs2 <- append(tabs2, tabs)
    
    # alt_tot for tauargus
    
    liste_alt_tot <- setNames(lapply(1:n, function(i) {
      totali <- c(codes_split[[i]][1])
      totali <- setNames(list(totali), var_fus)
      totali <- c(totali, autre_totaux)
      return(totali) }), noms)
    all_tot_stock <- append(all_tot_stock, liste_alt_tot)
    
    # list of variables for the created tables

    var <- replicate(n, list(res$vars[[1]]))
    list_add <- replicate(n, list(res$vars[[1]]))
    names(list_add) <- noms
    list_vars <- append(list_vars, list_add)
    
    # remove hierarchies from the variable we split and naming it
    
    res$hrcs[[t]][[var_fus]] <- NULL
    
    if (length(res$hrcs[[t]]) != 0) {
      
      hrc_e <- list(res$hrcs[[t]])
      names(hrc_e) <- names(res$hrcs[[t]])
      
      alt_hrcs <- replicate(n, hrc_e)
      names(alt_hrcs) <- noms
      
      list_alt_hrcs <- append(list_alt_hrcs, alt_hrcs)
    } 
    }
  
  # adding the names tables we created to the already existing tables 
  
  table <- names(res$tabs[!(names(res$tabs) %in% table_a_gerer)])
  tabs_tot <- append(res$tabs[table], tabs2)
  alt_tot <- append(res$alt_tot[table],all_tot_stock)
  vars <- append(res$vars[table], list_vars)
  hrcs <- append( res$hrcs[table],list_alt_hrcs) 
  if (length(hrcs) == 0) { hrcs <- NULL }
  
  
  res = list(
    tabs = tabs_tot,
    vars = vars,
    sep = res$sep,
    hrcs = hrcs,
    totcode = res$totcode,
    alt_tot = alt_tot,
    hrcfile = res$hrcfile,
    fus_vars = res$fus_vars
  )
  return(res)
}
