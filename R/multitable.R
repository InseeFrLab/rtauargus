library(dplyr)
# source("fonction_traiter2.R") # fonction qui ajoute du "secret" au hasard
# #source("fonction_traiter0.R") # fonction qui appelle tau-argus

journal_add_break_line <- function(journal){
  sep_char_jour <- "-----------------------------------------"
  cat(sep_char_jour, file = journal, fill = TRUE, append = TRUE)
}

journal_add_line <- function(journal,...){
  cat(..., file = journal, fill = TRUE, append = TRUE)
}


multi_linked_tables <- function(
    liste_tbx,
    list_explanatory_vars,
    list_hrc,
    dir_name = NULL,
    totcode = "total",
    value = "value",
    freq = "freq",
    cost_var = NULL,
    secret_var = "is_secret_prim",
    func_to_call = "tab_rtauargus2",
    ip_start = 10,
    ip_end = 0,
    num_iter_max = 1000,
    ...
){

  #' Attendu :
  #' - liste_tbx : une liste de tableaux à secrétiser
  #' - list_explanatory_vars : une liste de même longueur, où
  #' list_explanatory_vars[[j]] = c(...les noms des vars catégorielles du tableau j...)
  #' - freq, maxscore, is_secret_primaire : les colonnes de travail pour tau-Argus
  #' - totcode : c'est important que le code des totaux soit le même dans toutes les
  #' tables et pour toutes les variables
  #' - indice_depart_tableau : par quel n° de tableau on commence. Si un jour on a une
  #' idée pertinente sur comment choisir d'où on commence, il faudrait aussi l'implémenter
  #' dans la suite du code : quand on ajoute 3 trucs à la file, dans quel ordre ; quand
  #' on va piocher un tableau oublié, par lequel on commence ; etc.

  ############################################################################
  ############################################################################
  dir_name <- if(is.null(dir_name)) getwd() else dir_name
  dir.create(dir_name, recursive = TRUE, showWarnings = FALSE)
  journal <- file.path(dir_name,"journal.txt")
  if(file.exists(journal)) invisible(file.remove(journal))

  # cat("Start time: ", format(Sys.time(), "%Y-%m-%d  %H:%M:%S"), "\n", file = journal, fill = TRUE, append = TRUE)
  journal_add_line(journal, "Start time: ", format(Sys.time(), "%Y-%m-%d  %H:%M:%S"))
  journal_add_break_line(journal)

  ####### Function to use to make secret ######
  .dots = list(...)
  params <- param_function(eval(parse(text=func_to_call)), .dots)
  params$dir_name = dir_name
  params$cost_var = cost_var
  params$value = value
  params$freq = freq

  journal_add_line(journal, "Function called to protect the tables: ", func_to_call)
  journal_add_line(journal, "Interval Protection Level for first iteration: ", ip_start)
  journal_add_line(journal, "Interval Protection Level for other iterations: ", ip_end)

  ####### === A === INITIALISATION

  n_tbx = length(liste_tbx) # nombre de tableaux
  journal_add_line(journal, "Nb of tables to treat: ", n_tbx)
  journal_add_break_line(journal)


  if(is.null(names(liste_tbx))){
    names(liste_tbx) <- paste0("tab", 1:n_tbx)
    names(list_explanatory_vars) <- paste0("tab", 1:n_tbx)
    names(list_hrc) <- paste0("tab", 1:n_tbx)
  }
  noms_tbx <- names(liste_tbx)
  journal_add_line(journal, "Tables to treat: ", noms_tbx)
  journal_add_break_line(journal)
  all_expl_vars <- unique(unname(unlist(list_explanatory_vars)))
  journal_add_line(journal, "All explanatory variables: ", all_expl_vars)
  journal_add_break_line(journal)
  # list_totcode management
  # first case : list_totcode is one length-character vector :
  # all the expl variables in all the tables have the same value to refer to the total
  if(is.character(totcode)){
    if(length(totcode) == 1){
      list_totcode <- purrr::map(
        list_explanatory_vars,
        function(nom_tab){
          setNames(
            rep(totcode, length(nom_tab)),
            nom_tab
          )
        }
      )
    }else if(length(totcode) == length(all_expl_vars)){
      if(is.null(names(totcode))){
        stop("totcode of length > 1 must have names (explanatory_vars)")
      }else{
        if(!all(sort(names(totcode)) == sort(all_expl_vars))){
          stop("Names of explanatory vars mentioned in totcode are not consistent with those used in list_explanatory_vars")
        }else{
          list_totcode <- purrr::map(
            list_explanatory_vars,
            function(nom_vars){
              totcode[nom_vars]
            }
          )
        }
      }
    }else{
      stop("totcode has to be a character vector of length 1 or a named vector of length equal to the number of unique explanatory vars")
    }
  }else{
    stop("totcode has to be a character vector of length 1 or a named vector of length equal to the number of unique explanatory vars")
  }

  # 1. Mise en forme préalable

  noms_vars_init <- c()
  for (tab in liste_tbx){
    noms_vars_init <- c(noms_vars_init, names(tab))
  }
  noms_vars_init <- noms_vars_init[!duplicated(noms_vars_init)]

  noms_col_T <- setNames(paste0("T_", noms_tbx), noms_tbx)

  table_majeure <- purrr::imap(
    .x = liste_tbx,
    .f = function(tableau,nom_tab){

      tableau <- tableau[, c(list_explanatory_vars[[nom_tab]], value, freq, cost_var, secret_var)]

      var_a_ajouter <- setdiff(all_expl_vars, names(tableau))
      for (nom_col in var_a_ajouter){
        tableau[[nom_col]] <- unname(
          purrr::keep(
            list_totcode, function(x) nom_col %in% names(x)
          )[[1]][nom_col]
        )
      }

      tableau[[noms_col_T[[nom_tab]]]] <- TRUE

      return(tableau)
    }
  )

  by_vars = setdiff(unique(unlist(purrr::map(table_majeure, names))), noms_col_T)
  table_majeure <- purrr::reduce(
    .x = table_majeure,
    .f = merge,
    by = by_vars,
    all = TRUE
  )

  # noms_cols_corres <- setdiff(names(table_majeure),noms_vars_init)
  liste_pour_na <- setNames(as.list(rep(F,length(noms_col_T))), noms_col_T)

  table_majeure[noms_col_T] <- tidyr::replace_na(table_majeure[noms_col_T],
                                                 replace = liste_pour_na)

  # 2. Préparation des listes de travail
  todolist <- noms_tbx

  ############################################################################
  ############################################################################
  ####### === B === ITÉRATIONS

  num_iter_par_tab = setNames(rep(0, length(liste_tbx)), noms_tbx)
  num_iter_all = 0

  journal_add_line(journal, "Initialisation work completed")
  journal_add_break_line(journal)
  journal_add_break_line(journal)

  common_cells_modified <- as.data.frame(matrix(ncol = length(all_expl_vars)+1))
  names(common_cells_modified) <- c(all_expl_vars, "iteration")

  while (length(todolist) > 0 & all(num_iter_par_tab <= num_iter_max)){

    num_iter_all <- num_iter_all + 1
    num_tableau <- todolist[1]
    num_iter_par_tab[num_tableau] <- num_iter_par_tab[num_tableau] + 1
    cat("\n #####  Traitement tableau ", num_tableau, " ###########\n")
    journal_add_line(journal, num_iter_all, "-Treatment of table ", num_tableau)
    journal_add_break_line(journal)

    # nom_tableau <- names()
    nom_col_identifiante <- paste0("T_", num_tableau)
    tableau_a_traiter <- which(table_majeure[[nom_col_identifiante]])

    # 1. Traiter & récupérer le masque

    var_secret_apriori <- ifelse(
      num_iter_all > 1,
      paste0("is_secret_", num_iter_all-1, collapse = ""),
      secret_var
    )
    vrai_tableau <- table_majeure[tableau_a_traiter,]

    ex_var <- list_explanatory_vars[[num_tableau]]

    vrai_tableau <- vrai_tableau %>%
      select(all_of(c(ex_var, value, freq, var_secret_apriori )))

    # Other settings of the function to make secret ----
    params$tabular = vrai_tableau
    params$files_name = num_tableau
    params$explanatory_vars = ex_var
    params$totcode = list_totcode[[num_tableau]]
    params$hrc = list_hrc[[num_tableau]]
    params$secret_var = var_secret_apriori
    params$ip = if(num_iter_par_tab[num_tableau] == 1) ip_start else ip_end

    res <- do.call(func_to_call, params)
    res$is_secret <- res$Status != "V"
    journal_add_line(journal, "New cells status counts: ")
    journal_add_line(journal, "- apriori (primary) secret: ", table(res$Status)["B"], "(", round(table(res$Status)["B"]/nrow(res)*100,1), "%)")
    journal_add_line(journal, "- secondary secret: ", table(res$Status)["D"], "(", round(table(res$Status)["D"]/nrow(res)*100,1), "%)")
    journal_add_line(journal, "- valid cells: ", table(res$Status)["V"], "(", round(table(res$Status)["V"]/nrow(res)*100,1), "%)")
    journal_add_break_line(journal)
    # print(table(res$Status))
    res <- subset(res, select = -Status)

    var_secret <- paste0("is_secret_", num_iter_all)
    table_majeure <- merge(table_majeure, res, all = TRUE)
    table_majeure[[var_secret]] <- table_majeure$is_secret
    table_majeure <- subset(table_majeure, select = -is_secret)

    table_majeure[[var_secret]] <- ifelse(
      is.na(table_majeure[[var_secret]]),
      table_majeure[[var_secret_apriori]],
      table_majeure[[var_secret]]
    )

    lignes_modifs <- which(table_majeure[[var_secret_apriori]] != table_majeure[[var_secret]])

    for (tab in noms_tbx){
      # pour tout tableau j, si j n'est pas déjà dans la todolist
      # et s'il y a au moins 1 ligne de j qui a été modifiée, ajouter j à la todolist
      nom_col_identifiante <- paste0("T_", tab)
      if( !(tab %in% todolist)
          & (any(table_majeure[[nom_col_identifiante]][lignes_modifs]))
      ){
        todolist <- append(todolist,tab)
      }
    }

    todolist <- todolist[-1]

    cur_tab <- paste0("T_", num_tableau)
    common_cells <- purrr::map_dfr(
      setdiff(noms_col_T, cur_tab),
      function(col_T){
        table_majeure[table_majeure[[col_T]] & table_majeure[[cur_tab]],]
      }
    ) %>% unique()

    modified <- common_cells[common_cells[[var_secret_apriori]] != common_cells[[var_secret]],all_expl_vars]
    if(nrow(modified)>0){
      common_cells_modified <- rbind(
        common_cells_modified,
        cbind(
          modified,
          iteration = num_iter_all
        )
      )
    }

    journal_add_line(journal, "Nb of new common cells hit by the secret:", nrow(modified))
    journal_add_break_line(journal)
    journal_add_break_line(journal)
    # print(common_cells_modified)
  }

  purrr::iwalk(
    num_iter_par_tab,
    function(num,tab) journal_add_line(journal, "End of iterating after ", num, " iterations for ", tab, "\n")
  )
  ############################################################################
  ############################################################################
  ####### === C === FINALISATION

  # Reconstruire la liste des tableaux d'entrée
  liste_tbx_res <- purrr::imap(
    liste_tbx,
    function(tab,nom){
      expl_vars <- list_explanatory_vars[[nom]]
      tab_rows <- table_majeure[[paste0("T_", nom)]]
      secret_vars <- names(table_majeure)[grep("^is_secret_[1-9]", names(table_majeure))]
      secret_vars <- secret_vars[order(as.integer(gsub("is_secret_", "", secret_vars)))]
      res <- merge(
        tab,
        table_majeure[tab_rows, c(expl_vars, secret_vars)],
        all.x = TRUE, all.y = FALSE, by = expl_vars
      )
    }
  )
  last_secret <- paste0("is_secret_", num_iter_all)

  stats <- purrr::imap_dfr(
    liste_tbx_res,
    function(tab, name){
      # require(data.table)
      tab$primary_secret <- tab[[secret_var]]
      tab$total_secret <- tab[[last_secret]]
      tab$secondary_secret <- tab$total_secret & !tab$primary_secret
      tab$valid_cells <- !tab$total_secret
      # tab <- data.table(tab)
      res <- data.frame(
        tab_name = name,
        primary_secret = sum(tab$primary_secret),
        secondary_secret = sum(tab$secondary_secret),
        total_secret = sum(tab$total_secret),
        valid_cells = sum(tab$valid_cells)
        # tab[,lapply(.SD, sum), .SDcols = c("primary_secret","secondary_secret","total_secret", "valid_cells")]
      )
    }
  )

  journal_add_break_line(journal)
  journal_add_line(journal, "Final Summary")
  journal_add_break_line(journal)
  journal_add_line(journal, "Secreted cells counts per table")
  suppressWarnings(gdata::write.fwf(stats, file = journal, append = TRUE))
  journal_add_break_line(journal)
  journal_add_break_line(journal)
  journal_add_line(journal, "Common cells hit by the secret:")
  # width <- max(nchar(colnames(common_cells_modified)), sapply(common_cells_modified[-1,], function(var) max(nchar(var))))
  suppressWarnings(gdata::write.fwf(common_cells_modified[-1,], file = journal, append = TRUE))
  journal_add_break_line(journal)
  journal_add_line(journal, "End time: ", format(Sys.time(), "%Y-%m-%d  %H:%M:%S"))
  journal_add_break_line(journal)

  return(liste_tbx_res)
}
