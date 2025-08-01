journal_add_break_line <- function(journal){
  sep_char_jour <- "-----------------------------------------"
  cat(sep_char_jour, file = journal, fill = TRUE, append = TRUE)
}

journal_add_line <- function(journal,...){
  cat(..., file = journal, fill = TRUE, append = TRUE)
}

#' Manages the secondary secret of a list of tables
#' @inheritParams tab_rtauargus
#' @param list_tables named list of `data.frame` or `data.table` representing the tables to protect
#' @param list_explanatory_vars named list of character vectors of explanatory
#' variables of each table mentionned in list_tables. Names of the list are the same as of the list of tables.
#' @param alt_hrc named list for alternative hierarchies (useful for non nested-hierarchies)
#' @param alt_totcode named list for alternative codes
#' @param ip_start integer: Interval protection level to apply at first treatment of each table
#' @param ip_end integer: Interval protection level to apply at other treatments
#' @param num_iter_max integer: Maximum of treatments to do on each table (default to 10)
#' @param ... other arguments of `tab_rtauargus2()`
#'
#' @return original list of tables. Secret Results of each iteration is added to each table.
#' For example, the result of first iteration is called 'is_secret_1' in each table.
#' It's a boolean variable, whether the cell has to be masked or not.
#'
#' @seealso `tab_rtauargus2`
#'
#' @examples
#' library(rtauargus)
#' library(dplyr)
#' data(turnover_act_size)
#' data(turnover_act_cj)
#' data(activity_corr_table)
#'
#' #0-Making hrc file of business sectors ----
#' hrc_file_activity <- activity_corr_table %>%
#'   write_hrc2(file_name = "hrc/activity")
#'
#' #1-Prepare data ----
#' #Indicate whether each cell complies with the primary rules
#' #Boolean variable created is TRUE if the cell doesn't comply.
#' #Here the frequency rule is freq in (0;3)
#' #and the dominance rule is NK(1,85)
#' list_data_2_tabs <- list(
#'   act_size = turnover_act_size,
#'   act_cj = turnover_act_cj
#' ) %>%
#' purrr::map(
#'   function(df){
#'     df %>%
#'       mutate(
#'         is_secret_freq = N_OBS > 0 & N_OBS < 3,
#'         is_secret_dom = ifelse(MAX == 0, FALSE, MAX/TOT>0.85),
#'         is_secret_prim = is_secret_freq | is_secret_dom
#'       )
#'   }
#' )
#' \dontrun{
#' options(
#'   rtauargus.tauargus_exe =
#'     "Y:/Logiciels/TauArgus/TauArgus4.2.3/TauArgus.exe"
#' )
#' res_1 <- tab_multi_manager(
#'   list_tables = list_data_2_tabs,
#'   list_explanatory_vars = list(
#'     act_size = c("ACTIVITY", "SIZE"),
#'     act_cj = c("ACTIVITY", "CJ")
#'   ),
#'   hrc = c(ACTIVITY = hrc_file_activity),
#'   dir_name = "tauargus_files",
#'   value = "TOT",
#'   freq = "N_OBS",
#'   secret_var = "is_secret_prim",
#'   totcode =  "Total"
#' )
#'
#'
#' # With the reduction dimensions feature
#'
#' data("datatest1")
#' data("datatest2")
#'
#' datatest2b <- datatest2 %>%
#'   filter(cj == "Total", treff == "Total", type_distrib == "Total") %>%
#'   select(-cj, -treff, -type_distrib)
#'
#' str(datatest2b)
#'
#' res <- tab_multi_manager(
#'   list_tables = list(d1 = datatest1, d2 = datatest2b),
#'     list_explanatory_vars = list(
#'         d1 = names(datatest1)[1:4],
#'         d2 = names(datatest2b)[1:2]
#'     ),
#'  dir_name = "tauargus_files",
#'  value = "pizzas_tot_abs",
#'  freq = "nb_obs_rnd",
#'  secret_var = "is_secret_prim",
#'  totcode =  "Total",
#'  split_tab = TRUE
#' )
#'
#' }
#'
#' @importFrom rlang .data
#'
#' @export

tab_multi_manager <- function(
    list_tables,
    list_explanatory_vars,
    dir_name = NULL,
    hrc = NULL,
    alt_hrc = NULL,
    totcode = getOption("rtauargus.totcode"),
    alt_totcode = NULL,
    value = "value",
    freq = "freq",
    secret_var = "is_secret_prim",
    cost_var = NULL,
    suppress = "MOD(1,5,1,0,0)",
    ip_start = 10,
    ip_end = 0,
    num_iter_max = 10,
    split_tab = FALSE,
    nb_tab_option = "smart",
    limit = 14700,
    ...
){
  start_time <- Sys.time()
  dir_name <- if(is.null(dir_name)) getwd() else dir_name
  dir.create(dir_name, recursive = TRUE, showWarnings = FALSE)


  func_to_call <- "tab_rtauargus2"
  .dots = list(...)
  params <- param_function(eval(parse(text=func_to_call)), .dots)
  params$dir_name = dir_name
  params$cost_var = cost_var
  params$value = value
  params$freq = freq
  params$suppress = suppress
  params$suppress = suppress
  params$split_tab = split_tab
  params$nb_tab_option = nb_tab_option
  params$limit = limit

  n_tbx = length(list_tables) # nombre de tableaux

  if(n_tbx == 0){
    stop("Your list of tables is empty !")
  }
  if(n_tbx == 1){
    stop("To protect a single table, please use the function `tab_rtauargus`.")
  }
  if(is.null(names(list_tables))){
    names(list_tables) <- paste0("tab", 1:n_tbx)
    names(list_explanatory_vars) <- paste0("tab", 1:n_tbx)
  }
  noms_tbx <- names(list_tables)
  all_expl_vars <- unique(unname(unlist(list_explanatory_vars)))

  if( (!is.null(hrc)) & is.list(hrc)) hrc <- unlist(hrc)

  if( (!is.null(hrc)) & (length(names(hrc)) == 0)){
    stop("hrc must have names corresponding to the adequate explanatory variables")
  }
  if(length(setdiff(names(hrc), all_expl_vars)) > 0){
    stop("some names in hrc argument are not mentionned in list_explanatory_vars")
  }
  if(!is.null(alt_hrc)){
    if((length(names(alt_hrc)) == 0)){
      stop("alt_hrc must have names corresponding to the adequate tables names")
    }
    if(length(setdiff(names(alt_hrc), noms_tbx)) > 0){
      stop("some names in alt_hrc argument are not mentionned in list_tables")
    }
  }
  if(!is.null(alt_totcode)){
    if((length(names(alt_totcode)) == 0)){
      stop("alt_totcode must have names corresponding to the adequate tables names")
    }
    if(length(setdiff(names(alt_totcode), noms_tbx)) > 0){
      stop("some names in alt_totcode argument are not mentionned in list_tables")
    }
  }

  # list_totcode management
  # first case : list_totcode is one length-character vector :
  # all the expl variables in all the tables have the same value to refer to the total
  if(is.character(totcode)){
    if(length(totcode) == 1){
      list_totcode <- purrr::map(
        list_explanatory_vars,
        function(nom_tab){
          stats::setNames(
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

  purrr::walk(
    names(alt_totcode),
    function(tab){
      purrr::walk(
        names(alt_totcode[[tab]]),
        function(var) list_totcode[[tab]][[var]] <<- alt_totcode[[tab]][[var]]
      )
    }
  )

  noms_vars_init <- c()
  for (tab in list_tables){
    noms_vars_init <- c(noms_vars_init, names(tab))
  }
  noms_vars_init <- noms_vars_init[!duplicated(noms_vars_init)]

  noms_col_T <- stats::setNames(paste0("T_", noms_tbx), noms_tbx)

  table_majeure <- purrr::imap(
    .x = list_tables,
    .f = function(tableau,nom_tab){

      if(!is.null(cost_var)){
        cost_var_tab <- if(cost_var %in% names(tableau)) cost_var else NULL
      }else{
        cost_var_tab <- NULL
      }
      secret_var_tab <- if(!is.null(params$secret_no_pl)) c(secret_var,params$secret_no_pl) else secret_var

      tableau <- as.data.frame(tableau)[, c(list_explanatory_vars[[nom_tab]], value, freq, cost_var_tab, secret_var_tab)]

      if(!is.null(params$secret_no_pl)){
        names(tableau)[names(tableau) == params$secret_no_pl] = "secret_no_pl"
      } else {
        tableau$secret_no_pl <- FALSE
      }

      var_a_ajouter <- setdiff(all_expl_vars, names(tableau))
      for (nom_col in var_a_ajouter){
        tableau[[nom_col]] <- unname(
          purrr::keep(
            list_totcode, function(x) nom_col %in% names(x)
          )[[1]][nom_col]
        )
      }

      tableau[[noms_col_T[[nom_tab]]]] <- TRUE

      return(as.data.frame(tableau))
    }
  )

  # by_vars = setdiff(unique(unlist(purrr::map(table_majeure, names))), noms_col_T)
  by_vars = purrr::reduce(purrr::map(table_majeure, names), intersect)
  table_majeure <- purrr::reduce(
    .x = table_majeure,
    .f = merge,
    by = by_vars,
    all = TRUE
  )

  table_majeure$secret_no_pl_iter <- table_majeure$secret_no_pl
  secret_no_pl_iter <- "secret_no_pl_iter"

  purrr::walk(
    noms_col_T,
    function(col_T){
      e_par <- rlang::env_parent()
      e_par$table_majeure[[col_T]] <- ifelse(
        is.na(e_par$table_majeure[[col_T]]),
        FALSE,
        e_par$table_majeure[[col_T]]
      )
    }
  )

  # Uniformisation des libelles des variables explicatives
  # res_unif <- uniformize_labels(table_majeure, all_expl_vars, hrc, list_totcode)
  # table_majeure <- res_unif$data
  # hrc_unif <- res_unif$hrc_unif

  list_hrc <- purrr::map(
    list_explanatory_vars,
    function(nom_vars){
      purrr::discard(hrc[nom_vars], is.na) %>%  unlist()
    }
  )

  list_hrc <- purrr::map(list_hrc, function(l) if(length(l) == 0) NULL else l)

  purrr::walk(
    names(alt_hrc),
    function(tab){
      purrr::walk(
        names(alt_hrc[[tab]]),
        function(var) list_hrc[[tab]][[var]] <<- alt_hrc[[tab]][[var]]
      )
    }
  )

  # listes de travail

  has_primary_secret <- purrr::map_lgl(
    list_tables,
    function(tab){
      sum(tab[[secret_var]]) != 0
    }
  )
  if(sum(has_primary_secret) == 0){
    message("None of the tables have any primary secret cells")
    return(list_tables)
  }
  todolist <- noms_tbx[has_primary_secret][1]
  remainlist <- noms_tbx[has_primary_secret][-1]

  num_iter_par_tab = stats::setNames(rep(0, length(list_tables)), noms_tbx)
  num_iter_par_tab[!has_primary_secret] <- 1
  num_iter_all = 0

  # common_cells_modified <- as.data.frame(matrix(ncol = length(all_expl_vars)+1))
  # names(common_cells_modified) <- c(all_expl_vars, "iteration")

  n_common_cells_modified <- 0

  journal <- file.path(dir_name,"journal.txt")
  if(file.exists(journal)) invisible(file.remove(journal))
  journal_add_line(journal, "Start time:", format(start_time, "%Y-%m-%d  %H:%M:%S"))
  journal_add_break_line(journal)
  journal_add_line(journal, "Function called to protect the tables:", func_to_call)
  journal_add_line(journal, "Interval Protection Level for primary secret cells:", ip_start)
  journal_add_line(journal, "Interval Protection Level for other iterations:", ip_end)
  journal_add_line(journal, "Nb of tables to treat: ", n_tbx)
  journal_add_break_line(journal)
  journal_add_line(journal, "Tables to treat:", noms_tbx)
  journal_add_break_line(journal)
  journal_add_line(journal, "All explanatory variables:", all_expl_vars)
  journal_add_break_line(journal)
  journal_add_line(journal, "Initialisation work completed")
  journal_add_break_line(journal)
  journal_add_break_line(journal)

  while(length(todolist) > 0 & all(num_iter_par_tab <= num_iter_max)){

    num_iter_all <- num_iter_all + 1
    num_tableau <- todolist[1]
    num_iter_par_tab[num_tableau] <- num_iter_par_tab[num_tableau] + 1
    cat("--- Current table to treat: ", num_tableau, "---\n")

    nom_col_identifiante <- paste0("T_", num_tableau)
    tableau_a_traiter <- which(table_majeure[[nom_col_identifiante]])

    if (num_iter_all == 1){
      var_secret_apriori <- secret_var
    } else {
      var_secret_apriori <- paste0("is_secret_", num_iter_all-1, collapse = "")
    }

    vrai_tableau <- table_majeure[tableau_a_traiter,]

    ex_var <- list_explanatory_vars[[num_tableau]]

    vrai_tableau <- vrai_tableau[,c(ex_var, value, freq,var_secret_apriori,secret_no_pl_iter, cost_var)]


    # Other settings of the function to make secret ----
    params$tabular = vrai_tableau
    params$files_name = num_tableau
    params$explanatory_vars = ex_var
    params$totcode = list_totcode[[num_tableau]]
    params$hrc = list_hrc[[num_tableau]]
    params$secret_var = var_secret_apriori
    params$secret_no_pl = secret_no_pl_iter
    params$suppress = if(
      substr(suppress,1,3) == "MOD" & num_iter_par_tab[num_tableau] != 1
    ){
      # if modular deactivation of singleton and multisingleton after the first iteration
      paste0(
        paste(
          c(strsplit(suppress, split = ",")[[1]][1:2], rep("0",3)), collapse = ","
        ),
        ")"
      )
    }else{
      suppress
    }
    params$ip = if(num_iter_par_tab[num_tableau] == 1) ip_start else ip_end
    # params$safety_rules <- "MAN(0)"

    res <- do.call(func_to_call, params)
    res$is_secret <- res$Status != "V"

    # Statistiques
    prim_stat <- sum(res$Status == "B", na.rm = TRUE)
    sec_stat <- sum(res$Status == "D", na.rm = TRUE)
    valid_stat <- sum(res$Status == "V", na.rm = TRUE)
    denom_stat <- nrow(res)

    res <- subset(res, select = setdiff(names(res), "Status"))

    var_secret <- paste0("is_secret_", num_iter_all)
    table_majeure <- merge(table_majeure, res, all = TRUE)
    table_majeure[[var_secret]] <- table_majeure$is_secret
    table_majeure <- subset(
      table_majeure,
      select = setdiff(names(table_majeure), "is_secret")
    )


    table_majeure[[var_secret]] <- ifelse(
      is.na(table_majeure[[var_secret]]),
      table_majeure[[var_secret_apriori]],
      table_majeure[[var_secret]]
    )

    table_majeure$secret_no_pl_iter <- ifelse(
      table_majeure[[secret_var]],
      table_majeure$secret_no_pl,
      table_majeure[[var_secret]]
    ) #TODO A REVOIR PR CORRIGER LES PL

    lignes_modifs <- which(table_majeure[[var_secret_apriori]] != table_majeure[[var_secret]])

    cur_tab <- paste0("T_", num_tableau)
    common_cells <- unique(
      purrr::map_dfr(
        setdiff(noms_col_T, cur_tab),
        function(col_T){
          table_majeure[table_majeure[[col_T]] & table_majeure[[cur_tab]],]
        }
      )
    )

    # update of common cells that have been modified
    modified <- common_cells[common_cells[[var_secret_apriori]] != common_cells[[var_secret]],all_expl_vars, drop=FALSE]
    # modified <- if(sum(is.na(modified))>0) modified[1,][-1,] else modified
    if(nrow(modified) > 0){
      modified <- cbind(modified, iteration = num_iter_all)
      common_cells_modified <- if(n_common_cells_modified == 0) modified else rbind(common_cells_modified, modified)
      n_common_cells_modified <- n_common_cells_modified + nrow(modified)
    }

    for(tab in noms_tbx){
      nom_col_identifiante <- paste0("T_", tab)
      if( !(tab %in% todolist)
          & (any(table_majeure[[nom_col_identifiante]][lignes_modifs]))
      ){
        todolist <- append(todolist,tab)
        remainlist <- remainlist[remainlist != tab]
      }
    }

    todolist <- todolist[-1]
    if(length(todolist) == 0){
      if(length(remainlist) > 0){
        todolist <- remainlist[1]
        remainlist <- remainlist[-1]
      }
    }

    journal_add_line(journal, num_iter_all, "-Treatment of table", num_tableau)
    journal_add_break_line(journal)
    journal_add_line(journal, "New cells status counts: ")
    journal_add_line(journal, "- apriori (primary) secret:", prim_stat, "(", round(prim_stat/denom_stat*100,1), "%)")
    journal_add_line(journal, "- secondary secret:", sec_stat , "(", round(sec_stat/denom_stat*100,1), "%)")
    journal_add_line(journal, "- valid cells:", valid_stat, "(", round(valid_stat/denom_stat*100,1), "%)")
    journal_add_break_line(journal)
    journal_add_line(journal, "Nb of new common cells hit by the secret:", nrow(modified))
    journal_add_break_line(journal)
    journal_add_break_line(journal)

  }

  # Reconstruire la liste des tableaux d'entrée
  liste_tbx_res <- purrr::imap(
    list_tables,
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
      tab$primary_secret <- tab[[secret_var]]
      tab$total_secret <- tab[[last_secret]]
      tab$secondary_secret <- tab$total_secret & !tab$primary_secret
      tab$valid_cells <- !tab$total_secret
      res <- data.frame(
        tab_name = name,
        primary_secret = sum(tab$primary_secret),
        secondary_secret = sum(tab$secondary_secret),
        total_secret = sum(tab$total_secret),
        valid_cells = sum(tab$valid_cells)
      )
    }
  )

  purrr::iwalk(
    num_iter_par_tab,
    function(num,tab){
      journal_add_line(
        journal,
        "End of iterating after", num, "iterations for", tab
      )
    }
  )
  journal_add_break_line(journal)
  journal_add_line(journal, "Final Summary")
  journal_add_break_line(journal)
  journal_add_line(journal, "Secreted cells counts per table")
  journal_add_break_line(journal)
  purrr::walk(
    noms_tbx,
    function(tab){
      journal_add_line(
        journal,
        "---TAB ", tab, " ---"
      )
      df <- t(stats[stats$tab_name == tab,-1,drop=FALSE])
      suppressWarnings(gdata::write.fwf(df, rownames = TRUE, colnames = FALSE, file = journal, append = TRUE))
      journal_add_break_line(journal)
    }
  )
  journal_add_break_line(journal)
  journal_add_line(journal, "Common cells hit by the secret:")
  if(n_common_cells_modified > 0){
    suppressWarnings(gdata::write.fwf(common_cells_modified, file = journal, append = TRUE))
  }
  journal_add_break_line(journal)
  journal_add_line(journal, "End time: ", format(Sys.time(), "%Y-%m-%d  %H:%M:%S"))
  journal_add_break_line(journal)

  return(liste_tbx_res)
}
