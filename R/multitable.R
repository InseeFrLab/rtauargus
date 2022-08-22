library(dplyr)
# source("fonction_traiter2.R") # fonction qui ajoute du "secret" au hasard
# #source("fonction_traiter0.R") # fonction qui appelle tau-argus


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
    ip_start = 1,
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

  ####### Function to use to make secret ######
  .dots = list(...)
  params <- param_function(eval(parse(text=func_to_call)), .dots)
  params$dir_name = dir_name
  params$cost_var = cost_var
  params$value = value
  params$freq = freq


  ####### === A === INITIALISATION

  n_tbx = length(liste_tbx) # nombre de tableaux

  if(is.null(names(liste_tbx))){
    names(liste_tbx) <- paste0("tab", 1:n_tbx)
    names(list_explanatory_vars) <- paste0("tab", 1:n_tbx)
    names(list_hrc) <- paste0("tab", 1:n_tbx)
  }
  noms_tbx <- names(liste_tbx)
  all_expl_vars <- unique(unname(unlist(list_explanatory_vars)))
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

  table_majeure <- purrr::imap(
    .x = liste_tbx,
    .f = function(tableau,nom_tab){
      var_a_ajouter <- setdiff(noms_vars_init, names(tableau))
      for (nom_col in var_a_ajouter){
        tableau[[nom_col]] <- list_totcode[[nom_tab]][nom_col]
      }

      tableau <- tableau %>%
        relocate(all_of(value),
                 all_of(secret_var),
                 all_of(freq),
                 all_of(maxscore), .after = last_col())

      nom_col_Tj <- paste0("T_", nom_tab)
      tableau[[nom_col_Tj]] <- TRUE

      return(tableau)
    }
  )
  table_majeure <- purrr::reduce(.x = table_majeure,
                                 .f = full_join)

  noms_cols_corres <- setdiff(names(table_majeure),noms_vars_init)
  liste_pour_na <- as.list(rep(F,length(noms_cols_corres)))
  names(liste_pour_na) <- noms_cols_corres

  table_majeure[noms_cols_corres] <- tidyr::replace_na(table_majeure[noms_cols_corres],
                                                       replace = liste_pour_na)


  # 2. Préparation des listes de travail
  # i0 = indice_depart_tableau
  # num du tableau par lequel commencer
  # i0 <- trouver_meilleur_point_de_depart()

  todolist <- noms_tbx #list()
  # todolist[[1]] <- noms_tbx[i0]
  # en fait la todolist va juste contenir des numéros de tableaux

  # oublis <- list()
  # oublis <- as.list(noms_tbx[-i0]) #1:n_tbx
  # oublis[i0] <- NULL

  ############################################################################
  ############################################################################
  ####### === B === ITÉRATIONS

  num_iter_par_tab = setNames(rep(0, length(liste_tbx)), noms_tbx)
  num_iter_all = 0
  journal = list()

  while (length(todolist) > 0 & all(num_iter_par_tab <= num_iter_max)){

    num_iter_all <- num_iter_all + 1
    num_tableau <- todolist[1]
    num_iter_par_tab[num_tableau] <- num_iter_par_tab[num_tableau] + 1
    cat("\n #####  Traitement tableau ", num_tableau, " ###########\n")
    # nom_tableau <- names()
    nom_col_identifiante <- paste0("T_", num_tableau)
    tableau_a_traiter <- which(table_majeure[[nom_col_identifiante]])

    # rappel : "todolist[[1]]" est un numéro (de tableau) ;
    # corresp[,num_tableau] est une colonne donnant, pour chaque ligne de nrow(table_majeure),
    # which(corresp...) est donc l'ens. des indices des lignes du tableau n°num_tableau

    # if (num_tableau %in% oublis) oublis[[which(oublis == num_tableau)]] <- NULL


    # 0. juste pour le journal :
    souvenir_todolist <- todolist


    # 1. Traiter & récupérer le masque
    # nom_nouveau_masque <- paste0("is_secret_aj_",num_iter, collapse = "")
    # nom_ancien_masque <- ifelse(
    #   num_iter > 1,
    #   paste0("is_secret_aj_", num_iter-1, collapse = ""),
    #   is_secret_primaire
    # )
    # table_majeure[[nom_nouveau_masque]] <- table_majeure[[nom_ancien_masque]]

    var_secret_apriori <- ifelse(
      num_iter_all > 1,
      paste0("is_secret_", num_iter_all-1, collapse = ""),
      secret_var
    )
    vrai_tableau <- table_majeure[tableau_a_traiter,]

    ex_var <- list_explanatory_vars[[num_tableau]]

    vrai_tableau <- vrai_tableau %>%
      select(all_of(c(ex_var, value, freq, maxscore, var_secret_apriori )))

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
    print(table(res$Status))
    res <- subset(res, select = -Status)

    # res <- traiter(
    #   vrai_tableau,
    #   explanatory_vars = ex_var,
    #   hrc = list_hrc[[num_tableau]],
    #   value = value,
    #   freq = freq,
    #   maxscore = maxscore,
    #   secret_var = var_secret_apriori,
    #   totcode = list_totcode[[num_tableau]]
    # )
    var_secret <- paste0("is_secret_", num_iter_all)
    table_majeure <- merge(table_majeure, res, all = TRUE)
    table_majeure[[var_secret]] <- table_majeure$is_secret
    table_majeure <- subset(table_majeure, select = -is_secret)
    # Rappel : traiter(...) renvoie un masque de secret (une colonne de booléens)
    # avec une ligne pour chaque ligne du tableau d'origine

    # Rmq : ici on crée juste à chaque étape une nouvelle colonne de secret ajusté
    # global. On ne crée pas la colonne intermédiaire avec masque local + des NA.
    # TODO si c'est nécessaire.

    # 2. Propager

    # var_secret_aj <- paste0("is_secret_aj_", num_iter)
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
      if (! (tab %in% todolist)
          & (any(table_majeure[[nom_col_identifiante]][lignes_modifs]))
      ) # TODO parenthéser la négation
        todolist <- append(todolist,tab)
    }

    todolist <- todolist[-1]
    # Rmq : à ce stade, todolist[[1]] est le tableau qu'on est en train de
    # traiter, donc son numéro ne va pas être ajouté à la todolist même si
    # (évidemment) les lignes modifiées lui appartiennent.

    # Rmq : c'est optimisé comme un tractopelle sur un circuit de formule 1

    #Intermède : update du journal
    # on enlève de la file ce tableau

    # 3. Sonder
    # if (length(todolist) == 0 & length(oublis) > 0){
    #   todolist[1] <- oublis[1]
    #   oublis[1] <- NULL
    # }
    # journal[[paste0("step_",num_iter)]] <-
    #   list(
    #     explique = paste0(
    #       "Le tableau en cours est le ",
    #       num_tableau,
    #       # ". Au debut de l'etape, la file contenait [",
    #       # paste(unlist(souvenir_todolist), collapse = ", "),
    #       # "] et maintenant elle contient [",
    #       # paste(unlist(todolist), collapse = ", "),
    #       # "]. Cette etape a modifie ", length(lignes_modifs), " lignes. ",
    #       # "Tableaux non visites : ",
    #       # ifelse(length(oublis) > 0,
    #       #        paste(unlist(oublis), collapse = ", "),
    #       #        "(plus aucun)")
    #       collapse = " "),
    #     tableau_en_cours = num_tableau
    #     # lignes_concernees = tableau_a_traiter,
    #     # lignes_touchees = lignes_modifs
    #   )
  }

  purrr::iwalk(
    num_iter_par_tab,
    function(num,tab) cat("End of iterating after ", num, " iterations for ", tab, "\n")
  )
  ############################################################################
  ############################################################################
  ####### === C === FINALISATION

  # names(table_majeure)[ncol(table_majeure)] <- "is_secret_final"
  #
  # # Calcul des stats de secret en nombre de cases touchées & en valeur totale
  # valeur_tot = sum(table_majeure[[value]])
  # nombre_cases_tot = nrow(table_majeure)
  # quantification <- data.frame(
  #   prop_cases_secret = round(c(sum(table_majeure[["is_secret_primaire"]]),
  #                               sum(table_majeure[["is_secret_final"]]),
  #                               sum(!table_majeure[["is_secret_final"]]))/nombre_cases_tot,2),
  #   prop_masse_secret = round(c(sum(table_majeure[[value]][which(table_majeure[["is_secret_primaire"]])]),
  #                               sum(table_majeure[[value]][which(table_majeure[["is_secret_final"]])]),
  #                               sum(table_majeure[[value]][which(!table_majeure[["is_secret_final"]])])
  #   )/valeur_tot,2)
  # )
  # rownames(quantification) <- c("Secret primaire", "Secret secondaire", "Diffusable")

  # Reconstruire la liste des tableaux d'entrée
  liste_tbx_res <- purrr::imap(
    liste_tbx,
    function(tab,nom){
      expl_vars <- list_explanatory_vars[[nom]]
      tab_rows <- table_majeure[[paste0("T_", nom)]]
      merge(tab, table_majeure[tab_rows, names(table_majeure) %in% c(expl_vars, paste0("is_secret_", 1:100))], all.x = TRUE, all.y = FALSE, by = expl_vars)
    }
  )

  # journal <- append(journal, list(quantification), after = 0)
  # journal <- append(journal, list(table_majeure), after = 0)
  # journal <- append(journal, list(liste_tbx2), after = 0)
  #
  # names(journal)[1:3] <- c("liste_tbx", "table_majeure", "quantification")
  # return(journal)

  return(liste_tbx_res)
}
