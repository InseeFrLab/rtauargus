#' Group Tables Based on Inclusion Relationships
#'
#' This function regroups tables that are included in each other into clusters,
#' keeping only the tables necessary for protection. Based on the inclusion
#' relationships detected by the `create_edges()` function, the tables are
#' aggregated to minimize redundancy. The output identifies the final tables
#' that need to be protected and provides a mapping of original table names
#' to their respective groups.
#'
#' @param list_split A list of data frames, where each data frame describes the
#'   inclusion relationships (`from` and `to`) between tables in a cluster.
#'   Typically, this is the output of the `create_edges()` function.
#'
#' @return A list of results for each cluster, where each result is a list
#'   containing:
#'   - `tab_finales`: A data frame describing the final relationships between
#'     grouped tables. It includes only the tables necessary for protection.
#'   - `passage_nom_tab`: A data frame mapping original table names (`Original`)
#'     to their respective groups (`Group`).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example data
#' data(metadata_pizza_lettuce)
#'
#' # Convert wide metadata to long format
#' metadata_pizza_lettuce_long <- wide_to_long(metadata_pizza_lettuce)
#'
#' # Identify hierarchical relationships
#' list_hrc_identified <- identify_hrc(metadata_pizza_lettuce_long)
#'
#' # Split tables into clusters
#' list_split <- split_in_clusters(list_hrc_identified)
#'
#' # Detect inclusion relationships
#' list_desc_links <- create_edges(list_split)
#'
#' # Group tables based on inclusion relationships
#' list_translation_tables <- grp_tab_names(list_desc_links)
#'
#' # View structure of the results
#' str(list_translation_tables)
#' }
#'
#' @importFrom dplyr ungroup
#' @importFrom dplyr rowwise
#' @importFrom dplyr rename
#' @importFrom dplyr left_join
#' @importFrom igraph graph_from_data_frame
#' @importFrom igraph which_mutual
grp_tab_names <- function(list_split) {
  list_split %>% map(function(ss_dem) {
    if (!is.null(ss_dem)) {
      # Création du graphe
      graph_ssdem <- igraph::graph_from_data_frame(ss_dem)

      # Identification des arêtes mutuelles
      ss_dem_mutual <- ss_dem %>%
        mutate(mutual = igraph::which_mutual(graph_ssdem))

      # Création d'une table des relations d'inclusion
      tab_eg <- ss_dem_mutual %>%
        mutate(mutual = paste(sort(c(from, to)), collapse = ".")) %>%
        distinct()

      # Détection des inclusions unidirectionnelles
      tab_eg_unique <- tab_eg %>%
        select(from, to) %>%
        mutate(mutual_full = paste(from, to, sep = ".")) %>%
        group_by(from) %>%
        summarise(mutual_full = paste(unique(mutual_full), collapse = "."), .groups = "drop")

      # Création des groupes d'inclusion
      mutual <- setNames(strsplit(tab_eg_unique$mutual_full, "\\."), tab_eg_unique$from)
      noms_tab <- names(mutual)
      l <- list()

      while (length(noms_tab) != 0) {
        tab <- noms_tab[1]
        noms_tab <- setdiff(noms_tab, tab)
        ch_old <- ch_new <- mutual[[tab]]
        linked <- setdiff(ch_old, tab)

        while (length(linked) != 0) {
          noms_tab <- setdiff(noms_tab, linked)
          for (t in linked) {
            ch_new <- unique(sort(c(ch_old, mutual[[t]])))
          }
          linked <- setdiff(ch_new, ch_old)
          ch_old <- ch_new
        }

        nom <- paste(ch_new, collapse = ".")
        l[[nom]] <- ch_new
      }

      # Création du mapping entre noms d'origine et groupes
      passage_nom_tab <- do.call(rbind, lapply(names(l), function(group_name) {
        data.frame(Original = l[[group_name]], Group = group_name, stringsAsFactors = FALSE)
      }))

      # Fusion des relations d'inclusion avec les groupes
      tab_from_to_eg <- ss_dem %>%
        left_join(passage_nom_tab, by = c("from" = "Original")) %>%
        rename(from.eg = Group) %>%
        left_join(passage_nom_tab, by = c("to" = "Original")) %>%
        rename(to.eg = Group) %>%
        mutate(from.eg = ifelse(is.na(from.eg), from, from.eg),
               to.eg = ifelse(is.na(to.eg), to, to.eg)) %>%
        select(from.eg, to.eg)

      # Identification des tables finales
      tab_egaux <- tab_from_to_eg %>%
        filter(from.eg == to.eg) %>%
        distinct(from.eg) %>%
        pull(from.eg)

      tab_fin_chaine_to <- tab_from_to_eg %>%
        filter(from.eg != to.eg) %>%
        distinct(to.eg) %>%
        pull(to.eg)

      tab_fin_chaine_from <- tab_from_to_eg %>%
        filter(from.eg != to.eg) %>%
        distinct(from.eg) %>%
        pull(from.eg)

      liste_finale <- setdiff(c(tab_egaux, tab_fin_chaine_to), tab_fin_chaine_from)
      tab_finales <- tab_from_to_eg %>% filter(to.eg %in% liste_finale)

      list_translation_tables <- list(tab_finales = tab_finales, passage_nom_tab = passage_nom_tab)
      return(list_translation_tables)
    }
  })
}

