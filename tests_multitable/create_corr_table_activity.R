
# devtools::load_all()

library(dplyr)

a <- sdcHierarchies::hier_import("tests_multitable/legumes.hrc", from = "hrc")
# hier_export(a, as = "argus", path = "tests_multitable/activity.hrc")

activity_corr_table <- purrr::reduce(
  purrr::map(
    sort(unique(a$level))[-1],
    function(lev){
      res <- a %>% filter(level == lev) %>% select(root, leaf)
      names(res) <- paste0("V", (lev-1:0))
      res
    }
  ),
  full_join
) %>%
  mutate(V4 = ifelse(is.na(V4), V3, V4)) %>%
  select(-V1) %>%
  as.data.frame()
names(activity_corr_table) <- c("A10","A21","A88")

save(activity_corr_table, file = "tests_multitable/activity_corr_table.RData")
