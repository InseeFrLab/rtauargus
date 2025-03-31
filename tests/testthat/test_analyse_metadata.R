# test data --------------------------------------------------------------------

data(metadata_pizza_lettuce)

baseline_res <- analyse_metadata(metadata_pizza_lettuce)

################################################################### INPUT CHECKS
# check that an error is returned if the names of the columns don't respect the
# expected format --------------------------------------------------------------
test_that("error message for wrong column name - fixed columns",{
  meta <- metadata_pizza_lettuce %>% rename(table = table_name)

  expect_error(analyse_metadata(meta),"The dataframe is missing one or more required columns")
})

test_that("error message for wrong column name - dynamic columns",{
  meta <- metadata_pizza_lettuce %>% rename(bonjour = hrc_spanning_1)

  expect_error(analyse_metadata(meta),"Missing corresponding")
})

# check that each table has its own unique name
test_that("unique table name", {
  meta <- metadata_pizza_lettuce %>% filter(table_name == "T7") %>%
    mutate(hrc_spanning_1 = as.character(NA)) %>%
    bind_rows(.,.)

  expect_error(analyse_metadata(meta),"Duplicate values found in 'table_name'")
})


# check that each table is named (table_name column) ---------------------------
test_that("error message when some tables are not named", {

  meta <- metadata_pizza_lettuce %>% mutate(table_name = NA)

  expect_error(analyse_metadata(meta),"Each table needs to be named")
}
)

##################################################################### HRC CHECKS
# check that hierarchies on indicators are handled properly --------------------
answer <- data.frame(
  cluster = "france_entreprises_2023.hrc_lettuce",
  table_name = "T11.T7.T9",
  field = "france_entreprises_2023",
  indicator = "LETTUCE",
  spanning_1 = "HRC_NAF",
  spanning_2 = "size",
  spanning_3 = "HRC_LETTUCE^h",
  hrc_spanning_1 = "hrc_naf",
  hrc_spanning_2 = NA,
  hrc_spanning_3 = "hrc_lettuce"
) %>% mutate(across(everything(),as.character))

test_that("hierarchies on indicators", {

  meta <- metadata_pizza_lettuce %>% filter(table_name %in% c("T7","T9","T11"))

  expect_equal(analyse_metadata(meta),answer)
}
)

############################################################### TABLE INCLUSIONS
# one table included in an other -----------------------------------------------
cc <- metadata_pizza_lettuce %>% filter(table_name == "T7") %>%
  mutate(hrc_spanning_1 = as.character(NA)) %>%
  bind_rows(.,.) %>%
  mutate(
    table_name = c("T7","T7_bis"),
    spanning_2 = c("size",as.character(NA)))

analyse_metadata(cc)

# Error in `map()`:
# i In index: 1.
# i With name: france_entreprises_2023.hrc_lettuce.
# Caused by error in `auto_copy()`:
#   ! `x` and `y` must share the same src.
# i `x` is a <data.frame> object.
# i `y` is `NULL`.
# i Set `copy = TRUE` if `y` can be copied to the same source as `x` (may be slow).
# Run `rlang::last_trace()` to see where the error occurred.

# two tables become one table once the hrc_spanning are taken into account -----
answer <- data.frame(
  cluster = "france_entreprises_2023.to_pizza",
  table_name = "T3.T5",
  field = "france_entreprises_2023",
  indicator = "to_pizza",
  spanning_1 = "HRC_NAF",
  hrc_spanning_1 = "hrc_naf"
)

test_that("hierarchies on indicators", {

  meta <- metadata_pizza_lettuce %>% filter(table_name %in% c("T3","T5")) %>%
    select(-c(spanning_2,hrc_spanning_2))

  expect_equal(analyse_metadata(meta),answer)
}
)
