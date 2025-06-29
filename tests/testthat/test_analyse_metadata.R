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
# two tables included in each other
answer <- data.frame(
  cluster = "france_entreprises_2023.to_pizza",
  table_name = "T1.T2",
  field = "france_entreprises_2023",
  indicator = "to_pizza",
  spanning_1 = "HRC_NUTS",
  spanning_2 = "size",
  hrc_spanning_1 = "hrc_nuts",
  hrc_spanning_2 = NA
) %>% mutate(across(everything(),as.character))

test_that("two tables included in each other", {

  meta <- metadata_pizza_lettuce %>% filter(table_name %in% c("T1","T2"))

  expect_equal(analyse_metadata(meta),answer)
})


# one table included in an other -----------------------------------------------
answer <- data.frame(
  cluster = "france_entreprises_2023.to_pizza",
  table_name = "T1.T1_bis",
  field = "france_entreprises_2023",
  indicator = "to_pizza",
  spanning_1 = "nuts2",
  spanning_2 = "size",
  hrc_spanning_1 = NA,
  hrc_spanning_2 = NA
) %>% mutate(across(everything(),as.character))

test_that("one-way table inclusion", {

  meta <- metadata_pizza_lettuce %>% filter(table_name == "T1") %>%
  mutate(hrc_spanning_1 = as.character(NA)) %>%
    bind_rows(.,.) %>%
    mutate(
      table_name = c("T1","T1_bis"),
      spanning_2 = c("size",as.character(NA)))

  expect_equal(analyse_metadata(meta),answer)

})

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
