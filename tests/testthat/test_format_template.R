# test data --------------------------------------------------------------------

data(enterprise_template)

baseline_res <- format_template(data = enterprise_template,
                                indicator_column = "INDICATOR",
                                spanning_var_tot = list(
                                  ACTIVITY = "BTSXO_S94",
                                  NUMBER_EMPL = "_T",
                                  LEGAL_FORM = "_T"),
                                field_columns = c("TIME_PERIOD"))

################################################################# WARNING CHECKS
# check that a warning message is returned if one table is actually only margins

test_that("warning for tables with only margins",{

  template <- enterprise_template %>% filter(INDICATOR == "SAL" |
                                             ACTIVITY == "BTSXO_S94",
                                             NUMBER_EMPL == "_T",
                                             LEGAL_FORM == "_T")

  expect_warning(format_template(data = template,indicator_column = "INDICATOR",
                                 spanning_var_tot = list(ACTIVITY = "BTSXO_S94",NUMBER_EMPL = "_T",LEGAL_FORM = "_T"),
                                 field_columns = c("TIME_PERIOD")),
                 "is not broken down by any spanning variable.")
})


################################################################### OTHER CHECKS
# check that it is possible to have only one spanning variable -----------------

test_that("only one spanning variable", {
  template <- enterprise_template %>% filter(ACTIVITY == "BTSXO_S94",LEGAL_FORM == "_T")

  result <- format_template(
    data = template,
    indicator_column = "INDICATOR",
    spanning_var_tot = list(ACTIVITY = "BTSXO_S94", NUMBER_EMPL = "_T", LEGAL_FORM = "_T"),
    field_columns = c("TIME_PERIOD")
  )$metadata

  # select all the columns starting with "spanning_"
  spanning_vars <- grep("^spanning_", names(result), value = TRUE)

  # check there is only one
  expect_equal(length(spanning_vars), 1)
})

