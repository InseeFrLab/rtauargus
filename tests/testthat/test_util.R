# cite --------------------------------------------------------------------

context("cite")

test_that("cite", {

  cite <- rtauargus:::cite

  expect_equal(
    cite(c("A", "", "C")),
    c("\"A\"", "", "\"C\"")
  )

  expect_equal(
    cite(c("A", "", "C"), guillemet = "*"),
    c("*A*", "", "*C*")
  )

  expect_equal(
    cite(c("A", "", "C"), ignore_vide = FALSE),
    c("\"A\"", "\"\"", "\"C\"")
  )

})


# df_param_defaut ---------------------------------------------------------

context("df_param_defaut")

test_that("df_param_defaut", {

  df_param_defaut <- rtauargus:::df_param_defaut

  # fonctions préremplies ...................................

  vn <- sprintf("V%i", 1:4)

  f_vn <- purrr::partial(df_param_defaut, varnames = vn)
  f_vn_totcode <- purrr::partial(f_vn, param_name = "totcode")

  df_attendu <-
    purrr::partial(
      data.frame,
      colname = vn,
      stringsAsFactors = FALSE,
      row.names = vn
    )

  # expect_ ................................................

  val_defaut <- "---"

  expect_equal(
    f_vn_totcode(val_defaut),
    df_attendu(totcode = val_defaut)
  )

  expect_equal(
    f_vn_totcode(c(val_defaut, V1 = "T1", V3 = "T3")),
    df_attendu(totcode = c("T1", val_defaut, "T3", val_defaut))
  )

  expect_equal(
    f_vn_totcode(c(val_defaut, V3 = "T3", V1 = "T1")),  # permutation ordre dans liste
    df_attendu(totcode = c("T1", val_defaut, "T3", val_defaut))
  )

  expect_equal(
    f_vn_totcode(c(V3 = "T3", V1 = "T1", val_defaut)), # permutation ordre dans liste
    df_attendu(totcode = c("T1", val_defaut, "T3", val_defaut))
  )

  expect_equal(
    f_vn("param1", c(V1 = "T1", V3 = "T3")), # pas de val defaut (NA)
    df_attendu(param1 = c("T1", NA, "T3", NA))
  )

  expect_equal(
    f_vn("param1", c(V1 = "T1", V3 = "T3")), # pas de val defaut (NA)
    df_attendu(param1 = c("T1", NA, "T3", NA))
  )

  expect_equal(
    f_vn("param1", c(V1 = "T1", V3 = 3)), # (types différents)
    df_attendu(param1 = c("T1", NA, "3", NA))
  )

  expect_equal(
    f_vn("param1", c(V1 = 1, V3 = 3)), # (types identiques)
    df_attendu(param1 = c(1, NA, 3, NA))
  )

})
