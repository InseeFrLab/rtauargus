# cite --------------------------------------------------------------------

context(":::cite")

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

context(":::df_param_defaut")

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

  expect_warning(
    deux_val_def <- f_vn_totcode(c(val_defaut, V3 = "T3", "drop")),
    "plusieurs valeurs par defaut"
  )

  expect_equal(
    deux_val_def,
    df_attendu(totcode = c(val_defaut, val_defaut, "T3", val_defaut))
  )

  expect_equal(
    f_vn_totcode(NULL),
    df_attendu(totcode = rep(NA, 4))
  )

  expect_equal(
    f_vn_totcode(val_defaut),
    df_attendu(totcode = val_defaut)
  )

  expect_equal(
    f_vn_totcode(c(val_defaut, V1 = "T1", V3 = "T3")),
    df_attendu(totcode = c("T1", val_defaut, "T3", val_defaut))
  )

  expect_equal(
    f_vn_totcode(c(val_defaut, V3 = "T3", V1 = "T1")), # permute ordre liste
    df_attendu(totcode = c("T1", val_defaut, "T3", val_defaut))
  )

  expect_equal(
    f_vn_totcode(c(V3 = "T3", V1 = "T1", val_defaut)), # permute ordre liste
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


# following_dup -----------------------------------------------------------

context(":::following_dup")

following_dup <- rtauargus:::following_dup

test_that("following_dup", {

  expect_equal(
    following_dup(1:3),
    c(FALSE, FALSE, FALSE)
  )

  expect_equal(
    following_dup(c(    1,    1,     2,     1,    1,     3,     2,    2,    2)),
                  c(FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE)
  )

  expect_error(
    following_dup(c(1, NA, NA)),
    "manquante"
  )

})
