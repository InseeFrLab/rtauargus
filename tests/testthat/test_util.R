context("util")


# cite --------------------------------------------------------------------

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


# df_param ----------------------------------------------------------------

test_that("df_param", {

  df_param <- rtauargus:::df_param

  df_AB <-
    purrr::partial(
      data.frame,
      stringsAsFactors = FALSE,
      row.names = c("A", "B"),
      colname = c("A", "B")
    )

  expect_error(
    df_param(list(4)),
    "doivent avoir un nom"
  )

  expect_error(
    df_param(list(A = 4, B = c(b1 = "x"))),
    "doivent etre nommés"
  )

  expect_equal(
    df_param(
      list(
        A = c(a1 = 4),
        B = c(b1 = "x", b2 = "y")
      )
    ),
    df_AB(
      a1 = c(4, NA),
      b1 = c(NA, "x"),
      b2 = c(NA, "y")
    )
  )

  expect_equal(
    df_param(
      list(
        A = 4,
        B = c(b1 = "x")
      )
    ),
    df_AB(b1 = c(NA, "x"))
  )

  # typage (warning ?)
  expect_equal(
    df_param(
      list(
        A = c(param = 4),
        B = c(param = "x")
      )
    ),
    df_AB(param = c("4", "x"))
  )


})
