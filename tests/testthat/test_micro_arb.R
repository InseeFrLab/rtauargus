# suppr_writetable --------------------------------------------------------

context("suppr_writetable")

suppr_writetable <- rtauargus:::suppr_writetable
out <- c("t1.csv",  "tmp/t2.csv", "~/t3.csv")
outn <- normalizePath(out, mustWork = FALSE)

test_that("suppress unique", {

  expect_equal(
    suppr_writetable(
      "GH(###, 100)",
      linked = FALSE,
      output_names = out[1:2],
      output_type = "2",
      output_options = ""
    ),
    c(
      sprintf("<SUPPRESS> GH(1,100)\n<WRITETABLE> (1,2,,\"%s\")", outn[1]),
      sprintf("<SUPPRESS> GH(2,100)\n<WRITETABLE> (2,2,,\"%s\")", outn[2])
    )
  )

  expect_equal(
    suppr_writetable(
      "MOD(0,3)",
      linked = FALSE,
      output_names = out,
      output_type = "2",
      output_options = "AS+"
    ),
    c(
      sprintf("<SUPPRESS> MOD(1,3)\n<WRITETABLE> (1,2,AS+,\"%s\")", outn[1]),
      sprintf("<SUPPRESS> MOD(2,3)\n<WRITETABLE> (2,2,AS+,\"%s\")", outn[2]),
      sprintf("<SUPPRESS> MOD(3,3)\n<WRITETABLE> (3,2,AS+,\"%s\")", outn[3])
    )
  )

  expect_equal(
    suppr_writetable(
      "MOD(n)", # un seul parametre (num tableau)
      linked = FALSE,
      output_names = out,
      output_type = "2",
      output_options = c("", "AS+", "AS+SE+")
    ),
    c(
      sprintf("<SUPPRESS> MOD(1)\n<WRITETABLE> (1,2,,\"%s\")", outn[1]),
      sprintf("<SUPPRESS> MOD(2)\n<WRITETABLE> (2,2,AS+,\"%s\")", outn[2]),
      sprintf("<SUPPRESS> MOD(3)\n<WRITETABLE> (3,2,AS+SE+,\"%s\")", outn[3])
    )
  )


})

test_that("suppress multiple", {

  expect_equal(
    suppr_writetable(
      c("GH(1,100)", "MOD(2)", "GH(###,100)"),
      linked = FALSE,
      output_names = out,
      output_type = "2",
      output_options = ""
    ),
    c(
      sprintf("<SUPPRESS> GH(1,100)\n<WRITETABLE> (1,2,,\"%s\")", outn[1]),
      sprintf("<SUPPRESS> MOD(2)\n<WRITETABLE> (2,2,,\"%s\")", outn[2]),
      sprintf("<SUPPRESS> GH(###,100)\n<WRITETABLE> (3,2,,\"%s\")", outn[3])
    )
  )

})

test_that("linked", {

  expect_error(
    suppr_writetable(
      c("GH(1,100)", "MOD(2)", "GH(###,100)"),
      linked = TRUE,
      output_names = out,
      output_type = "2",
      output_options = ""
    ),
    "un seul suppress permis quand linked = TRUE"
  )

  expect_equal(
    suppr_writetable(
      "GH(.,100)",
      linked = TRUE,
      output_names = out,
      output_type = c("2", "4", "1"),
      output_options = ""
    ),
    c(
      "<SUPPRESS> GH(0,100)",
      sprintf("<WRITETABLE> (1,2,,\"%s\")", outn[1]),
      sprintf("<WRITETABLE> (2,4,,\"%s\")", outn[2]),
      sprintf("<WRITETABLE> (3,1,,\"%s\")", outn[3])
    )
  )

})
