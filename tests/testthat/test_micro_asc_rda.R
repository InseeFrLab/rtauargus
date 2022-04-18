donnees <-
  data.frame(
    V1    = c("A", "A", "A", "A", "B", "B", "B", "C"),
    V2    = c("Y", "Z"),
    V3    = c("T1", "T2", "T1", "S_", "T1", "T1", "T1", "S_"),
    VAL   = c(100, 0, 7, 25, 0, 4, 0, 5),
    POIDS = c(1, 2.71, 4.2, 1),
    HOLD  = c("H1", "H2", "H3", "H4")
  )


context("micro_asc_rda")


# input et attendu --------------------------------------------------------

input <-
  data.frame(
    type_var        = c( rep("RECODEABLE", 3), "NUMERIC", "WEIGHT", "HOLDING"),
    colname         = c("V1",     "V2",  "V3",     "VAL",  "POIDS",    "HOLD"),
    position        = c(   1,        3,     5,         8,       12,        17),
    width           = c(  1L,       1L,    2L,        3L,       4L,        2L),
    digits          = c(   0,        0,     0,         0,        2,         0),
    missing         = c( "?",       "",   "#",    "9999",       "#",      "#"),
    totcode         = c(   rep("Total", 3)   ,        NA,       NA,   "Total"),
    codelist        = c(  NA,    NA, "V3.cdl",        NA,       NA,        NA),
    hierarchical    = c(  NA, "V2.hrc", "1 1",        NA,       NA,        NA),
    hierleadstring  = c(  NA,      "@",    NA,        NA,       NA,        NA),
    stringsAsFactors = FALSE
  ) %>%
  purrr::transpose()

hrc_full <- normalizePath("V2.hrc", mustWork = FALSE)
cdl_full <- normalizePath("V3.cdl", mustWork = FALSE)

attendu <- c(
  "V1 1 1 ?",
  "<RECODEABLE>",
  "<TOTCODE> \"Total\"",
  "V2 3 1",
  "<RECODEABLE>",
  "<TOTCODE> \"Total\"",
  "<HIERARCHICAL>",
  "<HIERCODELIST> \"V2.hrc\"",
  "<HIERLEADSTRING> \"@\"",
  "V3 5 2 #",
  "<RECODEABLE>",
  "<TOTCODE> \"Total\"",
  "<CODELIST> \"%s\"" %>% sprintf(cdl_full),
  "<HIERARCHICAL>",
  "<HIERLEVELS> 1 1",
  "VAL 8 3 9999",
  "<NUMERIC>",
  "<DECIMALS> 0",
  "POIDS 12 4 #",
  "<WEIGHT>",
  "<DECIMALS> 2",
  "HOLD 17 2 #",
  "<HOLDING>",
  "<TOTCODE> \"Total\""
)


# write_rda ---------------------------------------------------------------

test_that("write_rda", {

  write_rda <- rtauargus:::write_rda

  expect_equal(
    write_rda(input) %>% strsplit("\n  +") %>% unlist(),
    attendu
  )

})


# micro_asc_rda -----------------------------------------------------------

test_that("micro_asc_rda", {

  # genere fichiers
  tmp <-
    micro_asc_rda(
      donnees,
      weight_var = "POIDS",
      holding_var = "HOLD",
      hrc = c(V2 = "V2.hrc", V3 = "1 1"),
      missing = c("#", V1 = "?", V2 = NA, VAL = 9999),
      codelist = c(V3 = "V3.cdl")
    )

  # .asc
  expect_equal(
    readLines(tmp$asc_filename),
    c(
      "A Y T1 100 1.00 H1",
      "A Z T2   0 2.71 H2",
      "A Y T1   7 4.20 H3",
      "A Z S_  25 1.00 H4",
      "B Y T1   0 1.00 H1",
      "B Z T1   4 2.71 H2",
      "B Y T1   0 4.20 H3",
      "C Z S_   5 1.00 H4"
    )
  )

  # .rda
  expect_equal(
    trimws(readLines(tmp$rda_filename)),
    attendu %>% sub("V2.hrc", hrc_full, ., fixed = TRUE)
  )

})
