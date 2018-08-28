# données test ------------------------------------------------------------

df <-
  data_frame(
    niv1 = rep(
      c("A", "B"),
      c(27, 23)
    ),
    niv2 = rep(
      c("A1", "A2", "B1", "B2"),
      c(17, 10, 10, 13)
    ),
    niv3 = rep(
      c("A1x", "A1z", "A2y", "A2z", "B1x", "B1y", "B2x", "B2z"),
      c(16L, 1L, 8L, 2L, 9L, 1L, 10L, 3L)
    ),
    niv4 = rep(
      c("A1x*", "A1x_", "A1z_", "A2y*", "A2y_", "A2z*", "A2z_",
        "B1x*", "B1x_", "B1y_", "B2x*", "B2x_", "B2z_"),
      c(7L, 9L, 1L, 5L, 3L, 1L, 1L, 6L, 3L, 1L, 7L, 3L, 3L)
    )
  )


# fill_na_hrc -------------------------------------------------------------

context("fill_na_hrc")

fill_na_hrc <- rtauargus:::fill_na_hrc

test_that("différentes configurations et ordre des variables", {

  df_NA <-
    data_frame(
      niv1 = c("A"  , "A" , "B", NA,  NA,  NA,  NA ),
      niv2 = c("A1" , "A2",  NA, NA, "D",  NA, "F1"),
      niv3 = c("A1x",  NA ,  NA, NA,  NA, "E", "F" )
    )

  expect_equal(
    fill_na_hrc(
      df_NA,
      c("niv1", "niv2", "niv3")
    ),
    data_frame(
      niv1 = c("A"  , "A" , "B", NA,  NA,  NA,  NA ),
      niv2 = c("A1" , "A2", "B", NA, "D",  NA, "F1"),
      niv3 = c("A1x", "A2", "B", NA, "D", "E", "F" )
    )
  )

  expect_equal(
    fill_na_hrc(
      df_NA,
      c("niv3", "niv2", "niv1") # inversion ordre
    ),
    data_frame(
      niv1 = c("A"  , "A" , "B", NA, "D", "E", "F1"),
      niv2 = c("A1" , "A2",  NA, NA, "D", "E", "F1"),
      niv3 = c("A1x",  NA ,  NA, NA,  NA, "E", "F" )
    )
  )

})


# sublevels ---------------------------------------------------------------

context("sublevels")

sublevels_t <- rtauargus:::sublevels

subl12 <-
  list(
    A = list(A1 = NULL, A2 = NULL),
    B = list(B1 = NULL, B2 = NULL)
  )

subl13 <-
  list(
    A = list(A1x = NULL, A1z = NULL, A2y = NULL, A2z = NULL),
    B = list(B1x = NULL, B1y = NULL, B2x = NULL, B2z = NULL)
  )

subl14 <-
  list(
    A = list(
          `A1x*` = NULL, A1x_ = NULL, A1z_   = NULL,
          `A2y*` = NULL, A2y_ = NULL, `A2z*` = NULL, A2z_ = NULL
        ),
    B = list(
          `B1x*` = NULL, B1x_ = NULL, B1y_ = NULL,
          `B2x*` = NULL, B2x_ = NULL, B2z_ = NULL
        )
  )

subl23 <-
  list(
    A1 = list(A1x = NULL, A1z = NULL),
    A2 = list(A2y = NULL, A2z = NULL),
    B1 = list(B1x = NULL, B1y = NULL),
    B2 = list(B2x = NULL, B2z = NULL)
  )

subl24 <-
  list(
    A1 = list(`A1x*` = NULL, A1x_ = NULL, A1z_   = NULL),
    A2 = list(`A2y*` = NULL, A2y_ = NULL, `A2z*` = NULL, A2z_ = NULL),
    B1 = list(`B1x*` = NULL, B1x_ = NULL, B1y_   = NULL),
    B2 = list(`B2x*` = NULL, B2x_ = NULL, B2z_   = NULL)
  )

subl34 <-
  list(
    A1x = list(`A1x*` = NULL, A1x_ = NULL),
    A1z = list(A1z_   = NULL),
    A2y = list(`A2y*` = NULL, A2y_ = NULL),
    A2z = list(`A2z*` = NULL, A2z_ = NULL),
    B1x = list(`B1x*` = NULL, B1x_ = NULL),
    B1y = list(B1y_   = NULL),
    B2x = list(`B2x*` = NULL, B2x_ = NULL),
    B2z = list(B2z_   = NULL)
  )

test_that("normal", {

  # niv 1 x (2 3 4) ...........................

  expect_equal(
    sublevels_t(df$niv2, df$niv1),
    subl12
  )

  expect_equal(
    sublevels_t(df$niv3, df$niv1),
    subl13
  )

  expect_equal(
    sublevels_t(df$niv4, df$niv1),
    subl14
  )

  # niv 2 x (3 4) ............................

  expect_equal(
    sublevels_t(df$niv3, df$niv2),
    subl23
  )

  expect_equal(
    sublevels_t(df$niv4, df$niv2),
    subl24
  )

  # niv 3 x 4 ...........................

  expect_equal(
    sublevels_t(df$niv4, df$niv3),
    subl34
  )

})

test_that("détecte variables non hierarchiques", {

  # niv 1 x (2 3 4) ...........................

  expect_error(
    sublevels_t(df$niv1, df$niv2),
    "variables non hierarchiques"
  )

  expect_error(
    sublevels_t(df$niv1, df$niv3),
    "variables non hierarchiques"
  )

  expect_error(
    sublevels_t(df$niv1, df$niv4),
    "variables non hierarchiques"
  )

  # niv 2 x (3 4) ............................

  expect_error(
    sublevels_t(df$niv2, df$niv3),
    "variables non hierarchiques"
  )

  expect_error(
    sublevels_t(df$niv2, df$niv4),
    "variables non hierarchiques"
  )

  # niv 3 x 4 ...........................

  expect_error(
    sublevels_t(df$niv3, df$niv4),
    "variables non hierarchiques"
  )


})


# imbrique ----------------------------------------------------------------

context("imbrique")

imbrique_t <- rtauargus:::imbrique

p_12 <-
  list(
    A = list(
      A1 = NULL,
      A2 = NULL
    ),
    B = list(
      B1 = NULL,
      B2 = NULL
    )
  )

p_23 <-
  list(
    A1 = list(
      A1x = NULL,
      A1z = NULL
    ),
    A2 =list(
      A2y = NULL,
      A2z = NULL
    ),
    B1 = list(
      B1x = NULL,
      B1y = NULL
    ),
    B2 = list(
      B2x = NULL,
      B2z = NULL
    )
  )

i_13 <-
  list(
    A = list(
      A1 = list(A1x = NULL, A1z = NULL),
      A2 = list(A2y = NULL, A2z = NULL)
    ),
    B = list(
      B1 = list(B1x = NULL, B1y = NULL),
      B2 = list(B2x = NULL, B2z = NULL)
    )
  )

test_that("normal", {

  expect_equal(
   imbrique_t(p_23, p_12),
    i_13
  )

})

test_that("melange", {

  # melange input 1
  expect_equal(
   imbrique_t(
     p_23[c(3, 2, 4, 1)],
     p_12
    ),
    i_13
  )

  # melange input 2
  expect_equal(
   imbrique_t(p_23, p_12[2:1]),
    i_13[2:1]
  )

  # melange inputs 1 et 2
  expect_equal(
   imbrique_t(p_23[c(3, 2, 4, 1)], p_12[2:1]),
    i_13[2:1]
  )

})


# hrc_list -----------------------------------------------------------------

context("hrc_list")

hrc_123 <- list(
  A = list(
    A1 = list(
      A1x = NULL,
      A1z = NULL
    ),
    A2 = list(
      A2y = NULL,
      A2z = NULL
    )
  ),
  B = list(
    B1 = list(
      B1x = NULL,
      B1y = NULL
    ),
    B2 = list(
      B2x = NULL,
      B2z = NULL
    )
  )
)

hrc_124 <- list(
  A = list(
    A1 = list(
      `A1x*` = NULL,
       A1x_  = NULL,
       A1z_  = NULL
    ),
    A2 = list(
      `A2y*` = NULL,
       A2y_  = NULL,
      `A2z*` = NULL,
       A2z_  = NULL
    )
  ),
  B = list(
    B1 = list(
      `B1x*` = NULL,
       B1x_  = NULL,
       B1y_  = NULL
    ),
    B2 = list(
      `B2x*` = NULL,
       B2x_  = NULL,
       B2z_  = NULL
    )
  )
)

hrc_1234 <- list(
  A = list(
    A1 = list(
      A1x = list(`A1x*` = NULL, A1x_ = NULL),
      A1z = list( A1z_  = NULL)
    ),
    A2 = list(
      A2y = list(`A2y*` = NULL, A2y_ = NULL),
      A2z = list(`A2z*` = NULL, A2z_ = NULL)
    )
  ),
  B = list(
    B1 = list(
      B1x = list(`B1x*` = NULL, B1x_ = NULL),
      B1y = list( B1y_  = NULL)
    ),
    B2 = list(
      B2x = list(`B2x*` = NULL, B2x_ = NULL),
      B2z = list (B2z_  = NULL)
    )
  )
)

hrc_list <- rtauargus:::hrc_list

test_that("2 input (équivaut à sublevels)", {

  # 2 inputs équivaut à sublevels
  expect_equal(
    hrc_list(df, c("niv2", "niv1")),
    subl12
  )

  expect_equal(
    hrc_list(df, c("niv4", "niv1")),
    subl14
  )

  expect_equal(
    hrc_list(df, c("niv4", "niv2")),
    subl24
  )

  expect_equal(
    hrc_list(df, c("niv3", "niv2")),
    subl23
  )
  #...

})

test_that("3 input", {

  expect_equal(
    hrc_list(df, c("niv3", "niv2", "niv1")),
    hrc_123
  )

  expect_equal(
    hrc_list(df, c("niv4", "niv2", "niv1")),
    hrc_124
  )

})

test_that("4 input", {

  expect_equal(
    hrc_list(df, c("niv4", "niv3", "niv2", "niv1")),
    hrc_1234
  )

})


# prof_list ---------------------------------------------------------------

context("prof_list")

prof_list <- rtauargus:::prof_list

test_that("2 niveaux", {

  expect_equal(
    prof_list(subl12),
    c(A = 0, A1 = 1, A2 = 1, B = 0, B1 = 1, B2 = 1)
  )

})

test_that("3 niveaux", {

  expect_equal(
    prof_list(hrc_124),
    c(A = 0,
        A1 = 1,
         `A1x*` = 2, A1x_ = 2, A1z_ = 2,
        A2 = 1,
         `A2y*` = 2, A2y_ = 2, `A2z*` = 2, A2z_ = 2,
      B = 0,
        B1 = 1,
          `B1x*` = 2, B1x_ = 2, B1y_ = 2,
        B2 = 1,
          `B2x*` = 2, B2x_ = 2, B2z_ = 2
    )
  )

})

test_that("4 niveaux", {

  expect_equal(
    prof_list(hrc_1234),
    c(A = 0,
        A1 = 1,
          A1x = 2,
            `A1x*` = 3, A1x_ = 3,
          A1z = 2,
            A1z_ = 3,
        A2 = 1,
          A2y = 2,
            `A2y*` = 3, A2y_ = 3,
          A2z = 2,
            `A2z*` = 3, A2z_ = 3,
      B = 0,
        B1 = 1,
          B1x = 2,
            `B1x*` = 3, B1x_ = 3,
          B1y = 2,
            B1y_ = 3,
        B2 = 1,
          B2x = 2,
            `B2x*` = 3, B2x_ = 3,
          B2z = 2,
            B2z_ = 3
    )
  )

})


# check_seq_prof ----------------------------------------------------------

context("check_seq_prof")

check_seq_prof <- rtauargus:::check_seq_prof

test_that("input invalide", {

  expect_false(check_seq_prof(integer(0)))
  expect_false(check_seq_prof(1:2))
  expect_false(check_seq_prof(0:-1))
  expect_false(check_seq_prof((0:1) / 2))

})

test_that("input valide", {

  expect_true(check_seq_prof(0:2))
  expect_false(check_seq_prof(c(0, 2)))
  expect_true(check_seq_prof(c(0, 1, 1, 0, 1, 2)))
  expect_true(check_seq_prof(c(0, 1, 2, 3, 2)))
  expect_true(check_seq_prof(c(0, 1, 2, 3, 1)))
  expect_true(check_seq_prof(c(0, 1, 1, 2, 3, 3, 2, 3, 3, 2, 0, 1, 0)))

})


# normalise_hrc -----------------------------------------------------------

context("normalise_hrc")

normalise_hrc <- rtauargus:::normalise_hrc

test_that("rien à normaliser", {

  expect_null(normalise_hrc(NULL))

  expect_equal(
    normalise_hrc(c("1 2", "3 4 5")),
    c("1 2", "3 4 5")
  )

})

test_that("syntaxe valeur incorrecte", {

  expect_error(
    normalise_hrc("fich"),
    "Parametres hrc incorrects"
  )

  expect_error(
    normalise_hrc("3 Q 5"),
    "Parametres hrc incorrects"
  )

})

test_that("microdata absent si V1>V2...", {

  expect_error(
    normalise_hrc("V1>V2"),
    "specifier microdata"
  )

})


# df_hierlevels -----------------------------------------------------------

context("df_hierlevels")

df_hierlevels <- rtauargus:::df_hierlevels

test_that("base", {

  expect_equal(
    df_hierlevels(df$niv2, "1 1"),
    data.frame(
      var_hrc = c("A1", "A2", "B1", "B2"),
      V2      = c("A",  "A",  "B",  "B" ),
      stringsAsFactors = FALSE
    )
  )

  expect_equal(
    df_hierlevels(df$niv3, "1 1 1"),
    data.frame(
      var_hrc = c("A1x", "A1z", "A2y", "A2z", "B1x", "B1y", "B2x", "B2z"),
      V2      = c("A1" , "A1" , "A2" , "A2" , "B1" , "B1" , "B2" , "B2" ),
      V3      = c("A"  , "A"  , "A"  , "A"  , "B"  , "B"  , "B"  , "B"  ),
      stringsAsFactors = FALSE
    )
  )


})

test_that("error", {

  expect_error(
    df_hierlevels(df$niv2, "1 un"),
    "chiffres separes par des espaces"
  )

  expect_error(
    df_hierlevels(df$niv4, "1 2 3"),
    "somme de hierlevels"
  )

  expect_error(
    df_hierlevels(c(df$niv3, df$niv2), "1 3"),
    "nombre de caracteres"
  )

})

# write_hrc ---------------------------------------------------------------
