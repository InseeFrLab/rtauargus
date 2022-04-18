# données test ------------------------------------------------------------

df <-
  tibble(
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
      c("A1x6", "A1x7", "A1z6", "A2y7", "A2y6", "A2z7", "A2z6",
        "B1x7", "B1x6", "B1y6", "B2x7", "B2x6", "B2z6"),
      c(7L, 9L, 1L, 5L, 3L, 1L, 1L, 6L, 3L, 1L, 7L, 3L, 3L)
    )
  )


# fill_na_hrc -------------------------------------------------------------

context(":::fill_na_hrc")

test_that("différentes configurations et ordre des variables", {

  df_NA <-
    tibble(
      niv1 = c("A"  , "A" , "B", NA,  NA,  NA,  NA ),
      niv2 = c("A1" , "A2",  NA, NA, "D",  NA, "F1"),
      niv3 = c("A1x",  NA ,  NA, NA,  NA, "E", "F" )
    )

  expect_equal(
    fill_na_hrc(
      df_NA,
      c("niv1", "niv2", "niv3")
    ),
    tibble(
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
    tibble(
      niv3 = c("A1x",  NA ,  NA, NA,  NA, "E", "F" ),
      niv2 = c("A1" , "A2",  NA, NA, "D", "E", "F1"),
      niv1 = c("A"  , "A" , "B", NA, "D", "E", "F1")
    )
  )

})


# sublevels ---------------------------------------------------------------

context(":::sublevels")

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
          A1x6 = NULL, A1x7 = NULL, A1z6 = NULL,
          A2y6 = NULL, A2y7 = NULL, A2z6 = NULL, A2z7 = NULL
        ),
    B = list(
          B1x6 = NULL, B1x7 = NULL, B1y6 = NULL,
          B2x6 = NULL, B2x7 = NULL, B2z6 = NULL
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
    A1 = list(A1x6 = NULL, A1x7 = NULL, A1z6 = NULL),
    A2 = list(A2y6 = NULL, A2y7 = NULL, A2z6 = NULL, A2z7 = NULL),
    B1 = list(B1x6 = NULL, B1x7 = NULL, B1y6 = NULL),
    B2 = list(B2x6 = NULL, B2x7 = NULL, B2z6 = NULL)
  )

subl34 <-
  list(
    A1x = list(A1x6 = NULL, A1x7 = NULL),
    A1z = list(A1z6 = NULL),
    A2y = list(A2y6 = NULL, A2y7 = NULL),
    A2z = list(A2z6 = NULL, A2z7 = NULL),
    B1x = list(B1x6 = NULL, B1x7 = NULL),
    B1y = list(B1y6 = NULL),
    B2x = list(B2x6 = NULL, B2x7 = NULL),
    B2z = list(B2z6 = NULL)
  )

test_that("normal", {

  # niv 1 x (2 3 4) ...........................

  expect_equal(
    sublevels(df$niv2, df$niv1),
    subl12
  )

  expect_equal(
    sublevels(df$niv3, df$niv1),
    subl13
  )

  expect_equal(
    sublevels(df$niv4, df$niv1),
    subl14
  )

  # niv 2 x (3 4) ............................

  expect_equal(
    sublevels(df$niv3, df$niv2),
    subl23
  )

  expect_equal(
    sublevels(df$niv4, df$niv2),
    subl24
  )

  # niv 3 x 4 ...........................

  expect_equal(
    sublevels(df$niv4, df$niv3),
    subl34
  )

})

test_that("présence NAs", {

  expect_error(
    sublevels(c(NA, NA), c(NA , NA)),
    "aucun croisement exploitable"
  )

  expect_error(
    sublevels(c("A", NA), c(NA , NA)),
    "aucun croisement exploitable"
  )

  expect_error(
    sublevels(c("A1", NA), c(NA , "B")),
    "aucun croisement exploitable"
  )

  res <- list(A = list(A1 = NULL)) # (attendu)

  expect_equal(
    sublevels(c("A1", NA), c("A", NA)),
    res
  )

  expect_equal(
    sublevels(c("A1", NA), c("A", "B")),
    res # B disparaît à cause du NA au niveau inférieur
  )

  expect_equal(
    sublevels(c("A1", "B1"), c("A", NA)),
    res # B1 disparaît à cause du NA au niveau supérieur
  )

  expect_equal(
    sublevels(c("A1", "A1"), c("A", NA)),
    res
  )

  # plusieurs cas combinés
  expect_equal(
    sublevels(c("A1", "B2", NA, NA), c("A", NA, "B", NA)),
    res
  )

})

test_that("détecte variables non hierarchiques", {

  # niv 1 x (2 3 4) ...........................

  expect_error(
    sublevels(df$niv1, df$niv2),
    "variables non hierarchiques"
  )

  expect_error(
    sublevels(df$niv1, df$niv3),
    "variables non hierarchiques"
  )

  expect_error(
    sublevels(df$niv1, df$niv4),
    "variables non hierarchiques"
  )

  # niv 2 x (3 4) ............................

  expect_error(
    sublevels(df$niv2, df$niv3),
    "variables non hierarchiques"
  )

  expect_error(
    sublevels(df$niv2, df$niv4),
    "variables non hierarchiques"
  )

  # niv 3 x 4 ...........................

  expect_error(
    sublevels(df$niv3, df$niv4),
    "variables non hierarchiques"
  )


})


# imbrique ----------------------------------------------------------------

context(":::imbrique")

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
   imbrique(p_23, p_12),
    i_13
  )

})

test_that("melange", {

  # melange input 1
  expect_equal(
   imbrique(
     p_23[c(3, 2, 4, 1)],
     p_12
    ),
    i_13
  )

  # melange input 2
  expect_equal(
   imbrique(p_23, p_12[2:1]),
    i_13[2:1]
  )

  # melange inputs 1 et 2
  expect_equal(
   imbrique(p_23[c(3, 2, 4, 1)], p_12[2:1]),
    i_13[2:1]
  )

})


# hrc_list -----------------------------------------------------------------

context(":::hrc_list")

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
       A1x6 = NULL,
       A1x7 = NULL,
       A1z6 = NULL
    ),
    A2 = list(
       A2y6 = NULL,
       A2y7 = NULL,
       A2z6 = NULL,
       A2z7 = NULL
    )
  ),
  B = list(
    B1 = list(
       B1x6 = NULL,
       B1x7 = NULL,
       B1y6 = NULL
    ),
    B2 = list(
       B2x6 = NULL,
       B2x7 = NULL,
       B2z6 = NULL
    )
  )
)

hrc_1234 <- list(
  A = list(
    A1 = list(
      A1x = list(A1x6 = NULL, A1x7 = NULL),
      A1z = list(A1z6 = NULL)
    ),
    A2 = list(
      A2y = list(A2y6 = NULL, A2y7 = NULL),
      A2z = list(A2z6 = NULL, A2z7 = NULL)
    )
  ),
  B = list(
    B1 = list(
      B1x = list(B1x6 = NULL, B1x7 = NULL),
      B1y = list(B1y6 = NULL)
    ),
    B2 = list(
      B2x = list(B2x6 = NULL, B2x7 = NULL),
      B2z = list(B2z6 = NULL)
    )
  )
)

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

context(":::prof_list")

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
         A1x6 = 2, A1x7 = 2, A1z6 = 2,
        A2 = 1,
         A2y6 = 2, A2y7 = 2, A2z6 = 2, A2z7 = 2,
      B = 0,
        B1 = 1,
          B1x6 = 2, B1x7 = 2, B1y6 = 2,
        B2 = 1,
          B2x6 = 2, B2x7 = 2, B2z6 = 2
    )
  )

})

test_that("4 niveaux", {

  expect_equal(
    prof_list(hrc_1234),
    c(A = 0,
        A1 = 1,
          A1x = 2,
            A1x6 = 3, A1x7 = 3,
          A1z = 2,
            A1z6 = 3,
        A2 = 1,
          A2y = 2,
            A2y6 = 3, A2y7 = 3,
          A2z = 2,
            A2z6 = 3, A2z7 = 3,
      B = 0,
        B1 = 1,
          B1x = 2,
            B1x6 = 3, B1x7 = 3,
          B1y = 2,
            B1y6 = 3,
        B2 = 1,
          B2x = 2,
            B2x6 = 3, B2x7 = 3,
          B2z = 2,
            B2z6 = 3
    )
  )

})


# is_hrc ------------------------------------------------------------------

context(":::is_hrc")

test_that("depuis precomptage", {

  mat2col <- purrr::partial(matrix, ncol = 2, byrow = TRUE)

  expect_true(
    mat2col(
      c(1, 0,
        0, 2)
    ) %>% is_hrc()
  )

  expect_true(
    mat2col(
      c(1, 0,
        0, 2,
        8, 0)
    ) %>% is_hrc()
  )

  expect_false(
    mat2col(
      c(7, 1, # deux niveaux agrégés pour un niveau fin
        0, 2)
    ) %>% is_hrc()
  )

  expect_true(
    matrix(
      nrow = 2,
      c(1, 0,
        0, 0
      )
    ) %>% is_hrc()
  )

})

test_that("sortie table()", {

  # TRUE ......................

  expect_true(
    is_hrc(
      table(
        fin = c("A1", "A2"),
        agr = c("A" , "A" )
      )
    )
  )

  expect_true(
    is_hrc(
      table(
        fin = c("A1", "A2", "B1"),
        agr = c("A" , "A" , "B" )
      )
    )
  )

  expect_true(
    is_hrc(
      table(
        fin = c("A1", "A2"),
        agr = c("A" ,  NA )
      )
    )
  )

  expect_true(
    is_hrc(
      table(
        fin = c("A1", NA ),
        agr = c("A" , "B")
      )
    )
  )

  expect_true(
    is_hrc(
      table(
        fin = c("A1", "A2", "B1"),
        agr = c("A" , "A" , "B" )
      )
    )
  )

  expect_true(
    is_hrc(
      table(
        fin = c("A1", "A2", "B1",  NA),
        agr = c("A" , "A" , "B" , "B")
      )
    )
  )

  expect_true(
    is_hrc(
      table(
        fin = c("A1", "A2", "B1",  NA),
        agr = c("A" , "A" , "B" , "C")
      )
    )
  )

  expect_true(
    is_hrc(
      table(
        fin = c("A1", "B1", "B2"), # ligne de 0 pour B2
        agr = c("A" , "B" ,  NA )
      )
    )
  )

  # FALSE ......................

  expect_false(
    is_hrc(
      table(
        fin = c("A1", "A1"),
        agr = c("A" , "B" ) # deux niveaux agrégés pour un niveau fin
      )
    )
  )

  expect_false(
    is_hrc(
      table(
        fin = c("A1", "A2"),
        agr = c( NA ,  NA )
      )
    )
  )

  expect_false(
    is_hrc(
      table(
        fin = c( NA , NA ),
        agr = c("A" , "A")
      )
    )
  )

})


# check_seq_prof ----------------------------------------------------------

context(":::check_seq_prof")

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

context(":::normalise_hrc")

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

context(":::df_hierlevels")

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

context("write_hrc")

test_that("hierlevels", {

  expect_error(
    write_hrc(df, c("niv1", "niv2"), hierlevels = "2 1"),
    "une seule variable hierarchique a specifier"
  )

  expect_equal(
    df %>% write_hrc("niv2", hierlevels = "1 1") %>% readLines(),
    c("A", "@A1", "@A2", "B", "@B1", "@B2")
  )

})

test_that("colonne absente", {

  expect_error(
    write_hrc(df, c("niv1", "niv0")),
    "introuvable.+ niv0$"
  )

})

test_that("un seul niveau", {

  expect_warning(
    res <- write_hrc(df, "niv2"),
    "hierarchie d'un seul niveau"
  )
  expect_equal(readLines(res), c("A1", "A2", "B1", "B2"))

})

test_that("fill_na", {

  hrc_complet <- purrr::partial(write_hrc, compact = FALSE) # [n'élague pas]

  df$niv2[df$niv2 == "A1"] <- NA

  # fill_na "up" ...................................................

  expect_warning(
    res <- hrc_complet(df, c("niv2", "niv1"), fill_na = "up"),
    "valeurs manquantes imputees"
  )
  expect_equal(readLines(res), c("A", "@A", "@A2", "B", "@B1", "@B2"))
    # ex-A1 imputés par le haut deviennent A

  # fill_na "down" .................................................

  expect_warning(
    res <- hrc_complet(df, c("niv2", "niv1"), fill_na = "down"),
    "valeurs manquantes imputees"
  )
  expect_equal(readLines(res), c("A", "@A2", "B", "@B1", "@B2"))
    # pas de niveau inférieur à niv2 : imputation down sans effet,
    # donc croisement A x NA simplement ignorés

  df <- df[df$niv1 == "A", ]
  expect_warning(
    res <- hrc_complet(df, c("niv3", "niv2"), fill_na = "down"),
    "valeurs manquantes imputees"
  )
  expect_equal(
    readLines(res),
    c("A1x", "@A1x", "A1z", "@A1z", "A2", "@A2y", "@A2z")
  )  # ex-A1 imputés par le bas deviennent A1x et A1z

})

test_that("compact", {

  df$niv3[df$niv1 == "A"] <- NA # vide niveau 3 pour A

  expect_warning(
    res <- write_hrc(df, paste0("niv", 3:1), compact = TRUE),
    "valeurs manquantes imputees"
  )

  expect_equal(
    readLines(res),
    c("A", "@A1", "@A2", "B", "@B1", "@@B1x", "@@B1y", "@B2", "@@B2x", "@@B2z")
  )

  df$niv2[df$niv1 == "A"] <- NA # vide aussi niveau 2 pour A

  expect_warning(
    res <- write_hrc(df, paste0("niv", 3:1), compact = TRUE),
    "valeurs manquantes imputees"
  )

  expect_equal(
    readLines(res),
    c("A", "B", "@B1", "@@B1x", "@@B1y", "@B2", "@@B2x", "@@B2z")
  )

})

test_that("validité arbre", {

  # vide niveau intermédiaire (produira une erreur si compactage)
  df$niv2<- NA

  expect_error(
    suppressWarnings(write_hrc(df, paste0("niv", 3:1), compact = TRUE)),
    "Niveaux de hierarchie incoherents"
  )

})

test_that("hierleadstring", {

  expect_equal(
    df %>%
      write_hrc(paste0("niv", 2:1), hierleadstring = ":") %>%
      readLines(),
    c("A", ":A1", ":A2", "B", ":B1", ":B2")
  )

})

test_that("cas exotiques", {
  skip("a->b b->a")
  # expect_error ?
  write_hrc(
    data.frame(niv1 = letters[1:2], niv2 = letters[2:1]),
    c("niv2", "niv1")
  )
})
