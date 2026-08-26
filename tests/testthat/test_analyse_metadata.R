# test data --------------------------------------------------------------------

data(metadata_pizza_lettuce)
baseline_res <- analyse_metadata(metadata_pizza_lettuce)

df_eq_ex <- data.frame(
  eq_name = c("eq1", "eq2", "eq3", "eq4"),
  eq_indicator = c("A = B + C", "A = D + E", "D = F + G", "Z = X + Y + S"),
  unit = c("EUR", "EUR", "EUR", "EUR"),
  stringsAsFactors = FALSE
)
df_meta_for_eq <- data.frame(
  table_name = c("Ta","Tb","Tc","Td","Te","Tf","Tg","Tz","Tx","Ty","Ts"),
  field = NA,
  hrc_field = NA,
  indicator = c("A","B","C","D","E","F","G","Z","X","Y","S"),
  hrc_indicator = NA,
  spanning_1 = "age_class",
  hrc_spanning_1 = NA
)

################################################################### INPUT CHECKS
# check that an error is returned if the names of the columns don't respect the
# expected format --------------------------------------------------------------
test_that("error message for wrong column name - fixed columns",{
  meta <- metadata_pizza_lettuce %>% rename(table = table_name)

  expect_error(analyse_metadata(meta),"one or more required columns: table_name, field, hrc_field, indicator, hrc_indicator")
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

# check column names of df_eq_indicator
test_that("error message for wrong column name - df_eq_indicator", {
  df_eq_ex_modif <- df_eq_ex %>% rename(eq_indicators = eq_indicator)

  expect_error(
    suppressWarnings(analyse_metadata(df_meta_for_eq, df_eq_ex_modif)),
    "one or more required columns: eq_name, eq_indicator, unit"
  )
})

# check that the user will be warned the hrc_indicator column will be ignored if
# df_eq_indicator is used -----------------------------------------------------
test_that("warning when hrc_indicator is ignored because df_eq_indicator used", {
  expect_warning(
    analyse_metadata(df_meta_for_eq, df_eq_ex),
    "the hrc_indicator column will be ignored"
  )
})

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

# check that the hierarchies on fields are handled properly --------------------
answer <- data.frame(
  cluster = "hrc_geo.to_pizza",
  table_name = "T1.T2.T3",
  field = "france_entreprises_2023",
  indicator = "to_pizza",
  spanning_1 = "size",
  spanning_2 = "HRC_GEO^h",
  hrc_spanning_1 = NA_character_,
  hrc_spanning_2 = "hrc_geo"
)

test_that("hierarchies on fields", {

  meta <- data.frame(
    table_name = paste0("T", 1:3),
    field = c("france_entreprises_2023","metro_entreprises_2023","dom_entreprises_2023"),
    hrc_field = "hrc_geo",
    indicator = "to_pizza",
    hrc_indicator = NA_character_,
    spanning_1 = "size",
    hrc_spanning_1 = NA_character_
  )

  expect_equal(analyse_metadata(meta),answer)
}
)

# check that the hierarchies on fields and on indicators -----------------------
answer <- data.frame(
  cluster = "hrc_geo.hrc_salades",
  table_name = "T1.T2.T3",
  field = "france_entreprises_2023",
  indicator = "SALADES",
  spanning_1 = "size",
  spanning_2 = "HRC_SALADES^h",
  spanning_3 = "HRC_GEO^h",
  hrc_spanning_1 = NA_character_,
  hrc_spanning_2 = "hrc_salades",
  hrc_spanning_3 = "hrc_geo"
)

test_that("hierarchies on fields and indicators", {

  meta <- data.frame(
    table_name = paste0("T", 1:3),
    field = c("france_entreprises_2023","metro_entreprises_2023","dom_entreprises_2023"),
    hrc_field = "hrc_geo",
    indicator = c("to_batavia","to_arugula","to_lettuce"),
    hrc_indicator = "hrc_salades",
    spanning_1 = "size",
    hrc_spanning_1 = NA_character_
  )

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

##################################################### INDICATOR EQUATIONS CHECKS
# all the spanning variables are taken into account when using equations on
# indicators -------------------------------------------------------------------
df_eq_lettuce_1 <- data.frame(
  eq_name = c("eq1"),
  eq_indicator = c("to_lettuce = to_batavia + to_arugula"),
  unit = c("EUR"),
  stringsAsFactors = FALSE
)

df_eq_lettuce_2 <- data.frame(
  eq_name = c("eq1","eq2"),
  eq_indicator = c("to_lettuce = to_batavia + to_arugula",
                   "to_pizza = to_tomates + to_pate"),
  unit = c("EUR","EUR"),
  stringsAsFactors = FALSE
)

answer <- data.frame(
  cluster = c(
    "france_entreprises_2023.EUR",
    "france_entreprises_2023.EUR",
    "france_entreprises_2023.to_pizza",
    "france_entreprises_2023.to_pizza"
  ),
  table_name = c(
    "T10.T12.T8",
    "T11.T7.T9",
    "T1.T2",
    "T3.T4.T5.T6"
  ),
  field = rep("france_entreprises_2023", 4),
  indicator = c("to_lettuce", "to_lettuce", "to_pizza", "to_pizza"),
  spanning_1 = c("HRC_NAF", "HRC_NAF", "HRC_NUTS", "HRC_NAF"),
  spanning_2 = c("cj", "size", "size", "HRC_NUTS"),
  spanning_3 = c("EQ1^h", "EQ1^h", NA, NA),
  hrc_spanning_1 = c("hrc_naf", "hrc_naf", "hrc_nuts", "hrc_naf"),
  hrc_spanning_2 = c(NA, NA, NA, "hrc_nuts"),
  hrc_spanning_3 = c("hrc_EQ1.totcode.to_lettuce", "hrc_EQ1.totcode.to_lettuce", NA, NA)
)

test_that("indicators equation", {
  expect_warning(
    expect_equal(
      analyse_metadata(df_metadata = metadata_pizza_lettuce,df_eq_indicator = df_eq_lettuce_1),
      answer
    ),
    "hrc_indicator column will be ignored"
  )

}
)

# All indicators in the same equation broken down by the same spanning variable ----
answer <- data.frame(
  cluster = c("france_entreprises_2023.EUR"),
  table_name = c("T11.T7.T9"),
  field = c("france_entreprises_2023"),
  indicator = c("to_lettuce"),
  spanning_1 = c("a10"),
  spanning_2 = c("EQ1^h"),
  hrc_spanning_1 = NA_character_,
  hrc_spanning_2 = c("hrc_EQ1.totcode.to_lettuce")
)

test_that("meme_var_crois_1", {
  meta <- metadata_pizza_lettuce[,c(1:7)] %>% filter(table_name %in% c("T7","T9","T11"))
  meta$hrc_spanning_1 <- NA_character_

  expect_warning(
    expect_equal(
      analyse_metadata(df_metadata = meta,df_eq_indicator = df_eq_lettuce_1),
      answer
    ),
    "hrc_indicator column will be ignored"
  )

}
)

# All indicators in the same equation broken down by the same spanning variables ----
answer <- data.frame(
  cluster = c("france_entreprises_2023.EUR","france_entreprises_2023.EUR"),
  table_name = c("T10.T12.T8","T11.T7.T9"),
  field = c("france_entreprises_2023","france_entreprises_2023"),
  indicator = c("to_lettuce","to_lettuce"),
  spanning_1 = c("a10","a10"),
  spanning_2 = c("cj","size"),
  spanning_3 = c("EQ1^h","EQ1^h"),
  hrc_spanning_1 = NA_character_,
  hrc_spanning_2 = NA_character_,
  hrc_spanning_3 = c("hrc_EQ1.totcode.to_lettuce","hrc_EQ1.totcode.to_lettuce")
)

test_that("meme_var_crois_2", {
  meta <- metadata_pizza_lettuce[c(7:12),]
  meta$hrc_spanning_1 <- NA_character_

  expect_warning(
    expect_equal(
      analyse_metadata(df_metadata = meta,df_eq_indicator = df_eq_lettuce_1),
      answer
    ),
    "hrc_indicator column will be ignored"
  )

}
)

# Two equations, all indicators broken down by the same spanning variable -----
answer <- data.frame(
  cluster = c("france_entreprises_2023.EUR","france_entreprises_2023.EUR"),
  table_name = c("T11.T7.T9","T4.T5.T6"),
  field = c("france_entreprises_2023","france_entreprises_2023"),
  indicator = c("to_lettuce","to_pizza"),
  spanning_1 = c("a10","a10"),
  spanning_2 = c("EQ1^h","EQ2^h"),
  hrc_spanning_1 = NA_character_,
  hrc_spanning_2 = c("hrc_EQ1.totcode.to_lettuce","hrc_EQ2.totcode.to_pizza")
)

test_that("meme_var_crois_1_deux_eq", {
  meta <- metadata_pizza_lettuce[c(4:7,9,11),c(1:7)]
  meta$indicator <- c("to_pizza","to_tomates","to_pate","to_batavia","to_arugula","to_lettuce")
  meta <- meta %>% mutate(spanning_1 = "a10",hrc_spanning_1 = NA_character_)

  expect_warning(
    expect_equal(
      analyse_metadata(df_metadata = meta,df_eq_indicator = df_eq_lettuce_2),
      answer
    ),
    "hrc_indicator column will be ignored"
  )

}
)

# Two equations, all indicators in each equations are broken down by the same
# spanning variables -----------------------------------------------------------
answer <- data.frame(
  cluster = rep("france_entreprises_2023.EUR"),
  table_name = c("T10.T12.T8","T11.T7.T9","T4.T5.T6"),
  field = rep("france_entreprises_2023"),
  indicator = c("to_lettuce","to_lettuce","to_pizza"),
  spanning_1 = rep("a10"),
  spanning_2 = c("cj","size","size"),
  spanning_3 = c("EQ1^h","EQ1^h","EQ2^h"),
  hrc_spanning_1 = NA_character_,
  hrc_spanning_2 = NA_character_,
  hrc_spanning_3 = c("hrc_EQ1.totcode.to_lettuce","hrc_EQ1.totcode.to_lettuce","hrc_EQ2.totcode.to_pizza")
)

test_that("meme_var_crois_2_deux_eq", {
  meta <- metadata_pizza_lettuce[c(4:12),]
  meta$indicator <- c("to_pizza","to_tomates","to_pate","to_batavia","to_batavia","to_arugula","to_arugula","to_lettuce","to_lettuce")
  meta <- meta %>% mutate(spanning_1 = "a10",
                          hrc_spanning_1 = NA_character_,
                          spanning_2 = c("size","size","size","size","cj","size","cj","size","cj"),
                          hrc_spanning_2 = NA_character_)

  expect_warning(
    expect_equal(
      analyse_metadata(df_metadata = meta,df_eq_indicator = df_eq_lettuce_2),
      answer
    ),
    "hrc_indicator column will be ignored"
  )

}
)

# One equation, but the indicators are not broken down by the same spanning
# variables (only one spanning variable by table) ------------------------------
answer <- data.frame(
  cluster = rep("france_entreprises_2023.EUR"),
  table_name = c("T11","T7.T9"),
  field = rep("france_entreprises_2023"),
  indicator = c("to_lettuce","to_lettuce"),
  spanning_1 = c("cj","a10"),
  spanning_2 = c("EQ1^h","EQ1^h"),
  hrc_spanning_1 = NA_character_,
  hrc_spanning_2 = c("hrc_EQ1.totcode.to_lettuce","hrc_EQ1.totcode.to_lettuce")
)


test_that("pas_meme_var_crois_1", {
  meta <- metadata_pizza_lettuce[,c(1:7)] %>%
    filter(table_name %in% c("T7","T9","T11")) %>%
    mutate(spanning_1 = c("a10","a10","cj"))
  meta$hrc_spanning_1 <- NA_character_

  expect_warning(
    expect_equal(
      analyse_metadata(df_metadata = meta,df_eq_indicator = df_eq_lettuce_1),
      answer
    ),
    "hrc_indicator column will be ignored"
  )

}
)

# One equation, but the indicators are not broken down by the same spanning
# variables (one or two spanning variable by table) ----------------------------
answer <- data.frame(
  cluster = rep("france_entreprises_2023.EUR"),
  table_name = c("T1.T2","T1.T2.T3"),
  field = rep("france_entreprises_2023"),
  indicator = c("EUR","to_lettuce"),
  spanning_1 = c("a10","a10"),
  spanning_2 = c("EQ1^h","size"),
  spanning_3 = c(NA,"EQ1^h"),
  hrc_spanning_1 = NA_character_,
  hrc_spanning_2 = c("hrc_EQ1.totcode.to_lettuce",NA),
  hrc_spanning_3 = c(NA,"hrc_EQ1.totcode.to_lettuce")
)

test_that("pas_meme_var_crois_2", {
  meta <- metadata_pizza_lettuce %>% filter(table_name %in% c("T7","T9","T11"))
  meta$spanning_2 <- c(NA,NA,"size")
  meta$hrc_spanning_1 <- NA_character_
  meta$hrc_indicator <- NA_character_
  meta$table_name <- c("T1","T2","T3")

  expect_warning(
    expect_equal(
      analyse_metadata(df_metadata = meta,df_eq_indicator = df_eq_lettuce_1),
      answer
    ),
    "hrc_indicator column will be ignored"
  )

}
)

# Two equations linked by one indicator ----------------------------------------
meta <- metadata_pizza_lettuce |>
  filter(hrc_indicator == "hrc_lettuce")

meta <- bind_rows(
  meta,
  meta |>
    dplyr::slice(1:2) |>
    mutate(indicator = c("to_bat1", "to_bat2"),
           table_name = c("T1", "T2"))
) |>
  mutate(hrc_indicator = NA_character_)

df_eq_lettuce_included <- data.frame(
  eq_name = c("eq1","eq2"),
  eq_indicator = c("to_lettuce = to_batavia + to_arugula",
                   "to_batavia = to_bat1 + to_bat2"),
  unit = c("EUR","EUR"),
  stringsAsFactors = FALSE
)

answer <- data.frame(
  cluster = rep("france_entreprises_2023.EUR"),
  table_name = c("T1.T11.T7.T9","T10.T12.T2.T8"),
  field = rep("france_entreprises_2023"),
  indicator = c("to_batavia","to_batavia"),
  spanning_1 = c("HRC_NAF","HRC_NAF"),
  spanning_2 = c("size","cj"),
  spanning_3 = c("EQ1_EQ2^h","EQ1_EQ2^h"),
  hrc_spanning_1 = c("hrc_naf","hrc_naf"),
  hrc_spanning_2 = NA_character_,
  hrc_spanning_3 = c("hrc_EQ1_EQ2.totcode.to_batavia","hrc_EQ1_EQ2.totcode.to_batavia")
)

test_that("eqs_linked_by_one_indicator", {
  expect_warning(
    expect_equal(
      analyse_metadata(df_metadata = meta,df_eq_indicator = df_eq_lettuce_included),
      answer
    ),
    "hrc_indicator column will be ignored"
  )

}
)

# the hrc on fields are handled properly ---------------------------------------
answer <- data.frame(
  cluster = "hrc_geo.EUR",
  table_name = "T1.T2.T3",
  field = "hrc_geo",
  indicator = "to_lettuce",
  spanning_1 = "HRC_GEOh",
  spanning_2 = "size",
  spanning_3 = "EQ1^h",
  hrc_spanning_1 = NA_character_,
  hrc_spanning_2 = NA_character_,
  hrc_spanning_3 = "hrc_EQ1.totcode.to_lettuce"
)

test_that("hierarchies on fields with eq on indicators", {

  meta <- data.frame(
    table_name = paste0("T", 1:3),
    field = c("france_entreprises_2023","metro_entreprises_2023","dom_entreprises_2023"),
    hrc_field = "hrc_geo",
    indicator = c("to_lettuce","to_batavia","to_arugula"),
    hrc_indicator = NA_character_,
    spanning_1 = "size",
    hrc_spanning_1 = NA_character_
  )
  expect_warning(
    expect_equal(analyse_metadata(meta,df_eq_lettuce_1),answer),
    "hrc_indicator column will be ignored"
  )
}
)



