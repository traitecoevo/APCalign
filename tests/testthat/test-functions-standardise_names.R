test_that("Extract genus", {
  
  taxa <-
    c(
      NA,
      "Banksia integrifolia",
      "Acacia longifolia",
      "Commersonia rosea",
      "Thelymitra pauciflora",
      "Justicia procumbens",
      "Hibbertia",
      "Rostellularia long leaves",
      "Hibbertia sericea var  silliafolius",
      "Hibbertia sp.",
      "x Cynochloris macivorii",
      "(Dockrillia pugioniformis x Dockrillia striolata) x Dockrillia pugioniformis"
    )

  expected <- c(NA, "Banksia", "Acacia", "Commersonia", "Thelymitra", 
                "Justicia", "Hibbertia", "Rostellularia", "Hibbertia", 
                "Hibbertia", "x Cynochloris", "Dockrillia")
  out <- extract_genus(taxa)
  expect_equal(out, expected)
})

test_that("Standardise names names", {
  
  expected <- 
    readr::read_csv("benchmarks/standardise_names.csv", show_col_types = FALSE)
  
  out <-
    dplyr::tibble(taxon_names = expected$taxon_names, 
        standardised_names = standardise_names(taxon_names),
        genus = extract_genus(standardised_names),  
        stripped_names = strip_names(standardised_names), 
        stripped_names_extra = strip_names_extra(stripped_names),
        ) 
  #out %>% readr::write_csv("benchmarks/standardise_names.csv")
  for(v in names(out)){
    expect_equal(out[[v]], expected[[v]], info=v)
  }

})

test_that("Standardise taxon rank translates Latin ranks without mangling already-English ones", {

  # Regression test: "sectio" is a literal prefix of its own English translation ("section"), so a
  # fixed-string substring replacement anywhere in the input used to also match (and mangle) values
  # that are *already* English -- "section"/"subsection"/"zoosection"/"zoosubsection" all contain
  # "sectio" as a substring, producing "sectionn"/"subsectionn"/"zoosectionn"/"zoosubsectionn" (an
  # extra trailing "n"). Found via real AFD-derived invertebrate data (411/314218 rows affected) in the
  # sibling taxonAlign package (traitecoevo/taxonAlign#11).
  #
  # Real data is a *mix* of already-English and still-Latin values in the same column -- so Latin and
  # English terms are deliberately interleaved in one input vector here (not tested as two separate,
  # internally-uniform vectors), confirming standardise_taxon_rank() gets both right in a single,
  # realistic vectorised call rather than only when each kind is fed in isolation.
  expect_equal(
    standardise_taxon_rank(c(
      "section", "sectio", "subsection", "subsectio", "zoosection", "zoosectio",
      "zoosubsection", "zoosubsectio", "supersectio",
      "kingdom", "regnum", "class", "classis", "order", "ordo",
      "family", "familia", "variety", "varietas", "form", "forma"
    )),
    c(
      "section", "section", "subsection", "subsection", "zoosection", "zoosection",
      "zoosubsection", "zoosubsection", "supersection",
      "kingdom", "kingdom", "class", "class", "order", "order",
      "family", "family", "variety", "variety", "form", "form"
    )
  )
})
