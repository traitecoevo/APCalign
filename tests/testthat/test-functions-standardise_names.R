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
      "(Dockrillia pugioniformis x Dockrillia striolata) x Dockrillia pugioniformis"
    )

  expected <- c(NA, "Banksia", "Acacia", "Commersonia", "Thelymitra", 
                "Justicia", "Hibbertia", "Rostellularia", "Hibbertia", 
                "Hibbertia", "(Dockrillia")
  out <- extract_genus(taxa)
  expect_equal(out, expected)
})

test_that("Standardise names names", {
  
  expected <- 
    readr::read_csv("benchmarks/standardise_names.csv", show_col_types = FALSE)
  
  out <-
    tibble(taxon_names = expected$taxon_names, 
        standardised_names = standardise_names(taxon_names),
        genus = extract_genus(standardised_names),  
        stripped_names = strip_names(standardised_names), 
        stripped_names_extra = strip_names_2(standardised_names),
        ) 
  #out %>% readr::write_csv("benchmarks/standardise_names.csv")
  expect_equal(out, expected)  
    
})
