test_that("Extract genus retruns expected results", {
  
  taxa <-
    c(
      "Banksia integrifolia",
      "Acacia longifolia",
      "Commersonia rosea",
      "Thelymitra pauciflora",
      "Justicia procumbens",
      "Hibbertia",
      "Rostellularia long leaves",
      "Hibbertia sericea var  silliafolius",
      "Hibbertia sp."
    )

  expected <- c("Banksia", "Acacia", "Commersonia", "Thelymitra", "Justicia", "Hibbertia", "Rostellularia", "Hibbertia", "Hibbertia")
  out <- extract_genus(taxa)
  expect_equal(out, expected)
  })
