resources <- load_taxonomic_resources()

test_that("create_taxonomic_update_lookup() works with full", {
  create_taxonomic_update_lookup(
    c(
      "Banksia integrifolia",
      "Acacia longifolia",
      "Commersonia rosea",
      "Thelymitra pauciflora",
      "Justicia procumbens",
      "Hibbertia stricta",
      "Rostellularia adscendens",
      "Hibbertia sericea",
      "Hibbertia sp.",
      "Athrotaxis laxiflolia",
      "Genoplesium insigne",
      "Polypogon viridis",
      "Acacia aneura",
      "Acacia paraneura",
      "Galactia striata"
    ),
    resources = resources,
    full = TRUE
  ) -> zz
  expect_more_than(nrow(zz), 80)
})

test_that("create_taxonomic_update_lookup() works with collapse_to_higher_taxon",
          {
            create_taxonomic_update_lookup(
              c(
                "Banksia integrifolia",
                "Acacia longifolia",
                "Commersonia rosea",
                "Thelymitra pauciflora",
                "Justicia procumbens",
                "Hibbertia stricta",
                "Rostellularia adscendens",
                "Hibbertia sericea",
                "Hibbertia sp.",
                "Athrotaxis laxiflolia",
                "Genoplesium insigne",
                "Polypogon viridis",
                "Acacia aneura"
              ),
              one_to_many = "collapse_to_higher_taxon",
              resources = resources
            ) -> zz
            expect_more_than(nrow(zz), 11)
          })
