test_that("create_species_state_origin_matrix() works", {
  state_matrix <- create_species_state_origin_matrix(resources = resources, include_infrataxa = TRUE)
  
  expect_gt(nrow(state_matrix), 30000)
  expect_contains(state_matrix$family, "Myrtaceae")
  expect_contains(state_matrix$species, "Avicennia marina subsp. eucalyptifolia")
  expect_equal(intersect(state_matrix$species, c("Avicennia marina subsp. eucalyptifolia", "Avicennia marina")) |> length(), 2)

  state_matrix_species_only <- create_species_state_origin_matrix(resources = resources, include_infrataxa = FALSE)
  
  expect_lt(nrow(state_matrix_species_only), 30000)
  expect_contains(state_matrix_species_only$family, "Myrtaceae")
  expect_equal(intersect(state_matrix_species_only$species, c("Avicennia marina subsp. eucalyptifolia", "Avicennia marina")) |> length(), 1)
})


test_that("state_diversity() works", {
  nsw_species_counts <-
    state_diversity_counts(state = "NSW", resources = resources, include_infrataxa = FALSE)
  expect_true(
    sum(nsw_species_counts$num_species) > 7000 &
      sum(nsw_species_counts$num_species) < 10000
  )
  expect_error(state_diversity_counts(state = "NOTASTATE", resources = resources))
  ss <- create_species_state_origin_matrix(resources = resources)
  
  sd <- readr::read_csv("benchmarks/state_diversity.csv", 
                        show_col_types = FALSE)
  ss_subset <- dplyr::filter(ss, ss$species %in% sd$species)
  
  #readr::write_csv(ss,"tests/testthat/benchmarks/state_diversity.csv")
  expect_equal(ss_subset, sd)
})

test_that("state_diversity() works with `include_infrataxa = TRUE`", {

  nsw_species_counts_infrataxa <-
    state_diversity_counts(state = "NSW", resources = resources, include_infrataxa = TRUE)

  nsw_species_counts <-
    state_diversity_counts(state = "NSW", resources = resources, include_infrataxa = FALSE)
  
  expect_gt(
    sum(nsw_species_counts_infrataxa$num_species),
    sum(nsw_species_counts$num_species)
  )

})

test_that("native_anywhere_in_australia() works", {
  expect_warning(native_check <-
    native_anywhere_in_australia(
      c(
        "Eucalyptus globulus",
        "Pinus radiata",
        "Brassica rapa",
        "banksis notaspecies"
      ),
      resources = resources
    ))
  # readr::write_csv(native_check,"tests/testthat/benchmarks/native_check.csv")
  previous_check <- readr::read_csv("benchmarks/native_check.csv", show_col_types = FALSE)
  expect_equal(native_check, previous_check)
  expect_warning(native_anywhere_in_australia(species = "NOTASPECIES", resources = resources))
})


test_that("is_native_anywhere() reads only the state columns", {
  # `nativitatis` is a real APC epithet (three Christmas Island taxa), so the
  # taxon name must never be allowed to decide native status on its own.
  lookup <- dplyr::tibble(
    family = c("Orchidaceae", "Poaceae", "Pittosporaceae"),
    species = c("Flickingeria nativitatis", "Ischaemum nativitatis", "Pittosporum undulatum"),
    taxon_ID = c("id-1", "id-2", "id-3"),
    ChI = c("native", "naturalised", "not present"),
    NSW = c("not present", "not present", "native and naturalised")
  )

  expect_equal(is_native_anywhere(lookup), c(TRUE, FALSE, TRUE))
})


test_that("get_apc_genus_family_lookup() works", {
  expect_warning(family_check <-
                   get_apc_genus_family_lookup(
                     c(
                       "Eucalyptus",
                       "Pinus",
                       "Brassica",
                       "not a species"
                     ),
                     resources = resources
                   ))
  # readr::write_csv(family_check,"tests/testthat/benchmarks/family_check.csv")
  previous_check <- readr::read_csv("benchmarks/family_check.csv", show_col_types = FALSE)
  expect_equal(family_check, previous_check)
})


