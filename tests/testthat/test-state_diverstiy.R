# ss<-create_species_state_origin_matrix(resources=resources)
# 
# ss %>%
#   slice(sample(1:n(),1000) %>%sort()) %>%
#   write_csv("state_diversity.csv")

test_that("state_diversity() works", {
  nsw_species_counts <-
    state_diversity_counts(state = "NSW", resources = resources)
  expect_true(
    sum(nsw_species_counts$num_species) > 7000 &
      sum(nsw_species_counts$num_species) < 10000
  )
  expect_error(state_diversity_counts(state = "NOTASTATE", resources = resources))
  expect_equal(nrow(native_anywhere_in_australia(
    c("Eucalyptus globulus", "Pinus radiata"), resources = resources
  )), 2)
  ss<-create_species_state_origin_matrix(resources=resources)
  sd<-read_csv("state_diversity.csv")
  ss_subset<-filter(ss,ss$species %in% sd$species)
  expect_equal(ss_subset,sd)
})