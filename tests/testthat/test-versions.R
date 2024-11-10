test_that("Retrieval is possible", {
  versions <- get_versions()
  
  expect_visible(versions)
  expect_named(versions)
})


test_that("Load different versions are possible", {
  versions <- get_versions()
  
  targets <- versions$versions |> sample(3)
  
  versions_3 <- targets |> 
    purrr::map(~load_taxonomic_resources(version = .x))
})