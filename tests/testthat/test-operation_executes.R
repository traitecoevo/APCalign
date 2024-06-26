# The tests in this file are primarily designed to check that the functions for 
# taxonomy updating execute with various inputs. Limited attention is given to 
# the results of the calls, only to check behaviour against input options.
# More extensive testing to assess quality of results 
# occurs in the file "test-alignment-results.R"

test_that("create_taxonomic_update_lookup() returns more/less rows as requested", {
  original_name <- 
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
    )
  
  out1 <- 
    create_taxonomic_update_lookup(
      original_name,
      resources = resources,
      taxonomic_splits = "most_likely_species",
      quiet = TRUE
    )

  expect_equal(out1$original_name, original_name)
  
  out2 <- 
    create_taxonomic_update_lookup(
      original_name,
      resources = resources,
      taxonomic_splits = "return_all",
      quiet = TRUE
    )

  # order and number of unique strings same as input
  expect_equal(unique(out2$original_name), original_name)
  
  out3 <- 
    create_taxonomic_update_lookup(
      original_name,
      resources = resources,
      taxonomic_splits = "collapse_to_higher_taxon",
      quiet = TRUE
    ) %>%
    dplyr::mutate(
      should_collapse = ifelse(number_of_collapsed_taxa > 1, TRUE, FALSE),
      number_of_collapsed_taxa = ifelse(is.na(number_of_collapsed_taxa), 1, number_of_collapsed_taxa)
    )

  expect_equal(nrow(out3), length(original_name))
  expect_equal(out3$original_name, original_name)
  expect_equal(sum(out3$number_of_collapsed_taxa)-1, nrow(out2))
  })

test_that("align_taxa() executes - no/with fuzzy", {
  
  original_name <- c("Dryandra preissii", "Banksia acuminata", "Bannksia accuminata")
  aligned_name <- c("Dryandra preissii", "Banksia acuminata", "Banksia acuminata")
  aligned_no_fuzzy <- c("Dryandra preissii", "Banksia acuminata", NA)
  
  out1 <- 
    align_taxa(original_name, resources = resources, fuzzy_matches = TRUE,
               quiet = TRUE)
  out2 <- 
    align_taxa(original_name, resources = resources, fuzzy_matches = FALSE,
               quiet = TRUE)
  
  expect_equal(original_name, out1$original_name)
  expect_equal(aligned_name, out1$aligned_name)
  expect_equal(original_name, out2$original_name)
  expect_equal(aligned_no_fuzzy, out2$aligned_name)
})


test_that("quiet can be turned on and off", {
  
  original_name <- c("Dryandra preissii", "Banksia acuminata", "Bannksia accuminata")

  expect_silent(
    out1 <- 
      align_taxa(original_name, resources = resources, fuzzy_matches = TRUE,
               quiet = TRUE)
    )

  out1 <- 
    capture_messages(align_taxa(original_name, resources = resources, fuzzy_matches = TRUE,
                 quiet = FALSE))
  expect_true(length(out1) > 1)

})

test_that("align_taxa() executes with longer list", {
  species_list <-
    readr::read_csv(system.file("extdata", "species.csv", package = "APCalign"),
                    show_col_types = FALSE) %>% 
    dplyr::slice(1:50)
  aligned_data <- align_taxa(species_list$name, resources = resources, 
                             quiet = TRUE)
  
  expect_equal(nrow(aligned_data), 50)
  expect_equal(species_list$name, aligned_data$original_name)
  })

test_that("update_taxonomy() runs and prdouces suitable structure", {
  
  original_name <- c("Dryandra preissii", "Banksia acuminata")
  
  aligned_data <- 
    align_taxa(original_name, resources = resources, quiet = TRUE)
  
  out1 <-
    update_taxonomy(
      aligned_data = aligned_data,
      resources = resources,
      taxonomic_splits = "most_likely_species"
    )
  
  v <- c("original_name", "aligned_name")
  expect_equal(aligned_data[,v], out1[,v])
  
  out2 <-
    create_taxonomic_update_lookup(
      aligned_data$original_name, resources = resources,
      taxonomic_splits = "most_likely_species",
      quiet = TRUE
    )

  v <- intersect(names(out1) , names(out2))
  expect_equal(out1[,v], out2[,v])

  expect_equal(out1$suggested_name, rep(aligned_data$aligned_name[2], 2))
  expect_equal(out2$accepted_name, rep(aligned_data$aligned_name[2], 2))
})

test_that("check runs with weird hybrid symbols", {
  original_name <- c("Platanus × acerifolia", "Platanus × hispanica")
  
  out <- align_taxa(original_name, resources = resources, quiet = TRUE)
  
  expect_equal(standardise_names(original_name), out$cleaned_name)
  expect_equal(standardise_names(original_name), out$aligned_name)
  
})

test_that("handles NAs inn inputs", {
  original_name <- c("Acacia aneura", NA)

  out1 <- align_taxa(original_name, resources = resources, quiet = TRUE)

  expect_equal(original_name, out1$original_name)
  
  out2 <- 
    create_taxonomic_update_lookup(
      original_name, 
      taxonomic_splits = "most_likely_species",
      resources = resources,
      quiet = TRUE
      )
  
  expect_equal(original_name, out2$original_name)
  expect_equal(original_name, out2$aligned_name)
  expect_equal(original_name, out2$accepted_name)
  expect_equal(original_name[1], word(out2$suggested_name[1], start = 1, end = 2))

  })


test_that("handles weird strings", {
  test_strings <- c("", "''", "'", "          ", "\t", "\n", "stuff with      ",
                    "test'string'withquotes", 
                    "!@#$%^&*()_+", 
                    rep("abc", times= 10),
                    "print('whoops no cleaning')",
                    "Doesthislook likeaspeciesi",
                    "Doesn'tlook likeaspeciesi",
                    "Banksia  serrata"
  )

  out1 <- 
    align_taxa(test_strings, resources = resources, quiet = TRUE)
  
  expect_equal(test_strings, out1$original_name)
  
  out2 <- 
    create_taxonomic_update_lookup(
      test_strings, 
      taxonomic_splits = "most_likely_species",
      resources = resources,
      quiet = TRUE)

  expect_equal(nrow(out1), length(test_strings))
  expect_equal(out1$original_name, test_strings)
  expect_equal(out2$original_name, test_strings)
  
  v <- intersect(names(out1) , names(out2))
  
  expect_equal(out1[,v], out2[,v])
  
  out_v <- c(rep(NA_character_, nrow(out1)-1), "Banksia serrata")
  expect_equal(out2$aligned_name, out_v)
  expect_equal(out2$suggested_name, out2$suggested_name)
  
  })

test_that("handles APNI taxa and genus level IDs",{
  
  original_name <- c("Acacia sp.", "Dendropanax amplifolius", "Acanthopanax divaricatum", "Eucalyptus sp.")
  taxon_rank <- c("genus", "species", "species", "genus")
  taxonomic_dataset <- c("APC", "APNI", "APNI", "APC")
  genus_updated <- c("Acacia", "Dendropanax", "Acanthopanax", "Eucalyptus")
  
  out1 <- 
    align_taxa(original_name, resources = resources, quiet = TRUE)
  
  out2 <- 
    create_taxonomic_update_lookup(
      original_name, 
      taxonomic_splits = "most_likely_species",
      resources = resources, 
      quiet = TRUE,
      output = NULL)
  
  expect_equal(original_name, out1$original_name)
  expect_equal(original_name, out2$original_name)
  expect_equal(taxon_rank, out2$taxon_rank)
  expect_equal(taxonomic_dataset, out2$taxonomic_dataset)
  expect_equal(genus_updated, out2$genus)
  expect_equal(out2$aligned_name, out2$suggested_name)
  expect_equal(length(unique(out2$aligned_reason)), 2)
  expect_equal(length(unique(out2$accepted_name)), 1)
  
  expect_gte(nrow(out1), 4)
  
  expect_false(any(stringr::str_detect(out2$suggested_name, "NA sp.")))
  expect_equal(out2$accepted_name, rep(NA_character_, nrow(out2)))
  
  })

test_that("Runs when neither taxa in in APC", {
  original_name <- c("Acacia sp", "Banksia sp")
  
  out <- 
    create_taxonomic_update_lookup(
      taxa = original_name,
      resources = resources, taxonomic_splits = "most_likely_species",
      quiet = TRUE
    )
  
  # output should be same order and length as input
  expect_equal(out$original_name, original_name)
  })

test_that("no matches to APC accepted names are required", {
  # some genus matches
  out1 <- create_taxonomic_update_lookup(
    taxa = c("Eucalyptus", "Banksia asdasd", "Ryandra sp"), 
    resources = resources, quiet = TRUE)
  expect_equal(nrow(out1), 3)
  
  # all garbage
  out2 <- create_taxonomic_update_lookup(
    taxa = c("Aucalyptus", "Danksia asdasd", "Ryandra sp"), 
    resources = resources, quiet = TRUE)
  expect_equal(nrow(out2), 3)
  expect_equal(out2$aligned_name, c(NA, "Dansiea sp. [Danksia asdasd]", "Randia sp."))
})

test_that("returns same number of rows as input, even with duplicates", {
  
  original_name <-
    c("Dryandra preissii", "Banksia acuminata", 
      "Doesthislook likeaspeciesi", "Doesthislook likeaspeciesi", 
      "Banksia acuminata", "Banksia acuminata", "Hibbertia sericea")
  
  out1 <- 
    align_taxa(
      original_name <- original_name,
      resources = resources, 
      quiet = TRUE
      )

  out2 <-
    update_taxonomy(
      out1, 
      taxonomic_splits = "most_likely_species",
      resources = resources
      )

  out3 <- 
    create_taxonomic_update_lookup(
      taxa = original_name,
      resources = resources, 
      taxonomic_splits = "most_likely_species", 
      quiet = TRUE)
  
  
  out4 <- 
    align_taxa(
      original_name <- original_name,
      resources = resources,
      full = TRUE, quiet = TRUE
    )
  
# outputs should be same order and length as input
  expect_equal(out1$original_name, original_name)
  expect_equal(out2$original_name, original_name)
  expect_equal(out3$original_name, original_name)
  expect_equal(out4$original_name, original_name)
  
  # same alignments
  expect_equal(out2$aligned_name, out1$aligned_name)
  expect_equal(out3$aligned_name, out1$aligned_name)
  expect_equal(out4$aligned_name, out1$aligned_name)

    
  expect_equal(subset(out2$aligned_name, !duplicated(out2$aligned_name)), subset(out1$aligned_name, !duplicated(out1$aligned_name)))
  expect_equal(subset(out2$aligned_name, !duplicated(out2$aligned_name)), subset(out1$aligned_name, !duplicated(out1$aligned_name)))
  expect_gte(length(out2$aligned_name), length(out1$aligned_name))
  expect_equal(ncol(out1), 7) #limited columns (full = FALSE, the default)
  expect_equal(ncol(out4), 26) #all columns (full = TRUE)
  
  #
  expect_equal(out3$original_name, original_name)
  })
