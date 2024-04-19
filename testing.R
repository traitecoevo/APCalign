devtools::load_all()

system.time({
resources <- load_taxonomic_resources(version = "0.0.4.9000")
})

library(profvis)
profvis({
  resources <- load_taxonomic_resources(version = "0.0.4.9000")
})

system.time({
  #taxon_name %>% stringr::str_replace_all("\\.", "") 
  
  gsub("\\.", "", taxon_name, perl=TRUE)
})


system.time({
  str_to_lower(taxon_name)
})
