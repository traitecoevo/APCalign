
library(tidyverse)
#remotes::install_github("traitecoevo/ausflora")
library(ausflora)
#devtools::load_all()

# Check datastorr can retrieve APC data
# On first run will download and store the data locally
tmp <- dataset_access_function("0.0.0.9000")

# Now check names. Load some example data
original_names <- readr::read_csv(system.file("extdata", "species.csv", package = "ausflora"))

# first we align the names against APC and APNI
data_aligned_names <- align_taxa(original_names$name)

# Now get updated taxonomy for all aligned names
data_taxon_list <- update_taxonomy(data_aligned_names$aligned_name)


# Also have option to pass in names to save output files

# first we align the names against APC and APNI
data_aligned_names <- align_taxa(original_names$name, output = "taxonomic_updates.csv")

# Now get updated taxonomy for all aligned names
data_taxon_list <- update_taxonomy(data_aligned_names$aligned_name, output = "taxon_list.csv")
