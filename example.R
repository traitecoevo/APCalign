
library(tidyverse)
remotes::install_github("traitecoevo/apcnames")
library(apcnames)

# Check datastorr can retrieve APC data
# On first run will download and store the data locally
tmp <- dataset_access_function("0.0.0.9000")

# Now check names. Load some example data
original_names <- read_csv(system.file("extdata", "species.csv", package = "apcnames"))

# first we aligne the names against APC and APNI
data_aligned_names <- align_taxa(original_names$name)

# Now get updated taxonomy for all aligned names
data_taxon_list <- update_taxonomy(data_aligned_names$aligned_name)

