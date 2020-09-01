
devtools::load_all()

x <- read_csv('/Users/dfalster/Dropbox/tmp/austraits.build_Will_NSW/data/Ruby_ausplots/data.csv')$taxon_name

metadata_check_taxa(x[1:1000]) -> y

update_taxonomy(y$aligned_name) -> z

y %>% left_join(by="aligned_name", z)
