# Load taxonomic reference lists, APC & APNI

This function loads two taxonomic datasets for Australia's vascular
plants, the APC and APNI. It creates several data frames by filtering
and selecting data from the loaded lists.

## Usage

``` r
load_taxonomic_resources(
  stable_or_current_data = "stable",
  version = default_version(),
  quiet = FALSE
)
```

## Arguments

- stable_or_current_data:

  Type of dataset to access. The default is "stable", which loads the
  dataset from a github archived file. If set to "current", the dataset
  will be loaded from a URL which is the cutting edge version, but this
  may change at any time without notice. Loading "current" data requires
  the 'readr' package.

- version:

  The version number of the dataset to use (see
  [`get_versions()`](https://traitecoevo.github.io/APCalign/reference/get_versions.md)).
  Defaults to
  [`default_version()`](https://traitecoevo.github.io/APCalign/reference/default_version.md),
  the most recent release. Ignored for "current" data.

- quiet:

  A logical indicating whether to print status of loading to screen.
  Defaults to FALSE.

## Value

A list of taxonomic resource data frames. Its `"version"` attribute
records which data were loaded: the `version` for stable data, or
`"current, downloaded <date>"` for current data.
[`align_taxa()`](https://traitecoevo.github.io/APCalign/reference/align_taxa.md)
writes it into each `aligned_reason`.

## Details

- It accesses taxonomic data from a dataset using the provided version
  number or the default version.

- The output is several dataframes that include subsets of the APC/APNI
  based on taxon rank and taxonomic status.

- Results are cached in memory for the R session so that repeated calls
  with the same `version` and `stable_or_current_data` arguments return
  immediately without re-downloading or re-processing the data. Use
  [`clear_cached_resources()`](https://traitecoevo.github.io/APCalign/reference/clear_cached_resources.md)
  to force a reload.

- `"current"` data is not cached because it may change between calls.

- Stable versions are downloaded once and kept in
  `tools::R_user_dir("APCalign")`, so later sessions, and sessions
  without an internet connection, can load them without downloading
  again. Each version takes about 20 MB; delete the files there to
  remove versions you no longer need.

## Examples

``` r
# \donttest{
load_taxonomic_resources(stable_or_current_data="stable", 
version="2024-10-11")# }
#> Downloading apc.parquet...
#> File downloaded successfully.
#> Downloading apni.parquet...
#> File downloaded successfully.
#> Loading resources into memory...
#> ...done
#> $APC
#> # A tibble: 109,883 × 38
#>    taxon_ID                 name_type accepted_name_usage_ID accepted_name_usage
#>    <chr>                    <chr>     <chr>                  <chr>              
#>  1 https://id.biodiversity… scientif… https://id.biodiversi… Plantae Haeckel    
#>  2 https://id.biodiversity… scientif… https://id.biodiversi… Charophyta Sachs   
#>  3 https://id.biodiversity… scientif… https://id.biodiversi… Charophyta Sachs   
#>  4 https://id.biodiversity… scientif… https://id.biodiversi… Equisetopsida C.Ag…
#>  5 https://id.biodiversity… scientif… https://id.biodiversi… Equisetopsida C.Ag…
#>  6 https://id.biodiversity… scientif… https://id.biodiversi… Equisetopsida C.Ag…
#>  7 https://id.biodiversity… scientif… https://id.biodiversi… Cycadidae Pax      
#>  8 https://id.biodiversity… scientif… https://id.biodiversi… Cycadales Pers. ex…
#>  9 https://id.biodiversity… scientif… https://id.biodiversi… Cycadales Pers. ex…
#> 10 https://id.biodiversity… scientif… https://id.biodiversi… Cycadales Pers. ex…
#> # ℹ 109,873 more rows
#> # ℹ 34 more variables: nomenclaturalStatus <chr>, nomIlleg <chr>,
#> #   nomInval <chr>, taxonomic_status <chr>, pro_parte <lgl>,
#> #   scientific_name <chr>, scientific_name_ID <chr>, canonical_name <chr>,
#> #   scientific_name_authorship <chr>, parentNameUsageID <chr>,
#> #   taxon_rank <chr>, taxon_rank_sort_order <dbl>, kingdom <chr>, class <chr>,
#> #   subclass <chr>, family <chr>, taxonConceptID <chr>, …
#> 
#> $APNI
#> # A tibble: 24,562 × 48
#>    scientific_name_ID               name_type scientific_name scientificNameHTML
#>    <chr>                            <chr>     <chr>           <chr>             
#>  1 https://id.biodiversity.org.au/… autonym   Acanthus ebrac… <scientific><name…
#>  2 https://id.biodiversity.org.au/… autonym   Acanthus ilici… <scientific><name…
#>  3 https://id.biodiversity.org.au/… scientif… Acanthus ilici… <scientific><name…
#>  4 https://id.biodiversity.org.au/… scientif… Acanthus ilici… <scientific><name…
#>  5 https://id.biodiversity.org.au/… scientif… Acanthus volub… <scientific><name…
#>  6 https://id.biodiversity.org.au/… scientif… Asystasia chel… <scientific><name…
#>  7 https://id.biodiversity.org.au/… phrase-n… Asystasia sp. … <scientific><name…
#>  8 https://id.biodiversity.org.au/… scientif… Avicennia alba… <scientific><name…
#>  9 https://id.biodiversity.org.au/… scientif… Avicennia alba… <scientific><name…
#> 10 https://id.biodiversity.org.au/… scientif… Avicennia mind… <scientific><name…
#> # ℹ 24,552 more rows
#> # ℹ 44 more variables: canonical_name <chr>, canonicalNameHTML <chr>,
#> #   name_element <chr>, nomenclaturalStatus <chr>,
#> #   scientific_name_authorship <chr>, autonym <lgl>, hybrid <lgl>,
#> #   cultivar <lgl>, formula <lgl>, scientific <lgl>, nomInval <lgl>,
#> #   nomIlleg <lgl>, namePublishedIn <chr>, namePublishedInID <chr>,
#> #   namePublishedInYear <dbl>, nameInstanceType <chr>, …
#> 
#> $APC_accepted
#> # A tibble: 30,565 × 16
#>    canonical_name   scientific_name taxonomic_status taxon_ID scientific_name_ID
#>    <chr>            <chr>           <chr>            <chr>    <chr>             
#>  1 Cycas angulata   Cycas angulata… accepted         https:/… https://id.biodiv…
#>  2 Cycas arenicola  Cycas arenicol… accepted         https:/… https://id.biodiv…
#>  3 Cycas armstrong… Cycas armstron… accepted         https:/… https://id.biodiv…
#>  4 Cycas arnhemica  Cycas arnhemic… accepted         https:/… https://id.biodiv…
#>  5 Cycas badensis   Cycas badensis… accepted         https:/… https://id.biodiv…
#>  6 Cycas basaltica  Cycas basaltic… accepted         https:/… https://id.biodiv…
#>  7 Cycas brunnea    Cycas brunnea … accepted         https:/… https://id.biodiv…
#>  8 Cycas cairnsiana Cycas cairnsia… accepted         https:/… https://id.biodiv…
#>  9 Cycas calcicola  Cycas calcicol… accepted         https:/… https://id.biodiv…
#> 10 Cycas canalis    Cycas canalis … accepted         https:/… https://id.biodiv…
#> # ℹ 30,555 more rows
#> # ℹ 11 more variables: accepted_name_usage_ID <chr>, name_type <chr>,
#> #   taxon_rank <chr>, family <chr>, genus <chr>, stripped_canonical <chr>,
#> #   stripped_canonical2 <chr>, stripped_scientific <chr>, binomial <chr>,
#> #   trinomial <chr>, taxonomic_dataset <chr>
#> 
#> $APC_synonyms
#> # A tibble: 69,850 × 16
#>    canonical_name   scientific_name taxonomic_status taxon_ID scientific_name_ID
#>    <chr>            <chr>           <chr>            <chr>    <chr>             
#>  1 Cycas arnhemica… Cycas arnhemic… taxonomic synon… https:/… https://id.biodiv…
#>  2 Cycas arnhemica… Cycas arnhemic… taxonomic synon… https:/… https://id.biodiv…
#>  3 Cycas canalis s… Cycas canalis … taxonomic synon… https:/… https://id.biodiv…
#>  4 Cycas sp., Pine… Cycas sp., Pin… taxonomic synon… https:/… https://id.biodiv…
#>  5 Cycas sp. 'Pine… Cycas sp. 'Pin… taxonomic synon… https:/… https://id.biodiv…
#>  6 Cycas maconochi… Cycas maconoch… taxonomic synon… https:/… https://id.biodiv…
#>  7 Cycas kennedyana Cycas kennedya… taxonomic synon… https:/… https://id.biodiv…
#>  8 Cycas gracilis … Cycas gracilis… taxonomic synon… https:/… https://id.biodiv…
#>  9 Cycas gracilis … Cycas gracilis… taxonomic synon… https:/… https://id.biodiv…
#> 10 Cycas rumphii s… Cycas rumphii … taxonomic synon… https:/… https://id.biodiv…
#> # ℹ 69,840 more rows
#> # ℹ 11 more variables: accepted_name_usage_ID <chr>, name_type <chr>,
#> #   taxon_rank <chr>, family <chr>, genus <chr>, stripped_canonical <chr>,
#> #   stripped_canonical2 <chr>, stripped_scientific <chr>, binomial <chr>,
#> #   trinomial <chr>, taxonomic_dataset <chr>
#> 
#> $APNI_names
#> # A tibble: 12,204 × 13
#>    canonical_name        scientific_name scientific_name_ID name_type taxon_rank
#>    <chr>                 <chr>           <chr>              <chr>     <chr>     
#>  1 (Eucalyptus globulus… (Eucalyptus gl… https://id.biodiv… hybrid-f… species   
#>  2 (Eucalyptus globulus… (Eucalyptus gl… https://id.biodiv… hybrid-f… species   
#>  3 (Eucalyptus johnston… (Eucalyptus jo… https://id.biodiv… hybrid-f… species   
#>  4 (Eucalyptus johnston… (Eucalyptus jo… https://id.biodiv… hybrid-f… species   
#>  5 (Eucalyptus urnigera… (Eucalyptus ur… https://id.biodiv… hybrid-f… species   
#>  6 (Eucalyptus urnigera… (Eucalyptus ur… https://id.biodiv… hybrid-f… species   
#>  7 Abacopteris presliana Abacopteris pr… https://id.biodiv… scientif… species   
#>  8 Abarema clypearia f.… Abarema clypea… https://id.biodiv… scientif… form      
#>  9 Abarema pruinosa      Abarema pruino… https://id.biodiv… scientif… species   
#> 10 Abelia rupestris      Abelia rupestr… https://id.biodiv… scientif… species   
#> # ℹ 12,194 more rows
#> # ℹ 8 more variables: taxonomic_status <chr>, stripped_canonical <chr>,
#> #   stripped_canonical2 <chr>, stripped_scientific <chr>, binomial <chr>,
#> #   trinomial <chr>, genus <chr>, taxonomic_dataset <chr>
#> 
#> $genera_accepted
#> # A tibble: 3,093 × 11
#>    canonical_name accepted_name_usage accepted_name_usage_ID     scientific_name
#>    <chr>          <chr>               <chr>                      <chr>          
#>  1 Cycas          Cycas L.            https://id.biodiversity.o… Cycas L.       
#>  2 Bowenia        Bowenia Hook.f.     https://id.biodiversity.o… Bowenia Hook.f.
#>  3 Lepidozamia    Lepidozamia Regel   https://id.biodiversity.o… Lepidozamia Re…
#>  4 Macrozamia     Macrozamia Miq.     https://id.biodiversity.o… Macrozamia Miq.
#>  5 Equisetum      Equisetum L.        https://id.biodiversity.o… Equisetum L.   
#>  6 Isoetes        Isoetes L.          https://id.biodiversity.o… Isoetes L.     
#>  7 Huperzia       Huperzia Bernh.     https://id.biodiversity.o… Huperzia Bernh.
#>  8 Lycopodiella   Lycopodiella Holub  https://id.biodiversity.o… Lycopodiella H…
#>  9 Lycopodium     Lycopodium L.       https://id.biodiversity.o… Lycopodium L.  
#> 10 Phlegmariurus  Phlegmariurus Holub https://id.biodiversity.o… Phlegmariurus …
#> # ℹ 3,083 more rows
#> # ℹ 7 more variables: taxonomic_status <chr>, taxon_ID <chr>,
#> #   scientific_name_ID <chr>, name_type <chr>, taxon_rank <chr>, genus <chr>,
#> #   taxonomic_dataset <chr>
#> 
#> $genera_synonym
#> # A tibble: 2,858 × 11
#>    canonical_name   accepted_name_usage  accepted_name_usage_ID  scientific_name
#>    <chr>            <chr>                <chr>                   <chr>          
#>  1 Catakidozamia    Lepidozamia Regel    https://id.biodiversit… Catakidozamia …
#>  2 Calamaria        Isoetes L.           https://id.biodiversit… Calamaria Boeh…
#>  3 Lepidotis        Lycopodiella Holub   https://id.biodiversit… Lepidotis P.Be…
#>  4 Lateristachys    Lycopodiella Holub   https://id.biodiversit… Lateristachys …
#>  5 Pseudodiphasium  Lycopodium L.        https://id.biodiversit… Pseudodiphasiu…
#>  6 Diphasium        Lycopodium L.        https://id.biodiversit… Diphasium C.Pr…
#>  7 Austrolycopodium Lycopodium L.        https://id.biodiversit… Austrolycopodi…
#>  8 Pseudolycopodium Lycopodium L.        https://id.biodiversit… Pseudolycopodi…
#>  9 Stachygynandrum  Selaginella P.Beauv. https://id.biodiversit… Stachygynandru…
#> 10 Lycopodioides    Selaginella P.Beauv. https://id.biodiversit… Lycopodioides …
#> # ℹ 2,848 more rows
#> # ℹ 7 more variables: taxonomic_status <chr>, taxon_ID <chr>,
#> #   scientific_name_ID <chr>, name_type <chr>, taxon_rank <chr>, genus <chr>,
#> #   taxonomic_dataset <chr>
#> 
#> $genera_APNI
#> # A tibble: 1,227 × 8
#>    canonical_name scientific_name  taxonomic_status scientific_name_ID name_type
#>    <chr>          <chr>            <chr>            <chr>              <chr>    
#>  1 Hemiadelphis   Hemiadelphis Ne… unplaced         https://id.biodiv… scientif…
#>  2 Leptosiphonium Leptosiphonium … unplaced         https://id.biodiv… scientif…
#>  3 Meyenia        Meyenia Nees     unplaced         https://id.biodiv… scientif…
#>  4 Simonachne     Simonachne E.J.… unplaced         https://id.biodiv… scientif…
#>  5 Thyrsacanthus  Thyrsacanthus N… unplaced         https://id.biodiv… scientif…
#>  6 Synarrhena     Synarrhena F.Mu… unplaced         https://id.biodiv… scientif…
#>  7 Allosorus      Allosorus Bernh. unplaced         https://id.biodiv… scientif…
#>  8 Austrogramme   Austrogramme E.… unplaced         https://id.biodiv… scientif…
#>  9 Neurogramme    Neurogramme Die… unplaced         https://id.biodiv… scientif…
#> 10 Chlamydia      Chlamydia Gaert… unplaced         https://id.biodiv… scientif…
#> # ℹ 1,217 more rows
#> # ℹ 3 more variables: taxon_rank <chr>, genus <chr>, taxonomic_dataset <chr>
#> 
#> $genera_all
#> # A tibble: 7,178 × 12
#>    canonical_name accepted_name_usage accepted_name_usage_ID     scientific_name
#>    <chr>          <chr>               <chr>                      <chr>          
#>  1 Cycas          Cycas L.            https://id.biodiversity.o… Cycas L.       
#>  2 Bowenia        Bowenia Hook.f.     https://id.biodiversity.o… Bowenia Hook.f.
#>  3 Lepidozamia    Lepidozamia Regel   https://id.biodiversity.o… Lepidozamia Re…
#>  4 Macrozamia     Macrozamia Miq.     https://id.biodiversity.o… Macrozamia Miq.
#>  5 Equisetum      Equisetum L.        https://id.biodiversity.o… Equisetum L.   
#>  6 Isoetes        Isoetes L.          https://id.biodiversity.o… Isoetes L.     
#>  7 Huperzia       Huperzia Bernh.     https://id.biodiversity.o… Huperzia Bernh.
#>  8 Lycopodiella   Lycopodiella Holub  https://id.biodiversity.o… Lycopodiella H…
#>  9 Lycopodium     Lycopodium L.       https://id.biodiversity.o… Lycopodium L.  
#> 10 Phlegmariurus  Phlegmariurus Holub https://id.biodiversity.o… Phlegmariurus …
#> # ℹ 7,168 more rows
#> # ℹ 8 more variables: taxonomic_status <chr>, taxon_ID <chr>,
#> #   scientific_name_ID <chr>, name_type <chr>, taxon_rank <chr>, genus <chr>,
#> #   taxonomic_dataset <chr>, cleaned_name <chr>
#> 
#> $family_accepted
#> # A tibble: 307 × 38
#>    taxon_ID                 name_type accepted_name_usage_ID accepted_name_usage
#>    <chr>                    <chr>     <chr>                  <chr>              
#>  1 https://id.biodiversity… scientif… https://id.biodiversi… Cycadaceae Pers.   
#>  2 https://id.biodiversity… scientif… https://id.biodiversi… Zamiaceae Horan.   
#>  3 https://id.biodiversity… scientif… https://id.biodiversi… Equisetaceae Michx…
#>  4 https://id.biodiversity… scientif… https://id.biodiversi… Isoetaceae Rchb.   
#>  5 https://id.biodiversity… scientif… https://id.biodiversi… Lycopodiaceae P.Be…
#>  6 https://id.biodiversity… scientif… https://id.biodiversi… Selaginellaceae Wi…
#>  7 https://id.biodiversity… scientif… https://id.biodiversi… Apiaceae Lindl.    
#>  8 https://id.biodiversity… scientif… https://id.biodiversi… Araliaceae Juss.   
#>  9 https://id.biodiversity… scientif… https://id.biodiversi… Griseliniaceae J.R…
#> 10 https://id.biodiversity… scientif… https://id.biodiversi… Myodocarpaceae Dow…
#> # ℹ 297 more rows
#> # ℹ 34 more variables: nomenclaturalStatus <chr>, nomIlleg <chr>,
#> #   nomInval <chr>, taxonomic_status <chr>, pro_parte <lgl>,
#> #   scientific_name <chr>, scientific_name_ID <chr>, canonical_name <chr>,
#> #   scientific_name_authorship <chr>, parentNameUsageID <chr>,
#> #   taxon_rank <chr>, taxon_rank_sort_order <dbl>, kingdom <chr>, class <chr>,
#> #   subclass <chr>, family <chr>, taxonConceptID <chr>, …
#> 
#> $family_synonym
#> # A tibble: 244 × 11
#>    canonical_name    accepted_name_usage  accepted_name_usage_ID scientific_name
#>    <chr>             <chr>                <chr>                  <chr>          
#>  1 Boweniaceae       Zamiaceae Horan.     https://id.biodiversi… Boweniaceae D.…
#>  2 Lycopodia         Lycopodiaceae P.Bea… https://id.biodiversi… Lycopodia Mirb.
#>  3 Umbelliferae      Apiaceae Lindl.      https://id.biodiversi… Umbelliferae J…
#>  4 Araliae           Araliaceae Juss.     https://id.biodiversi… Araliae Juss.  
#>  5 Griselineae       Griseliniaceae J.R.… https://id.biodiversi… Griselineae A.…
#>  6 Pittosporeae      Pittosporaceae R.Br. https://id.biodiversi… Pittosporeae R…
#>  7 Cardiopterideae   Cardiopteridaceae B… https://id.biodiversi… Cardiopteridea…
#>  8 Quintiniaceae     Paracryphiaceae Air… https://id.biodiversi… Quintiniaceae …
#>  9 Sphenostemonaceae Paracryphiaceae Air… https://id.biodiversi… Sphenostemonac…
#> 10 Astereae          Asteraceae Bercht. … https://id.biodiversi… Astereae Berch…
#> # ℹ 234 more rows
#> # ℹ 7 more variables: taxonomic_status <chr>, taxon_ID <chr>,
#> #   scientific_name_ID <chr>, name_type <chr>, taxon_rank <chr>, genus <chr>,
#> #   taxonomic_dataset <chr>
#> 
#> $hyphenated_words
#>   [1] "agnus-castus"                "alba-compacta"              
#>   [3] "alba-odorata"                "alba-superba"               
#>   [5] "albert-smithii"              "albo-hirta"                 
#>   [7] "albo-lutea"                  "albo-purpuratum"            
#>   [9] "albo-purpurea"               "albo-rosea"                 
#>  [11] "albo-violacea"               "alexander-morrison"         
#>  [13] "alpine-subalpine"            "amnis-edith"                
#>  [15] "anagallis-aquatica"          "anschmidt-lebuhn"           
#>  [17] "atro-virens"                 "augusti-oldfieldii"         
#>  [19] "aurea-floribunda"            "aureo-nitens"               
#>  [21] "aureo-purpurea"              "austro-caledonica"          
#>  [23] "austro-montana"              "autro-orientales"           
#>  [25] "balls-headleya"              "balls-headleyi"             
#>  [27] "banda-new"                   "barba-jovis"                
#>  [29] "barrow-ben"                  "bellairs-kalflora"          
#>  [31] "bellenden-keriense"          "bellenden-keriensis"        
#>  [33] "bencubbin-koorda"            "bindoon-mogumber"           
#>  [35] "binnu-yuna"                  "blunt-leaf"                 
#>  [37] "blunt-lobed"                 "bona-nox"                   
#>  [39] "bonus-henricus"              "boreali-atlanticum"         
#>  [41] "boreali-atlanticus"          "boreo-atlantica"            
#>  [43] "boreo-atlanticum"            "boreo-atlanticus"           
#>  [45] "broad-leaf"                  "broad-leaved"               
#>  [47] "broad-lobed"                 "broad-petaled"              
#>  [49] "broader-leaved"              "broader-lobed"              
#>  [51] "brookton-corrigen"           "buller-snowy"               
#>  [53] "burrell-mt"                  "burrundie-fergusson"        
#>  [55] "bursa-pastoris"              "busselton-scott"            
#>  [57] "cadoux-mthampton"            "caeli-rosa"                 
#>  [59] "canningvale-walpole"         "capillus-veneris"           
#>  [61] "capitis-york"                "capitis-yorkii"             
#>  [63] "caput-bovis"                 "caput-casuarii"             
#>  [65] "caput-medusae"               "caroli-huegelii"            
#>  [67] "carolorum-henricorum"        "castelli-arminii"           
#>  [69] "castello-paivae"             "castlereagh-agnes"          
#>  [71] "caulibus-purpureis"          "caves-snowy"                
#>  [73] "chamaecristae-verae"         "cinereo-puberulae"          
#>  [75] "clunies-rossiae"             "cobar-coolabah"             
#>  [77] "coeli-rosa"                  "coeruleo-punctata"          
#>  [79] "coma-aurea"                  "cometae-vallis"             
#>  [81] "comitae-vallis"              "compacta-rosea"             
#>  [83] "coor-de-wandy"               "coot-tha"                   
#>  [85] "copper-lode"                 "cordate-leaved"             
#>  [87] "cornu-damae"                 "corrigin-traysurin"         
#>  [89] "corymbosae-peltatae"         "creek-keep"                 
#>  [91] "crenato-serrata"             "crista-galli"               
#>  [93] "crus-galli"                  "crus-pavonis"               
#>  [95] "cunderdin-bruce"             "cuneate-leaved"             
#>  [97] "cycnei-sinus"                "dandaragan-badg"            
#>  [99] "dandaragan-badgingarra"      "daniell-esp"                
#> [101] "darwin-tipperary"            "dcsc-opp"                   
#> [103] "dens-leonis"                 "dense-tomentosa"            
#> [105] "dense-tomentosum"            "derby-port"                 
#> [107] "digitate-leaved"             "divided-leaved"             
#> [109] "dorrien-smithii"             "douglas-denison"            
#> [111] "drake-castilloi"             "dryandra-peringillup"       
#> [113] "dubium-traceyi"              "dudinin-lake"               
#> [115] "duval-jouvea"                "ebarren-malando-yungaburra" 
#> [117] "eneabba-arrino"              "eneabba-gillingarra"        
#> [119] "eneabba-kulin"               "espce-hyden"                
#> [121] "esperance-ravensthorpe"      "eu-acutiloba"               
#> [123] "eu-angustifolia"             "eu-didymus"                 
#> [125] "eu-fragilis"                 "eu-isatidea"                
#> [127] "eu-longisetis"               "eu-mucronata"               
#> [129] "eu-ovina"                    "eu-spongiosa"               
#> [131] "ferdinandi-muelleri"         "fernandi-muelleri"          
#> [133] "ficus-indica"                "filix-fragile"              
#> [135] "filix-mas"                   "fine-leaved"                
#> [137] "flammea-elegans"             "flat-fiery"                 
#> [139] "flat-leaf"                   "flat-leaved"                
#> [141] "flavo-virens"                "flore-albo"                 
#> [143] "flore-albo-rubro-marginato"  "flore-plena"                
#> [145] "flore-rubro"                 "flos-cuculi"                
#> [147] "flos-reginae"                "foenum-graecum"             
#> [149] "fontis-woodii"               "fortunae-hibernae"          
#> [151] "friderici-augusta"           "fructo-tecto"               
#> [153] "gadgingarra-eneabba"         "gaudium-solis"              
#> [155] "gillingarra-calingiri"       "gingin-nnorcia"             
#> [157] "glass-house"                 "gloria-medii"               
#> [159] "golden-flowered"             "grace-near"                 
#> [161] "grandiflora-rubra"           "green-leaved"               
#> [163] "gwardell-johnson"            "hairy-leafed"               
#> [165] "heywood-near"                "hill-highland"              
#> [167] "hill-iron"                   "hoptn-isiteb"               
#> [169] "howei-insulae"               "hyacinthiflora-candidissima"
#> [171] "inciso-dentata"              "insulae-howei"              
#> [173] "insulari-sylvatica"          "j-russell-smith"            
#> [175] "jcoleby-williams"            "johannis-lehmannii"         
#> [177] "johannis-moorei"             "jrussell-smith"             
#> [179] "kanangra-boyd"               "kok-saghyz"                 
#> [181] "kulin-pingaring-hyden"       "kulin-pingrup"              
#> [183] "lacryma-jobi"                "lane-poolei"                
#> [185] "large-leaved"                "laxe-ramosa"                
#> [187] "laxe-ramosum"                "leeuwin-naturaliste"        
#> [189] "lepidotae-fimbriatae"        "lignum-vitae"               
#> [191] "lilacina-alba"               "lilacina-carmina"           
#> [193] "linum-stellatum"             "live-bearing"               
#> [195] "long-inflorescence"          "long-pedicelled"            
#> [197] "long-styled"                 "longe-exsertus"             
#> [199] "longer-leaved"               "major-atroviolacea"         
#> [201] "manners-suttoniae"           "maroota-berrilee"           
#> [203] "merdn-holt"                  "mid-north"                  
#> [205] "miniata-minor"               "minus-diffusa"              
#> [207] "minute-flowered"             "montis-cole"                
#> [209] "montis-douglas"              "montis-jayanum"             
#> [211] "montis-sellae"               "montis-zamia"               
#> [213] "montis-zamiae"               "moora-brktn-gnarlbine"      
#> [215] "morsus-ranae"                "mountain-port"              
#> [217] "mtcaroline-chiddarcooping"   "mtwarning-lamngtn"          
#> [219] "muelleri-ferdinandi"         "muelleri-ferdinandii"       
#> [221] "mullewa-morawa"              "murchison-gascoyne"         
#> [223] "n-e"                         "n-w"                        
#> [225] "nanum-album"                 "nanum-atrococcineum"        
#> [227] "nanum-atrosanguineum"        "nanum-luteum"               
#> [229] "nanum-roseum"                "narogn-criche-jerngp"       
#> [231] "narrow-leaved"               "narrow-linear"              
#> [233] "narrow-lobed"                "narrower-leaved"            
#> [235] "newdegate-kondinin"          "ninderry-gold"              
#> [237] "non-scripta"                 "norcia-ongerup"             
#> [239] "north-central"               "north-western"              
#> [241] "notho-manihot"               "nova-anglica"               
#> [243] "nova-guineensis"             "novae-angliae"              
#> [245] "novae-austrocambriae"        "novae-caledoniae"           
#> [247] "novae-cambriae"              "novae-georgiae"             
#> [249] "novae-guineae"               "novae-guineensis"           
#> [251] "novae-hiberniae"             "novae-hollandiae"           
#> [253] "novae-zealandiae"            "novae-zeelandiae"           
#> [255] "novae-zelandiae"             "novi-belgii"                
#> [257] "novo-anglica"                "novo-guineense"             
#> [259] "novo-guineensis"             "novo-hollandiae"            
#> [261] "novo-hollandica"             "novo-hollandicum"           
#> [263] "np-dongara"                  "numeralla-kybean"           
#> [265] "numerous-nerved"             "nux-vomica"                 
#> [267] "nyabing-kulin"               "nyabing-porongurup"         
#> [269] "oblong-lobed"                "once-pinnate"               
#> [271] "one-sided"                   "pallarup-oldfield"          
#> [273] "pallide-fusca"               "pallide-fuscum"             
#> [275] "pantogynae-pleurogynae"      "parvo-angustifolium"        
#> [277] "pauli-guilielmi"             "pauli-gulielmi"             
#> [279] "paulli-guilielmi"            "paulo-gulielmi"             
#> [281] "pecten-veneris"              "penna-marina"               
#> [283] "perth-bunbury"               "pes-caprae"                 
#> [285] "pes-corvus"                  "pes-tigridis"               
#> [287] "pilbara-desert"              "piloso-hispidum"            
#> [289] "plantago-aquatica"           "populnea-acerifolium"       
#> [291] "populneo-acerifolium"        "populneo-acerifolius"       
#> [293] "portae-tartari"              "portus-darwini"             
#> [295] "pseudo-elephantopus"         "pseudo-flavellata"          
#> [297] "pseudo-neurachne"            "pseudo-piperita"            
#> [299] "pseudo-stringybarks"         "pubescenti-hirta"           
#> [301] "purpureo-album"              "purpureo-caerulea"          
#> [303] "purpureo-sericeum"           "pyramidales-d-shaped"       
#> [305] "range-malara"                "ranges-rawlinson"           
#> [307] "red-blotched"                "red-brown"                  
#> [309] "red-eyed"                    "red-leaved"                 
#> [311] "riversleigh-cloncurry"       "robusta-rosea"              
#> [313] "rosa-sinensis"               "rosea-major"                
#> [315] "roseo-albus"                 "rough-haired"               
#> [317] "rubro-aurantiacum"           "rubro-grandiflora"          
#> [319] "run-on"                      "russell-smith"              
#> [321] "russell-smithii"             "s-w"                        
#> [323] "salix-tristis"               "sanguineo-fuscus"           
#> [325] "sapoi-riverensis"            "scott-sellickiana"          
#> [327] "semi-angulata"               "setosa-asperula"            
#> [329] "short-leaved"                "short-lipped"               
#> [331] "short-spined"                "short-tailed"               
#> [333] "simple-haired"               "simple-leaf"                
#> [335] "simple-leaved"               "small-leaved"               
#> [337] "smooth-leaved"               "snow-patch"                 
#> [339] "solms-laubachii"             "south-western"              
#> [341] "spade-leaf"                  "spathulate-leaved"          
#> [343] "spica-venti"                 "spica-ventii"               
#> [345] "spina-christi"               "spreading-leaves"           
#> [347] "springs-eneabba"             "springsure-busthinia"       
#> [349] "st-johni"                    "stout-pedicelled"           
#> [351] "style-topped"                "supra-axillare"             
#> [353] "supra-axillaris"             "tammn-yealrng"              
#> [355] "tank-grasspatch"             "tenterfield-cunningham"     
#> [357] "terete-leaved"               "terra-guilelmii"            
#> [359] "terrae-reginae"              "terrae-victoriae"           
#> [361] "theodori-wolfii"             "tomentoso-hirsuta"          
#> [363] "tomentoso-hirsutum"          "triangular-lobed"           
#> [365] "triumphus-milneri"           "ty-gwyn"                    
#> [367] "unguis-cati"                 "urva-ursi"                  
#> [369] "uva-crispa"                  "uva-ursi"                   
#> [371] "van-gaertii"                 "van-geertii"                
#> [373] "van-houtteana"               "van-royena"                 
#> [375] "velutino-eriocarpa"          "villous-flowered"           
#> [377] "viridiflora-alba"            "vscarth-johnson"            
#> [379] "walpole-nornalup"            "warning-gibraltar"          
#> [381] "warning-gibralter"           "wassi-kussae"               
#> [383] "watheroo-moora"              "watheroo-simple"            
#> [385] "west-central"                "white-mahoganies"           
#> [387] "woodanilling-katanning"      "yellow-flowered"            
#> [389] "york-narrogin"               "z-bend"                     
#> 
#> attr(,"version")
#> [1] "2024-10-11"
```
