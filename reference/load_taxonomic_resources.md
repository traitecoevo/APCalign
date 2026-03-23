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
  may change at any time without notice.

- version:

  The version number of the dataset to use. Defaults to the default
  version.

- quiet:

  A logical indicating whether to print status of loading to screen.
  Defaults to FALSE.

## Value

A list of taxonomic resource data frames.

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

## Examples

``` r
# \donttest{
load_taxonomic_resources(stable_or_current_data="stable", 
version="2024-10-11")# }
#> Downloading...
#> File downloaded successfully.
#> File downloaded successfully.
#> 
#> Loading resources into memory...
#> ================================================================================================================================================================
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
#> $`APC list (accepted)`
#> # A tibble: 30,565 × 15
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
#> # ℹ 10 more variables: accepted_name_usage_ID <chr>, name_type <chr>,
#> #   taxon_rank <chr>, genus <chr>, stripped_canonical <chr>,
#> #   stripped_canonical2 <chr>, stripped_scientific <chr>, binomial <chr>,
#> #   trinomial <chr>, taxonomic_dataset <chr>
#> 
#> $`APC list (known names)`
#> # A tibble: 69,850 × 15
#>    canonical_name   scientific_name taxonomic_status taxon_ID scientific_name_ID
#>    <chr>            <chr>           <chr>            <chr>    <chr>             
#>  1 Pterostylis cli… Pterostylis cl… alternative name https:/… https://id.biodiv…
#>  2 Pterostylis cre… Pterostylis cr… alternative name https:/… https://id.biodiv…
#>  3 Eucalyptus resi… Eucalyptus res… alternative name https:/… https://id.biodiv…
#>  4 Macrozamia paul… Macrozamia pau… basionym         https:/… https://id.biodiv…
#>  5 Lycopodium creb… Lycopodium cre… basionym         https:/… https://id.biodiv…
#>  6 Lycopodium lock… Lycopodium loc… basionym         https:/… https://id.biodiv…
#>  7 Aralia polaris   Aralia polaris… basionym         https:/… https://id.biodiv…
#>  8 Sium nodiflorum  Sium nodifloru… basionym         https:/… https://id.biodiv…
#>  9 Trachymene koch… Trachymene koc… basionym         https:/… https://id.biodiv…
#> 10 Schefflera brac… Schefflera bra… basionym         https:/… https://id.biodiv…
#> # ℹ 69,840 more rows
#> # ℹ 10 more variables: accepted_name_usage_ID <chr>, name_type <chr>,
#> #   taxon_rank <chr>, genus <chr>, stripped_canonical <chr>,
#> #   stripped_canonical2 <chr>, stripped_scientific <chr>, binomial <chr>,
#> #   trinomial <chr>, taxonomic_dataset <chr>
#> 
#> $`APNI names`
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
#>    canonical_name  accepted_name_usage accepted_name_usage_ID    scientific_name
#>    <chr>           <chr>               <chr>                     <chr>          
#>  1 Encephalartos   Encephalartos Lehm. https://id.biodiversity.… Encephalartos …
#>  2 Encephalartus   Encephalartos Lehm. https://id.biodiversity.… Encephalartus …
#>  3 Encephallartes  Encephalartos Lehm. https://id.biodiversity.… Encephallartes…
#>  4 Catakidozamia   Lepidozamia Regel   https://id.biodiversity.… Catakidozamia …
#>  5 Zamia           Zamia L.            https://id.biodiversity.… Zamia L.       
#>  6 Calamaria       Isoetes L.          https://id.biodiversity.… Calamaria Boeh…
#>  7 Lepidotis       Lycopodiella Holub  https://id.biodiversity.… Lepidotis P.Be…
#>  8 Lateristachys   Lycopodiella Holub  https://id.biodiversity.… Lateristachys …
#>  9 Pseudodiphasium Lycopodium L.       https://id.biodiversity.… Pseudodiphasiu…
#> 10 Diphasium       Lycopodium L.       https://id.biodiversity.… Diphasium C.Pr…
#> # ℹ 2,848 more rows
#> # ℹ 7 more variables: taxonomic_status <chr>, taxon_ID <chr>,
#> #   scientific_name_ID <chr>, name_type <chr>, taxon_rank <chr>, genus <chr>,
#> #   taxonomic_dataset <chr>
#> 
#> $genera_APNI
#> # A tibble: 1,228 × 8
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
#> # ℹ 1,218 more rows
#> # ℹ 3 more variables: taxon_rank <chr>, genus <chr>, taxonomic_dataset <chr>
#> 
#> $genera_all
#> # A tibble: 7,179 × 12
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
#> # ℹ 7,169 more rows
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
```
