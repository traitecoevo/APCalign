
##' Access Australian Plant Census Dataset
##'
##' This function provides access to the Australian Plant Census dataset containing information
##' about various species. The dataset can be loaded from a github for a stable file or from a URL for the most cutting-edge, but not stable version.
##'
##' @param version Version number. The default is NULL, which will load the most recent
##'   version of the dataset on your computer or the most recent version known
##'   to the package if you have never downloaded the data before. With 
##'   `plant_lookup_del`, specifying `version=NULL` will delete \emph{all} data sets.
##' @param path Path to store the data at. If not given, `datastorr` will use `rappdirs`
##'   to find the best place to put persistent application data on your system. You can
##'   delete the persistent data at any time by running `mydata_del(NULL)` (or 
##'   `mydata_del(NULL, path)` if you use a different path).
##' @param type Type of dataset to access. The default is "stable", which loads the 
##'   dataset from a github archived file. If set to "current", the dataset will be loaded from 
##'   a URL which is the cutting edge version, but this may change at any time without notice.
##' @export
##' @examples
##'
##' # Load the current version of the dataset direct from the APC website
##' dataset_access_function(type = "current")
##'
##' # Load the a stable version of the dataset 
##' dataset_access_function(version="0.0.2.9000",type = "stable")
##'


dataset_access_function <- function(version=default_version(), path=NULL, 
                                    type="stable") {
  if(type=="stable"){
    return(dataset_get(version,path))
  }
  if(type=="current"){
    APC <- readr::read_csv("https://biodiversity.org.au/nsl/services/export/taxonCsv",
                           col_types = 
                             readr::cols(
                               .default = readr::col_character(),
                               proParte = readr::col_logical(),
                               taxonRankSortOrder = readr::col_double(),
                               created = readr::col_datetime(format = ""),
                               modified = readr::col_datetime(format = "")))
    APNI <- readr::read_csv("https://biodiversity.org.au/nsl/services/export/namesCsv",
                            col_types = 
                              readr::cols(
                                .default = readr::col_character(),
                                autonym = readr::col_logical(),
                                hybrid = readr::col_logical(),
                                cultivar = readr::col_logical(),
                                formula = readr::col_logical(),
                                scientific = readr::col_logical(),
                                nomInval = readr::col_logical(),
                                nomIlleg = readr::col_logical(),
                                namePublishedInYear = readr::col_double(),
                                taxonRankSortOrder = readr::col_double(),
                                created = readr::col_datetime(format = ""),
                                modified = readr::col_datetime(format = "")))
    current_list<-list(APC,APNI)  
    names(current_list)<-c("APC", "APNI")
    return(current_list)                      
  }
}



dataset_get <- function(version = default_version(),
                        path = NULL) {
    #APC
    url <- paste0("https://github.com/traitecoevo/ausflora/releases/download/",
                  version,"/apc.parquet")
    apc_hash <- contentid::register(url)
    apc_file <- contentid::resolve(apc_hash,store=TRUE,dir = path)
    APC <- arrow::read_parquet(apc_file)
    
    #APNI
    url <- paste0("https://github.com/traitecoevo/ausflora/releases/download/",
                  version,"/apni.parquet")
    apni_hash <- contentid::register(url)
    apni_file <- contentid::resolve(apni_hash,store=TRUE,dir = path)
    APNI <- arrow::read_parquet(apni_file)
    
    #combine
    current_list <- list(APC, APNI)
    names(current_list) <- c("APC", "APNI")
    return(current_list)
}


