
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
##' @param filetype either parquet, which is faster, or csv which is easier with text editors
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
                                    type="stable",filetype = "parquet") {
  if(type=="stable"){
    return(dataset_get(version,path,filetype))
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

## This one is the important part; it defines the three core bits of
## information we need;
##   1. the repository name (traitecoevo/taxonlookup)
##   2. the file to download (plant_lookup.csv)
##   3. the function to read the file, given a filename (read_csv)
dataset_info <- function(path) {
  datastorr::github_release_info("traitecoevo/ausflora",
                                 filename=NULL,
                                 read=unpack_zip,
                                 path=path)
}



dataset_get <- function(version = default_version(),
                        path = NULL,
                        filetype = "parquet") {
  if (filetype == "csv") {
    return(datastorr::github_release_get(get_version_details(path, version), version))
  }
  if (filetype == "parquet") {
    # TO DO
    # use the stable location that Carl referenced (how?)
    #
    tmp_pqt <- tempfile(fileext = ".parquet")
    download.file(
      paste0("https://github.com/traitecoevo/ausflora/releases/download/",version,"/apc.parquet"),
      destfile = tmp_pqt
    )
    APC <- arrow::read_parquet(tmp_pqt)
    tmp_pqt2 <- tempfile(fileext = ".parquet")
    download.file(
      paste0("https://github.com/traitecoevo/ausflora/releases/download/",version,"/apni.parquet"),
      destfile = tmp_pqt2
    )
    APNI <- arrow::read_parquet(tmp_pqt2)
    current_list <- list(APC, APNI)
    names(current_list) <- c("APC", "APNI")
    return(current_list)
  }
}

##' Get Available Versions of the Australian Plant Census and Australian Plant Names Index Datasets
##'
##' This function returns a list of available versions of the Australian Plant Census
##' and Australian Plant Names Index datasets. The versions can be either local or
##' from the Github repository.
##'
##' @param local Logical indicating if local or Github versions should be polled.
##'   The default is TRUE, which means that only local versions will be checked.
##'   If set to FALSE, the function will check for the most recent Github version
##'   if there are no local versions available. Note that local versions take precedence
##'   over Github versions if there are conflicts.
##' @param path Path to store the data at.
##' @export
##' @examples
##' # Get available local versions of the Australian Plant Census and Australian Plant Names Index datasets
##' dataset_versions()
##'
##' # Get available Github versions of the Australian Plant Census and Australian Plant Names Index datasets
##' dataset_versions(local = FALSE)
##'
dataset_versions <- function(local = TRUE, path = NULL) {
  datastorr::github_release_versions(dataset_info(path), local)
}


##' @export
##' @rdname dataset_versions
dataset_version_current <- function(local=TRUE, path=NULL) {
  datastorr::github_release_version_current(dataset_info(path), local)
}

#' Delete a dataset release from GitHub
#'
#' This function deletes a dataset release from GitHub, using the datastorr package.
#' 
#' @param version The version number of the dataset release to delete.
#' @param path (optional) The path to the dataset in the datastorr registry. Defaults to NULL.
#'
#' @rdname dataset_del
#'
#'
#' @return NULL
#'
#' @importFrom datastorr github_release_del
#' 
dataset_del <- function(version, path=NULL) {
  datastorr::github_release_del(dataset_info(path), version)
}


get_version_details <- function(path=NULL, version=NULL) {
  info <- dataset_info(path)
  
  ## gets latest remote version if no local version exists,
  ## otherwise it fetches latest local version 
  if(is.null(version)) {
    version <- generate_version(path)
    print(version)
  }
#  if (numeric_version(version) >= numeric_version("0.0.1")) {
    info$filenames <- c("APC", "APNI")
    info$read <- c(
      #APC
      function(x) readr::read_csv(x, col_types = 
                                    readr::cols(
                                      .default = readr::col_character(),
                                      proParte = readr::col_logical(),
                                      taxonRankSortOrder = readr::col_double(),
                                      created = readr::col_datetime(format = ""),
                                      modified = readr::col_datetime(format = "")
                                      )),
      #APNI
      function(x) readr::read_csv(x, col_types = 
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
                                      modified = readr::col_datetime(format = "")
                                    ))
        )
    info 
#  }
}

dataset_release <- function(description, path=NULL, ...) {
  local_version <- desc_version()
  if(local_version %in% dataset_versions(local=FALSE)) 
    stop(paste0("Version ", local_version, " already exists. Update version field in DESCRIPTION before calling."))
  datastorr::github_release_create(get_version_details(path, local_version),
                                   description=description, ...)
}

