
##' 
##'
##'
##' @title dataset access 
##'
##' @param version Version number.  The default will load the most
##'   recent version on your computer or the most recent version known
##'   to the package if you have never downloaded the data before.
##'   With \code{plant_lookup_del}, specifying \code{version=NULL}
##'   will delete \emph{all} data sets.
##'
##' @param path Path to store the data at. If not given,
##'   \code{datastorr} will use \code{rappdirs} to find the best place
##'   to put persistent application data on your system.  You can
##'   delete the persistent data at any time by running
##'   \code{mydata_del(NULL)} (or \code{mydata_del(NULL, path)} if you
##'   use a different path).
##'
##' @export
##' @examples
##' #
##' # see the format of the resource
##' #
##' #
##' #

dataset_access_function <- function(version=NULL, path=NULL) {
  dataset_get(version, path)
}

## This one is the important part; it defines the three core bits of
## information we need;
##   1. the repository name (traitecoevo/taxonlookup)
##   2. the file to download (plant_lookup.csv)
##   3. the function to read the file, given a filename (read_csv)
dataset_info <- function(path) {
  datastorr::github_release_info("traitecoevo/apcnames",
                                 filename=NULL,
                                 read=unpack_zip,
                                 path=path)
}

dataset_get <- function(version=NULL, path=NULL) {
  datastorr::github_release_get(get_version_details(path, version), version)
}

##' @export
##' @rdname fungal_traits
##' @param local Logical indicating if local or github versions should
##'   be polled.  With any luck, \code{local=FALSE} is a superset of
##'   \code{local=TRUE}.  For \code{mydata_version_current}, if
##'   \code{TRUE}, but there are no local versions, then we do check
##'   for the most recent github version.
dataset_versions <- function(local=TRUE, path=NULL) {
  datastorr::github_release_versions(dataset_info(path), local)
}

##' @export
##' @rdname fungal_traits
dataset_version_current <- function(local=TRUE, path=NULL) {
  datastorr::github_release_version_current(dataset_info(path), local)
}

##' @export
##' @rdname fungal_traits
##' 
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

