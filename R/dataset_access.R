
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
  datastorr::github_release_info("FabriceSamonte/datastorrtest",
                                 filename=NULL,
                                 read=unpack_zip,
                                 path=path)
}

versioned_dataset_info <- function(path, version=NULL) {
  
  versioned_package_info <- dataset_info(path)
  if(is.null(version)) {
    ## gets latest remote version if no local version exists,
    ## otherwise it fetches latest local version 
    version <- generate_version(path)
  }
  if(!(version %in% dataset_versions(local=FALSE))) {
    stop(paste0("Version ", version, " does not exist."))
  }
  if(version < local_package_version()) {
    versioned_package_info <- adjust_dataset_info_fields(versioned_package_info, version)
  } else if(version > local_package_version()) {
    if(major_version_change(local_package_version(), version))
      warning(paste0("Warning"))
  }
  versioned_package_info
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
dataset_del <- function(version, path=NULL) {
  datastorr::github_release_del(dataset_info(path), version)
}

get_version_details <- function(path=NULL, version=NULL) {
  info <- dataset_info(path)
  
  ## gets latest remote version if no local version exists,
  ## otherwise it fetches latest local version 
  if(is.null(version)) {
    version <- generate_version(path)
  }
  
  if(major_version_change(desc_version(), version)) {
      warning(paste0("Current package is outdated. Attempting to retrieve newer datasets may cause an error"))
    info$filenames <- NULL 
    info$read <- c(unzip)
    info
  } else if (numeric_version(version) >= numeric_version("2.1.0")){ 
    message("Using unpack methods from version 2.1.0")
    info$filenames <- c("Central_Coast_Leaderboard.csv")
    info$read <- c(read_csv)
    info 
  } else if (numeric_version(version) >= numeric_version("2.0.0")) {
    message("Using unpack methods from version 2.0.0")
    info$filenames <- c("Globcover_Legend.xls", "baad_with_map.csv")
    info$read <- c(read_spreadsheet, read_csv)
    info 
  } else if (numeric_version(version) >= numeric_version("1.0.0")) {
    info$filenames <- c("Globcover_Legend.xls")
    info$read <- c(read_spreadsheet)
    info 
  } else if (numeric_version(version) >= numeric_version("0.0.2")) {
    info$filenames <- "Source.zip"
    info$read <- c(unzip)
    info
  } else if (numeric_version(version) >= numeric_version("0.0.1")) {
    info$filenames <- NULL
    info$read <- c(length)
    info 
  }
}


update_lookaside_table <- function(path=NULL, binary=TRUE) {
  package_info <- dataset_info(path)
  ## NULL filename case should should 
  ## add source repository zip to the table 
  if(!binary) {
    package_info$filenames <- "Source.zip"
  }
  
  ## version check to prevent double entries 
  local_version <- desc_version()
  if(local_version %in% dataset_versions(local=FALSE)) 
    stop(paste0("Version ", local_version, " already exists. Update version field in DESCRIPTION before calling."))
  
  ## file and function assertions, if new release 
  ## consists of binaries 
  if(binary) {
    for(read_function in package_info$read) {
      assert_function(read_function)
    }
    for(filename in package_info$filenames) {
      assert_file(filename)
    }
  
    # apply functions 
    message("Test: loading binaries into R environment")
    for(index in 1:length(package_info$filenames)) {
      switch(unpack(package_info$read[[index]], package_info$filenames[index]),
             "error"= {
               stop()
             }, 
             "ok" = {
               message(paste0("Loading ", package_info$filenames[index]), " succeeded")
             })  
    }
  } 
  
  # update table
  # stored internally via usethis::use_data()
  if(!exists("lookaside_table")) {
    lookaside_table <- tibble::tibble(version = character(),
                                      filename = character(),
                                      unpack_function = character())
  } else if(exists("lookaside_table")) { 
    # clean/reset entries to deal avoid repetition
    lookaside_table <- lookaside_table[lookaside_table$version %in% dataset_versions(local=FALSE) ,]
  }
  
  for(index in 1:length(package_info$filenames)) {
    lookaside_table <- append_lookaside_entry(lookaside_table, local_version, package_info$filename[index], package_info$read[[index]])
  }
  usethis::use_data(lookaside_table, internal=TRUE, overwrite=TRUE)
}

dataset_release <- function(description, path=NULL, ...) {
  local_version <- desc_version()
  if(local_version %in% dataset_versions(local=FALSE)) 
    stop(paste0("Version ", local_version, " already exists. Update version field in DESCRIPTION before calling."))
  
  datastorr::github_release_create(get_version_details(path, local_version),
                                   description=description, ...)
}


