vlapply <- function(X, FUN, ...) {
  vapply(X, FUN, logical(1), ...)
}
viapply <- function(X, FUN, ...) {
  vapply(X, FUN, integer(1), ...)
}
vnapply <- function(X, FUN, ...) {
  vapply(X, FUN, numeric(1), ...)
}
vcapply <- function(X, FUN, ...) {
  vapply(X, FUN, character(1), ...)
}

pastec <- function(...) {
  paste(..., collapse=", ")
}

split_genus <- function(str) {
  str_split <- strsplit(str, "[_ ]+")
  vcapply(str_split, "[[", 1L)
}

last <- function(x) {
  x[[length(x)]]
}

assert_file <- function(filename) {
  if (!file.exists(filename)) {
    stop(sprintf("%s doesn't exist or cannot be found", filename, 
                 call. = FALSE))
  }
}

assert_function <- function (x, name = deparse(substitute(x))) {
  if (!is.function(x)) {
    stop(sprintf("%s must be a function", name), call. = FALSE)
  }
}

is_version <- function(version) {
  !inherits(try(numeric_version(version), silent=TRUE), "try-error")
}

desc_version <- function() {
  dcf <- as.list(read.dcf(file.path("DESCRIPTION"))[1,])
  dcf$Version
}

generate_version <-  function(path) {
  local_versions <- dataset_versions(local = TRUE, path)
  if(identical(character(0), local_versions)) 
    version <- dataset_version_current(local = FALSE, path) 
  else 
    version <- dataset_version_current(path=path)
}

major_version_change <- function(curr_package_version, requested_version) {
  curr_major_version_num <- as.numeric(regmatches(curr_package_version, regexpr("^\\d", curr_package_version)))
  req_major_version_num <- as.numeric(regmatches(requested_version,regexpr("^\\d",requested_version)))
  
  ifelse(req_major_version_num > curr_major_version_num, TRUE, FALSE)
}
