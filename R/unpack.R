# unpack.R 

read_csv <- function(...) {
  read.csv(..., stringsAsFactors=FALSE)
}

read_spreadsheet <- function(...) {
  readxl::read_xls(...)
}

read_raster <- function(...) {
  raster::raster(...)
}

unpack_zip <- function(...) {
  files <- unzip(...)
  files
}

read_shp <- function(...) {
  
}

unpack <- function(read_function, filename) {
  out <- tryCatch(
    {
      read_function(filename)
      "ok"
    }, 
    error = function(e) {
      message(paste0("Loading ", filename, " failed."))
      message("Check that your unpack function can be applied to the respective file")
      message(e)
      return("error")
    }, 
    warning = function(w) {
      message(paste0("Loading ", filename, " produced some warnings.")) 
      message(w)
      return("warning")
    }, 
    finally = {
      # message(paste0("opening ", filename, " succeeded"))
    }
  )
}