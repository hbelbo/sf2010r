#' Get path to Stanford2010 example files in the package
#'
#' sf2010r comes bundled with a number of sample files in its `inst/extdata`
#' directory. This function make them easy to access.
#' The function is inspired by the readr_example in the readr package
#'
#' @param file Name of file. If `NULL`, the example files will be listed.
#' @param fileending one of the file endings in StanFord2010; "hpr", "hqc" etc
#' @export
#' @examples
#' sf2010r_example()
#' sf2010r_example(fileending = "hpr")
sf2010r_example <- function( fileending = NULL) {
  if ( is.null(fileending)) {
    directory <- system.file("extdata", package = "sf2010r")
    files <- dir(system.file("extdata", package = "sf2010r"))
    return(paste(directory, files, sep = "/"))
  } else if(  !is.null(fileending)) {
    directory <- system.file("extdata", package = "sf2010r")
    files <- dir(system.file("extdata", package = "sf2010r"))
    files <- files[which(stringr::str_detect(files, fileending))]
    return(paste(directory, files, sep = "/"))
  }
  }

