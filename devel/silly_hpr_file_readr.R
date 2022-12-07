## require('sf2010r')
require('tidyverse')
require('R.utils')
source('~/bin/helpers.r')
wd('sf2010r/sf2010r/R')
sourceDirectory('.', modifiedOnly = TRUE, verbose = TRUE)
wd('/sf2010/devel')


#' Hpr-file reader function
#'
#' @param hprfile filename and path of the hpr file to read

#' @return A list of data.frames: stems, products, logs, machinereport_meta,
#'  operators, objects, stem_grades, pricematrixes,
#'  stemdiametervectors, and stemtypes.
#' @export
#'
#' @examples
#' hprfiles <- list.files(path =  system.file(package = "sf2010r"),
#' pattern = ".hpr", recursive = TRUE, full.names= TRUE)
#' hprtest1 <- hprdata(hprfiles[1])
#' hprtest2 <- hprdata(hprfiles[2])
#' hprtest3 <- hprdata(hprfiles[3])
#'

lfiles <- system.file(package = "sf2010r")

hpr_iterator <- function(hprfile,y = 1, ...){
    hprfile <- list.files(hprfile, pattern = NULL,...)
    return(hprfile)
    fn. <- 'hprdata'
  hprdataMap <- Map(function(x)
    do.call(fn., list(hprfile[x])), y)
    ## hprdata(hprfile[x]), y)
  return(hprdataMap)}


tmp <- hpr_iterator(lfiles, 1, recursive = TRUE, full.names= TRUE)

temp <- hprdata(lfiles, 1,recursive = TRUE, full.names= TRUE)


str(temp)


