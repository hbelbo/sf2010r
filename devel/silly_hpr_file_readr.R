require('sf2010r')
require('tidyverse')
require('R.utils')

source('~/bin/helpers.r')
## wd('sf2010r/sf2010r/R')
setwd('/home/wihe/Documents/sf2010r/sf2010r/R')
sourceDirectory('.', modifiedOnly = TRUE, verbose = TRUE)
## wd('sf2010r/devel')
setwd('/home/wihe/Documents/sf2010r/devel')

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

hprfile <- list.files(lfiles, pattern = '.hpr', ignore.case = TRUE,
                          recursive = TRUE, full.names= TRUE)
   

hpr_iterator <- function(hprfile,y = 1){
    hprfile <- list.files(hprfile, pattern = '.hpr', ignore.cas = TRUE,
                          recursive = TRUE, full.names= TRUE)
    fn. <- c('hprdata')
    ext. <- c('.hpr')
    names(fn.) <- ext.
  data_mapper <- Map(function(x)
      do.call(fn., list(hprfile = hprfile[x])), y)
    names(data_mapper) <- basename(hprfile[y])
  return(data_mapper)}



tmp <- hpr_iterator(lfiles, 1)

basename(names(tmp))

str(temp)


