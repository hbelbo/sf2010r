% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sf2010_general_functions.R
\name{getProductLengthDefs}
\alias{getProductLengthDefs}
\title{get Product length def limits}
\usage{
getProductLengthDefs(doc)
}
\arguments{
\item{doc}{a StanFord2010 xml document}
}
\value{
a data.frame having diameter limits for each product
}
\description{
get Product length def limits
}
\examples{
sffiles <-  list.files(path =  system.file(package = "sf2010r"),
   pattern = ".hpr|.fpr", recursive = TRUE, full.names= TRUE)
docs <- lapply(X = sffiles, FUN = function(X){xml2::read_xml(X)})
getProductLengthDefs(docs[[3]]) \%>\% str()
getProductLengthDefs(docs[[4]]) \%>\% str()
getProductLengthDefs(docs[[5]]) \%>\% str()

}
