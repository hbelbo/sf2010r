% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fpr_functions.R
\name{getLoads}
\alias{getLoads}
\title{getLoads get all loads (at partial load resolution) from a .fpr-file}
\usage{
getLoads(doc)
}
\arguments{
\item{doc}{a StanFord2010 .fpr xml-document}
}
\value{
a tibble
}
\description{
getLoads get all loads (at partial load resolution) from a .fpr-file
}
\examples{
fprfiles <- list.files(path =  system.file(package = "sf2010r"),
   pattern = ".fpr", recursive = TRUE, full.names= TRUE)
doc <- xml2::read_xml(fprfiles[1])
str(getLoads(doc))
str(getLoads(xml2::read_xml(fprfiles[2])))
}
