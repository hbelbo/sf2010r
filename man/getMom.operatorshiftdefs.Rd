% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mom_functions.R
\name{getMom.operatorshiftdefs}
\alias{getMom.operatorshiftdefs}
\title{get OperatorShiftDefinitions}
\usage{
getMom.operatorshiftdefs(doc)
}
\arguments{
\item{doc}{a StanFord2010 .mom xml-document.}
}
\value{
a tibble having data from the OperatorShiftDefinitions in .mom files,
one entry = one work shift.
}
\description{
get OperatorShiftDefinitions
}
\examples{
momfiles <- list.files(path =  system.file(package = "sf2010r"),
   pattern = ".mom", recursive = TRUE, full.names= TRUE)
docs <- lapply(X = momfiles, FUN = function(X){xml2::read_xml(X)})
str(getMom.operatorshiftdefs(docs[[2]]))
str(getMom.operatorshiftdefs(docs[[3]]))
str(getMom.operatorshiftdefs(docs[[1]]))
}
