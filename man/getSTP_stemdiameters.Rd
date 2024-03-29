% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hpr_doc_support_functions.R
\name{getSTP_stemdiameters}
\alias{getSTP_stemdiameters}
\title{get SingleTreeProcessed tree's diameter vector. By default not present in the hpr files.}
\usage{
getSTP_stemdiameters(doc)
}
\arguments{
\item{doc}{a hpr document (xml)}
}
\value{
a tibble. If no diametervector is present, a message and NULL.
}
\description{
get SingleTreeProcessed tree's diameter vector. By default not present in the hpr files.
}
\examples{
hprfiles <- list.files(path =  system.file(package = "sf2010r"),
pattern = ".hpr", recursive = TRUE, full.names= TRUE)
docs <- lapply(X = hprfiles, FUN = function(X){xml2::read_xml(X)})
getSTP_stemdiameters(docs[[1]]) \%>\% str()  # does not have stemdiameters
getSTP_stemdiameters(docs[[3]]) \%>\% str()  # does  have stemdiameters
}
