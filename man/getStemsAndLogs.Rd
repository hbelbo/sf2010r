% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hpr_doc_support_functions.R
\name{getStemsAndLogs}
\alias{getStemsAndLogs}
\title{Fetch all stems and all logs in hpr and hqc}
\usage{
getStemsAndLogs(doc)
}
\arguments{
\item{doc}{a StanFord2010 .hpr document}
}
\value{
a list with five data frames.
}
\description{
the function returns a list with five data frames;
stems is all the stems
stplogs is all single tree processed logs
mtplogs is all multi tree processed logs
stemgrades is the grades of all stems
stemdias is the diameter vector for the stem.
}
\examples{
hprfiles <- list.files(path =  system.file(package = "sf2010r"),
pattern = ".hpr", recursive = TRUE, full.names= TRUE)
docs <- lapply(X = hprfiles, FUN = function(X){xml2::read_xml(X)})
getStemsAndLogs(docs[[1]]) \%>\% str()
getStemsAndLogs(docs[[2]]) \%>\% str()
getStemsAndLogs(docs[[3]]) \%>\% str() # MTPS and BoomPositioning
hqcfiles <- list.files(path =  system.file(package = "sf2010r"),
pattern = ".hqc", recursive = TRUE, full.names= TRUE)
hqcdocs <- lapply(X = hqcfiles, FUN = function(X){xml2::read_xml(X)})
getStemsAndLogs(hqcdocs[[1]]) \%>\% str()
getStemsAndLogs(hqcdocs[[2]]) \%>\% str()

}
