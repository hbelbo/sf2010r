% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hqc_doc_support_functions.R
\name{getControlLogDiameters}
\alias{getControlLogDiameters}
\title{Fetch all control log diameters from one log entry (.hqc files)}
\usage{
getControlLogDiameters(x)
}
\arguments{
\item{x}{a StanFord2010 .hqc log node-tree}
}
\value{
a tibble with all control log diameters belonging to one stem
}
\description{
Fetch all control log diameters from one log entry (.hqc files)
}
\examples{
hqcfiles <- list.files(path =  system.file(package = "sf2010r"),
   pattern = "hqc", recursive = TRUE, full.names= TRUE)
doc <- xml2::read_xml(hqcfiles[1])
hqc_stems <- xml2::xml_find_all(doc, ".//d1:Stem")
hqc_logs <- xml2::xml_find_all(hqc_stems[1], ".//d1:Log")
getControlLogDiameters(hqc_logs[1]) \%>\% dplyr::glimpse()
}
