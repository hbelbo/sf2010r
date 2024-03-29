% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sf2010_general_functions.R
\name{getProductMatrixItems}
\alias{getProductMatrixItems}
\title{Get the Product Matrix items}
\usage{
getProductMatrixItems(x)
}
\arguments{
\item{x}{is a Product definition tree from StanFord2010 xml document}
}
\value{
A tibble, or NULL if no products are defined
}
\description{
Get the Product Matrix items
}
\examples{
hprfiles <-  list.files(path =  system.file(package = "sf2010r"),
   pattern = ".hpr", recursive = TRUE, full.names= TRUE)
doc <- xml2::read_xml(hprfiles[1])
Productslist <- xml2::xml_find_all(doc, ".//d1:ProductDefinition" )
getProductMatrixItems(Productslist[[1]])
}
