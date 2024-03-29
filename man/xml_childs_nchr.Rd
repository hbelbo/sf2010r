% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sf2010_general_functions.R
\name{xml_childs_nchr}
\alias{xml_childs_nchr}
\title{Make a named character vector of children values}
\usage{
xml_childs_nchr(y)
}
\arguments{
\item{y}{a list xml_node with some children nodes}
}
\value{
a named vector
}
\description{
This function takes a xml node and return a
named character vector, where the names are the child node names
and the values are the node values.
No attributes or attribute values are included.
}
\examples{
hprfiles <-  list.files(path =  system.file(package = "sf2010r"),
   pattern = ".hpr", recursive = TRUE, full.names= TRUE)
doc <- xml2::read_xml(hprfiles[1])
x <-  xml2::xml_children(doc)[1] # Get one node of doc, the first is normally a header
xml_childs_nchr(x[[1]]) # The function returns a named vector
x \%>\% purrr::map_dfr( ~ xml_childs_nchr(.x))  # Converted to tibble
}
