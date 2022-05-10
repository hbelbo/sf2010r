#' Location data from one Location node
#' @param x is a node tree for one location
#'
#' @export
#'
#' @examples
#' fprfiles <- list.files(path =  system.file(package = "sf2010r"),  pattern = ".fpr", ignore.case = TRUE,  recursive = TRUE, full.names= TRUE)
#' doc <- xml2::read_xml(fprfiles[1])
#' locationlist <- xml2::xml_find_all(doc, ".//d1:LocationDefinition")
#' getLocation(locationlist[[1]]) %>% dplyr::glimpse()
#' plyr::ldply(locationlist[1], getLocation)
getLocation <- function(x) {
  #   x = locationlist[[1]]

  chld.dat <- xml_childs_nchr(x)

  #gps <-  xml2::xml_find_all(x, "./d1:LocationCoordinates[@receiverPosition='Base machine position']") %>%
  #  purrr::map_dfr( ~ sf2010r::xml_childs_nchr(.x)) %>%
  #  dplyr::rename_with(~paste0(., "_bm"))
  # Then make the resulting tibble:
  chld.dat <- dplyr::bind_rows(chld.dat)
  chld.dat <- chld.dat %>% dplyr::mutate(dplyr::across(tidyselect::ends_with("Key"), as.integer))
  return(chld.dat)
}


#' Delivery data from one delivery definition node
#' @param x is a node tree for one delivery definition
#'
#' @export
#'
#' @examples
#' fprfiles <- list.files(path =  system.file(package = "sf2010r"),  pattern = ".fpr", ignore.case = TRUE,  recursive = TRUE, full.names= TRUE)
#' doc <- xml2::read_xml(fprfiles[1])
#' nodelist <- xml2::xml_find_all(doc, ".//d1:DeliveryDefinition")
#' getDelivery(nodelist[[1]]) %>% dplyr::glimpse()
#' plyr::ldply(nodelist[1], getDelivery)
  getDelivery <- function(x) {
  # x = nodelist[[1]]

  chld.dat <- xml_childs_nchr(x)

  #gps <-  xml2::xml_find_all(x, "./d1:LocationCoordinates[@receiverPosition='Base machine position']") %>%
  #  purrr::map_dfr( ~ sf2010r::xml_childs_nchr(.x)) %>%
  #  dplyr::rename_with(~paste0(., "_bm"))
  # Then make the resulting tibble:
  chld.dat <- dplyr::bind_rows(chld.dat)
  chld.dat <- chld.dat %>% dplyr::mutate(dplyr::across(tidyselect::ends_with("Key"), as.integer))
  return(chld.dat)
}




