#' Location data from one Location node
#' @param x is a node tree for one location
#'
#' @export
#'
#' @examples
#' fprfiles <- list.files(path =  system.file(package = "sf2010r"),
#'    pattern = ".fpr", ignore.case = TRUE,  recursive = TRUE, full.names= TRUE)
#' doc <- xml2::read_xml(fprfiles[1])
#' locationlist <- xml2::xml_find_all(doc, ".//d1:LocationDefinition")
#' getLocation(locationlist[[1]]) %>% dplyr::glimpse()
#' plyr::ldply(locationlist[1], getLocation)
getLocation <- function(x) {
  #   x = locationlist[[1]]
  # fprfiles <- list.files(path =  paste0(getwd(), "/inst/extdata"),  pattern = ".fpr", ignore.case = TRUE,  recursive = TRUE, full.names= TRUE)
  chld.dat <- xml_childs_nchr(x)

  #gps <-  xml2::xml_find_all(x, "./d1:LocationCoordinates[@receiverPosition='Base machine position']") %>%
  #  purrr::map_dfr( ~ sf2010r::xml_childs_nchr(.x)) %>%
  #  dplyr::rename_with(~paste0(., "_bm"))
  # Then make the resulting tibble:
  chld.dat <- dplyr::bind_rows(chld.dat)
  chld.dat <- chld.dat %>% dplyr::mutate(dplyr::across(tidyselect::ends_with("Key"), as.integer))
  return(chld.dat)
}


#' Get location data for all locations within a SF2010 .fpr file
#'
#' @param doc a StanFord2010 .hpr xml-document
#'
#' @return a tibble
#' @export
#'
#' @examples
#' fprfiles <- list.files(path =  system.file(package = "sf2010r"),
#'    pattern = ".fpr", recursive = TRUE, full.names= TRUE)
#' doc <- xml2::read_xml(fprfiles[1])
#' getLocations(doc)
getLocations <- function(doc){
  locationlist <- xml2::xml_find_all(doc, ".//d1:LocationDefinition")
  bmatrix <- plyr::ldply(locationlist, sf2010r::getLocation)
  #bmatrix <- plyr::ldply(stemlist, getStemdata, ns = ns)
  MachineKey <-xml2::xml_text(  xml2::xml_find_first(doc, ".//d1:MachineKey"))
  bmatrix$MachineKey = MachineKey
  return(bmatrix)
}



#' Delivery data from one delivery definition node
#' @param x is a node tree for one delivery definition
#'
#' @export
#'
#' @examples
#' fprfiles <- list.files(path =  system.file(package = "sf2010r"),
#'    pattern = ".fpr", ignore.case = TRUE,  recursive = TRUE, full.names= TRUE)
#' doc <- xml2::read_xml(fprfiles[1])
#' nodelist <- xml2::xml_find_all(doc, ".//d1:DeliveryDefinition")
#' getDelivery(nodelist[[1]]) %>% dplyr::glimpse()
#' plyr::ldply(nodelist[1], getDelivery)
#' plyr::ldply(nodelist, getDelivery)
  getDelivery <- function(x) {
  # x = nodelist[[1]]

  chld.dat <- xml_childs_nchr(x)

  # Then make the resulting tibble:
  chld.dat <- dplyr::bind_rows(chld.dat)
  chld.dat <- chld.dat %>% dplyr::mutate(dplyr::across(tidyselect::ends_with("Key"), as.integer))
  return(chld.dat)
}

  #' Get deliveries data for all deliveries within a SF2010 .fpr file
  #'
  #' @param doc a StanFord2010 .hpr xml-document
  #'
  #' @return a tibble
  #' @export
  #'
  #' @examples
  #' fprfiles <- list.files(path =  system.file(package = "sf2010r"),
  #'    pattern = ".fpr", recursive = TRUE, full.names= TRUE)
  #' doc <- xml2::read_xml(fprfiles[1])
  #' getDeliveries(doc)
  getDeliveries <- function(doc){
    deliverieslist <- xml2::xml_find_all(doc, ".//d1:DeliveryDefinition")
    bmatrix <- plyr::ldply(deliverieslist, sf2010r::getDelivery)


    oneDelivry <-xml2::xml_find_first(doc, ".//d1:DeliveryDefinition")
    plvsc <-xml2::xml_attr(xml2::xml_find_all(oneDelivry, ".//d1:Density"), attr = "densityCategory"  )
    plvsc <- gsub('\\b(\\pL)\\pL{1,}|.','\\L\\1',plvsc,perl = TRUE) # Shortening the density categories
    plvsc <- paste("Density_", plvsc, sep = "")
    tst <- names(bmatrix)
    tst[which( stringr::str_detect(tst, "Density" ) == TRUE)] <- plvsc
    names(bmatrix) <- tst

    #bmatrix <- plyr::ldply(stemlist, getStemdata, ns = ns)
    MachineKey <-xml2::xml_text(  xml2::xml_find_first(doc, ".//d1:MachineKey"))
    bmatrix$MachineKey = MachineKey
    return(bmatrix)
  }



  #' Partial Load data from one Load  node
  #' @param x is a node tree for one Load
  #'
  #' @export
  #'
  #' @examples
  #' fprfiles <- list.files(path =  system.file(package = "sf2010r"),
  #'  pattern = ".fpr", ignore.case = TRUE,  recursive = TRUE, full.names= TRUE)
  #' doc <- xml2::read_xml(fprfiles[1])
  #' nodelist <- xml2::xml_find_all(doc, ".//d1:Load")
  #' getPartialLoad(nodelist[[1]]) %>% dplyr::glimpse()
  #' plyr::ldply(nodelist[1], getPartialLoad)
  #' plyr::ldply(nodelist, getPartialLoad)
  getPartialLoad <- function(x) {
    # x = nodelist[[1]]
    #NB: As loadVolumeCategories attributes seems equal across all partial loads in each fpr report, these are fetced once for all for the entire report in another function.
    PartialLoads <- xml2::xml_find_all(x, ".//d1:PartialLoad")
    chld.dat <- plyr::ldply(PartialLoads, xml_childs_nchr)
    chld.dat <- dplyr::bind_rows(chld.dat)
    chld.dat <- chld.dat %>% dplyr::mutate(dplyr::across(tidyselect::ends_with("Key"), as.integer))
    return(chld.dat)
  }




  #' Load data from one Load  node
  #' @param x is a node tree for one Load
  #'
  #' @export
  #'
  #' @examples
  #' fprfiles <- list.files(path =  system.file(package = "sf2010r"),
  #'   pattern = ".fpr", ignore.case = TRUE,  recursive = TRUE, full.names= TRUE)
  #' doc <- xml2::read_xml(fprfiles[1])
  #' nodelist <- xml2::xml_find_all(doc, ".//d1:Load")
  #' getLoad(nodelist[[1]]) %>% dplyr::glimpse()
  #' plyr::ldply(nodelist[1], getLoad)
  #' plyr::ldply(nodelist, getLoad)
  getLoad <- function(x) {
    # x = nodelist[[1]]

    chld.dat <- xml_childs_nchr(x)
    chld.dat <- dplyr::bind_rows(chld.dat)
    chld.dat <- chld.dat %>% dplyr::mutate(dplyr::across(tidyselect::ends_with("Key"), as.integer))

    partial_loads <- getPartialLoad(x)
    partial_loads$LoadKey <- chld.dat$LoadKey
    load_dt <- dplyr::right_join(chld.dat, partial_loads, by = "LoadKey")
    return(load_dt)
  }




  #' Get load data for all loads within a SF2010 .fpr file
  #'
  #' @param doc a StanFord2010 .fpr xml-document
  #'
  #' @return a tibble
  #' @export
  #'
  #' @examples
  #' fprfiles <- list.files(path =  system.file(package = "sf2010r"),
  #'    pattern = ".fpr", recursive = TRUE, full.names= TRUE)
  #' doc <- xml2::read_xml(fprfiles[1])
  #' getLoads(doc)
  getLoads <- function(doc){
    loadlist <- xml2::xml_find_all(doc, ".//d1:Load")
    bmatrix <- plyr::ldply(loadlist, sf2010r::getLoad)
    #bmatrix <- plyr::ldply(loadlist, getLoad)
    onepartialload <- xml2::xml_find_first(loadlist[[1]], ".//d1:PartialLoad")
    plvsc <-xml2::xml_attr(xml2::xml_find_all(onepartialload, ".//d1:LoadVolume"), attr = "loadVolumeCategory"  )
    plvsc <- stringr::str_replace(ifelse(stringr::str_detect(plvsc, pattern = "m3sob|m3sub"), yes = plvsc, no = "Load_othervm"), pattern = "Volume, ", "Load_")

    tst <- names(bmatrix)
    tst[which( stringr::str_detect(tst, "LoadVolume.." ) == TRUE)] <- plvsc
    names(bmatrix) <- tst

    MachineKey <-xml2::xml_text(  xml2::xml_find_first(doc, ".//d1:MachineKey"))
    bmatrix$MachineKey = MachineKey
    return(bmatrix)
  }

