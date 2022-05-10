
#' Indivdual machine work time activity data from one indivdual machine work time node
#' @param x is a node tree for one Indivdual machine work time entry
#'
#' @export
#'
#' @examples
#' momfiles <- list.files(path =  system.file(package = "sf2010r"),  pattern = ".mom", ignore.case = TRUE, recursive = TRUE, full.names= TRUE)
#' momfiles_imwt <- momfiles[which(stringr::str_detect(string = momfiles, pattern = "individual_mwt"))]
#' doc <- xml2::read_xml(momfiles_imwt[2])
#' imwtlist <- xml2::xml_find_all(doc, ".//d1:IndividualMachineWorkTime")
#' getMom.imwt.activity(imwtlist[[1]]) %>% dplyr::glimpse()
#' plyr::ldply(imwtlist[1:5], getMom.imwt.activity)
getMom.imwt.activity <- function(x) {
   #x = imwtlist[[3]]
  cmwt.1 <- data.table::as.data.table(as.list(xml_childs_nchr(x))) # Get all daughters of the MachineWorkTime element
  if ("IndividualMachineRunTimeCategory" %in% names(cmwt.1)){
    cmwt.1 <- data.table::setnames(cmwt.1, old = c("IndividualMachineRunTimeCategory"), new = c("Activity"))
    cmwt.1$timecat = "imwt"
      #cmwt.1 <- cmwt.1 %>%
      #dplyr::rename(Activity = IndividualMachineRunTimeCategory) %>%
      #dplyr::mutate(timecat = "imwt")
    }
#  OtherMachineData <- dplyr::bind_rows(xml_childs_nchr(xml2::xml_find_all(x, ".//d1:OtherMachineData")))
  OtherMachineData <-  data.table::as.data.table(as.list(xml_childs_nchr(xml2::xml_find_all(x, ".//d1:OtherMachineData"))))

#  IndividualMachineDownTime <- dplyr::bind_rows(xml_childs_nchr(xml2::xml_find_all(x, ".//d1:IndividualMachineDownTime")))

  length_imwt_imdt_nodeset <- length(xml2::xml_find_all(x, ".//d1:IndividualMachineDownTime"))
  if(length_imwt_imdt_nodeset) {
    imwt_imdt_nodename <- xml2::xml_name(xml2::xml_child(xml2::xml_find_all(x, ".//d1:IndividualMachineDownTime")))
    imwt_imdt_dt <-  data.table::as.data.table(as.list(xml_childs_nchr(xml2::xml_find_all(x, paste0(".//d1:", imwt_imdt_nodename)))))

    names(imwt_imdt_dt)[1] <- "Activity"
    imwt_imdt_dt$timecat = "imdt"
  } else {imwt_imdt_dt <- data.table::data.table(NULL)}

#   dt1 <- dplyr::bind_cols(cmwt.1, OtherMachineData)
#   cmwt.data.w <- dplyr::bind_cols(dt1, imwt_imdt_dt)
#   ret <- cmwt.data.w

  ret <- data.table::setDT(unlist(list(cmwt.1, OtherMachineData, imwt_imdt_dt), recursive = FALSE))
  return(ret)
  }


#' Indivdual machine work time production data from one indivdual machine work time node
#' @param x is a node tree for one Indivdual machine work time entry
#'
#' @export
#'
#' @examples
#' momfiles <- list.files(path =  system.file(package = "sf2010r"),  pattern = ".mom", ignore.case = TRUE, recursive = TRUE, full.names= TRUE)
#' momfiles_imwt <- momfiles[which(stringr::str_detect(string = momfiles, pattern = "individual_mwt"))]
#' doc <- xml2::read_xml(momfiles_imwt[2])
#' imwtlist <- xml2::xml_find_all(doc, ".//d1:IndividualMachineWorkTime")
#' getMom.imwt.production(imwtlist[[3]]) %>% dplyr::glimpse()
#' plyr::ldply(imwtlist[1:5], getMom.imwt.production)
getMom.imwt.production <- function(x) {
   # x = imwtlist[[3]]
  cmwt.1 <- data.table::as.data.table(as.list(xml_childs_nchr(x))) # Get all daughters of the MachineWorkTime element
  HarvesterDataNodeSets <- xml2::xml_find_all(x, ".//d1:OtherMachineData/d1:HarvesterData")
  if(length(HarvesterDataNodeSets)){
    HarvesterData <- data.table::as.data.table(purrr::map_dfr(HarvesterDataNodeSets, ~ dplyr::bind_rows(xml_childs_nchr(.x))))
    HarvesterData <- dplyr::bind_cols(HarvesterData, cmwt.1[,2:4])

    data.table::setcolorder(HarvesterData, c("SpeciesGroupKey", "ProcessingCategory", "NumberOfHarvestedStems" ))
    return(HarvesterData)
  } else {
    return(data.table::data.table(NULL))
  }

}





#' Combined machine work time data from one combined machine work time node
#' @param x is a node tree for one Combined machine work time entry
#'
#' @export
#'
#' @examples
#' momfiles <- list.files(path =  system.file(package = "sf2010r"),  pattern = ".mom", ignore.case = TRUE, recursive = TRUE, full.names= TRUE)
#' momfiles_cmwt <- momfiles[which(stringr::str_detect(string = momfiles, pattern = "combined_mwt"))]
#' doc <- xml2::read_xml(momfiles_cmwt[2])#'
#' cmwtlist <- xml2::xml_find_all(doc, ".//d1:CombinedMachineWorkTime")
#' getMom.cmwt.data(cmwtlist[[1]]) %>% dplyr::glimpse()
#' plyr::ldply(cmwtlist[1:2], getMom.cmwt.data)
getMom.cmwt.data <- function(x) {
  # x = cmwtlist[[2]]

  cmwt.1 <- dplyr::bind_rows(xml_childs_nchr(x)) # Get all daughters of the MachineWorkTime element
  OtherMachineData <- dplyr::bind_rows(xml_childs_nchr(xml2::xml_find_all(x, ".//d1:OtherMachineData")))

  CombinedMachineRunTime <- xml_childs_nchr(xml2::xml_find_all(x, ".//d1:CombinedMachineRunTime"))
  colnames <- unique(names(CombinedMachineRunTime))
  if (length(colnames)) {
  cmrt <- data.table::setDT(as.data.frame(matrix(CombinedMachineRunTime, ncol = length(colnames), byrow = TRUE)))
  names(cmrt) <- colnames

    cmrt$TimeCategory <- "CombinedMachineRunTime"
    cmrt$TimeCat <- "cmrt"
    data.table::setnames(cmrt, old = c("MachineRunTimeCategory"), new = c("Activity"))
    #cmrt <- cmrt %>% dplyr::rename(Activity = .data$MachineRunTimeCategory)
    cmrt$Activity <- stringr::str_replace_all(stringr::str_to_title(cmrt$Activity), "\\s", "")

  } else { cmrt <- data.table::data.table(NULL)}

  cmwt.data <- cmrt

  CombinedUnutilizedTime <- xml_childs_nchr(xml2::xml_find_all(x, ".//d1:CombinedUnutilizedTime"))
  colnames <- unique(names(CombinedUnutilizedTime))
  if (length(colnames)) {
    cmut <- data.table::setDT(as.data.frame(matrix(CombinedUnutilizedTime, ncol = length(colnames), byrow = TRUE)))
    names(cmut) <- colnames
    cmut$TimeCategory <- "CombinedUnutilizedTime"
    cmut$TimeCat <- "cut"
    data.table::setnames(cmut, old = c("UnutilizedTimeCategory"), new = c("Activity"))
    #cmut <- cmut %>% dplyr::rename(.data,  Activity = .data$UnutilizedTimeCategory )
    cmut$Activity <- stringr::str_replace_all(stringr::str_to_title(cmut$Activity), "\\s", "")
    cmwt.data <- dplyr::bind_rows(cmrt, cmut)
  }

  cmwt.data <- cmwt.data[, c("Activity", "TimeCat", "TimeLength")]


    cmwt.data.w <-
      tidyr::pivot_wider(cmwt.data,
          names_from = c("TimeCat", "Activity"),
          values_from = c("TimeLength"))

  dt1 <- dplyr::bind_cols(cmwt.1, OtherMachineData)
  cmwt.data.w <- dplyr::bind_cols(dt1, cmwt.data.w)


  return(cmwt.data.w)
}


#' Get Combined Machine Work Time for all and within a SF2010 .hpr file
#'
#' @param doc a StanFord2010 .mom xml-document
#'
#' @return a tibble
#' @export
#'
#' @examples
#' momfiles <- list.files(path =  system.file(package = "sf2010r"), pattern = ".mom", ignore.case = TRUE, recursive = TRUE, full.names= TRUE)
#' momfiles_cmwt <- momfiles[which(stringr::str_detect(string = momfiles, pattern = "combined_mwt"))]
#' doc <- xml2::read_xml(momfiles_cmwt[2])
#' getCombined.mwt(doc)
getCombined.mwt <- function(doc){

  cmwtlist <- xml2::xml_find_all(doc, ".//d1:CombinedMachineWorkTime")
  #bmatrix <- plyr::ldply(cmwtlist, sf2010r::getMom.cmwt.data)
  bmatrix <- plyr::ldply(cmwtlist, getMom.cmwt.data)
  MachineKey <-xml2::xml_text(  xml2::xml_find_first(doc, ".//d1:MachineKey"))
  bmatrix$MachineKey = MachineKey
  return(bmatrix)
}






#' Tracking data from mom-files#' @param x is a node tree for one Combined machine work time entry
#'
#' @param x a xml_node with TrackingCoordinates nodes
#' @export
#'
#' @examples
#' momfiles <- list.files(path =  system.file(package = "sf2010r"),  pattern = ".mom", ignore.case = TRUE, recursive = TRUE, full.names= TRUE)
#' doc <- xml2::read_xml(momfiles[3])#'
#' trackinglist <- xml2::xml_find_all(doc, ".//d1:Tracking")
#' getTracking.data(trackinglist) %>% dplyr::glimpse()
#' plyr::ldply(trackinglist, getTracking.data)
getTracking.data <- function(x) {
  # x = trackinglist
  bmtracknodes <- xml2::xml_find_all(x, './/d1:TrackCoordinates[@receiverPosition ="Base machine position"]')

  ret <-
    data.table::data.table(
      bmp_latitude = xml2::xml_double(xml2::xml_find_all(bmtracknodes, ".//d1:Latitude"))
      , bmp_longitude = xml2::xml_double(xml2::xml_find_all(bmtracknodes, ".//d1:Longitude"))
      , bmp_altitude = xml2::xml_double(xml2::xml_find_all(bmtracknodes, ".//d1:Altitude"))
      , bmp_coordinatedate = lubridate::ymd_hms(xml2::xml_text(xml2::xml_find_all(bmtracknodes, ".//d1:CoordinateDate")))

    ) %>%  mutate(difftime = c(0, diff.difftime(bmp_coordinatedate)))



  return(ret)
}
