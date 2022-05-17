
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
#' getMom.imwt.activity(imwtlist[[41]]) %>% dplyr::glimpse()
#'  plyr::ldply(imwtlist[25:42], getMom.imwt.activity)
getMom.imwt.activity <- function(x) {
  # x = imwtlist[[43]]
  #cmwt.1 <- data.table::as.data.table(as.list(xml_childs_nchr(x))) # Get all daughters of the MachineWorkTime element
  cmwt.1 <- dplyr::bind_rows(xml_childs_nchr(x)) # Get all daughters of the MachineWorkTime element

    if ("IndividualMachineRunTimeCategory" %in% names(cmwt.1)){
    # cmwt.1 <- data.table::setnames(cmwt.1, old = c("IndividualMachineRunTimeCategory"), new = c("Activity"))
    # cmwt.1$timecat = "imwt"
    cmwt.1 <- cmwt.1 %>% dplyr::rename("Activity"= "IndividualMachineRunTimeCategory") %>%
      dplyr::mutate(timecat = "imwt")
    }

    #OtherMachineData <-  data.table::as.data.table(as.list(xml_childs_nchr(xml2::xml_find_all(x, ".//d1:OtherMachineData"))))
    OtherMachineData <- dplyr::bind_rows(xml_childs_nchr(xml2::xml_find_all(x, ".//d1:OtherMachineData")))

    #  IndividualMachineDownTime <- dplyr::bind_rows(xml_childs_nchr(xml2::xml_find_all(x, ".//d1:IndividualMachineDownTime")))

  length_imwt_imdt_nodeset <- length(xml2::xml_find_all(x, ".//d1:IndividualMachineDownTime"))
  if(length_imwt_imdt_nodeset) {
    imwt_imdt_nodename <- xml2::xml_name(xml2::xml_child(xml2::xml_find_all(x, ".//d1:IndividualMachineDownTime")))
    #imwt_imdt_dt <-  data.table::as.data.table(as.list(xml_childs_nchr(xml2::xml_find_all(x, paste0(".//d1:", imwt_imdt_nodename)))))
    imwt_imdt_dt <-  dplyr::bind_rows(xml_childs_nchr(xml2::xml_find_all(x, paste0(".//d1:", imwt_imdt_nodename))))
    if(nrow(imwt_imdt_dt)== 0){
      imwt_imdt_dt <- data.table::data.table(Activity = imwt_imdt_nodename)

    }
    names(imwt_imdt_dt)[1] <- "Activity"
    imwt_imdt_dt$timecat = "imdt"
  } else {imwt_imdt_dt <- data.table::data.table(NULL)}

#   dt1 <- dplyr::bind_cols(cmwt.1, OtherMachineData)
#   cmwt.data.w <- dplyr::bind_cols(dt1, imwt_imdt_dt)
#   ret <- cmwt.data.w

  ret <- data.table::setDT(unlist(list(cmwt.1, OtherMachineData, imwt_imdt_dt), recursive = FALSE))
  return(ret)
    }



#' Indivdual machine work time production data from one individual machine work time node
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
#' plyr::ldply(imwtlist[92:94], getMom.imwt.production)
getMom.imwt.production <- function(x) {
   # x = imwtlist[[91]]


  #varbl = child_name = attrs = grp_id = child_vals = NULL # due to NSE notes in R CMD check

  cmwt.1 <- dplyr::bind_rows(xml_childs_nchr(x)) # Get all daughters of the MachineWorkTime element
  #cmwt.1 <- data.table::as.data.table(as.list(xml_childs_nchr(x))) # Get all daughters of the MachineWorkTime element
  HarvesterDataNodeSets <- xml2::xml_find_all(x, ".//d1:OtherMachineData/d1:HarvesterData")
  if(length(HarvesterDataNodeSets)){
    #xml_childs_nchr(HarvesterDataNodeSets[[1]])
    #xml_childs_dt(HarvesterDataNodeSets[[1]])
    HD <-  purrr::map_dfr(HarvesterDataNodeSets, ~ dplyr::bind_rows(xml_childs_dt(.x))) %>%
      dplyr::mutate(varbl = dplyr::case_when(
        stringr::str_detect(.data$attrs, pattern = "m3s") ~ stringr::str_extract(.data$attrs, "m3s\\w*"),
        TRUE ~ .data$child_name))
    Nvars <- purrr::map_dbl(HarvesterDataNodeSets, ~ nrow(xml_childs_dt(.x)))

    # HD <- data.table::setDT(purrr::map_dfr(HarvesterDataNodeSets, ~ dplyr::bind_rows(xml_childs_dt(.x))))
    # HD[, varbl := child_name][
    #   stringr::str_detect(attrs, pattern = "m3s"), varbl := stringr::str_extract(attrs, "m3s\\w*")]


      HD <- HD %>%
        mutate( grp_id =  (rep(1:length(HarvesterDataNodeSets), times = Nvars))) %>%
        dplyr::select(.data$varbl, .data$child_vals, .data$grp_id) %>%
        tidyr::pivot_wider( names_from = .data$varbl, values_from = .data$child_vals)

      #HD[, grp_id := (rep(1:length(HarvesterDataNodeSets), times = Nvars))]
      #HD <- HD[,list(varbl, child_vals, grp_id)]
      #HD <- tidyr::pivot_wider(HD, names_from = varbl, values_from = child_vals)
        #HarvesterData$ncand <- with(HarvesterData, ifelse(is.na(attrs), cild_name, attrs))
    HarvesterData <- dplyr::bind_cols(HD, cmwt.1[,2:4])

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
#' @param doc a StanFord2010 .mom xml-document
#' @export
#'
#' @examples
#' momfiles <- list.files(path =  system.file(package = "sf2010r"),  pattern = ".mom", ignore.case = TRUE, recursive = TRUE, full.names= TRUE)
#' doc <- xml2::read_xml(momfiles[3])#'
#' getTracking.data(doc) %>% dplyr::glimpse()
getTracking.data <- function(doc) {

  x <- xml2::xml_find_all(doc, ".//d1:Tracking")
  if(length(x)>0){
    bmtracknodes <- xml2::xml_find_all(x, './/d1:TrackCoordinates[@receiverPosition ="Base machine position"]')

    ret <-
    data.table::data.table(
      bmp_latitude = xml2::xml_double(xml2::xml_find_all(bmtracknodes, ".//d1:Latitude"))
      , bmp_longitude = xml2::xml_double(xml2::xml_find_all(bmtracknodes, ".//d1:Longitude"))
      , bmp_altitude = xml2::xml_double(xml2::xml_find_all(bmtracknodes, ".//d1:Altitude"))
      , bmp_coordinatedate = lubridate::ymd_hms(xml2::xml_text(xml2::xml_find_all(bmtracknodes, ".//d1:CoordinateDate")))

    ) %>%  dplyr::mutate(difftime = c(0, diff.difftime(.data$bmp_coordinatedate)))
} else {ret = data.table::data.table() }


  return(ret)
}
