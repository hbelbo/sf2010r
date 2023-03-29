
#' Indivdual machine work time activity data from one indivdual machine work time node
#' @param x is a node tree for one Indivdual machine work time entry
#'
#' @export
#'
#' @examples
#' pth. <- system.file(package = "sf2010r")
#' momfiles <- list.files(pth.,".mom",ignore.case=TRUE,recursive=TRUE,full.names=TRUE)
#' momfiles_imwt <- momfiles[which(stringr::str_detect(momfiles,pattern="individual_mwt"))]
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
#' pth <- system.file(package = "sf2010r")
#' momfiles <- list.files(pth,".mom",ignore.case=TRUE,recursive=TRUE,full.names=TRUE)
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





#' Tracking data from mom-files#'
#'
#' @param doc a StanFord2010 .mom xml-document
#' @export
#'
#' @examples
#' pth <- system.file(package = "sf2010r")
#' momfiles <- list.files(pth,".mom",ignore.case=TRUE,recursive=TRUE,full.names=TRUE)
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
