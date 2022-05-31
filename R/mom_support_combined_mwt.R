


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

  ForwarderData <- dplyr::bind_rows(xml_childs_nchr(xml2::xml_find_all(x, ".//d1:ForwarderData")))
  LoadVolume_l <- xml2::xml_find_all(x, ".//d1:TotalForwardedVolume")
  LoadVolumeKat <- LoadVolume_l %>%
    purrr::map(~ xml2::xml_attr(.x, attr = "forwardedVolumeCategory")) %>%
    unlist()
  LoadVolume <- LoadVolume_l %>%
    purrr::map(~ xml2::xml_double(.x )) %>%
    unlist()
  ##!! Intention here is to add colume category and corresponding load volume to the ForwarderData

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
  cmwt.data.w <- dplyr::bind_cols(dt1, cmwt.data.w, ForwarderData)


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




