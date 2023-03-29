

#' Stem data from one Stem node
#' @param x is a node tree for one stem
#'
#' @export
#'
#' @examples
#' pth <- system.file(package = "sf2010r")
#' hprfiles <- list.files(pth,".hpr",recursive=TRUE,full.names=TRUE)
#' doc <- xml2::read_xml(hprfiles[3])
#' stemlist <- xml2::xml_find_all(doc, ".//d1:Stem")
#' getStemdata(stemlist[[1]]) %>% dplyr::glimpse()
#' plyr::ldply(stemlist[1:10], getStemdata)
getStemdata <- function(x) {
  # x = stemlist[[1]]

  stm <- xml_childs_nchr(x)

  gps_bm <-  xml2::xml_find_all(x, "./d1:StemCoordinates[@receiverPosition='Base machine position']") %>%
    purrr::map_dfr( ~ sf2010r::xml_childs_nchr(.x)) %>%
    dplyr::rename_with(~paste0(., "_bm"))


  CoordinateDate = xml2::xml_text(  xml2::xml_find_first(x, ".//d1:CoordinateDate"))

  gps_ctf <-  xml2::xml_find_all(x, "./d1:StemCoordinates[@receiverPosition='Crane tip position when felling the tree']") %>%
    purrr::map_dfr( ~ sf2010r::xml_childs_nchr(.x)) %>%
    dplyr::rename_with(~paste0(., "_ctf"))

  lat_dir =  xml2::xml_attr(xml2::xml_find_first(x,  "./d1:StemCoordinates/d1:Latitude"), attr = "latitudeCategory")
  lon_dir =  xml2::xml_attr(xml2::xml_find_first(x,  "./d1:StemCoordinates/d1:Longitude"), attr = "longitudeCategory")

  # Then make the resulting tibble:
  stemdat <- dplyr::bind_rows(stm)

  if(nrow(gps_bm)) {
    stemdat <- dplyr::mutate(stemdat, gps_bm)
    stemdat <- dplyr::mutate(stemdat, CoordinateDate, lat_dir, lon_dir)
  }
  if(nrow(gps_ctf)) { stemdat <- dplyr::mutate(stemdat, gps_ctf)}

  stemdat <- stemdat %>% dplyr::mutate(dplyr::across(tidyselect::ends_with("Key"), as.integer))
  return(stemdat)
}


#' Get stemdata for all stems within a SF2010 .hpr file
#'
#' @param doc a StanFord2010 .hpr xml-document
#'
#' @return a tibble
#' @export
#'
#' @examples
#' pth <- system.file(package = "sf2010r")
#' hprfiles <- list.files(pth,".hpr",recursive=TRUE,full.names= TRUE)
#' doc <- xml2::read_xml(hprfiles[3])
#' getStems(doc)
getStems <- function(doc){
  stemlist <- xml2::xml_find_all(doc, ".//d1:Stem")
  bmatrix <- plyr::ldply(stemlist, sf2010r::getStemdata)
  #bmatrix <- plyr::ldply(stemlist, getStemdata, ns = ns)
  MachineKey <-xml2::xml_text(  xml2::xml_find_first(doc, ".//d1:MachineKey"))
  bmatrix$MachineKey = MachineKey
  return(bmatrix)
}



#' Fetch the stem grades for each stem in hpr
#' @param x a StanFord2010 stem nodetree
#'
#' @return a tibble.
#' @export
#'
#' @details  NB: One stem section might have several grades simultaneously
#' @examples
#' pth <- system.file(package = "sf2010r")
#' hprfiles <- list.files(pth,".hpr",recursive = TRUE, full.names= TRUE)
#' doc <- xml2::read_xml(hprfiles[1])
#' stemlist <- xml2::xml_find_all(doc, ".//d1:Stem")
#' getStemGrades(stemlist[[1]])
#' plyr::ldply(stemlist[1:3], getStemGrades)
getStemGrades <- function(x){
  # x <- stemlist[[29]]

  StemKey <- xml2::xml_integer(  xml2::xml_find_all(x, ".//d1:StemKey"))
  GradesL <- xml2::xml_find_all(x,  ".//d1:StemGrade/d1:GradeValue")

  Grades <- xml2::xml_integer( GradesL)
  GradeStartPosL <- as.integer(xml2::xml_attr(GradesL,  attr = "gradeStartPosition"))
  GradeStemKey <- rep.int(StemKey, times = length(Grades))

    Gradesdf <- tibble::tibble(StemKey = GradeStemKey,
                     gradestartpos_cm = GradeStartPosL,
                     grades = Grades)
  return(Gradesdf)
}
# getStemGrades(stemlist[[29]])



#' Fetch the single tree processed logs from one stem node tree
#'
#' @param x a StanFord2010 .hpr stem node tree
#'
#' @return a tibble with all logs belonging to one stem
#' @export
#'
#' @examples
#' pth <- system.file(package = "sf2010r")
#' hprfiles <- list.files(pth,".hpr",recursive=TRUE,full.names=TRUE)
#' doc <- xml2::read_xml(hprfiles[1])
#' stemlist <- xml2::xml_find_all(doc, ".//d1:Stem")
#' pcat <- ".//d1:ProcessingCategory"
#' wtch <-  which(xml2::xml_text(xml2::xml_find_all(stemlist,pcat))=="MultiTreeProcessing")
#' getSTPlogs(stemlist[1]) %>% dplyr::glimpse()
#' if(length(wtch) > 0) { getSTPlogs(stemlist[wtch[1]]) %>% dplyr::glimpse()}
getSTPlogs <- function(x) {
  # x <- stemlist[1]
  # x <- stemlist[wtch[1]]
  ProcessingCategory =  xml2::xml_text(  xml2::xml_find_all(x, ".//d1:ProcessingCategory"))

  if( ProcessingCategory == "SingleTreeProcessing"){
    StemKey <-  xml2::xml_integer(  xml2::xml_find_all(x, ".//d1:StemKey"))

    logs  <- xml2::xml_find_all(x, ".//d1:Log")
    logkeys <- logs %>% purrr::map(~
                                     xml2::xml_integer(xml2::xml_find_all(.x, "./d1:LogKey")))

    ProductKey <- logs %>% purrr::map(~  xml2::xml_integer(  xml2::xml_find_all(.x, ".//d1:ProductKey")))

    LogLength <- logs %>%
      purrr::map(~  xml2::xml_integer(  xml2::xml_find_all(.x, ".//d1:LogMeasurement[@logMeasurementCategory='Machine']/d1:LogLength")))

    # Volume data
    LogVolume_l <- logs %>% purrr::map(~ xml2::xml_double(xml2::xml_find_all(.x, ".//d1:LogVolume[@logMeasurementCategory='Machine']")))
    logvolumecategories <- logs %>% purrr::map(~ xml2::xml_attr(xml2::xml_find_all(.x, ".//d1:LogVolume[@logMeasurementCategory='Machine']"), attr = "logVolumeCategory"  ))
    logkeys_vol <-  unlist(purrr::map2(logkeys, LogVolume_l, ~ rep( .x, times = length(.y)) ))
    data.frame(LogKey = logkeys_vol) %>%
      dplyr::mutate(logVolumeCategory = (unlist(logvolumecategories))) %>%
      dplyr::mutate(LogVolume = unlist(LogVolume_l)) -> LogVolume_l

    LogVolume <- tidyr::pivot_wider(LogVolume_l, names_from = "logVolumeCategory", values_from = "LogVolume")

    # Log diameters
    logdias <- logs %>% purrr::map(~ xml2::xml_integer(xml2::xml_find_all(.x, ".//d1:LogMeasurement[@logMeasurementCategory='Machine']/d1:LogDiameter")))
    categories <- logs %>% purrr::map_dfr(~ xml2::xml_attrs(xml2::xml_find_all(.x, ".//d1:LogMeasurement[@logMeasurementCategory='Machine']/d1:LogDiameter")))
    logkeys_l <-  purrr::map2(logkeys, logdias, ~ rep( .x, times = length(.y)) )
    logdiasl <- purrr::map2_dfr(logdias, logkeys_l, ~ dplyr::bind_cols(LogKey = .y, LogDiameter = .x)) %>%
      dplyr::bind_cols(categories)

    LogDiameters <- tidyr::pivot_wider(logdiasl, names_from = "logDiameterCategory", values_from = "LogDiameter")


    # Then make the resulting tibble:
    logdat <- tibble::tibble(
      StemKey = StemKey
      , LogKey =  unlist(logkeys)
      , ProductKey = unlist(ProductKey)
      , LogLength = unlist(LogLength)) %>%
      dplyr::left_join(LogVolume, by = "LogKey") %>%
      dplyr::left_join(LogDiameters, by = "LogKey")



  } else {logdat = NULL}
  return(logdat)
}
# getSTPlogs(stemlist[1])



#' Fetch the multi tree processed logs belonging to one multi-stem entry in hpr
#'
#' @param x a StanFord2010 .hpr stem node tree
#'
#' @return a tibble with all logs belonging to one stem
#' @export
#'
#' @examples
#' pth <- system.file(package = "sf2010r")
#' hprfiles <- list.files(pth,".hpr",recursive=TRUE,full.names=TRUE)
#' doc <- xml2::read_xml(hprfiles[1])
#' stemlist <- xml2::xml_find_all(doc, ".//d1:Stem")
#' pcat <- ".//d1:ProcessingCategory"
#' wtch <- which(xml2::xml_text(xml2::xml_find_all(stemlist,pcat))=="MultiTreeProcessing")
#' getMTPlogs(stemlist[1])
#' if(length(wtch) > 0) { getMTPlogs(stemlist[wtch[1]])}
getMTPlogs <- function(x) {
  # x <- stemlist[1]
  # x <- stemlist[wtch[1]]
  ProcessingCategory <-  xml2::xml_text(  xml2::xml_find_first(x, ".//d1:ProcessingCategory"))

  if( ProcessingCategory == "MultiTreeProcessing"){


    StemBunchKey <-  xml2::xml_integer(  xml2::xml_find_all(x, ".//d1:StemBunchKey"))
    LogKey   <- xml2::xml_integer(  xml2::xml_find_all(x, ".//d1:LogKey"))


    logs  <- xml2::xml_find_all(x, ".//d1:Log")
    logkeys <- logs %>% purrr::map(~
                                     xml2::xml_integer(xml2::xml_find_all(.x, "./d1:LogKey")))

    StemBunchKey <- rep(StemBunchKey, length(logs))

    ProductKey <-  xml2::xml_integer(  xml2::xml_find_all(x, ".//d1:ProductKey"))
    LogLength <- xml2::xml_integer(  xml2::xml_find_all(x, ".//d1:LogLength"))

    # Volume data
    # xml_find_all(logs[[1]], ".//d1:LogVolume[@logMeasurementCategory='Machine']")
    LogVolume_l <- logs %>% purrr::map(~ xml2::xml_double(xml2::xml_find_all(.x, ".//d1:LogVolume[@logMeasurementCategory='Machine']")))
    logvolumecategories <- logs %>% purrr::map(~ xml2::xml_attr(xml2::xml_find_all(.x, ".//d1:LogVolume[@logMeasurementCategory='Machine']"), attr = "logVolumeCategory"  ))
    logkeys_vol <-  unlist(purrr::map2(logkeys, LogVolume_l, ~ rep( .x, times = length(.y)) ))
    data.frame(LogKey = logkeys_vol) %>%
      dplyr::mutate(logVolumeCategory = (unlist(logvolumecategories))) %>%
      dplyr::mutate(LogVolume = unlist(LogVolume_l)) -> LogVolume_l

    LogVolume <- tidyr::pivot_wider(LogVolume_l, names_from = "logVolumeCategory", values_from = LogVolume)

    # Log diameters
    # xml_find_all(logs, ".//d1:LogMeasurement[@logMeasurementCategory='Machine']/d1:LogDiameter")
    logdias <- logs %>% purrr::map(~ xml2::xml_integer(xml2::xml_find_all(.x, ".//d1:LogMeasurement[@logMeasurementCategory='Machine']/d1:LogDiameter")))
    categories <- logs %>% purrr::map_dfr(~ xml2::xml_attrs(xml2::xml_find_all(.x, ".//d1:LogMeasurement[@logMeasurementCategory='Machine']/d1:LogDiameter")))
    logkeys_l <-  unlist(purrr::map2(logkeys, logdias, ~ rep( .x, times = length(.y)) ))
    logdiasl <- tibble::tibble(LogDiameter = unlist(logdias))  %>% dplyr::bind_cols(categories) %>%
      dplyr::mutate(LogKey = logkeys_l)

    LogDiameters <- tidyr::pivot_wider(logdiasl, names_from = "logDiameterCategory", values_from = "LogDiameter")

    # Then make the resulting tibble:
    logdat <- tibble::tibble(
      StemBunchKey = StemBunchKey
      , LogKey = unlist(logkeys)
      , ProductKey
      , LogLength
    ) %>%
      dplyr::left_join( LogVolume, by = "LogKey" ) %>%
      dplyr::left_join(LogDiameters, by = "LogKey")
  } else {logdat = NULL}

  return(logdat)
}




#' Get logsdata for all stems within a SF2010 .hpr file
#'
#' @param doc a StanFord2010 .hpr xml-document
#'
#' @return a tibble
#' @export
#'
#' @examples
#' pth <- system.file(package = "sf2010r")
#' hprfiles <- list.files(pth,".hpr",recursive=TRUE,full.names=TRUE)
#' doc <- xml2::read_xml(hprfiles[1])
#' getLogs(doc)
getLogs <- function(doc){
  stemlist <- xml2::xml_find_all(doc, ".//d1:Stem")
  STPlogs <- purrr::map_dfr(stemlist, ~getSTPlogs(.x))
  #STPlogs <- plyr::ldply(stemlist, getSTPlogs)
  MTPlogs <- purrr::map_dfr(stemlist, ~getMTPlogs(.x))
  MachineKey <-xml2::xml_text(  xml2::xml_find_first(doc, ".//d1:MachineKey"))


  logs <- tibble::tibble()
  if(dim(STPlogs)[1]>0){

    logs <- dplyr::bind_rows(logs, STPlogs)
  }
  if(dim(MTPlogs)[1]>0){
    logs <- dplyr::bind_rows(logs, MTPlogs) }

  logs$MachineKey = MachineKey
  return(logs)
}





#' get SingleTreeProcessed tree's diametres
#'
#' @param doc a hpr document (xml)
#' @return a tibble
#' @export
#'
#' @examples
#' pth <- system.file(package = "sf2010r")
#' hprfiles <- list.files(pth,".hpr",recursive=TRUE,full.names= TRUE)
#' doc <- xml2::read_xml(hprfiles[1])
#' getSTP_diameters(doc)
#' doc <- xml2::read_xml(hprfiles[2])
#' getSTP_diameters(doc)
#' doc <- xml2::read_xml(hprfiles[3])
#' getSTP_diameters(doc)
getSTP_diameters <- function(doc) {

  x <- xml2::xml_find_all(doc, ".//d1:Stem")

  ProcessingCategory =  xml2::xml_text(  xml2::xml_find_all(x, "./d1:ProcessingCategory"))

  x <- x[which(ProcessingCategory == "SingleTreeProcessing")]

  # For each x, find all stem diameter nodes, find all measurements over bark, tie to StemKey

  n_dias_per_stem <- purrr::map(x, ~length(xml2::xml_find_all(.x, ".//d1:DiameterValue")))

  StemKey <-  xml2::xml_integer(xml2::xml_find_all(x, ".//d1:StemKey"))

  stem_dia_nodes <- xml2::xml_find_all(x, "./d1:SingleTreeProcessedStem/d1:StemDiameters/d1:DiameterValue[@diameterMeasurementCategory='First']")
  DiameterPositions <- as.integer(xml2::xml_attr(stem_dia_nodes, attr = "diameterPosition"))
  StemDiameters <- xml2::xml_integer(stem_dia_nodes)
  StemKey2 <- rep(StemKey, times = n_dias_per_stem)


    if(length(StemDiameters)){
      diadat <- tibble::tibble(DiameterPositions = DiameterPositions,
                             StemDiameters = StemDiameters,
                             StemKey = StemKey2)
    } else {
      diadat <- tibble::tibble()
    }

  return(diadat)
}


#' Fetch all stems and all logs in hpr
#'
#' @param doc a StanFord2010 .hpr document
#'
#' @return a list with five data frames.
#' @description the function returns a list with five data frames;
#' stems is all the stems
#' stplogs is all single tree processed logs
#' mtplogs is all multi tree processed logs
#' stemgrades is the grades of all stems
#' stemdias is the diameter vector for the stem.
#' @export
#'
#' @examples
#' pth <- system.file(package = "sf2010r")
#' hprfiles <- list.files(pth,".hpr",recursive=TRUE,full.names=TRUE)
#' doc <- xml2::read_xml(hprfiles[1])
#' stl <- getStemsAndLogs(doc)
#' doc <- xml2::read_xml(hprfiles[2])
#' stl <- getStemsAndLogs(doc)
#' doc <- xml2::read_xml(hprfiles[3])
#' stl <- getStemsAndLogs(doc)
#'
#' @export
getStemsAndLogs <- function(doc){
  stemlist <- xml2::xml_find_all(doc, ".//d1:Stem")
  ## cat("getStemsAndLogs-getStemdata ")
  stems <- plyr::ldply(stemlist, sf2010r::getStemdata)
  ## cat("getStemsAndLogs-getSTPlogs ")
  stplogs <- plyr::ldply(stemlist, sf2010r::getSTPlogs)
  ## cat("getStemsAndLogs-getMTPlogs ")
  mtplogs <- plyr::ldply(stemlist, sf2010r::getMTPlogs)
  ## cat("getStemsAndLogs-getStemGrades ")
  stemgrades <- plyr::ldply(stemlist, sf2010r::getStemGrades)
  ## cat("getStemsAndLogs-getSTP_diameters ")
  stemdias <- sf2010r::getSTP_diameters(doc)

    retlist <- list(stems=stems, stplogs = stplogs, mtplogs = mtplogs, stemgrades = stemgrades, stemdias = stemdias)
  return(retlist)
}

