#' Fetch all control log diameters from one log entry (.hqc files)
#'
#' @param doc  a StanFord2010 .hqc document
#'
#' @return a list with three data frames: ControlLogDiameters, ControlLogLength, ControlLogVolumes
#' @export
#'
#' @examples
#' hqcfiles <- list.files(path =  system.file(package = "sf2010r"),
#'    pattern = "hqc", recursive = TRUE, full.names= TRUE)
#' doc <- xml2::read_xml(hqcfiles[1])
#' hqc_control_stem_values <- getControlStemValues(doc)
getControlStemValues <- function(doc){


  # Define the namespace
  ns <- xml2::xml_ns_rename(xml2::xml_ns(doc), d1 = "d1")

  # Get all 'Stem' nodes
  stems <- xml2::xml_find_all(doc, ".//d1:Stem", ns)

  # Extract ControlLogDiameters
  ControlLogDiameters <- purrr::map_df(stems, ~{
    stemKey <- xml2::xml_text(xml2::xml_find_first(., ".//d1:StemKey", ns))
    logs <- xml2::xml_find_all(., ".//d1:Log", ns)
    purrr::map_df(logs, function(log) {
      logKey <- xml2::xml_text(xml2::xml_find_first(log, ".//d1:LogKey", ns))
      logMeasurements <- xml2::xml_find_all(log, ".//d1:LogMeasurement", ns)
      purrr::map_df(logMeasurements, function(logMeasurement) {
        #logMeasurementCategory <- xml2::xml_text(xml2::xml_find_first(logMeasurement, ".//d1:logMeasurementCategory", ns))
        logMeasurementCategory <- xml2::xml_attr(logMeasurement, "logMeasurementCategory")
        MeasurementDate <- xml2::xml_text(xml2::xml_find_first(logMeasurement, ".//d1:MeasurementDate"), ns)
        controlLogDiameters <- xml2::xml_find_all(logMeasurement, ".//d1:ControlLogDiameter", ns)
        purrr::map_df(controlLogDiameters, function(controlLogDiameter) {
          diameterPosition <- xml2::xml_attr(controlLogDiameter, "diameterPosition")
          diameterMeasurementCategory <- xml2::xml_attr(controlLogDiameter, "diameterMeasurementCategory")
          diameterValue <- xml2::xml_text(controlLogDiameter)
          tibble::tibble(StemKey = stemKey, LogKey = logKey, ControlLogDiameter = diameterValue, diameterPosition = diameterPosition, diameterMeasurementCategory, logMeasurementCategory = logMeasurementCategory)
        })
      })
    })
  })
  ControlLogDiameters <- ControlLogDiameters %>% utils::type.convert(as.is = TRUE)

  # Extract log length

  LogLengths <- purrr::map_df(stems, ~{
    stemKey <- xml2::xml_text(xml2::xml_find_first(., ".//d1:StemKey", ns))
    logs <- xml2::xml_find_all(., ".//d1:Log", ns)
    purrr::map_df(logs, function(log) {
      logKey <- xml2::xml_text(xml2::xml_find_first(log, ".//d1:LogKey", ns))
      logMeasurements <- xml2::xml_find_all(log, ".//d1:LogMeasurement", ns)
      purrr::map_df(logMeasurements, function(logMeasurement) {
        #logMeasurementCategory <- xml2::xml_text(xml2::xml_find_first(logMeasurement, ".//d1:logMeasurementCategory", ns))
        logMeasurementCategory <- xml2::xml_attr(logMeasurement, "logMeasurementCategory")
        LogLength <- xml2::xml_text(xml2::xml_find_first(logMeasurement, ".//d1:LogLength"), ns)
        MeasurementDate <- xml2::xml_text(xml2::xml_find_first(logMeasurement, ".//d1:MeasurementDate"), ns)
        tibble::tibble(StemKey = stemKey, LogKey = logKey,
                       logMeasurementCategory = logMeasurementCategory,
                       LogLength = LogLength,
                       MeasurementDate = MeasurementDate)


      })
    })
  })
  LogLengths <- LogLengths %>% utils::type.convert(as.is = TRUE)

  # Extract log volume
  LogVols <- purrr::map_df(stems, ~{
    stemKey <- xml2::xml_text(xml2::xml_find_first(., ".//d1:StemKey", ns))
    logs <- xml2::xml_find_all(., ".//d1:Log", ns)
    purrr::map_df(logs, function(log) {
      logKey <- xml2::xml_text(xml2::xml_find_first(log, ".//d1:LogKey", ns))
      logVolumes <- xml2::xml_find_all(log, ".//d1:LogVolume", ns)
      purrr::map_df(logVolumes, function(logVolume) {
        #logMeasurementCategory <- xml2::xml_text(xml2::xml_find_first(logMeasurement, ".//d1:logMeasurementCategory", ns))
        logMeasurementCategory <- xml2::xml_attr(logVolume, "logMeasurementCategory")
        logVolumeCategory <- xml2::xml_attr(logVolume, "logVolumeCategory")
        LogVolume <- xml2::xml_text(logVolume)
        tibble::tibble(StemKey = stemKey, LogKey = logKey,
                       logMeasurementCategory = logMeasurementCategory,
                       logVolumeCategory = logVolumeCategory,
                       LogVolume = LogVolume)


      })
    })
  })
  LogVols <- LogVols %>% utils::type.convert(as.is = TRUE)


  returnlist <- list(ControlLogDiameters = ControlLogDiameters, ControlLogLength = LogLengths, ControlLogVolumes = LogVols)

  return(returnlist)
}




