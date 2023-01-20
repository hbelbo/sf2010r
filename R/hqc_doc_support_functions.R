


#' Fetch all control log diameters from one log entry (.hqc files)
#'
#' @param x a StanFord2010 .hqc log node-tree
#'
#' @return a tibble with all control log diameters belonging to one stem
#' @export
#'
#' @examples
#' hqcfiles <- list.files(path =  system.file(package = "sf2010r"),  pattern = "hqc", recursive = TRUE, full.names= TRUE)
#' doc <- xml2::read_xml(hqcfiles[1])
#' hqc_stems <- xml2::xml_find_all(doc, ".//d1:Stem")
#' hqc_logs <- xml2::xml_find_all(hqc_stems[1], ".//d1:Log")
#' getControlLogDiameters(hqc_logs[1]) %>% dplyr::glimpse()
getControlLogDiameters <- function(x) {
  # x <- hqc_logs[1]
  machine_log_dias <- xml2::xml_integer(xml2::xml_find_all(x, "./d1:LogMeasurement[@logMeasurementCategory='Machine']/d1:ControlLogDiameter[@diameterMeasurementCategory='Average']"))
  operator_log_dias <- xml2::xml_integer(xml2::xml_find_all(x, "./d1:LogMeasurement[@logMeasurementCategory='Operator']/d1:ControlLogDiameter[@diameterMeasurementCategory='Average']"))
  machine_poss <- as.integer(xml2::xml_attr(xml2::xml_find_all(x, "./d1:LogMeasurement[@logMeasurementCategory='Machine']/d1:ControlLogDiameter[@diameterMeasurementCategory='Average']"), attr = "diameterPosition"))
  operator_poss <- as.integer(xml2::xml_attr(xml2::xml_find_all(x, "./d1:LogMeasurement[@logMeasurementCategory='Operator']/d1:ControlLogDiameter[@diameterMeasurementCategory='Average']"), attr = "diameterPosition"))
  LogKey <-  xml2::xml_integer(xml2::xml_find_all(x, "./d1:LogKey"))

  # Then make the resulting tibble:
  control_log_dias <- tibble::tibble(
    machine_dia_pos = machine_poss
    , diameter_machine = machine_log_dias
    , operator_dia_pos =  operator_poss
    , diameter_operator = operator_log_dias
    , LogKey = LogKey)

  return(control_log_dias)
}




#' Fetch all control logs  from one stem entry (.hqc files)
#'
#' @param x a StanFord2010 .hqc stem node-tree
#'
#' @return a tibble with all control log diameters belonging to one stem
#' @export
#'
#' @examples
#' hqcfiles <- list.files(path =  system.file(package = "sf2010r"),  pattern = "hqc", recursive = TRUE, full.names= TRUE)
#' doc <- xml2::read_xml(hqcfiles[1])
#' hqc_stems <- xml2::xml_find_all(doc, ".//d1:Stem")
#' getControlLogs(hqc_stems[1]) %>% dplyr::glimpse()
getControlLogs <- function(x) {
  # x <- hqc_stems[1]

  ProcessingCategory =  xml2::xml_text(  xml2::xml_find_all(x, ".//d1:ProcessingCategory"))

  if( ProcessingCategory == "SingleTreeProcessing"){
    StemKey <-  xml2::xml_integer(  xml2::xml_find_first(x, ".//d1:StemKey"))

    logs  <- xml2::xml_find_all(x, ".//d1:Log")
    logkeys <- logs %>% purrr::map(~
                                     xml2::xml_integer(xml2::xml_find_first(.x, "./d1:LogKey"))) %>% unlist()

    ProductKey <- logs %>% purrr::map(~  xml2::xml_integer(  xml2::xml_find_first(.x, ".//d1:ProductKey"))) %>% unlist()

    LogLengthMachine <- logs %>%
      purrr::map(~  xml2::xml_integer(  xml2::xml_find_first(.x, ".//d1:LogMeasurement[@logMeasurementCategory='Machine']/d1:LogLength"))) %>% unlist()
    LogLengthOperator <- logs %>%
      purrr::map(~  xml2::xml_integer(  xml2::xml_find_first(.x, ".//d1:LogMeasurement[@logMeasurementCategory='Operator']/d1:LogLength"))) %>% unlist()

    LogMeasurementDateMachine <- logs %>%
      purrr::map(~  xml2::xml_text(  xml2::xml_find_first(.x, ".//d1:LogMeasurement[@logMeasurementCategory='Machine']/d1:MeasurementDate"))) %>% unlist()
    LogMeasurementDateOperator <- logs %>%
      purrr::map(~  xml2::xml_text(  xml2::xml_find_first(.x, ".//d1:LogMeasurement[@logMeasurementCategory='Operator']/d1:MeasurementDate"))) %>% unlist()

    ControlLogDiameters <- logs %>%
      purrr::map_dfr(~  getControlLogDiameters(.x))

    ControlLogs <- list(
      ControlLogDiameters = ControlLogDiameters %>% mutate(StemKey = StemKey)
      , ControlLogs = tibble::tibble(LogKey = logkeys, ProductKey = ProductKey
                                     , LogLengthMachine = LogLengthMachine
                                     , LogLengthOperator = LogLengthOperator
                                     , LogMeasurementDateMachine = LogMeasurementDateMachine
                                     , LogMeasurementDateOperator = LogMeasurementDateOperator)
    )
    } else { ControlLogs = NULL}


  return(ControlLogs)
}


