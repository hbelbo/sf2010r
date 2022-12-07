

#' Tracking data from mom-files#'
#'
#' @param doc a StanFord2010 .mom xml-document
#' @export
#'
#' @examples
#' pth <- system.file(package = "sf2010r")
#' momfiles <- list.files(pth,".mom",ignore.case=TRUE,recursive=TRUE,full.names= TRUE)
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
