
#' Fpr-file reader function
#'
#' @param fprfile filename and path of the fpr file to read

#' @return A list of data.frames: stems, products, logs, machinereport_meta,
#'  operators, objects, stem_grades, pricematrixes,
#'  stemdiametervectors, and stemtypes.
#' @export
#'
#' @examples
#' fprfiles <- list.files(path =  system.file(package = "sf2010r"),
#' pattern = ".fpr", recursive = TRUE, full.names= TRUE)
#' fpr_file_readr2(fprfiles[1]) %>% str()
#' fpr_file_readr2(fprfiles[2]) %>% str()
fpr_file_readr2 <- function(fprfile){
  # fprfiles <- list.files(path =  system.file(package = "sf2010r"), pattern = ".fpr", recursive = TRUE, full.names= TRUE)
  # fprfile = fprfiles[1]
  # fprfile = fprfiles[2]
   doc <- xml2::read_xml(fprfile)
   md5 <-  digest::digest(file(fprfile))

   if(nchar(Sys.getlocale()) < 3){
     Sys.setlocale(category = "LC_ALL", locale = "") #because of an R-studio Rstartup issue at HB's computer
     }


  ####
  #Then validate the file is providing loads and other relevant info. This might be lacking in case the machine is reporting files automatically
   LoadKey =  xml2::xml_integer(  xml2::xml_find_first(doc, ".//d1:LoadKey"))


  if(length(LoadKey)){

    ## then extract values

    # .. cut object info
    filename <- fprfile
    tmp <- nchar(filename)
    filetype <- substring(filename, tmp-2, tmp)


    # .. from the header
    header <- sf2010r::getMachineReportHeader(doc)
    # header %>% dplyr::glimpse()

    if(header$diameterUnit[1]!="mm"){
      print(paste(fprfile, ": Diameter Unit is NOT mm - please modify script for diasOB in Rfpr.R"))
    }
    TZ = fTZ(header$CountryCode)

    operators <- sf2010r::getOperators(doc)
    # operators %>% dplyr::glimpse()

    # Object definition (Harvest site)
    objects <- sf2010r::getObjects( doc) # returns data_frame(ObjectKey, SubObjectKey, ObjectUserID, ObjectName, SubObjectName,  LoggingFormCode,  LoggingFormDesc )
    # objects %>% dplyr::glimpse()

    returnlist <- list(header = header, operators = operators,  objects = objects)

    # Species and product definitions ----
    cat(" -fpr_file_readr-getSpeciesGroupDefs- ")
    speciesgroups <- sf2010r::getSpeciesGroupDefinitions(doc)
    # speciesgroups %>% dplyr::glimpse()
    if(length(speciesgroups)){
      speciesgroups <- c(returnlist, speciesgroups = list(speciesgroups))
    }


    cat(" -fpr_file_readr-getProductDefs \n")
    products <- sf2010r::getProductDefs(doc)
    # str(products)
    if(length(products)){
      products <- sf2010r::type_convert_sf2010(products) %>%
      dplyr::mutate(  MachineKey = header$MachineKey[1]
                    #  , CreationDate = header$CreationDate
             )
      returnlist <- c(returnlist, products = list(products))
    }


    cat(" -fpr_file_readr-getStemTypes \n")
    stemtypes <- sf2010r::getStemTypes(doc)
    if(length(stemtypes)){
      stemtypes <- stemtypes %>% mutate( MachineKey = header$MachineKey[1])
      returnlist <- c(returnlist, stemtypes = list(stemtypes))
    }

    cat(" -fpr_file_readr-getLocations \n")
    locations <- sf2010r::getLocations(doc)
    if(length(locations)){
      returnlist <- c(returnlist, locations = list(locations))
    }


    cat(" -fpr_file_readr-getDeliveries \n")
    deliveries <- sf2010r::getDeliveries2(doc)

    if(length(deliveries)){
      deliveries <- sf2010r::type_convert_sf2010(deliveries)
      returnlist <- c(returnlist, deliveries = list(deliveries %>% mutate( MachineKey = header$MachineKey)))
    }


    ## forwarded loads  ----
    cat(" -fpr_file_readr-getLoads \n")
    Loads <- sf2010r::getLoads2(doc)
    names(Loads) <- stringr::str_extract(names(Loads), pattern = "\\w*$")
    # str(Loads)
    Loads <- sf2010r::type_convert_sf2010(Loads) %>%
      dplyr::mutate(  MachineKey = header$MachineKey[1]
                      #  , CreationDate = header$CreationDate
      )

    returnlist <- c(returnlist, Loads = list(Loads))


    # dplyr::glimpse(Loads)


    ByLoad <-
      Loads %>%
      dplyr::group_by(.data$MachineKey, .data$LoadKey) %>%
     dplyr::summarise(
       OperatorKey = dplyr::first(.data$OperatorKey),
        dplyr::across(.cols = tidyselect::starts_with("m3"), ~ sum(.x)),
        DistanceFromLastUnloading = dplyr::last(.data$DistanceFromLastUnloading),
        UnloadingTime = dplyr::last(.data$UnloadingTime)
      ) %>% dplyr::ungroup()
    returnlist <- c(returnlist, ByLoad = list(ByLoad))


    ByDelivery <-
      Loads %>%
      dplyr::group_by(.data$MachineKey, .data$DeliveryKey) %>%
      dplyr::summarise(
        dplyr::across(.cols = tidyselect::starts_with("LoadVolume."), ~ sum(.x)),
        min_UnloadingTime = min(.data$UnloadingTime),
        max_UnloadingTime = max(.data$UnloadingTime)


      ) %>%
      dplyr::left_join(
        dplyr::select(deliveries, tidyselect::starts_with("Delivery")), by = "DeliveryKey") %>%
      dplyr::ungroup()
    # str(ByDelivery)


    returnlist <- c(returnlist, ByDelivery = list(ByDelivery))

    # Set up machine report table. One machine report = one observation. ----

    machinereport_meta <- tibble::tibble(MachineKey = header$MachineKey,
                                    filename = filename,
                                    file_md5 = md5,
                                    CreationDate = header$CreationDate,
                                    object_keys = paste(objects$ObjectKey, collapse=", "),
                                    object_ids = paste(objects$ObjectUserID, collapse=", "),
                                    filetype = filetype
                                    )
    returnlist <- c(returnlist, machinereport_meta = list(machinereport_meta))


    #---------------

  } else {
  returnlist = NULL
  }

  return(returnlist)
}




