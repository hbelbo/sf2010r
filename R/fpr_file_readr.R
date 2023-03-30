
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
#' fprtest1 <- fprdata(fprfiles[1])
#' fprtest2 <- fprdata(fprfiles[2])
#' fprtest3 <- fprdata(fprfiles[3])
fprdata<- function(fprfile){
  # fprfiles <- list.files(path =  system.file(package = "sf2010r"), pattern = ".fpr", recursive = TRUE, full.names= TRUE)
  # fprfile = fprfiles[1]
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
    MachineReportHeader <- sf2010r::getMachineReportHeader(doc)
    # MachineReportHeader %>% dplyr::glimpse()

    if(MachineReportHeader$diameterUnit[1]!="mm"){
      print(paste(fprfile, ": Diameter Unit is NOT mm - please modify script for diasOB in Rfpr.R"))
    }
    TZ = fTZ(MachineReportHeader$CountryCode)

    operators <- sf2010r::getOperators(doc)
    # operators %>% dplyr::glimpse()

    # Object definition (Harvest site)
    objects <- sf2010r::getObjects( doc) # returns data_frame(ObjectKey, SubObjectKey, ObjectUserID, ObjectName, SubObjectName,  LoggingFormCode,  LoggingFormDesc )
    # objects %>% dplyr::glimpse()


    # Species and product definitions ----
    cat(" -fprdata-getSpeciesGroupDefs- ")
    speciesgroups <- sf2010r::getSpeciesGroupDefinitions(doc)
    # speciesgroups %>% dplyr::glimpse()

    cat(" -fprdata-getProductDefs \n")
    products <- sf2010r::getProductDefs(doc) %>%
      dplyr::mutate(  MachineKey = MachineReportHeader$MachineKey,
             CreationDate = MachineReportHeader$CreationDate)
    # products %>% dplyr::glimpse()


    cat(" -fprdata-getStemTypes \n")
    stemtypes <- sf2010r::getStemTypes(doc) %>%
      mutate( MachineKey = MachineReportHeader$MachineKey)
    # stemtypes %>% dplyr::glimpse()

    cat(" -fprdata-getLocations \n")
    locations <- sf2010r::getLocations(doc)

    cat(" -fprdata-getDeliveries \n")
    deliveries <- sf2010r::getDeliveries(doc)


    ## forwarded loads  ----
    cat(" -fprdata-getLoads \n")
    Loads <- sf2010r::getLoads(doc)
    Loads <- readr::type_convert(Loads)
    # dplyr::glimpse(Loads)


    ByLoad <-
      Loads %>%
      dplyr::group_by(.data$MachineKey, LoadKey) %>%
     dplyr::summarise(
       dplyr::across(3:4, ~ dplyr::first(.x)),
        dplyr::across(.cols = tidyselect::starts_with("Load_"), ~ sum(.x))
      )


    ByDelivery <-
      Loads %>%
      dplyr::group_by(.data$MachineKey, .data$DeliveryKey) %>%
      dplyr::summarise(
        dplyr::across(3:4, ~ dplyr::first(.x)),
        dplyr::across(.cols = tidyselect::starts_with("Load_"), ~ sum(.x))
      ) %>% dplyr::left_join( select(deliveries, tidyselect::starts_with("Delivery")), by = "DeliveryKey")

    # Set up machine report table. One machine report = one observation. ----

    machinereport_meta <- tibble::tibble(MachineKey = MachineReportHeader$MachineKey,
                                    filename = filename,
                                    file_md5 = md5,
                                    CreationDate = MachineReportHeader$CreationDate,
                                    object_keys = paste(objects$ObjectKey, collapse=", "),
                                    object_ids = paste(objects$ObjectUserID, collapse=", "),
                                    sub_obj_keys  = paste(objects$SubObjectKey, collapse=", "),
                                    sub_obj_ids  = paste(objects$SubObjectUserID, collapse=", "),
                                    filetype = filetype
                                    )


    #---------------

  } else {
    Loads = NULL
    machinereport_meta = NULL
    products = NULL
    objects = NULL
    Deliveries = NULL
    ByLoad = ByDelivery = NULL

  }

  Ret <- list(Loads=Loads, Deliveries = deliveries, machinereport_meta =  machinereport_meta,
              operators = operators, objects = objects,
              ByLoad = ByLoad, ByDelivery = ByDelivery
              )
  return(Ret)
}




