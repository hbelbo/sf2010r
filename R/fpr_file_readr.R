
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
#' fprtest1 <- fpr_file_readr(fprfiles[1])
#' fprtest2 <- fpr_file_readr(fprfiles[2])
fpr_file_readr <- function(fprfile){
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
    if(length(products)){
      products <- products %>%
      dplyr::mutate(  MachineKey = header$MachineKey
                    #  , CreationDate = header$CreationDate
             )
      returnlist <- c(returnlist, products = list(products))
    }


    cat(" -fpr_file_readr-getStemTypes \n")
    stemtypes <- sf2010r::getStemTypes(doc)
    if(length(stemtypes)){
      stemtypes <- stemtypes %>% mutate( MachineKey = header$MachineKey)
      returnlist <- c(returnlist, stemtypes = list(stemtypes))
    }

    cat(" -fpr_file_readr-getLocations \n")
    locations <- sf2010r::getLocations(doc)
    if(length(locations)){
      returnlist <- c(returnlist, locations = list(locations))
    }


    cat(" -fpr_file_readr-getDeliveries \n")
    deliveries <- sf2010r::getDeliveries(doc)
    if(length(deliveries)){
      returnlist <- c(returnlist, deliveries = list(deliveries))
    }


    ## forwarded loads  ----
    cat(" -fpr_file_readr-getLoads \n")
    Loads <- sf2010r::getLoads(doc)
    Loads <- readr::type_convert(Loads)
    returnlist <- c(returnlist, Loads = list(Loads))


    # dplyr::glimpse(Loads)


    ByLoad <-
      Loads %>%
      dplyr::group_by(.data$MachineKey, .data$LoadKey, .data$LocationKey) %>%
     dplyr::summarise(
       dplyr::across(3:4, ~ dplyr::first(.x)),
        dplyr::across(.cols = tidyselect::starts_with("Load_"), ~ sum(.x))
      ) %>% ungroup()
    returnlist <- c(returnlist, ByLoad = list(ByLoad))


    ByDelivery <-
      Loads %>%
      dplyr::group_by(.data$MachineKey, .data$DeliveryKey) %>%
      dplyr::summarise(
        dplyr::across(3:4, ~ dplyr::first(.x)),
        dplyr::across(.cols = tidyselect::starts_with("Load_"), ~ sum(.x))
      ) %>%
      dplyr::left_join(
        select(deliveries, tidyselect::starts_with("Delivery")), by = "DeliveryKey") %>%
      ungroup()


    returnlist <- c(returnlist, ByDelivery = list(ByDelivery))

    # Set up machine report table. One machine report = one observation. ----

    machinereport_meta <- tibble::tibble(MachineKey = header$MachineKey,
                                    filename = filename,
                                    file_md5 = md5,
                                    CreationDate = header$CreationDate,
                                    object_keys = paste(objects$ObjectKey, collapse=", "),
                                    object_ids = paste(objects$ObjectUserID, collapse=", "),
                                    sub_obj_keys  = paste(objects$SubObjectKey, collapse=", "),
                                    sub_obj_ids  = paste(objects$SubObjectUserID, collapse=", "),
                                    filetype = filetype
                                    )
    returnlist <- c(returnlist, machinereport_meta = list(machinereport_meta))


    #---------------

  } else {
  returnlist = NULL
  }

  return(returnlist)
}




