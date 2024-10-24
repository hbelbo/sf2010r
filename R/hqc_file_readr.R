
#' HQC-file reader function
#'
#' @param hqcfile file filename and path of the hqc file to read

#' @return A list of data.frames: stems, products, logs, machinereport_meta,
#'  operators, objects, stem_grades,
#'  stemdiametervectors, and stemtypes.
#' @export
#'
#' @examples
#' hqcfiles <- list.files(path =  system.file(package = "sf2010r"),
#' pattern = ".hqc", recursive = TRUE, full.names= TRUE)
#'  hqc_file_readr(hqcfiles[1]) %>% str()
#'  hqc_file_readr(hqcfiles[2]) %>% str()
#'  hqc_file_readr(hqcfiles[3]) %>% str()
hqc_file_readr<- function(hqcfile){
  ## TMP for assisting function development
  #
  # hqcfiles <- list.files(path =  "./inst/extdata",  pattern = ".hqc", recursive = TRUE, full.names= TRUE)
  # hqcfile <- hqcfiles[1]

  tmp <- nchar(hqcfile)
  filetype <- substring(hqcfile, tmp-2, tmp)

  stopifnot(filetype == "hqc")

   doc <- xml2::read_xml(hqcfile)
   md5 <-  digest::digest(file(hqcfile))

  ####
  #Then validate the file is providing stems and other relevant info. This might be lacking in case the machine is reporting files automatically
   StemKey =  xml2::xml_integer(  xml2::xml_find_all(doc, ".//d1:StemKey"))

  if(length(StemKey)){

    ## then extract values

    # .. cut object info
    filename <- hqcfile
    tmp <- nchar(filename)
    filetype <- substring(filename, tmp-2, tmp)


    # .. from the header
    MachineReportHeader <- sf2010r::getMachineReportHeader(doc)
    # MachineReportHeader %>% dplyr::glimpse()

    if(MachineReportHeader$diameterUnit[1]!="mm"){
      print(paste(hqcfile, ": Diameter Unit is NOT mm - please modify script for diasOB in Rhqc.R"))
    }
    TZ = fTZ(MachineReportHeader$CountryCode)

    operators <- sf2010r::getOperators(doc)
    # operators %>% dplyr::glimpse()

    # Object definition (Harvest site)
    objects <- sf2010r::getObjects( doc) # returns data_frame(ObjectKey, SubObjectKey, ObjectUserID, ObjectName, SubObjectName,  LoggingFormCode,  LoggingFormDesc )
    # objects %>% dplyr::glimpse()


    # Species and product definitions ----
    speciesgroups <- sf2010r::getSpeciesGroupDefinitions(doc)
    # speciesgroups %>% dplyr::glimpse()

    products <- sf2010r::getProductDefs(doc) %>%
      dplyr::mutate(  MachineKey = MachineReportHeader$MachineKey)
    # products %>% str()

    # Creating list of return elements on things to return
    returnlist <- list( objects = objects,
                        products = products,
                        speciesgroups = speciesgroups,
                        operators = operators)


    #ControlValues
    controlvalues <- sf2010r::getControlStemValues(doc)
    controlvalues$ControlLogDiameters$MachineKey <- MachineReportHeader$MachineKey
    controlvalues$ControlLogLength$MachineKey <- MachineReportHeader$MachineKey
    controlvalues$ControlLogVolumes$MachineKey <- MachineReportHeader$MachineKey

    returnlist <- c(returnlist, controlvalues)



    # Set up machine report table. One machine report = one observation. ----

    machinereport_meta <- tibble::tibble(MachineKey = MachineReportHeader$MachineKey,
                                    filename = filename,
                                    file_md5 = md5,
                                    CreationDate = MachineReportHeader$CreationDate,
                                    object_keys = paste(objects$ObjectKey, collapse=", "),
                                    object_ids = paste(objects$ObjectUserID, collapse=", "),
                                    returnlist_content = paste0(names(returnlist), collapse = ", "),
                                    filetype = filetype
                                    )
   returnlist <- c(returnlist, machinereport_meta = list(machinereport_meta))

  return(returnlist)
  }
}




