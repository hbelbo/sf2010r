
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
#' hqctest1 <- hqcdata(hqcfiles[1])
#' hqctest2 <- hqcdata(hqcfiles[2])
#' hqctest3 <- hqcdata(hqcfiles[3])
hqcdata<- function(hqcfile){
  ## TMP for assisting function development
  #
  # hqcfiles <- list.files(path =  "./inst/extdata",  pattern = ".hqc", recursive = TRUE, full.names= TRUE)
  # hqcfile <- hqcfiles[1]


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
      dplyr::mutate(  MachineKey = MachineReportHeader$MachineKey,
             CreationDate = MachineReportHeader$CreationDate)
    # products %>% dplyr::glimpse()



    stemtypes <- sf2010r::getStemTypes(doc) %>%
      mutate( MachineKey = MachineReportHeader$MachineKey)
    # stemtypes %>% dplyr::glimpse()



    ## Harvested stems and logs ----
    StemsLogs <- sf2010r::getStemsAndLogs(doc)


    # Stemdat modifications, joining summary data  from logs  ----------
    Stemdat <- StemsLogs$stems
    # Stemdat %>% dplyr::glimpse()
    Stemdat <- Stemdat %>%
      mutate(
             MachineKey = MachineReportHeader$MachineKey,
             CreationDate = MachineReportHeader$CreationDate
             ) %>%
      dplyr::left_join( (speciesgroups %>% dplyr::select( .data$SpeciesGroupKey, .data$SpeciesGroupName)), by = "SpeciesGroupKey")
    # Stemdat %>% dplyr::glimpse()


    stemdatfromlogs = StemsLogs$stplogs %>% #dplyr::glimpse()
      dplyr::group_by( .data$StemKey) %>%
      dplyr::summarise(
            num_logs = dplyr::n(),
            stem_length = sum(.data$LogLength),
            stem_vol_m3sub = sum(.data$m3sub),
            stem_vol_m3sob = sum(.data$m3sob)
      )
    Stemdat <- dplyr::left_join(Stemdat, stemdatfromlogs, by = "StemKey")




    # Grade vector for each tree: -------

    #Denne IF() skyldes at JD har en feil i TimbermaticH / SF2010V3.2; oppgir kvalitetsvektor i dm i stede for cm.
    if (min(StemsLogs$stemgrades$gradestartpos_cm[StemsLogs$stemgrades$gradestartpos_cm > 0])<20){ #DETTE skyldes at JD har en feil i TimbermaticH / SF2010V3.2; oppgir kvalitetsvektor i dm i stede for cm.
      StemsLogs$stemgrades$gradestartpos_cm = StemsLogs$stemgrades$gradestartpos_cm*10
    }
    grades <- StemsLogs$stemgrades %>% dplyr::mutate( MachineKey = MachineReportHeader$MachineKey)


    # height diameter dataset: StemKey diaheight dia_ob_cm, dia_ub_cm -------

    logmeter <- StemsLogs$stplogs %>%
      select( -tidyselect::starts_with("m3"))  %>%
      dplyr::ungroup() %>%
      dplyr::group_by( .data$StemKey) %>%
      dplyr:: mutate( LogEndHeight   = cumsum(.data$LogLength)) %>%
      dplyr::mutate(LogStartHeight = .data$LogEndHeight - .data$LogLength) %>%
      dplyr::mutate(LogMidHeight   = .data$LogStartHeight + 0.5* .data$LogLength) %>%
      dplyr::ungroup()

    topsonbark = logmeter %>%
      dplyr::select( .data$StemKey, .data$LogKey, diapos = .data$LogEndHeight, dia = .data$`Top ob`)
    midsonbark = logmeter %>%
      dplyr::select( .data$StemKey, .data$LogKey, diapos = .data$LogMidHeight, dia = .data$`Mid ob`)
    stemdiasonbark <- dplyr::bind_rows(midsonbark, topsonbark)


    if("`Butt ob`" %in% colnames(logmeter) ){
      butsonbark = logmeter %>%
        dplyr::select( .data$StemKey, .data$LogKey, diapos = .data$LogStartHeight,
                       dia = .data$`Butt ob`)
      stemdiasonbark <- dplyr::bind_rows(stemdiasonbark, butsonbark)
    }
    stemdiasonbark <- stemdiasonbark %>% dplyr::arrange( .data$StemKey, .data$diapos)


    topsubark = logmeter %>% dplyr::select( .data$StemKey, .data$LogKey,
                                     diapos = .data$LogEndHeight, dia = .data$`Top ub`)
    midsubark = logmeter %>% dplyr::select( .data$StemKey, .data$LogKey,
                                     diapos = .data$LogMidHeight, dia = .data$`Mid ub`)

    stemdiasubark <- dplyr::bind_rows(midsubark, topsubark)
    if("`Butt ub`" %in% colnames(logmeter) ){
      butsubark = logmeter %>%
        dplyr::select( .data$StemKey, .data$LogKey, diapos = .data$LogStartHeight,
                       dia = .data$`Butt ub`)
      stemdiasubark <- bind_rows(stemdiasubark, butsubark)
    }
    stemdiasubark <- stemdiasubark %>% arrange( .data$StemKey, .data$diapos)

    # Logs products productmatrixes

    logs <- StemsLogs$stplogs  %>%
      dplyr::mutate(MachineKey = MachineReportHeader$MachineKey,
              CreationDate = MachineReportHeader$CreationDate)


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
    machinereport_meta = NULL
    Stemdat = NULL
    products = NULL
    objects = NULL
    grades = NULL
    logs = NULL
    stemdiasonbark = NULL
    stemtypes = NULL
  }

  Ret <- list(stems=Stemdat, products=products, logs = logs, machinereport_meta =  machinereport_meta,
              operators = operators, objects = objects,
              grades = grades,
              stemdiasonbark = stemdiasonbark, stemtypes = stemtypes)
  return(Ret)
}




