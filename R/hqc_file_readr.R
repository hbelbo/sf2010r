
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
hqc_file_readr<- function(hqcfile){
  ## TMP for assisting function development
  #
  # hqcfiles <- list.files(path =  "./inst/extdata",  pattern = ".hqc", recursive = TRUE, full.names= TRUE)
  # hqcfile <- hqcfiles[2]


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

    stemtypes <- sf2010r::getStemTypes(doc) %>%
      mutate( MachineKey = MachineReportHeader$MachineKey[1])
    # stemtypes %>% dplyr::glimpse()
    if(is.data.frame(stemtypes)){
      returnlist <- c(returnlist, stemtypes = list(stemtypes))
    }

    ## Harvested stems and logs ----
    StemsLogs <- sf2010r::getStemsAndLogs(doc)

    if(length(StemsLogs$stems) > 0){
      # str(StemsLogs)

      # Stemdat modifications, joining summary data  from logs  ----------

      # Stemdat %>% dplyr::glimpse()
      Stemdat <-  StemsLogs$stems %>%
        mutate(
               MachineKey = MachineReportHeader$MachineKey[1]
               ) %>%
        dplyr::left_join( (speciesgroups %>% dplyr::select( "SpeciesGroupKey", "SpeciesGroupName")), by = "SpeciesGroupKey")
      # Stemdat %>% dplyr::glimpse()

      stpstem_sumvars <- c(names(StemsLogs$stplogs[stringr::str_detect(names(StemsLogs$stplogs), "m3")]), names(StemsLogs$stplogs)[stringr::str_detect(names(StemsLogs$stplogs), "Length")])
      stemdatfromlogs = StemsLogs$stplogs %>% # str()
        dplyr::group_by( .data$StemKey) %>%
        dplyr::summarise(
              num_logs = dplyr::n(),
              dplyr::across(.cols = stpstem_sumvars, .fns = sum)
        )
      Stemdat <- dplyr::left_join(Stemdat, stemdatfromlogs, by = "StemKey")
      returnlist <- c(returnlist, Stemdat = list(Stemdat))


      # Grade vector for each tree: -------

      #Denne IF() skyldes at JD har en feil i TimbermaticH / SF2010V3.2; oppgir kvalitetsvektor i dm i stede for cm.
      if("stemgrades" %in% names(StemsLogs)){
        if(any(StemsLogs$stemgrades$gradestartpos_cm > 0)){
          if (min(StemsLogs$stemgrades$gradestartpos_cm[StemsLogs$stemgrades$gradestartpos_cm > 0])<20){ #DETTE skyldes at JD har en feil i TimbermaticH / SF2010V3.2; oppgir kvalitetsvektor i dm i stede for cm.
            StemsLogs$stemgrades$gradestartpos_cm = StemsLogs$stemgrades$gradestartpos_cm*10
          }
        }
        grades <- StemsLogs$stemgrades %>% dplyr::mutate( MachineKey = MachineReportHeader$MachineKey)
        returnlist <- c(returnlist, grades = list(grades))
      }



      # height diameter dataset: StemKey diaheight dia_ob_cm, dia_ub_cm -------
      #### height diameter dataset based on logs: StemKey diaheight dia_ob_cm, dia_ub_cm -------
      cat(" - hpr_file_readr- create height diameter dataset from logs- \n")
      logmeter <- StemsLogs$stplogs %>%
        dplyr::select( -tidyselect::starts_with("m3"))  %>%
        dplyr::ungroup() %>%
        dplyr::group_by( StemKey) %>%
        dplyr::mutate( LogEndHeight   = cumsum(.data$LogLength)) %>%
        dplyr::mutate(LogStartHeight = .data$LogEndHeight - .data$LogLength) %>%
        dplyr::mutate(LogMidHeight   = .data$LogStartHeight + 0.5* .data$LogLength) %>%
        dplyr::ungroup()

      logmeternames <- names(logmeter)
      gselector <- c("StemKey", "LogKey")

      if( any( stringr::str_detect(logmeternames, "Top.ob"))){
        topdia_ob <- c(logmeternames[stringr::str_detect(logmeternames, "Top.ob")], "diapos" = "LogEndHeight")
        topsonbark = logmeter %>% dplyr::select(tidyselect::all_of(gselector), tidyselect::all_of(topdia_ob)) %>%
          dplyr::mutate(logdiapos = "Top") %>%
          dplyr::rename(LogDiameter = "LogDiameter_Top.ob")
        stemdias_ob <- topsonbark

        if( any( stringr::str_detect(logmeternames, "Mid.ob"))){
          middia_ob <- c(logmeternames[stringr::str_detect(logmeternames, "Mid.ob")], "diapos" = "LogMidHeight")
          midsonbark = logmeter %>%
            dplyr::select(tidyselect::all_of(gselector), tidyselect::all_of(middia_ob)) %>%
            dplyr::mutate(logdiapos = "Mid") %>%
            dplyr::rename(LogDiameter = "LogDiameter_Mid.ob") ##### is this the way to put it?
          #dplyr::select( "StemKey", "LogKey", "diapos" = "LogMidHeight", "dia" = "LogDiameter_Mid.ob")
          stemdias_ob <- dplyr::bind_rows(stemdias_ob, midsonbark)
        }


        if( any(stringr::str_detect(logmeternames, "Butt.ob"))){
          butdia_ob <- c(logmeternames[stringr::str_detect(logmeternames, "Butt.ob")], "diapos" =  "LogStartHeight")
          butsonbark = logmeter %>%
            dplyr::select(tidyselect::all_of(gselector),  tidyselect::all_of(butdia_ob)) %>%
            dplyr::mutate(logdiapos = "Butt") %>%
            dplyr::rename(LogDiameter = "LogDiameter_Butt.ob")
          #dplyr::select( "StemKey", "LogKey", "diapos" = "LogStartHeight", "dia" = "Butt.ob")
          stemdias_ob <- dplyr::bind_rows(stemdias_ob, butsonbark)
        }
        stemdias_ob <- stemdias_ob %>% dplyr::arrange( .data$StemKey, .data$diapos)

        returnlist <- c(returnlist, stemdias_ob = list(stemdias_ob))
      }

      if(any(stringr::str_detect(logmeternames, "Top.ub"))){
        topdia_ub <- c(logmeternames[stringr::str_detect(logmeternames, "Top.ub")], "diapos" = "LogEndHeight")
        topsubark = logmeter %>% dplyr::select(tidyselect::all_of(gselector), tidyselect::all_of(topdia_ub)) %>%
          dplyr::mutate(logdiapos = "Top") %>%
          dplyr::rename(LogDiameter = "LogDiameter_Top.ub")
        stemdias_ub <- topsubark

        if( any( stringr::str_detect(logmeternames, "Mid.ub"))){
          middia_ub <- c(logmeternames[stringr::str_detect(logmeternames, "Mid.ub")], "diapos" = "LogMidHeight")
          midsubark = logmeter %>%
            dplyr::select(tidyselect::all_of(gselector), tidyselect::all_of(middia_ub)) %>%
            dplyr::mutate(logdiapos = "Mid") %>%
            dplyr::rename(LogDiameter = "LogDiameter_Mid.ub")
          #dplyr::select( "StemKey", "LogKey", "diapos" = "LogMidHeight", "dia" = "LogDiameter_Mid.ub")
          stemdias_ub <- dplyr::bind_rows(stemdias_ub, midsubark)
        }


        if( any(stringr::str_detect(logmeternames, "Butt.ub"))){
          butdia_ub <- c(logmeternames[stringr::str_detect(logmeternames, "Butt.ub")], "diapos" =  "LogStartHeight")
          butsubark = logmeter %>%
            dplyr::select(tidyselect::all_of(gselector),  tidyselect::all_of(butdia_ub)) %>%
            dplyr::mutate(logdiapos = "Butt") %>%
            dplyr::rename(LogDiameter = "LogDiameter_Butt.ub")
          #dplyr::select( "StemKey", "LogKey", "diapos" = "LogStartHeight", "dia" = "Butt.ub")
          stemdias_ub <- dplyr::bind_rows(stemdias_ub, butsubark)
        }
        stemdias_ub <- stemdias_ub %>% dplyr::arrange( .data$StemKey, .data$diapos)

        returnlist <- c(returnlist, stemdias_ub = list(stemdias_ub))
      }

      ######## Logs products productmatrixes ------------------

      logs <- StemsLogs$stplogs  %>%
        dplyr::mutate(MachineKey = MachineReportHeader$MachineKey[1])
      returnlist <- c(returnlist, logs = list(logs))
    }



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




