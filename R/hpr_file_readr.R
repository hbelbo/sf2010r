
#' Hpr-file reader function
#'
#' @param hprfile filename and path of the hpr file to read
#' @param read.diavector flag to read diameter vector or not.
#'  if read.diavector is TRUE, function may take long time.

#' @return A list of data.frames: stems, products, logs, machinereport_meta,
#'  operators, objects, stem_grades, pricematrixes,
#'  stemdiametervectors, and stemtypes.
#' @export
#'
#'@details
#'The function does not yet fetch volumes from multi-tree processed tree-bunches.
#'
#'
#' @examples
#' hprfiles <- list.files(path =  system.file(package = "sf2010r"),  pattern = ".hpr", recursive = TRUE, full.names= TRUE)
#' hpr_file_readr(hprfiles[1], read.diavector = TRUE)
#' hpr_file_readr(hprfiles[2]) %>% str()
#' hpr_file_readr(hprfiles[3]) %>% str()
#' hpr_file_readr(hprfiles[3], read.diavector = TRUE) %>% str()
hpr_file_readr <- function(hprfile, read.diavector = FALSE){
  # hprfiles <- list.files(path =  system.file(package = "sf2010r"), pattern = ".hpr", recursive = TRUE, full.names= TRUE)
  # hprfile = hprfiles[3]
  # hprfile = unparsedfiles[1]
#  hprtest4 <- hpr_file_readr(unparsedfiles[i], read.diavector = TRUE)
#  hprtest4 <- hpr_file_readr(unparsedfiles[i])

  cat("\n-hpr_file_readr() parsing ", hprfile,"- \n")
  filename <- basename(hprfile)

  tmp <- nchar(filename)
  filetype <- substring(filename, tmp-2, tmp)

  stopifnot(filetype == "hpr")

  detected_encoding <- stringi::stri_enc_detect(stringi::stri_flatten(readLines(hprfile, n = 1000), collapse = '\n'))[[1]]$Encoding[1]
  # If the encoding is not already UTF-8, convert it
  if (detected_encoding != "UTF-8") {
    # Read the file as a character vector with the original encoding
    text <- stringi::stri_flatten(readLines(hprfile, encoding = detected_encoding), collapse = '\n')


    # Convert the encoding to UTF-8
    text_utf8 <- gsub("<U\\+FEFF>", "", text)
    text_utf8 <- iconv(text_utf8, from = detected_encoding, to = "UTF-8")


    # Write the converted text back to the file with UTF-8 encoding
    writeLines(text_utf8, hprfile, useBytes = TRUE )
  }

   # Read the file as raw text
  #raw_text <- readr::read_file(hprfile)

  # Convert the encoding to UTF-8
 # utf8_text <- iconv(raw_text, from = "windows-1252", to = "UTF-8")


  # Parse the XML content

  #doc <- xml2::read_xml(utf8_text)
  #md5 <- digest::digest(utf8_text)
  doc <- xml2::read_xml(hprfile)
  md5 <- digest::digest(hprfile)

   if(nchar(Sys.getlocale()) < 3){
     Sys.setlocale(category = "LC_ALL", locale = "") #because of an R-studio Rstartup issue at HB's computer
     }


  ####
  #Then validate the file is providing stems and other relevant info. This might be lacking in case the machine is reporting files automatically
   StemKey =  xml2::xml_integer(  xml2::xml_find_all(doc, ".//d1:StemKey"))

  if(length(StemKey)){

    ## then extract values
    # .. from the header
    MachineReportHeader <- sf2010r::getMachineReportHeader(doc)
    # MachineReportHeader %>% dplyr::glimpse()

    if(MachineReportHeader$diameterUnit[1]!="mm"){
      print(paste(hprfile, ": Diameter Unit is NOT mm - please modify script for diasOB in Rhpr.R"))
    }
    TZ = fTZ(MachineReportHeader$CountryCode)

    operators <- sf2010r::getOperators(doc)
    # operators %>% dplyr::glimpse()

    # Object definition (Harvest site)
    objects <- sf2010r::getObjects( doc) # returns data_frame(ObjectKey, SubObjectKey, ObjectUserID, ObjectName, SubObjectName,  LoggingFormCode,  LoggingFormDesc )
    # objects %>% dplyr::glimpse()


    # Species and product definitions ----
    cat(" -hpr_file_readr-getSpeciesGroupDefs- \n")
    speciesgroups <- sf2010r::getSpeciesGroupDefinitions(doc)
    # speciesgroups %>% dplyr::glimpse()

    cat(" -hpr_file_readr-getProductDefs- \n")
    products <- sf2010r::getProductDefs(doc) #%>%
      #dplyr::mutate(  MachineKey = MachineReportHeader$MachineKey)
    # products %>% dplyr::glimpse()


    returnlist <- list( objects = objects,
                 products = products,
                 speciesgroups = speciesgroups,
                 operators = operators)

    cat(" -hpr_file_readr-getPricematrixes \n")
    pricematrixes <- sf2010r::getProductMatrixes(doc) # %>%
    # mutate( MachineKey = MachineReportHeader$MachineKey)
    # pricematrixes %>% dplyr::glimpse()

    if(!is.null(pricematrixes)){
      returnlist <- c(returnlist, pricematrixes = list(pricematrixes))
    }


    cat(" -hpr_file_readr-getStemTypes; \n")
    stemtypes <- sf2010r::getStemTypes(doc) %>%
      dplyr::mutate( MachineKey = MachineReportHeader$MachineKey)
    # stemtypes %>% dplyr::glimpse()

    if(nrow(stemtypes)>0){
      returnlist <- c(returnlist, stemtypes = list(stemtypes))
    }

    ## Harvested stems and logs ----

    cat(" -hpr_file_readr-getStemsAndLogs;  \n")
    StemsLogs <- getStemsAndLogs(doc)
    #StemsLogs <- sf2010r::getStemsAndLogs(doc)
    # Logs (single tree processed logs. Need to include multistemming later)
    returnlist <- c(returnlist,
                    logs = list((StemsLogs$stplogs %>% dplyr::mutate(MachineKey = MachineReportHeader$MachineKey))))

    # Stemdat modifications, joining summary data  from logs  ----------

    Stemdat <-  StemsLogs$stems %>%
      dplyr::mutate(
             MachineKey = MachineReportHeader$MachineKey
             #, CreationDate = MachineReportHeader$CreationDate
             ) %>%
      dplyr::left_join( (speciesgroups %>% dplyr::select( "SpeciesGroupKey", "SpeciesGroupName")), by = "SpeciesGroupKey")
    # Stemdat %>% dplyr::glimpse()


    stemdatfromlogs = StemsLogs$stplogs %>% #dplyr::glimpse()
      dplyr::group_by( .data$StemKey) %>%
      dplyr::summarise(
            num_logs = dplyr::n()
            , stem_length = sum(.data$LogLength)
            , stem_vol_m3sub = sum(.data$m3sub)
            , stem_vol_m3sob = sum(.data$m3sob)
      )
    Stemdat <- dplyr::left_join(Stemdat, stemdatfromlogs, by = "StemKey")

    returnlist <- c(returnlist, stemdat = list(Stemdat))

    # Grade vector for each tree: -------

    # #Denne IF() skyldes at JD har en feil i TimbermaticH / SF2010V3.2; oppgir kvalitetsvektor i dm i stede for cm.
     if(sum(StemsLogs$stemgrades$gradestartpos_cm > 0)>0){
       if (min(StemsLogs$stemgrades$gradestartpos_cm[StemsLogs$stemgrades$gradestartpos_cm > 0])<20){ #DETTE skyldes at JD har en feil i TimbermaticH / SF2010V3.2; oppgir kvalitetsvektor i dm i stede for cm.
         StemsLogs$stemgrades$gradestartpos_cm = StemsLogs$stemgrades$gradestartpos_cm*10
       }}

     grades <- StemsLogs$stemgrades %>% dplyr::mutate( MachineKey = MachineReportHeader$MachineKey)
     returnlist <- c(returnlist, grades = list(grades))

    # height diameter dataset: StemKey diaheight dia_ob_cm, dia_ub_cm -------
     #### height diameter dataset based on logs: StemKey diaheight dia_ob_cm, dia_ub_cm -------
     # cat(" - hpr_file_readr- create height diameter dataset from logs- \n")
     logmeter <- StemsLogs$stplogs %>%
       dplyr::select( -tidyselect::starts_with("m3"))  %>%
       dplyr::ungroup() %>%
       dplyr::group_by( StemKey) %>%
       dplyr::mutate(LogEndHeight = cumsum(.data$LogLength)) %>%
       dplyr::mutate(LogStartHeight = .data$LogEndHeight - .data$LogLength) %>%
       dplyr::mutate(LogMidHeight = .data$LogStartHeight + 0.5* .data$LogLength) %>%
       dplyr::ungroup()

     logmeternames <- names(logmeter)
     gselector <- c("StemKey", "LogKey")
     log_stemdias <- tibble::tibble(MachineKey = MachineReportHeader$MachineKey)

     if( any( stringr::str_detect(logmeternames, "Top.ob|Top.ub"))){
       if( any( stringr::str_detect(logmeternames, "Top.ob"))){
         topdia_ob <- c(logmeternames[stringr::str_detect(logmeternames, "Top.ob")], "diapos" = "LogEndHeight")
         topsonbark = logmeter %>% dplyr::select(tidyselect::all_of(gselector), tidyselect::all_of(topdia_ob)) %>%
         dplyr::mutate(logdiapos = "Top") %>%
           dplyr::rename(LogDiameter = "LogDiameter_Top.ob")
         log_stemdias_ob <- topsonbark

         if( any( stringr::str_detect(logmeternames, "Mid.ob"))){
           middia_ob <- c(logmeternames[stringr::str_detect(logmeternames, "Mid.ob")], "diapos" = "LogMidHeight")
           midsonbark = logmeter %>%
             dplyr::select(tidyselect::all_of(gselector), tidyselect::all_of(middia_ob)) %>%
             dplyr::mutate(logdiapos = "Mid") %>%
             dplyr::rename(LogDiameter = "LogDiameter_Mid.ob") ##### is this the way to put it?
           #dplyr::select( "StemKey", "LogKey", "diapos" = "LogMidHeight", "dia" = "LogDiameter_Mid.ob")
           log_stemdias_ob <- dplyr::bind_rows(log_stemdias_ob, midsonbark)
         }


         if( any(stringr::str_detect(logmeternames, "Butt.ob"))){
           butdia_ob <- c(logmeternames[stringr::str_detect(logmeternames, "Butt.ob")], "diapos" =  "LogStartHeight")
           butsonbark = logmeter %>%
             dplyr::select(tidyselect::all_of(gselector),  tidyselect::all_of(butdia_ob)) %>%
             dplyr::mutate(logdiapos = "Butt") %>%
             dplyr::rename(LogDiameter = "LogDiameter_Butt.ob")
           #dplyr::select( "StemKey", "LogKey", "diapos" = "LogStartHeight", "dia" = "Butt.ob")
           log_stemdias_ob <- dplyr::bind_rows(log_stemdias_ob, butsonbark)
         }
         log_stemdias_ob <- log_stemdias_ob %>% dplyr::arrange( .data$StemKey, .data$diapos) %>%
           dplyr::rename(LogDia_ob = "LogDiameter" )

         log_stemdias <- dplyr::bind_cols(log_stemdias, log_stemdias_ob)
        # returnlist <- c(returnlist, log_stemdias_ob = list(log_stemdias_ob))
       }


       if(any(stringr::str_detect(logmeternames, "Top.ub"))){
         topdia_ub <- c(logmeternames[stringr::str_detect(logmeternames, "Top.ub")], "diapos" = "LogEndHeight")
         topsubark = logmeter %>% dplyr::select(tidyselect::all_of(gselector), tidyselect::all_of(topdia_ub)) %>%
           dplyr::mutate(logdiapos = "Top") %>%
           dplyr::rename(LogDiameter = "LogDiameter_Top.ub")
         log_stemdias_ub <- topsubark

         if( any( stringr::str_detect(logmeternames, "Mid.ub"))){
           middia_ub <- c(logmeternames[stringr::str_detect(logmeternames, "Mid.ub")], "diapos" = "LogMidHeight")
           midsubark = logmeter %>%
             dplyr::select(tidyselect::all_of(gselector), tidyselect::all_of(middia_ub)) %>%
             dplyr::mutate(logdiapos = "Mid") %>%
             dplyr::rename(LogDiameter = "LogDiameter_Mid.ub")
           #dplyr::select( "StemKey", "LogKey", "diapos" = "LogMidHeight", "dia" = "LogDiameter_Mid.ub")
           log_stemdias_ub <- dplyr::bind_rows(log_stemdias_ub, midsubark)
         }


         if( any(stringr::str_detect(logmeternames, "Butt.ub"))){
           butdia_ub <- c(logmeternames[stringr::str_detect(logmeternames, "Butt.ub")], "diapos" =  "LogStartHeight")
           butsubark = logmeter %>%
             dplyr::select(tidyselect::all_of(gselector),  tidyselect::all_of(butdia_ub)) %>%
             dplyr::mutate(logdiapos = "Butt") %>%
             dplyr::rename(LogDiameter = "LogDiameter_Butt.ub")
           #dplyr::select( "StemKey", "LogKey", "diapos" = "LogStartHeight", "dia" = "Butt.ub")
           log_stemdias_ub <- dplyr::bind_rows(log_stemdias_ub, butsubark)
         }
         log_stemdias_ub <- log_stemdias_ub %>% dplyr::arrange( .data$StemKey, .data$diapos) %>%
           dplyr::rename(LogDia_ub = "LogDiameter" )

         if(nrow(log_stemdias) > 1){
           log_stemdias <- dplyr::left_join(log_stemdias, log_stemdias_ub, by = c("StemKey", "LogKey", "diapos", "logdiapos") )
         } else {log_stemdias <-dplyr::bind_cols(log_stemdias, log_stemdias_ub) }

       }
       returnlist <- c(returnlist, log_stemdias = list(log_stemdias))
     }


              # cat(" - hpr_file_readr- create height diameter dataset from logs- \n")
              # logmeter <- StemsLogs$stplogs %>%
              #   dplyr::select( -tidyselect::starts_with("m3"))  %>%
              #   dplyr::ungroup() %>%
              #   dplyr::group_by( StemKey) %>%
              #   dplyr:: mutate( LogEndHeight   = cumsum(.data$LogLength)) %>%
              #   dplyr::mutate(LogStartHeight = .data$LogEndHeight - .data$LogLength) %>%
              #   dplyr::mutate(LogMidHeight   = .data$LogStartHeight + 0.5* .data$LogLength) %>%
              #   dplyr::ungroup()
              #
              # stemlogdiasonbark = logmeter %>%
              #   dplyr::select( "StemKey", "LogKey", diapos = "LogEndHeight", dia = "Top.ob")
              #
              # if("Midob" %in% colnames(logmeter) ){
              #   midsonbark = logmeter %>%
              #     dplyr::select( "StemKey", "LogKey", "diapos" = "LogMidHeight", dia = "Mid.ob")
              #   stemlogdiasonbark <- dplyr::bind_rows(stemlogdiasonbark, midsonbark)
              # }
              #
              #
              # if("Butt.ob" %in% colnames(logmeter) ){
              #   cat(" - hpr_file_readr- fetch 'Butt ob'")
              #   butsonbark = logmeter %>%
              #     dplyr::select( "StemKey", "LogKey", diapos = "LogStartHeight",
              #                    dia = "Butt.ob")
              #   stemlogdiasonbark <- dplyr::bind_rows(stemlogdiasonbark, butsonbark)
              # }
              #
              # if(nrow(stemlogdiasonbark)){
              #   stemlogdiasonbark <- stemlogdiasonbark %>%
              #   dplyr::arrange( .data$StemKey, .data$diapos) %>%
              #   dplyr::mutate( MachineKey = MachineReportHeader$MachineKey)
              #
              # returnlist <- c(returnlist, stemlogdiasonbark = list(stemlogdiasonbark))
              # }
              #
              # # Stem diameters under bark from log diameters under bark
              #
              # if("Top.ub" %in% colnames(logmeter) ){
              # stemlogdiasubark = logmeter %>%
              #   dplyr::select( "StemKey", "LogKey",
              #                  "diapos" = "LogEndHeight", "dia" = "Top.ub")
              #
              #      if("Mid.ub" %in% colnames(logmeter) ){
              #       midsubark = logmeter %>%
              #         dplyr::select( "StemKey", "LogKey",
              #                        diapos = "LogMidHeight", dia = "Mid.ub")
              #       stemlogdiasubark <- dplyr::bind_rows(stemlogdiasubark, midsubark)
              #       }
              #
              #      if("Butt.ub" %in% colnames(logmeter) ){
              #       butsubark = logmeter %>%
              #         dplyr::select( "StemKey", "LogKey", diapos = "LogStartHeight",
              #                        dia = "Butt.ub")
              #       stemlogdiasubark <- dplyr::bind_rows(stemlogdiasubark, butsubark)
              #     }
              #
              # stemlogdiasubark <- stemlogdiasubark %>%
              #   dplyr::arrange( .data$StemKey, .data$diapos) %>%
              #   dplyr::mutate( MachineKey = MachineReportHeader$MachineKey)
              # returnlist <- c(returnlist, stemlogdiasubark = list(stemlogdiasubark))
              #
              #
              # }





    if(read.diavector == TRUE){
     # cat(" -hpr_file_readr- getSTP_stemdiameters;\n")
      stemdias <- sf2010r::getSTP_stemdiameters(doc)

      if( !is.null(stemdias)){
        stemdias <- stemdias %>%
        dplyr::mutate(MachineKey = MachineReportHeader$MachineKey)
        returnlist <- c(returnlist, stemdias = list(stemdias))
      }
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

    } else {
    return(NULL)
  }


  cat(" -hpr_file_readr complete - \n")
}




