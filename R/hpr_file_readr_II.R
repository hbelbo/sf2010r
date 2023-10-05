
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
#' @examples
#' hprfiles <- list.files(path =  system.file(package = "sf2010r"),
#'   pattern = ".hpr", recursive = TRUE, full.names= TRUE)
#' hprtest1 <- hpr_file_readr_II(hprfiles[1], read.diavector = TRUE)
#' hprtest2 <- hpr_file_readr_II(hprfiles[2])
#' hprtest3 <- hpr_file_readr_II(hprfiles[3])
#' hprtest4 <- hpr_file_readr_II(hprfiles[3], read.diavector = TRUE)
hpr_file_readr_II <- function(hprfile, read.diavector = FALSE){
  # hprfiles <- list.files(path =  system.file(package = "sf2010r"), pattern = ".hpr", recursive = TRUE, full.names= TRUE)
  # hprfile = hprfiles[1]

  cat("\n-hpr_file_readr() parsing ", hprfile,"- \n")
  filename <- hprfile
  tmp <- nchar(filename)
  filetype <- substring(filename, tmp-2, tmp)
  stopifnot(filetype == "hpr")

   doc <- xml2::read_xml(hprfile)
   md5 <- digest::digest(file(hprfile))

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
    StemsLogs <- getStemsAndLogs_II(doc)
    #StemsLogs <- sf2010r::getStemsAndLogs_II(doc)
    # Logs (single tree processed logs. Need to include multistemming later)
    returnlist <- c(returnlist,
                    logs = list((StemsLogs$stplogs %>% dplyr::mutate(MachineKey = MachineReportHeader$MachineKey))))

    # Stemdat modifications, joining summary data  from logs  ----------
    Stemdat <- StemsLogs$stems
    # Stemdat %>% dplyr::glimpse()
    Stemdat <- Stemdat %>%
      mutate(
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
    # if(sum(StemsLogs$stemgrades$gradestartpos_cm > 0)>0){
    #   if (min(StemsLogs$stemgrades$gradestartpos_cm[StemsLogs$stemgrades$gradestartpos_cm > 0])<20){ #DETTE skyldes at JD har en feil i TimbermaticH / SF2010V3.2; oppgir kvalitetsvektor i dm i stede for cm.
    #     StemsLogs$stemgrades$gradestartpos_cm = StemsLogs$stemgrades$gradestartpos_cm*10
    #   }}
    #
    # grades <- StemsLogs$stemgrades %>% dplyr::mutate( MachineKey = MachineReportHeader$MachineKey)
    # returnlist <- c(returnlist, grades = list(grades))

    # height diameter dataset: StemKey diaheight dia_ob_cm, dia_ub_cm -------

    cat(" - hpr_file_readr- create height diameter dataset from logs- \n")
    logmeter <- StemsLogs$stplogs %>%
      select( -tidyselect::starts_with("m3"))  %>%
      dplyr::ungroup() %>%
      dplyr::group_by( StemKey) %>%
      dplyr:: mutate( LogEndHeight   = cumsum(.data$LogLength)) %>%
      dplyr::mutate(LogStartHeight = .data$LogEndHeight - .data$LogLength) %>%
      dplyr::mutate(LogMidHeight   = .data$LogStartHeight + 0.5* .data$LogLength) %>%
      dplyr::ungroup()

    stemlogdiasonbark = logmeter %>%
      dplyr::select( "StemKey", "LogKey", diapos = "LogEndHeight", dia = "Top.ob")

    if("Midob" %in% colnames(logmeter) ){
      midsonbark = logmeter %>%
        dplyr::select( "StemKey", "LogKey", "diapos" = "LogMidHeight", dia = "Mid.ob")
      stemlogdiasonbark <- dplyr::bind_rows(stemlogdiasonbark, midsonbark)
    }


    if("Butt.ob" %in% colnames(logmeter) ){
      cat(" - hpr_file_readr- fetch 'Butt ob'")
      butsonbark = logmeter %>%
        dplyr::select( "StemKey", "LogKey", diapos = "LogStartHeight",
                       dia = "Butt.ob")
      stemlogdiasonbark <- dplyr::bind_rows(stemlogdiasonbark, butsonbark)
    }

    if(nrow(stemlogdiasonbark)){
      stemlogdiasonbark <- stemlogdiasonbark %>%
      dplyr::arrange( .data$StemKey, .data$diapos) %>%
      dplyr::mutate( MachineKey = MachineReportHeader$MachineKey)

    returnlist <- c(returnlist, stemlogdiasonbark = list(stemlogdiasonbark))
    }

    # Stem diameters under bark from log diameters under bark

    if("Top.ub" %in% colnames(logmeter) ){
    stemlogdiasubark = logmeter %>%
      dplyr::select( "StemKey", "LogKey",
                     "diapos" = "LogEndHeight", "dia" = "Top.ub")

         if("Mid.ub" %in% colnames(logmeter) ){
          midsubark = logmeter %>%
            dplyr::select( "StemKey", "LogKey",
                           diapos = "LogMidHeight", dia = "Mid.ub")
          stemlogdiasubark <- dplyr::bind_rows(stemlogdiasubark, midsubark)
          }

         if("Butt.ub" %in% colnames(logmeter) ){
          butsubark = logmeter %>%
            dplyr::select( "StemKey", "LogKey", diapos = "LogStartHeight",
                           dia = "Butt.ub")
          stemlogdiasubark <- bind_rows(stemlogdiasubark, butsubark)
        }

    stemlogdiasubark <- stemlogdiasubark %>%
      dplyr::arrange( .data$StemKey, .data$diapos) %>%
      dplyr::mutate( MachineKey = MachineReportHeader$MachineKey)
    returnlist <- c(returnlist, stemlogdiasubark = list(stemlogdiasubark))


    }





    if(read.diavector == TRUE){
      cat(" -hpr_file_readr- getSTP_stemdiameters;\n")
      stemdias <- sf2010r::getSTP_stemdiameters_II(doc)

      if( !is.null(stemdias)){
       stemdiametervector <- stemdias %>%
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
                                    sub_obj_keys  = paste(objects$SubObjectKey, collapse=", "),
                                    sub_obj_ids  = paste(objects$SubObjectUserID, collapse=", "),
                                    filetype = filetype
                                    )

    returnlist <- c(returnlist, machinereport_meta = list(machinereport_meta))

    return(returnlist)

    } else {
    return(NULL)
  }


  cat(" -hpr_file_readr complete - \n")
}




