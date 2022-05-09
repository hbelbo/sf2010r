#' ################
#' #  .mom decoder
#' ################
#'
#'
#' # sample files for testing:
#' #momfiles <- list.files(path = "./example_datasets/MOM", pattern="\\.mom|MOM", full.names = T)
#' #momfile = momfiles[3]
#' #test <- MomFileReadReportByShift(momfiles[1])
#'
#'
#'
#'
#'
#' #' mom-file reader function
#' #'
#' #' @param momfile filename and path of the mom file to read
#'
#' #' @return A list of data.frames:
#' #' machinereport_meta,
#' #' operators,
#' #' objects,
#' #' OperatorWorkTime,
#' #' CombProdDat = a table listing production data (stems, volumes, etc),
#' #' CombinedMachineWorkTime = a table listing work time,
#' #' OperatorWorkTime = a table listing work time for each operator,
#' #' IndividualMachineWorkTime,
#' #' @export
#' #'
#' #' @examples
#' #' momfiles <- list.files(path =  system.file(package = "sf2010r"),  pattern = ".mom", recursive = TRUE,  ignore.case = TRUE,   full.names= TRUE)
#' ##' momfiles <- list.files(path = "S:/PaTversAvProsjekter/R_koder/Helmer/R_pkgs/sf2010r/sf2010r/inst/extdata",  pattern = ".mom", ignore.case = TRUE, recursive = TRUE, full.names= TRUE)
#' #' momtest1 <- momdata(momfiles[1])
#' #' momtest2 <- momdata(momfiles[2])
#' #' momtest3 <- momdata(momfiles[3])
#' #' momtests <- purrr::map_dfr(momfiles, ~tibble::tibble(filename = .x, momdata = list(momdata(.x))))
#' MomFileReadReportByShift <- function(momfile){
#'
#'
#'   doc <- xml2::read_xml(hprfile)
#'   md5 <-  digest::digest(file(hprfile))
#'
#'   # .. cut object info
#'   filename <- momfile
#'   tmp <- nchar(filename)
#'   filetype <- substring(filename, tmp-2, tmp)
#'
#'
#'   # .. from the header
#'   MachineReportHeader <- sf2010r::getMachineReportHeader(doc)
#'   MachineReportHeader$md5 = md5
#'   # MachineReportHeader %>% dplyr::glimpse()
#'
#'   if(MachineReportHeader$diameterUnit[1]!="mm"){
#'     print(paste(hprfile, ": Diameter Unit is NOT mm - please modify script for diasOB in Rhpr.R"))
#'   }
#'   TZ = fTZ(MachineReportHeader$CountryCode)
#'
#'   operators <- sf2010r::getOperators(doc)
#'   # operators %>% dplyr::glimpse()
#'
#'   # Object definition (Harvest site)
#'   objects <- sf2010r::getObjects( doc) # returns data_frame(ObjectKey, SubObjectKey, ObjectUserID, ObjectName, SubObjectName,  LoggingFormCode,  LoggingFormDesc )
#'   # objects %>% dplyr::glimpse()
#'
#'
#'   # Species and product definitions ----
#'   speciesgroups <- sf2010r::getSpeciesGroupDefinitions(doc)
#'   # speciesgroups %>% dplyr::glimpse()
#'
#'   products <- sf2010r::getProductDefs(doc) %>%
#'     dplyr::mutate(  MachineKey = MachineReportHeader$MachineKey,
#'                     CreationDate = MachineReportHeader$CreationDate)
#'   # products %>% dplyr::glimpse()
#'
#'   # Individual machine time data
#'   imwtlist <- xml2::xml_find_all(doc, ".//d1:IndividualMachineWorkTime")
#'   imwt_activity <- plyr::ldply(imwtlist, getMom.imwt.activity)
#'   imwt_production <- plyr::ldply(imwtlist, getMom.imwt.production)
#'
#'
#'
#'   # Combined machine time data
#'
#'
#'       # fetching production data (volume etc)
#'       if(tolower(MachineReportHeader$machinetype) == "harvester"){
#'
#'         CombHarvDat <- getCombinedHrvProd(xmlfile, TimeZone = TZ, dnamespaces=d)
#'         if(length(CombHarvDat)){
#'           CombHarvDat$machinekey = MachineReportHeader$MachineKey
#'         }
#'
#'
#'
#'         returnlist = list(dfCombined = dfCombined, CombProdDat=CombHarvDat, CombinedMachineWorkTime = CombinedMachineWorkTime,
#'                           OperatorWorkTime = CombinedOperatorWorkTime, speciesgroups=speciesgroups, ObjectDefinitions = DefObject,
#'                           momreports_df=momreports_df, OperatorsDf=OperatorsDf)
#'
#'       }
#'
#'       if(tolower(machinetype) == "forwarder"){
#'         CombForwDat <- plyr::ldply(xpathApply(xmlfile,"//d:CombinedMachineWorkTime", getCombinedForwProd,  TimeZone = TZ, namespaces=d))
#'         CombForwDat$machinekey = MachineKey
#'
#'         returnlist = list(dfCombined = dfCombined, CombProdDat=CombForwDat,
#'                           CombinedMachineWorkTime = CombinedMachineWorkTime,
#'                           OperatorWorkTime = CombinedOperatorWorkTime,
#'                           speciesgroups=speciesgroups, ObjectDefinitions = DefObject,
#'                           momreports_df=momreports_df,  OperatorsDf=OperatorsDf)
#'
#'       }
#'
#'     } # End if length(CMTOperatorKey), i.e. the combined machine work time part
#'   } else {returnlist = list(dfCombined = NULL, # if no operator work times present in the files: Return empty lists
#'                             CombProdDat=NULL, CombinedMachineWorkTime = NULL,
#'                             OperatorWorkTime = NULL, speciesgroups=NULL, ObjectDefinitions = NULL,
#'                             momreports_df=NULL, OperatorsDf=NULL) }
#'   return(returnlist)
#' } #end function
#'
#'
