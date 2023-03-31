

#' mom-file reader function
#'
#' @param momfile filename and path of the mom file to read
#' @return A list of data.frames:
#' machinereport_meta,
#' operators,
#' objects,
#' tracking,
#' CombProdDat = a table listing production data (stems, volumes, etc),
#' CombinedMachineWorkTime = a table listing work time,
#' OperatorWorkTime = a table listing work time for each operator,
#' IndividualMachineWorkTime
#' @export
#'
#' @examples
#' momfiles <- list.files(path =  system.file(package = "sf2010r"),
#'    pattern = ".mom$", recursive = TRUE,  ignore.case = TRUE,   full.names= TRUE)
#' momtest1 <- getMom.all(momfiles[1])
#' momtest2 <- getMom.all(momfiles[2])
#' momtest3 <- getMom.all(momfiles[3])
#' momtest4 <- getMom.all(momfiles[4])
getMom.all <- function(momfile){
 # "/MOM_Komatsu_harvester_sf2010v30_combined_mwt.MOM"
 # "/MOM_Ponsse_Forw_sf2010v31_individual_mwt.mom"
 # "/MOM_V3_3_MaxiXT_1_7_combined_mwt.mom"
 # "/MOM_Vimek_harvester_sf2010v20_individual_mwt.MOM"

    # momfile <- momfiles[1]

  doc <- xml2::read_xml(momfile)
  con <- file(momfile)
  md5 <-  digest::digest(con)


  # .. cut object info
  filename <- momfile
  tmp <- nchar(filename)
  filetype <- tolower(substring(filename, tmp-2, tmp))


  # .. from the header
  header <- sf2010r::getMachineReportHeader(doc)
  header$md5 = md5
  # header %>% dplyr::glimpse()

  if(header$diameterUnit[1]!="mm"){
    print(paste(filename, ": Diameter Unit is NOT mm - please modify script for diasOB in Rhpr.R"))
  }
  #TZ = fTZ(header$CountryCode)

  operators <- sf2010r::getOperators(doc)
  # operators %>% dplyr::glimpse()

  # Object definition (Harvest site)
  objects <- sf2010r::getObjects( doc) # returns data_frame(ObjectKey, SubObjectKey, ObjectUserID, ObjectName, SubObjectName,  LoggingFormCode,  LoggingFormDesc )
  # objects %>% dplyr::glimpse()


  # Species and product definitions ----
  speciesgroups <- sf2010r::getSpeciesGroupDefinitions(doc)
  # speciesgroups %>% dplyr::glimpse()

  products <- sf2010r::getProductDefs(doc)

  # products %>% dplyr::glimpse()
  tracking <- sf2010r::getTracking.data(doc)

  returnlist <- list(header = header, speciesgroups = speciesgroups, objects = objects, products = products,
                     operators = operators,   tracking = tracking)




  # Individual machine time data
  imwtlist <- xml2::xml_find_all(doc, ".//d1:IndividualMachineWorkTime")


  if(length(imwtlist)) {
  imwt_activity <- plyr::ldply(imwtlist, getMom.imwt.activity)
  imwt_production <- plyr::ldply(imwtlist, getMom.imwt.production)
    returnlist <- c(returnlist, imwt_activity = list(imwt_activity), imwt_production = list(imwt_production))
  }


  # # Combined machine time data
   cmwtlist <- xml2::xml_find_all(doc, ".//d1:CombinedMachineWorkTime")
   if(length(cmwtlist)){
     cmwt_data <- plyr::ldply(cmwtlist, getMom.cmwt.data)
     returnlist <- c(returnlist, cmwt_data = list(cmwt_data))
   }

  #
  #     # fetching production data (volume etc)
  #     if(tolower(header$machinetype) == "harvester"){
  #
  #       CombHarvDat <- getCombinedHrvProd(xmlfile, TimeZone = TZ, dnamespaces=d)
  #       if(length(CombHarvDat)){
  #         CombHarvDat$machinekey = header$MachineKey
  #       }
  #
  #
  #
  #       returnlist = c(returnlist,
  #                      list(dfCombined = dfCombined, CombProdDat = CombHarvDat, CombinedMachineWorkTime = CombinedMachineWorkTime,
  #                         OperatorWorkTime = CombinedOperatorWorkTime))
  #
  #     }
  #
  #     if(tolower(machinetype) == "forwarder"){
  #       CombForwDat <- plyr::ldply(xpathApply(xmlfile,"//d:CombinedMachineWorkTime", getCombinedForwProd,  TimeZone = TZ, namespaces=d))
  #       CombForwDat$machinekey = MachineKey
  #
  #       returnlist = c(returnlist,
  #                      list(dfCombined = dfCombined,
  #                           CombProdDat = CombForwDat,
  #                           CombinedMachineWorkTime = CombinedMachineWorkTime,
  #                           OperatorWorkTime = CombinedOperatorWorkTime))
  #
  #     }

  #  } # End if length(CMTOperatorKey), i.e. the combined machine work time part

  return(returnlist)
 } #end function
#'
#'
