

#' Make a named character vector of children values
#' @description This function takes a xml node and return a
#' named character vector, where the names are the child node names
#' and the values are the node values.
#' No attributes or attribute values are included.
#' @param y a list xml_node with some children nodes
#'
#' @return a named vector
#' @export
#'
#' @examples
#' hprfiles <-  list.files(path =  system.file(package = "sf2010r"),  pattern = ".hpr", recursive = TRUE, full.names= TRUE)
#' doc <- xml2::read_xml(hprfiles[1])
#' x <-  xml2::xml_children(doc)[1] # Get one node of doc, the first is normally a header
#' xml_childs_nchr(x[[1]]) # The function returns a named vector
#' x %>% purrr::map_dfr( ~ xml_childs_nchr(.x))  # Converted to tibble
xml_childs_nchr <- function(y) {
  #  y <- x2[[1]]
  # y <- x
  #node_attrs <- xml2::xml_attrs(y )
  childrens <- xml2::xml_children(y)
  childrens <- childrens %>% purrr::keep(~ xml2::xml_length(.x)<1)
  child_names <- xml2::xml_name(childrens)
  child_vals <- xml2::xml_text(childrens)



  result <-child_vals %>% stats::setNames( nm = unlist(child_names))
  return(result)
}



#' Make a data table of node children values and corresponding attribute values
#' @description This function takes a xml node and return a
#' data.table dataframe, where the names are the child node names
#' and the values are the node values.
#' Attributes and attribute values are included.
#' @param y a list xml_node with some children nodes
#'
#' @return a data table
#' @export
#'
#' @examples
#' hprfiles <-  list.files(path =  system.file(package = "sf2010r"),  pattern = ".hpr", recursive = TRUE, full.names= TRUE)
#' doc <- xml2::read_xml(hprfiles[1])
#' x <- xml2::xml_children(doc)[2] # Get one node of doc, the first is normally a header
#' xml_childs_dt(x[[1]]) # The function returns a data.table
#' x <- xml2::xml_children(x)
#' xml_childs_dt(x[[27]])
#'  x <- xml2::xml_children(x[[27]])
#' xml_childs_dt(x)
#' x %>% purrr::map_dfr( ~ xml_childs_dt(.x))  # Converted to tibble
xml_childs_dt <- function(y) {
  #  y <- x2[[1]]
  # y <- x
  node_attrs <- xml2::xml_attrs(y )
  childrens <- xml2::xml_children(y)
  childrens <- childrens %>% purrr::keep(~ xml2::xml_length(.x)<1)
  child_names <- xml2::xml_name(childrens)

  child_attrs <- xml2::xml_attrs(childrens)
  #child_attrs <- unlist(lapply(child_attrs, function(x) ifelse(length(x)>0, x, NA_character_)))
  child_attrs <- unlist(lapply(child_attrs, function(x) ifelse(length(x)>0, paste0(names(x)," ", x), NA_character_)))

  if(sum(!is.na(child_attrs))>0){
    child_dt <-
      data.table::data.table(cild_name =  child_names,
                             attrs = child_attrs,
                             child_vals = xml2::xml_text(childrens))
  } else {
    child_dt <-
      data.table::data.table(cild_name =  child_names,
                             child_vals = xml2::xml_text(childrens))
  }


  return(child_dt)
}



#' Organize the header data of the StanForD2010 report into a tibble
#'
#' @param doc is an xml document
#' @return a tibble
#' @export
#'
#' @examples
#' hprfiles <-  list.files(path =  system.file(package = "sf2010r"),  pattern = ".hpr", recursive = TRUE, full.names= TRUE)
#' doc <- xml2::read_xml(hprfiles[1])
#' getMachineReportHeader(doc)
#' machinerep_h <- getMachineReportHeader(doc)
getMachineReportHeader <- function(doc){

   # .. from the header
  headerattributes <-  dplyr::bind_rows(xml2::xml_attrs(doc))
  headerdata <-  dplyr::bind_rows(xml2::xml_children(doc)[1] %>% xml_childs_nchr())

   ReportHeader <- dplyr::bind_rows(
    messageheader = xml2::xml_name(doc)) %>%
    dplyr::bind_cols( headerdata) %>%
    dplyr::bind_cols( headerattributes)




  # ..Machine info
  nodes <-  xml2::xml_find_all(doc,  "./d1:Machine")
  basemachinedata <- dplyr::bind_rows(nodes %>% xml_childs_nchr()) %>%
    dplyr::mutate(MachineCategory = xml2::xml_attr(nodes, attr = "machineCategory"))


  # ..Contractor info
  nodes <- xml2::xml_find_all(doc, ".//d1:LoggingContractor")
  contractordata <- dplyr::bind_rows(nodes %>% xml_childs_nchr())
  contractordata <- dplyr::rename_with(contractordata, ~ paste0("Contractor_", .x))

  machineReportHeader <- dplyr::bind_cols(ReportHeader, basemachinedata, contractordata)

  return(machineReportHeader)
}



#' Get Time zone from the StanForD2010 countrycode variables.
#'
#' @param CountryCode codes according to ISO3166-1 standard
#'
#' @return Time zone as Country/City
#' @export
#'
#' @examples fTZ("752")
fTZ <- function(CountryCode = ""){
  #Function to return time zone based on country code according to ISO3166-1 standard
  if(is.numeric(CountryCode)){CountryCode = as.character(CountryCode)}
  retval = switch(CountryCode,
                  "752" = "Europe/Stockholm", # ISO standard: https://www.iso.org/obp/ui + StanFOrD Appendix
                  "246" = "Europe/Helsinki", # ISO standard: https://www.iso.org/obp/ui + StanFOrD Appendix
                  "578" = "Europe/Oslo", # ISO standard: https://www.iso.org/obp/ui + StanFOrD Appendix
                  "840" = "Europe/Oslo", #According to StanForD Appendix 840 is USA
                  "208" = "Europe/Oslo" # According to StanForD Appendix 208 is Denmark
  )
  if (!length(retval)){
    retval = "UTC"
  }
  return(retval)
}


MtcClean <- function(mtc){
  # Function to replace "/" and spaces with _ in character variable values (e.g. machine time category)
  # Parameter is mtc; machine time category which is having these characters
  # returning the same text but replaced backslash
  #mtcc =  gsub(pattern="/", replacement = "_", x=mtc) #mtc = machine time category
 # mtcc <- gsub(pattern="[[:space:]_]{1,3}", replacement = "_", x = mtcc)
  mtcc <- stringr::str_replace_all(mtc, "[^[:alnum:]]", "_")
  return(mtcc)
}





#' Species group def from one SpeciesGroupDefinition node
#' @param x is a node tree for one SpeciesGrouDefinition
#'
#'
#' @export
#'
#' @examples
#' hprfiles <-  list.files(path =  system.file(package = "sf2010r"),  pattern = ".hpr", recursive = TRUE, full.names= TRUE)
#' doc <- xml2::read_xml(hprfiles[1])
#' SpeciesList <- xml2::xml_find_all(doc, ".//d1:SpeciesGroupDefinition" )
#' getSpeciesGroupDef(SpeciesList[[1]])
#' species <- plyr::ldply(SpeciesList, getSpeciesGroupDef)
getSpeciesGroupDef <- function(x) {
  # x <- SpeciesList[[1]]
  spcs_dt <- dplyr::bind_rows( sf2010r::xml_childs_nchr(x))
  spcs_dt$SpeciesGroupKey = as.integer(spcs_dt$SpeciesGroupKey)
  return(spcs_dt)
}




#' Get all species definitions within a SF2010 doc
#'
#' @param doc a StanFord2010 xml document
#'
#' @return a tibble
#' @export
#'
#' @examples
#' hprfiles <-  list.files(path =  system.file(package = "sf2010r"),  pattern = ".hpr", recursive = TRUE, full.names= TRUE)
#' doc <- xml2::read_xml(hprfiles[1])
#' getSpeciesGroupDefinitions(doc)
getSpeciesGroupDefinitions <- function(doc){
  SpeciesList <- xml2::xml_find_all(doc, ".//d1:SpeciesGroupDefinition" )
  bmatrix <- plyr::ldply(SpeciesList, sf2010r::getSpeciesGroupDef)
  #bmatrix <- plyr::ldply(SpeciesList, getSpeciesGroupDef)
  MachineKey <-  xml2::xml_text( xml2::xml_find_first(doc, ".//d1:Machine/d1:MachineKey"))
  bmatrix$MachineKey = MachineKey
  not_all_na <- function(x) {!all(is.na(x))}
  bmatrix <- bmatrix %>% dplyr::select_if(not_all_na)
  return(bmatrix)
}






#' get stem type definitions from one SpeciesGroupDefinition
#' @param x is a node tree of SpeciesGrouDefinition
#'
#' @export
#'
#' @examples
#' hprfiles <-  list.files(path =  system.file(package = "sf2010r"),  pattern = ".hpr", recursive = TRUE, full.names= TRUE)
#' doc <- xml2::read_xml(hprfiles[1])
#' SpeciesList <- xml2::xml_find_all(doc, ".//d1:SpeciesGroupDefinition" )
#' getStemTypeDefs(SpeciesList[[1]])
getStemTypeDefs <- function(x) {
  # x <- SpeciesList[[1]]
  SpeciesGroupKey = xml2::xml_integer( xml2::xml_find_all(x, ".//d1:SpeciesGroupKey"))
  StemTypeCode = xml2::xml_integer( xml2::xml_find_all(x, ".//d1:StemTypeDefinition/d1:StemTypeCode"))
  StemTypeName = xml2::xml_text( xml2::xml_find_all(x, ".//d1:StemTypeDefinition/d1:StemTypeName"))

  if(length(StemTypeCode)){
    stemtypes <-
      tibble::tibble(SpeciesGroupKey = SpeciesGroupKey,
                     StemTypeCode = StemTypeCode,
                     StemTypeName = StemTypeName
      )
  } else {stemtypes = NULL}
  return(stemtypes)
}



#' Get all stem type definitions for all SpeciesGroupDefinitions
#'
#' @param doc a StanFord2010 xml document
#' @return a list
#' @export
#'
#' @examples
#' hprfiles <-  list.files(path =  system.file(package = "sf2010r"),  pattern = ".hpr", recursive = TRUE, full.names= TRUE)
#' doc <- xml2::read_xml(hprfiles[1])
#' getStemTypes(doc)
getStemTypes <- function(doc){
  SpeciesList <- xml2::xml_find_all(doc, ".//d1:SpeciesGroupDefinition" )
  bmatrix <- plyr::ldply(SpeciesList, sf2010r::getStemTypeDefs)
  MachineKey <-  xml2::xml_text( xml2::xml_find_all(doc, ".//d1:Machine/d1:MachineKey"))
  bmatrix$MachineKey = MachineKey
  return(bmatrix)
}




#' Product def from product definition nodetree
#' @param x is a node tree of ProductDefinition
#'
#' @export
#'
#' @examples
#' hprfiles <-  list.files(path =  system.file(package = "sf2010r"),  pattern = ".hpr", recursive = TRUE, full.names= TRUE)
#' doc <- xml2::read_xml(hprfiles[1])
#' ProductsList <- xml2::xml_find_all(doc, ".//d1:ProductDefinition" )
#' getProductDef(ProductsList[[1]]) %>% dplyr::glimpse()
#' plyr::ldply(ProductsList, getProductDef )
getProductDef <- function(x) {
  # x <- ProductsList[[1]]
  ProductKey <- xml2::xml_integer( xml2::xml_find_first(x,  ".//d1:ProductKey"))
  cpd <- xml2::xml_find_first(x,  ".//d1:ClassifiedProductDefinition")

  if(length(cpd)>0){
    spcs_dt <- dplyr::bind_rows(cpd %>% xml_childs_nchr())
    spcs_dt$ProductKey <- as.integer(ProductKey)
    spcs_dt$SpeciesGroupKey <- as.integer(spcs_dt$SpeciesGroupKey)
    Pricedef_VolumeDiamAdj <- xml2::xml_text( xml2::xml_find_first(x, ".//d1:PriceDefinition/d1:VolumeDiameterAdjustment"))
    Pricedef_VolumeLDiamCat <- xml2::xml_text( xml2::xml_find_first(x, ".//d1:PriceDefinition/d1:VolumeDiameterCategory"))
    Pricedef_VolumeLengthCat <- xml2::xml_text( xml2::xml_find_first(x, ".//d1:PriceDefinition/d1:VolumeLengthCategory"))
    Pricedef_VolumeUnderBark <- xml2::xml_text( xml2::xml_find_first(x, ".//d1:PriceDefinition/d1:VolumeUnderBark"))
    DiaDef_DiameterClassMAX <- xml2::xml_text( xml2::xml_find_first(x, ".//d1:DiameterDefinition/d1:DiameterClasses/d1:DiameterClassMAX"))
    DiaDef_DiameterUnderBark <- xml2::xml_text( xml2::xml_find_first(x, ".//d1:DiameterDefinition/d1:DiameterClasses/d1:DiameterUnderBark"))
    DiaDef_DiameterMINTop <- xml2::xml_text( xml2::xml_find_first(x, ".//d1:DiameterDefinition/d1:DiameterMINTop"))
    DiaDef_DiameterMAXButt <- xml2::xml_text( xml2::xml_find_first(x, ".//d1:DiameterDefinition/d1:DiameterMAXButt"))
    DiaDef_DiameterTopPosition <- xml2::xml_text( xml2::xml_find_first(x, ".//d1:DiameterDefinition/d1:DiameterTopPosition"))
    LengthDef_LengthClassMin <- xml2::xml_integer( xml2::xml_find_first(x, ".//d1:LengthDefinition/d1:LengthClass/d1:LengthClassLowerLimit"))
      if(!is.na(LengthDef_LengthClassMin) & length(LengthDef_LengthClassMin)>0) {  LengthDef_LengthClassMin <- min(LengthDef_LengthClassMin) } else { LengthDef_LengthClassMin <- NA}
    LengthDef_LengthClassMAX <-  xml2::xml_integer( xml2::xml_find_first(x, ".//d1:LengthDefinition/d1:LengthClassMAX"))
    # err_test <-  xml2::xml_integer( xml2::xml_find_first(x, ".//d1:LengthDefinition/d1:Errr"))
    StemTypeCode <- xml2::xml_text( xml2::xml_find_first(x, ".//d1:StemTypeCode"))


   products <- spcs_dt %>%
     dplyr::mutate( Pricedef_VolumeDiamAdj
             , Pricedef_VolumeLDiamCat
             , Pricedef_VolumeLengthCat
             , Pricedef_VolumeUnderBark
             , DiaDef_DiameterClassMAX
             , DiaDef_DiameterUnderBark
             , DiaDef_DiameterMINTop
             , DiaDef_DiameterMAXButt
             , DiaDef_DiameterTopPosition
             , LengthDef_LengthClassMin
             , LengthDef_LengthClassMAX
             , StemTypeCode)
   products <- products %>%  dplyr::select(.data$ProductKey, .data$ProductName, tidyselect::everything())

   }

  else {products <- tibble::tibble(ProductKey  = as.integer(ProductKey)
                                   , ProductName = "Unclassified")}
  return(products)
}
#getProductDef(ProductsList[[1]])


#' Get all product definitions
#'
#' @param doc a StanFord2010 xml document
#'
#' @return a list
#' @export
#'
#' @examples
#' hprfiles <-  list.files(path =  system.file(package = "sf2010r"),  pattern = ".hpr", recursive = TRUE, full.names= TRUE)
#' doc <- xml2::read_xml(hprfiles[1])
#' getProductDefs(doc)
#' doc <- xml2::read_xml(hprfiles[2])
#' getProductDefs(doc)
getProductDefs <- function(doc){
  ProductsList <- xml2::xml_find_all(doc, ".//d1:ProductDefinition" )
  bmatrix <- plyr::ldply(ProductsList, getProductDef)
  #bmatrix <- plyr::ldply(ProductsList, sf2010r::getProductDef)
  MachineKey <- xml2::xml_text(xml2::xml_find_first(doc, ".//d1:Machine/d1:MachineKey"))
  bmatrix$MachineKey = MachineKey
  return(bmatrix)
}




#' Get the Product Matrix items
#'
#' @param x is a Product definition tree from StanFord2010 xml document
#'
#' @return A tibble, or NULL if no products are defined
#' @export
#'
#' @examples
#' hprfiles <-  list.files(path =  system.file(package = "sf2010r"),  pattern = ".hpr", recursive = TRUE, full.names= TRUE)
#' doc <- xml2::read_xml(hprfiles[1])
#' Productslist <- xml2::xml_find_all(doc, ".//d1:ProductDefinition" )
#' getProductMatrixItems(Productslist[[1]])
getProductMatrixItems <- function(x) {
  # x <- Productslist[[1]]
  ProductKey =  xml2::xml_integer( xml2::xml_find_first(x,  ".//d1:ProductKey"))
  nodes = xml2::xml_find_all(x,  ".//d1:ProductMatrixItem")
 if(length(nodes)){

  ItemLimits = xml2::xml_attrs(x = nodes)

  pm <- purrr::map_df(.x = nodes, ~ xml_childs_nchr(.x))


    dimnames1 = names(ItemLimits[[1]]) #Rownames
    pm <- pm %>% dplyr::mutate( ProductKey = as.numeric(ProductKey),
                     !!dimnames1[1] := as.numeric(sapply(ItemLimits, function(x) x[1])),
                     !!dimnames1[2] := as.numeric(sapply(ItemLimits, function(x) x[2])))

  } else {pm = NULL}
  return(pm)
}


#' Get all price matrixes
#'
#' @param doc an StanFord2010 xml document having product definitions
#'
#' @return A list
#' @export
#'
#' @examples
#' hprfiles <-  list.files(path =  system.file(package = "sf2010r"),  pattern = ".hpr", recursive = TRUE, full.names= TRUE)
#' doc <- xml2::read_xml(hprfiles[1])
#' pms <- getProductMatrixes(doc)
getProductMatrixes <- function(doc){
  Productslist <- xml2::xml_find_all(doc,  ".//d1:ProductDefinition")
  bmatrix <- plyr::ldply(Productslist, sf2010r::getProductMatrixItems)
  #bmatrix <- plyr::ldply(Productslist, getProductMatrixItems)
  MachineKey <- xml2::xml_text(xml2::xml_find_first(doc, ".//d1:Machine/d1:MachineKey"))
  bmatrix$MachineKey = MachineKey
  return(bmatrix)
}

#' finding price matrix entry for base logs
#'
#' @param pricematrixes is a data frame with all products diams lengths pricetags
#' @param base_lengthcm length of baselog, normally 49 dm
#' @param base_diamm diameter of baselog, normally 20 cm
#'
#' @description For all productKeys: find the 49/20 log value
#'
#' @return a tibble of all products' base logs and pricetags
#' @export
#'
#' @examples
#' hprfiles <-  list.files(path =  system.file(package = "sf2010r"),  pattern = ".hpr", recursive = TRUE, full.names= TRUE)
#' doc <- xml2::read_xml(hprfiles[1])
#' pricematrixes <- getProductMatrixes(doc)
#' price_matr_entry_base_log_class(pricematrixes)
price_matr_entry_base_log_class <- function(pricematrixes, base_lengthcm = 490, base_diamm = 200){
  toreturn <-
    pricematrixes %>%
    dplyr::group_by(.data$MachineKey, .data$ProductKey) %>%
    dplyr::filter(.data$diameterClassLowerLimit == max(.data$diameterClassLowerLimit[.data$diameterClassLowerLimit <= base_diamm]),
           .data$lengthClassLowerLimit == max(.data$lengthClassLowerLimit[.data$lengthClassLowerLimit<=base_lengthcm])) %>%

    dplyr::rename(baselog_diaclass = .data$diameterClassLowerLimit,  baselog_lengthclass = .data$lengthClassLowerLimit,
           baselog_price = .data$Price) # %>% dplyr::filter(ProductKey==170)%>% #%>% tail() %>% glimpse()3
  return(toreturn)
}

# makeRelativePrices_by_species <- function(products, pricematrixes) {
#
#   products2 <- products %>%
#           select(.data$ProductKey, .data$MachineKey, .data$SpeciesGroupName, .data$SpeciesGroupKey, .data$ProductGroupName, .data$ProductName, .data$CreationDate)
#      p_baselogs <- price_matr_entry_base_log_class( pricematrixes)
#   p_baselogs <- p_baselogs %>%
#     dplyr::left_join(products2)
#   conif_baselogs <- p_baselogs %>%
#
#     dplyr::filter(str_detect(str_to_lower(SpeciesGroupName), "gran|furu") & (str_detect(str_to_lower(ProductGroupName), "t?mmer"))) %>%
#     dplyr::filter(!str_detect(str_to_lower(ProductName), "skigard|emba")) %>%
#     dplyr::group_by(MachineKey, CreationDate, SpeciesGroupName) %>%
#     dplyr::summarize( baselog_price = mean(baselog_price), n_prices = dplyr::n()) %>%
#     #dplyr::rename( baselogSpeciesGroupName = SpeciesGroupName)   %>%
#     ungroup()
#
#   nonconif_baselogs <- p_baselogs %>% dplyr::filter(!str_detect(str_to_lower(SpeciesGroupName), "gran|furu")) %>%
#     dplyr::group_by(MachineKey, CreationDate, SpeciesGroupName) %>%
#     dplyr::summarize(baselog_price = max(baselog_price), n_prices = dplyr::n()) %>%
#     #dplyr::rename(baselogSpeciesGroupName = SpeciesGroupName) %>%
#     ungroup()
#
#   p_baselogs <- dplyr::bind_rows(conif_baselogs, nonconif_baselogs)
#
#
#   relpricematrixs <- tibble::as_tibble(pricematrixes) %>%
#     dplyr::left_join(products2 )    %>%
#     dplyr::left_join(p_baselogs)   %>%
#     dplyr::rowwise()%>%
#     dplyr::mutate(relprice = round(price * 100 / baselog_price , 0))
#
#   return(relpricematrixs)
# }
#



#' Get the operator definitions from a StanFord2010 xml document
#'
#' @param doc the xml document
#'
#' @return
#' @export
#'
#' @examples
#' hprfiles <-  list.files(path =  system.file(package = "sf2010r"),  pattern = ".hpr", recursive = TRUE, full.names= TRUE)
#' doc <- xml2::read_xml(hprfiles[1])
#' getOperators(doc)
getOperators <- function(doc){
  nodes <- xml2::xml_find_all(doc, "//d1:OperatorDefinition")
  if(length(nodes)){
    Operator <- purrr::map_df(.x = nodes, ~ xml_childs_nchr(.x))
    contactinfo <-
      purrr::map_df(.x = xml2::xml_find_all(nodes, ".//d1:ContactInformation"),
                        ~xml_childs_nchr(.x))
    operators <-  dplyr::bind_cols(Operator, contactinfo)

    if(length(names(contactinfo) %in% c("FirstName","LastName" )) == 2) {
       firstlast = c(contactinfo$FirstName, contactinfo$LastName)
        operators$Name <- paste0(firstlast, collapse=" ")

    }
    operators$MachineKey = xml2::xml_text(xml2::xml_find_first(doc, ".//d1:Machine/d1:MachineKey"))

 } else {operators <- NULL}
 return(operators)
}



#' Extracting all data defining one cut object from one ObjectDefinition xml-tree
#'
#' @param x the object definition XML-tree
#'
#' @return a tibble, number of rows = number of sub objects
#' @examples
#' hprfiles <-  list.files(path =  system.file(package = "sf2010r"),  pattern = ".hpr", recursive = TRUE, full.names= TRUE)
#' doc <- xml2::read_xml(hprfiles[1])
#' Objects_nodes <- xml2::xml_find_all(doc, "//d1:ObjectDefinition")
#' getObjectDefinition(Objects_nodes[1])
#' plyr::ldply(Objects_nodes, getObjectDefinition)
#' @export
getObjectDefinition <- function(x){
  # x =Objects_nodes[[1]]

  ObjectKey <- xml2::xml_integer(xml2::xml_find_first(x, "./d1:ObjectKey"))

  ObjectUserID <- xml2::xml_text(xml2::xml_find_first(x, "./d1:ObjectUserID")) # GUID, equals Logging unit (Var21.1) in old stanford.
  ObjectName <- xml2::xml_text(xml2::xml_find_first(x, "./d1:ObjectName"))

  #Tricky part; Logging form. Keep an eye on this approach
  LoggingFormCode <- 10
  LoggingFormDesc <- "unknown"
  ObjLoggingFormCode <-  xml2::xml_integer(xml2::xml_find_first(x, "./d1:LoggingForm/d1:LoggingFormCode"))
  ObjLoggingFormDesc <- xml2::xml_text(xml2::xml_find_first(x, "./d1:LoggingForm/d1:LoggingFormDescription"))


  ContractNumber <-  xml2::xml_text(xml2::xml_find_first(x, "./d1:ContractNumber"))
  RealEstateIDObject <-  xml2::xml_text(xml2::xml_find_first(x, "./d1RealEstateIDObject"))
  StartDate <-  xml2::xml_text(xml2::xml_find_first(x, "./d1:StartDate"))

  # Sub object data
  SubObjectKey <- xml2::xml_integer(xml2::xml_find_all(x, "./d1:SubObject/d1:SubObjectKey"))
  LL <- length(SubObjectKey)

  if(LL){
    LoggingFormCode <- rep(10, LL)
    LoggingFormDesc <- rep("unknown", LL)
  }

  if(!is.null(ObjLoggingFormCode)) {LoggingFormCode[1:LL] <- ObjLoggingFormCode}
  if(!is.null(ObjLoggingFormDesc)) {LoggingFormDesc[1:LL] <- ObjLoggingFormDesc}

  if(LL){
    SubObjectName <- xml2::xml_text(xml2::xml_find_all(x, "./d1:SubObject/d1:SubObjectName"))


    SubObjLoggingFormDesc <- rep(NA, LL)
    SubObjLoggingFormCode <- rep(NA, LL)
    SubObjectUserID <- rep(NA, LL)
    RealEstateIDSubObject <- xml2::xml_text(xml2::xml_find_all(x, "./d1:SubObject/d1:RealEstateIDSubObject"))

    subobjnodesets <- xml2::xml_find_all(x,  "./d1:SubObject")
    for (i in 1:length(subobjnodesets)){
      parsedset <- subobjnodesets[i]
      SubObjectUserID[i] <- xml2::xml_text(xml2::xml_find_all(parsedset, "./d1:SubObjectUserID"))

      lfc <- xml2::xml_text(xml2::xml_find_first(parsedset, ".//d1:LoggingFormCode"))
      lfd <- xml2::xml_text(xml2::xml_find_first(parsedset, ".//d1:LoggingFormDescription"))

      if(length(lfc)) {SubObjLoggingFormCode[i] <- lfc}
      if(length(lfd)) {SubObjLoggingFormDesc[i] <- lfd}
    }


  } else { #if there is no subobjectkey
    SubObjectUserID <-  "1"
    SubObjectKey <- 1
    SubObjectName <- "1"
    RealEstateIDSubObject<-  "1"

    SubObjLoggingFormDesc <- NA
    SubObjLoggingFormCode <- NA

  }

  LoggingFormCode[!is.na(SubObjLoggingFormCode)] = SubObjLoggingFormCode[!is.na(SubObjLoggingFormCode)]
  LoggingFormDesc[!is.na(SubObjLoggingFormDesc)] = SubObjLoggingFormDesc[!is.na(SubObjLoggingFormDesc)]

  ObjectUserID <- rep(ObjectUserID,  length(SubObjectKey))


  ObjectDefinition <- tibble::tibble(ObjectUserID, ObjectName, ObjectKey,
                                 SubObjectUserID, SubObjectName, SubObjectKey,
                                 LoggingFormCode,  LoggingFormDesc,
                                 RealEstateIDObject, RealEstateIDSubObject, ContractNumber, StartDate)


   return(ObjectDefinition)
}


#' Extracting all cut object definitions from doc
#' @param doc should be an Stanford2010 xml document
#'
#'
#' @return a tibble, number of rows = number of objects and sub objects in doc
#'
#' @examples
#' hprfiles <-  list.files(path =  system.file(package = "sf2010r"),  pattern = ".hpr", recursive = TRUE, full.names= TRUE)
#' doc <- xml2::read_xml(hprfiles[1])
#' getObjects(doc)
#' @export
getObjects <- function(doc){
  MachineKey = xml2::xml_text(xml2::xml_find_first(doc, "//d1:Machine/d1:MachineKey")) # alphanumeric key of length 18-60. GUI, following bucking computer.
  Objects_Nodesets <- xml2::xml_find_all(doc, "//d1:ObjectDefinition")
  #bmatrix <- plyr::ldply(Objects_Nodesets, getObjectDefinition)
  bmatrix <- plyr::ldply(Objects_Nodesets, sf2010r::getObjectDefinition)
  bmatrix$MachineKey = MachineKey
  return(bmatrix)

}



















