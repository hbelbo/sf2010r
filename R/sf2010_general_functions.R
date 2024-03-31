




#' Type converter for sf2010 data frames
#' @description This function applies readr::type_convert()
#' on character columns of a data frame,
#' except for column named MachineKey which is forced to keep the original type,
#' other *Key variables which are being converted to integer if they are numeric
#' and except non-character columns
#' @param df data.frame or tbl_df
#'
#' @return  tbl_df, with same columns as input data frame.
#' @export
#'
#' @examples
#' df <- data.frame(MachineKey = rep("11", 3), ObjectKey = "1",
#'   StemKey = 1:3, dbh = as.character(seq(20, 24, length = 3)))
#' str(type_convert_sf2010(df))
type_convert_sf2010 <- function(df){
  #dfch_tc <- df[, sapply(df, class) == 'character']
  dfch_tc <- df %>% dplyr::select(tidyselect::where(is.character))
  dfrest <- df %>% dplyr::select(!tidyselect::where(is.character))
  df_return <- data.frame(tmp1 = 1:nrow(df))
  if(ncol(dfrest)>0) {
    df_return <- dplyr::bind_cols(df_return, dfrest)
  }

  if("MachineKey" %in% names(dfch_tc)){
    df_return <- dplyr::bind_cols(df_return, dfch_tc %>% dplyr::select(.data$MachineKey))
    dfch_tc <- dfch_tc %>% dplyr::select(-.data$MachineKey)
  }

  if(ncol(dfch_tc)>0){
    if(any(stringr::str_detect(names(dfch_tc), "Key$"))){

      dfch_tc_keys <- readr::type_convert(dfch_tc %>% dplyr::select(dplyr::ends_with("Key")), guess_integer = TRUE)
      df_return <- dplyr::bind_cols(df_return, dfch_tc_keys )
    }
    if(any(!stringr::str_detect(names(dfch_tc), "Key$"))){
      dfch_tc_rest <- readr::type_convert(dfch_tc %>% dplyr::select(-dplyr::ends_with("Key")))
      df_return <- dplyr::bind_cols(df_return, dfch_tc_rest )
    }
  }
  df_return <- df_return %>% dplyr::select(-.data$tmp1)
  return(df_return)
}



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
#' hprfiles <-  list.files(path =  system.file(package = "sf2010r"),
#'    pattern = ".hpr", recursive = TRUE, full.names= TRUE)
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
#' hprfiles <-  list.files(path =  system.file(package = "sf2010r"),
#'    pattern = ".hpr", recursive = TRUE, full.names= TRUE)
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
      data.table::data.table(child_name =  child_names,
                             attrs = child_attrs,
                             child_vals = xml2::xml_text(childrens))
  } else {
    child_dt <-
      data.table::data.table(child_name =  child_names,
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
#' hprfiles <-  list.files(path =  system.file(package = "sf2010r"),
#'    pattern = ".hpr", recursive = TRUE, full.names= TRUE)
#' doc <- xml2::read_xml(hprfiles[1])
#' getMachineReportHeader(doc) %>% str()
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
  if(all(dim(contractordata)!=0)){
    contractordata <- dplyr::rename_with(contractordata, ~ paste0("Contractor_", .x))
    } else { contractordata <- NULL}

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
#' @return a tibble.
#' @export
#'
#' @examples
#' hprfiles <-  list.files(path =  system.file(package = "sf2010r"),
#'    pattern = ".hpr", recursive = TRUE, full.names= TRUE)
#' doc <- xml2::read_xml(hprfiles[1])
#' SpeciesList <- xml2::xml_find_all(doc, ".//d1:SpeciesGroupDefinition" )
#' getSpeciesGroupDef(SpeciesList[[1]]) %>% str()
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
#' @return a tibble. If not species defs: NULL
#' @export
#'
#' @examples
#' hprfiles <-  list.files(path =  system.file(package = "sf2010r"),
#'    pattern = ".hpr", recursive = TRUE, full.names= TRUE)
#' doc <- xml2::read_xml(hprfiles[1])
#' getSpeciesGroupDefinitions(doc) %>% str()
getSpeciesGroupDefinitions <- function(doc){
  SpeciesList <- xml2::xml_find_all(doc, ".//d1:SpeciesGroupDefinition" )
  if(length(SpeciesList)){
    bmatrix <- plyr::ldply(SpeciesList, sf2010r::getSpeciesGroupDef)
    #bmatrix <- plyr::ldply(SpeciesList, getSpeciesGroupDef)
    MachineKey <-  xml2::xml_text( xml2::xml_find_first(doc, ".//d1:Machine/d1:MachineKey"))
    bmatrix$MachineKey = MachineKey
    not_all_na <- function(x) {!all(is.na(x))}
    bmatrix <- bmatrix %>% dplyr::select_if(not_all_na)
    return(bmatrix)
  } else {
    return(NULL)}
}






#' get stem type definitions from one SpeciesGroupDefinition
#' @param x is a node tree of SpeciesGrouDefinition
#'
#' @export
#'
#' @examples
#' hprfiles <-  list.files(path =  system.file(package = "sf2010r"),
#'    pattern = ".hpr", recursive = TRUE, full.names= TRUE)
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
  } else {stemtypes = tibble::tibble()}
  return(stemtypes)
}



#' Get all stem type definitions for all SpeciesGroupDefinitions
#'
#' @param doc a StanFord2010 xml document
#' @return a list
#' @export
#'
#' @examples
#' hprfiles <-  list.files(path =  system.file(package = "sf2010r"),
#'    pattern = ".hpr", recursive = TRUE, full.names= TRUE)
#' doc <- xml2::read_xml(hprfiles[1])
#' getStemTypes(doc)
getStemTypes <- function(doc){
  SpeciesList <- xml2::xml_find_all(doc, ".//d1:SpeciesGroupDefinition" )
  if(length(SpeciesList)) {
    bmatrix <- plyr::ldply(SpeciesList, sf2010r::getStemTypeDefs)
    MachineKey <-  xml2::xml_text( xml2::xml_find_first(doc, ".//d1:Machine/d1:MachineKey"))
    bmatrix$MachineKey = MachineKey
    return(bmatrix)
  } else {
    return(NULL)
    }
}






#' get Product definitions
#'
#' @param doc  a StanFord2010 xml document
#'
#' @return a data.frame
#' @export
#'
#' @examples
#' sffiles <-  list.files(path =  system.file(package = "sf2010r"),
#'    pattern = ".hpr|.fpr", recursive = TRUE, full.names= TRUE)
#' docs <- lapply(X = sffiles, FUN = function(X){xml2::read_xml(X)})
#' getProductDefs(docs[[1]]) %>% str()
#' getProductDefs(docs[[2]]) %>% str()
#' getProductDefs(docs[[3]]) %>% str()
#' getProductDefs(docs[[4]]) %>% str()
#' getProductDefs(docs[[5]]) %>% str()
getProductDefs <- function(doc){
  # doc = docs[[3]]
  # doc = docs[[1]]

  xpt1 <- ".//d1:ProductDefinition"
  p1 <-  xml2::xml_find_first(doc, xpt1)
  if(!is.na(p1)){


    p_all <-  xml2::xml_find_all(doc, xpt1)
    p_all_ch_names <- xml2::xml_name(xml2::xml_children(p_all))
    p_unique_ch_names <- (unique(p_all_ch_names))

    xpt2 <- paste0( xpt1, "/d1:", p_unique_ch_names)
    p_all_ch_lengths <- lapply(xpt2, FUN = function(X){ xml2::xml_length(xml2::xml_find_first(doc, X))} )
    xpt2_multi <- xpt2[p_all_ch_lengths != 0]

    xpt3 <- unlist(lapply(xpt2_multi, FUN = function(X){ paste0(X, "/d1:", xml2::xml_name(xml2::xml_children(xml2::xml_find_first(doc, X))))}))

    ProductDefs <- data.frame()
    if(any(stringr::str_detect(xpt3, "ClassifiedProductDefinition"))) {
      xp3_classified <- xpt3[stringr::str_detect(xpt3, "ClassifiedProductDefinition")]

      xp3_classified_lengths <- unlist(lapply(xp3_classified, FUN  = function(X){ xml2::xml_length(xml2::xml_find_first(doc, X))} ))
      xp3_classified <- xp3_classified[xp3_classified_lengths == 0]
      vnames <- sapply(stringr::str_split(xp3_classified, "d1:"), FUN = function(X){X[4]})

      classified_product_defs2 <- lapply(xp3_classified, FUN = function(X){
        data.frame(v1 = xml2::xml_text(xml2::xml_find_all(doc, X)), ProductKey =
                     xml2::xml_integer(xml2::xml_find_first( xml2::xml_parent(xml2::xml_parent(xml2::xml_find_all(doc, X))),  xpath = "./d1:ProductKey")))
      })

      classified_product_defs2 <- lapply(seq_along(classified_product_defs2), function(i) {
        names(classified_product_defs2[[i]])[1] <- vnames[i]
        classified_product_defs2[[i]]
      })

      classified_product_defs <- Reduce(function(x,y) merge(x,y, by = "ProductKey", all = TRUE),classified_product_defs2 )


      LengthDefs <- getProductLengthDefs(doc)
      if(!is.null(LengthDefs)){
        classified_product_defs <- merge(classified_product_defs, LengthDefs, by = "ProductKey", all = TRUE)
      }

      DiaDefs <- getProductDiaDefs(doc)
      if(!is.null(DiaDefs)){
        classified_product_defs <- merge(classified_product_defs, DiaDefs, by = "ProductKey", all = TRUE)
      }
      # Then drop empty variables
      w <- apply(classified_product_defs, 2,  FUN = function(x) {ifelse(!all(is.na(x)), max(nchar(x[!is.na(x)])), 0)  })
      classified_product_defs <- classified_product_defs[,w!=0]

      ProductDefs <- dplyr::bind_rows(ProductDefs, classified_product_defs)
    }
    if(any(stringr::str_detect(xpt3, "UnclassifiedProductDefinition"))) {
      xp3_unclassified <- xpt3[stringr::str_detect(xpt3, "UnclassifiedProductDefinition")]

      xp3_unclassified_lengths <- unlist(lapply(xp3_unclassified, FUN  = function(X){ xml2::xml_length(xml2::xml_find_first(doc, X))} ))
      xp3_unclassified <- xp3_unclassified[xp3_unclassified_lengths == 0]
      vnames <- sapply(stringr::str_split(xp3_unclassified, "d1:"), FUN = function(X){X[4]})

      unclassified_product_defs2 <- lapply(xp3_unclassified, FUN = function(X){
        data.frame(v1 = xml2::xml_text(xml2::xml_find_all(doc, X)), ProductKey =
                     xml2::xml_integer(xml2::xml_find_first( xml2::xml_parent(xml2::xml_parent(xml2::xml_find_all(doc, X))),  xpath = "./d1:ProductKey")))
      })
      unclassified_product_defs2 <- lapply(seq_along(unclassified_product_defs2), function(i) {
        names(unclassified_product_defs2[[i]])[1] <- vnames[i]
        unclassified_product_defs2[[i]]
      })
      unclassified_product_defs <- Reduce(function(x,y) merge(x,y, by = "ProductKey", all = TRUE),unclassified_product_defs2 )

      ProductDefs <- dplyr::bind_rows(ProductDefs, unclassified_product_defs)
    }
    return(ProductDefs)
  } else {
    cat("\n No product definitions" )
  }




}


#' get Product diameter def limits
#'
#' @param doc  a StanFord2010 xml document
#'
#' @return a data.frame having diameter limits for each product
#' @export
#'
#' @examples
#' sffiles <-  list.files(path =  system.file(package = "sf2010r"),
#'    pattern = ".hpr|.fpr", recursive = TRUE, full.names= TRUE)
#' docs <- lapply(X = sffiles, FUN = function(X){xml2::read_xml(X)})
#' getProductDiaDefs(docs[[3]]) %>% str()
#' getProductDiaDefs(docs[[4]]) %>% str()
#' getProductDiaDefs(docs[[5]]) %>% str()
#'
getProductDiaDefs <- function(doc){
  # doc = docs[[3]]

  xpt1 <- ".//d1:ProductDefinition/d1:ClassifiedProductDefinition/d1:DiameterDefinition"
  p1 <-  xml2::xml_find_first(doc, xpt1)
  if( !is.na(p1) ){

    p_all <-  xml2::xml_find_all(doc, xpt1)
    p_all_ch_names <- xml2::xml_name(xml2::xml_children(p_all))
    p_unique_ch_names <- (unique(p_all_ch_names))

    xpt2 <- paste0( xpt1, "/d1:", p_unique_ch_names)
    p_all_ch_lengths <- lapply(xpt2, FUN = function(X){ xml2::xml_length(xml2::xml_find_first(doc, X))} )
    xpt2_multi <- xpt2[p_all_ch_lengths != 0]
    xpt2_single <- xpt2[p_all_ch_lengths == 0]
    vnames <- sapply(stringr::str_split(xpt2_single, "d1:"), FUN = function(X){X[5]})

    df <- lapply(xpt2_single, FUN = function(X){
      # X <- xpt2_single[1]
      data.frame(v1 = xml2::xml_text(xml2::xml_find_all(doc, X)), ProductKey =
                   xml2::xml_integer(xml2::xml_find_first( xml2::xml_parent(xml2::xml_parent(xml2::xml_parent(xml2::xml_find_all(doc, X)))),  xpath = "./d1:ProductKey")))
    })

    df <- lapply(seq_along(df), function(i) {
      names(df[[i]])[1] <- vnames[i]
      df[[i]]
    })

    df <- Reduce(function(x,y) merge(x,y, by = "ProductKey", all = TRUE), df)

    # Then a special trick because if Minimum and maximum diameters are not presented directly

    classified <- xml2::xml_parent(xml2::xml_parent(xml2::xml_find_all(doc, xpt1)))
    # From each: find first LengthClassLowerLimit
    DiameterLimits <- data.frame(
      DiaClassMin = unlist( lapply(classified, FUN = function(X){ xml2::xml_text(xml2::xml_find_first(X, xpath = ".//d1:DiameterClass/d1:DiameterClassLowerLimit"))})),
      DiaClassMax = unlist( lapply(classified, FUN = function(X){ xml2::xml_text(xml2::xml_find_first(X, xpath = ".//d1:DiameterClasses/d1:DiameterClassMAX"))})),
      ProductKey = unlist(lapply(classified, FUN = function(X){ xml2::xml_integer(xml2::xml_find_first(X,   xpath = "./d1:ProductKey"))}))
    )

    df <- merge(df, DiameterLimits, by = "ProductKey", all = TRUE)

  }  else { df <- NULL}
  return(df)
}


#' get Product length def limits
#'
#' @param doc  a StanFord2010 xml document
#'
#' @return a data.frame having diameter limits for each product
#' @export
#'
#' @examples
#' sffiles <-  list.files(path =  system.file(package = "sf2010r"),
#'    pattern = ".hpr|.fpr", recursive = TRUE, full.names= TRUE)
#' docs <- lapply(X = sffiles, FUN = function(X){xml2::read_xml(X)})
#' getProductLengthDefs(docs[[3]]) %>% str()
#' getProductLengthDefs(docs[[4]]) %>% str()
#' getProductLengthDefs(docs[[5]]) %>% str()
#'
getProductLengthDefs <- function(doc){
  # doc = docs[[3]]

  xpt1 <- ".//d1:ProductDefinition/d1:ClassifiedProductDefinition/d1:LengthDefinition"
  p1 <-  xml2::xml_find_first(doc, xpt1)
  if( !is.na(p1) ){

    p_all <-  xml2::xml_find_all(doc, xpt1)
    p_all_ch_names <- xml2::xml_name(xml2::xml_children(p_all))
    p_unique_ch_names <- (unique(p_all_ch_names))

    xpt2 <- paste0( xpt1, "/d1:", p_unique_ch_names)
    p_all_ch_lengths <- lapply(xpt2, FUN = function(X){ xml2::xml_length(xml2::xml_find_first(doc, X))} )
    xpt2_multi <- xpt2[p_all_ch_lengths != 0]
    xpt2_single <- xpt2[p_all_ch_lengths == 0]
    vnames <- sapply(stringr::str_split(xpt2_single, "d1:"), FUN = function(X){X[5]})

    df <- lapply(xpt2_single, FUN = function(X){
      # X <- xpt2_single[1]
      data.frame(v1 = xml2::xml_text(xml2::xml_find_all(doc, X)),
                 ProductKey =
                   xml2::xml_integer(xml2::xml_find_first( xml2::xml_parent(xml2::xml_parent(xml2::xml_parent(xml2::xml_find_all(doc, X)))),  xpath = "./d1:ProductKey")))
    })

    df <- lapply(seq_along(df), function(i) {
      names(df[[i]])[1] <- vnames[i]
      df[[i]]
    })

    df <- Reduce(function(x,y) merge(x,y, by = "ProductKey", all = TRUE), df)


    # Then a special trick because minimum lengths may be of interest
    # Find all classified product defs

    classified <- xml2::xml_parent(xml2::xml_parent(xml2::xml_find_all(doc, xpt1)))
    # From each: find first LengthClassLowerLimit
    LengthClassLowerLimit <- data.frame(
      LengthClassMIN = unlist( lapply(classified, FUN = function(X){ xml2::xml_text(xml2::xml_find_first(X, xpath = ".//d1:LengthClass/d1:LengthClassLowerLimit"))})),
    ProductKey = unlist(lapply(classified, FUN = function(X){ xml2::xml_integer(xml2::xml_find_first(X,   xpath = "./d1:ProductKey"))}))
    )
    df <- merge(df, LengthClassLowerLimit, by = "ProductKey", all = TRUE)
  }  else { df <- NULL}
  return(df)
}


#' Get the Product Matrix items
#'
#' @param x is a Product definition tree from StanFord2010 xml document
#'
#' @return A tibble, or NULL if no products are defined
#' @export
#'
#' @examples
#' hprfiles <-  list.files(path =  system.file(package = "sf2010r"),
#'    pattern = ".hpr", recursive = TRUE, full.names= TRUE)
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
#' hprfiles <-  list.files(path =  system.file(package = "sf2010r"),
#'    pattern = ".hpr", recursive = TRUE, full.names= TRUE)
#' doc <- xml2::read_xml(hprfiles[1])
#' pms <- getProductMatrixes(doc)
getProductMatrixes <- function(doc){
  Productslist <- xml2::xml_find_all(doc,  ".//d1:ProductDefinition")
  if(length(Productslist)){
    bmatrix <- plyr::ldply(Productslist, sf2010r::getProductMatrixItems)
    if(nrow(bmatrix)>0){
      #bmatrix <- plyr::ldply(Productslist, getProductMatrixItems)
      MachineKey <- xml2::xml_text(xml2::xml_find_first(doc, ".//d1:Machine/d1:MachineKey"))
      bmatrix$MachineKey = MachineKey
      return(bmatrix)
    } else { return(NULL)}
  } else { return(NULL)}
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
#' hprfiles <-  list.files(path =  system.file(package = "sf2010r"),
#'    pattern = ".hpr", recursive = TRUE, full.names= TRUE)
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



#' Operator def from operator definition nodetree
#' @param x is a node tree of operator definition
#'
#' @export
#'
#' @examples
#' sffiles <-  list.files(path =  system.file(package = "sf2010r"),
#'    pattern = ".hpr|.fpr", recursive = TRUE, full.names= TRUE)
#' docs <- lapply(X = sffiles, FUN = function(X){xml2::read_xml(X)})
#' OperatorList <- xml2::xml_find_all(docs[[1]], ".//d1:OperatorDefinition" )
#' getOperatorDef(OperatorList[[1]]) %>% str()
#' plyr::ldply(OperatorList, getOperatorDef )
getOperatorDef <- function(x) {
  # x <- OperatorList[[2]]
  OperatorKey <- xml2::xml_integer( xml2::xml_find_first(x,  ".//d1:OperatorKey"))
  od1 <-  data.frame(as.list(xml_childs_nchr(x)))
  contactinfo <- xml2::xml_find_all(x, ".//d1:ContactInformation")
  if(length(contactinfo)){
    contactinfo <- data.frame(as.list(xml_childs_nchr(contactinfo)))
    od1 <- cbind(od1, contactinfo)
  }
  return(od1)
}



#' Get the operator definitions from a StanFord2010 xml document
#'
#' @param doc the xml document
#' @export
#'
#' @return a tibble, one row for each operator
#'
#' @examples
#' hprfiles <-  list.files(path =  system.file(package = "sf2010r"),
#'    pattern = ".hpr", recursive = TRUE, full.names= TRUE)
#' doc <- xml2::read_xml(hprfiles[1])
#' getOperators(doc) %>% str()
getOperators <- function(doc){
 OperatorList <- xml2::xml_find_all(doc, "//d1:OperatorDefinition")
  if(length(OperatorList)){
    bmatrix <- plyr::ldply(OperatorList, getOperatorDef)
    MachineKey <- xml2::xml_text(xml2::xml_find_first(doc, ".//d1:Machine/d1:MachineKey"))
    bmatrix$MachineKey = MachineKey
    #bmatrix <- sf2010_type.convert(bmatrix)
    return(bmatrix)
    } else {
      return(NULL)
    }
}





#' Extracting all data defining one cut object from one ObjectDefinition xml-tree
#'
#' @param x the object definition XML-tree
#'
#' @return a tibble, number of rows = number of sub objects
#' @examples
#' sffiles <-  list.files(path =  system.file(package = "sf2010r"),
#'    pattern = ".hpr|.fpr", recursive = TRUE, full.names= TRUE)
#' print(sffiles)
#' docs <- lapply(X = sffiles, FUN = function(X){xml2::read_xml(X)})
#' Objects_nodes <- xml2::xml_find_all(docs[[3]], "//d1:ObjectDefinition")
#' getObjectDefinition(Objects_nodes[1]) %>% str()
#' plyr::ldply(Objects_nodes, getObjectDefinition)
#' Objects_nodes <- xml2::xml_find_all(docs[[5]], "//d1:ObjectDefinition")
#' getObjectDefinition(Objects_nodes[1]) %>% str()
#' plyr::ldply(Objects_nodes, getObjectDefinition)
#' @export
getObjectDefinition <- function(x){
  # x =Objects_nodes[[1]]
  # x = Objects_Nodesets[[1]]
  #    str(xml2::xml_integer(xml2::xml_find_first(x, "./d1:doesentexist")))
  #    str(xml2::xml_text(xml2::xml_find_first(x, "./d1:doesentexist")))

  object_def <- tibble::as_tibble(t(xml_childs_nchr(x) ), .name_repair = "universal") %>%
    dplyr::mutate(ObjectKey = as.integer(.data$ObjectKey))

  sub_object_nodes  <- xml2::xml_find_all(x, "//d1:SubObject")
  if(length(sub_object_nodes)>0){
    # replicate the object definition row n = number of sub object defs
    object_def <- do.call(rbind, replicate(length(sub_object_nodes), object_def, simplify = FALSE))

    subobj_defs <- lapply(X = sub_object_nodes, FUN = function(X){
      tibble::as_tibble(t(xml_childs_nchr(X)), .name_repair = "universal") %>%
        dplyr::mutate(SubObjectKey = as.integer(.data$SubObjectKey))})

    subobj_defs <- do.call(dplyr::bind_rows, lapply(subobj_defs, tibble::as_tibble))
    object_def <- dplyr::bind_cols(object_def, subobj_defs)

  }

   return(object_def)
}


#' Extracting all cut object definitions from doc
#' @param doc should be an Stanford2010 xml document
#'
#'
#' @return a tibble, number of rows = number of objects and sub objects in doc
#'
#' @examples
#' sffiles <-  list.files(path =  system.file(package = "sf2010r"),
#'    pattern = ".hpr|.fpr", recursive = TRUE, full.names= TRUE)
#' print(sffiles)
#' docs <- lapply(X = sffiles, FUN = function(X){xml2::read_xml(X)})
#' getObjects(docs[[1]]) %>% str()
#' lapply(docs[1:2], FUN = function(X){getObjects(X) %>% str()})
#' @export
getObjects <- function(doc){
  MachineKey = xml2::xml_text(xml2::xml_find_first(doc, "//d1:Machine/d1:MachineKey")) # alphanumeric key of length 18-60. GUI, following bucking computer.
  Objects_Nodesets <- xml2::xml_find_all(doc, "//d1:ObjectDefinition")
  #bmatrix <- plyr::ldply(Objects_Nodesets, getObjectDefinition)
  bmatrix <- plyr::ldply(Objects_Nodesets, sf2010r::getObjectDefinition)
  bmatrix$MachineKey = MachineKey
 # bmatrix <- sf2010_type.convert(bmatrix)
  return(bmatrix)

}

















