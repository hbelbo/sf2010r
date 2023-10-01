

#' Fetch all stems and all logs in hpr and hqc
#'
#' @param doc a StanFord2010 .hpr document
#'
#' @return a list with five data frames.
#' @description the function returns a list with five data frames;
#' stems is all the stems
#' stplogs is all single tree processed logs
#' mtplogs is all multi tree processed logs
#' stemgrades is the grades of all stems
#' stemdias is the diameter vector for the stem.
#' @export
#'
#' @examples
#' hprfiles <- list.files(path =  system.file(package = "sf2010r"),
#' pattern = ".hpr", recursive = TRUE, full.names= TRUE)
#' doc <- xml2::read_xml(hprfiles[1])
#' stl <- getStemsAndLogs2(doc)
#' doc <- xml2::read_xml(hprfiles[2])
#' stl <- getStemsAndLogs(doc)
#' doc <- xml2::read_xml(hprfiles[3])
#' stl <- getStemsAndLogs(doc)
#' doc <- xml2::read_xml(hprfiles[4]) # multitree processing stems
#' stl <- getStemsAndLogs(doc)
#' hqcfiles <- list.files(path =  system.file(package = "sf2010r"),
#' pattern = ".hqc", recursive = TRUE, full.names= TRUE)
#' doc <- xml2::read_xml(hqcfiles[1])
#' stl <- getStemsAndLogs(doc)
#' @export
getStemsAndLogs2 <- function(doc){


  Stemkeys <- xml2::xml_integer(xml2::xml_find_all(doc,  ".//d1:Stem/d1:StemKey"))


  ##### Coordinates dataset -----------------
  # Detect all StemmCoordinate locations  (Base machine, crane tip etc)
  # First stem where stem coordinates exists. Assuming all other have similar coordinate data and settings as the first.
  coord_stem1 <-  xml2::xml_parent(xml2::xml_find_first(doc, ".//d1:Stem/d1:StemCoordinates"))
  stm_coords_1 <- xml2::xml_find_all(coord_stem1, "./d1:StemCoordinates")
  coordinate_positions <- xml2::xml_attr(stm_coords_1, attr = "receiverPosition") %>% unique() #, ns = "d")
  by_stem_coord_length_1 <- xml2::xml_length(stm_coords_1)[1]
  coord_names <- xml2::xml_name(xml2::xml_children(stm_coords_1)) %>% unique()

  # Make xpaths to extract StemKey and gps positions
  # xpath for coordinates
  xpts_cpositions <- unname(unlist(Map(function(x) paste0(".//d1:StemCoordinates[@receiverPosition='", x, "']"), coordinate_positions))  )
  xpts_cpositions_xyz <- character()
  for(i in xpts_cpositions){
    for(y in coord_names){
      xpts_cpositions_xyz <- c(xpts_cpositions_xyz, paste0(i, "/d1:", y))
    }
  }


  # Create varnames for coordinates
  varnameending <-
    stringr::str_remove(xpts_cpositions_xyz, "\\.//d1:StemCoordinates\\[@receiverPosition='") %>%
    stringr::str_remove(., pattern = "'\\]\\/d1:\\w+") %>%
    stringr::str_remove_all(., pattern = "when |the|tree") %>%
    stringr::str_replace_all(., pattern = "position felling ", replacement = "felling") %>%
    gsub('\\b(\\pL)\\pL{2,}|.','\\L\\1',. ,perl = TRUE) %>% tolower()

  coord_names
  varnamesdf <- data.frame(cn = rep(tolower(coord_names), length(unique(varnameending))), vne = varnameending)
  varnames <- paste0(varnamesdf[,1], "_", varnamesdf[,2])
  # Fetch all values
  valuelist <- Map(function(x)  xml2::xml_double(xml2::xml_find_all(doc,x)), xpts_cpositions_xyz )
  coords_df <- list2DF(valuelist)
  names(coords_df) <- varnames
  coords_df$StemKey <- xml2::xml_integer(xml2::xml_find_all(xml2::xml_parent(xml2::xml_find_all(doc, ".//d1:Stem/d1:StemCoordinates")), "./d1:StemKey"))




  ##### getting general stem data from Stem childrens
  nodecase  <- xml2::xml_find_first(doc,  ".//d1:Stem")
  node_childrens <-  xml2::xml_children(nodecase)

  ws0 <- which(xml2::xml_length(node_childrens)==0)
  childrens_1 <- node_childrens[ws0]
  childrens_1_names <- xml2::xml_name(childrens_1)

  to_map <- paste(".//d1:Stem/d1:", childrens_1_names, sep = "")

  stmdt1 <- Map(function(x) xml2::xml_text(xml2::xml_find_all(doc, x)), to_map)
  names(stmdt1) <- stringr::str_remove(string = names(stmdt1), pattern = ".//d1:Stem/d1:")
  stmdt1 <- list2DF(stmdt1) %>% utils::type.convert(as.is = TRUE)

  # To have a look on the names of the stem childrens
  wsm <-  which(xml2::xml_length(node_childrens)!=0)
  childrens_2 <- node_childrens[wsm]
  # xml2::xml_name(childrens_2)

  stems <- dplyr::left_join(stmdt1, coords_df)

  ###### Geetting Extensions if present
  xpt1 <- ".//d1:Stem/d1:Extension"
  nodecase  <- xml2::xml_find_first(doc,  xpt1)
  if(!is.na(nodecase)){
    nodename <- xml2::xml_name(nodecase)
    node_childrens <-  xml2::xml_children(nodecase)
    ws0 <- which(xml2::xml_length(node_childrens)==0)
    childrens_1 <- node_childrens[ws0]
    childrens_1_names <- xml2::xml_name(childrens_1)

    to_map <- paste(".//d1:Stem/d1:", nodename, "/d1:",childrens_1_names, sep = "")

    dt1 <- Map(function(x) xml2::xml_text(xml2::xml_find_all(doc, x)), to_map)
    names(dt1) <- childrens_1_names
    nobs <- sapply(dt1, length)
    w <- which(nobs == max(nobs))
    dt1 <- dt1[w]
    dt1 <- list2DF(dt1) %>% utils::type.convert(as.is = TRUE)

    dt1$StemKey <- xml2::xml_integer(xml2::xml_find_all(xml2::xml_parent(xml2::xml_find_all(doc, xpt1)), "./d1:StemKey"))

    extensions <- dt1
    stems <- dplyr::left_join(stems, extensions)
  }

dplyr::glimpse(stems)


  ##### Single Tree Processed stems #####
  xpt1 <- ".//d1:Stem/d1:SingleTreeProcessedStem"
  nodecase  <- xml2::xml_find_first(doc,  xpt1)
  if(!is.na(nodecase)){
    nodename <- xml2::xml_name(nodecase)
    node_childrens <-  xml2::xml_children(nodecase)
    xml2::xml_length(node_childrens)


    ws0 <- which(xml2::xml_length(node_childrens)==0)
    childrens_1 <- node_childrens[ws0]
    childrens_1_names <- xml2::xml_name(childrens_1)

    to_map <- paste(".//d1:Stem/d1:", nodename, "/d1:",childrens_1_names, sep = "")

    dt1 <- Map(function(x) xml2::xml_text(xml2::xml_find_all(doc, x)), to_map)
    names(dt1) <- childrens_1_names
    dt1 <- list2DF(dt1) %>% utils::type.convert(as.is = TRUE)

    dt1$StemKey <- xml2::xml_integer(xml2::xml_find_all(xml2::xml_parent(xml2::xml_find_all(doc, xpt1)), "./d1:StemKey"))

    stp_stems <- dt1
    stems <- dplyr::left_join(stems, stp_stems)
  }

  ###### Multitree processed stems
  xpt1 <- ".//d1:Stem/d1:MultiTreeProcessedStem"
  nodecase  <- xml2::xml_find_first(doc,  xpt1)
  if(!is.na(nodecase)){
    nodename <- xml2::xml_name(nodecase)
    node_childrens <-  xml2::xml_children(nodecase)
    xml2::xml_length(node_childrens)


    ws0 <- which(xml2::xml_length(node_childrens)==0)
    childrens_1 <- node_childrens[ws0]
    childrens_1_names <- xml2::xml_name(childrens_1)

    to_map <- paste(".//d1:Stem/d1:", nodename, "/d1:",childrens_1_names, sep = "")

    dt1 <- Map(function(x) xml2::xml_text(xml2::xml_find_all(doc, x)), to_map)
    names(dt1) <- childrens_1_names
    dt1 <- list2DF(dt1) %>% utils::type.convert(as.is = TRUE)

    dt1$StemKey <- xml2::xml_integer(xml2::xml_find_all(xml2::xml_parent(xml2::xml_find_all(doc, xpt1)), "./d1:StemKey"))

    mtp_stems <- dt1
    stems <- dplyr::left_join(stems, mtp_stems)
  }

dplyr::glimpse(stems)
  returnlist <- list(stems = stems)


#### STP Logs ######

  xpt1 <- ".//d1:Stem/d1:SingleTreeProcessedStem/d1:Log"
  nodecase  <- xml2::xml_find_first(doc,  xpt1)
  if (!is.na(nodecase)){
    StemKeys <- xml2::xml_integer(xml2::xml_find_all( xml2::xml_parent(xml2::xml_parent(xml2::xml_find_all(doc, xpt1))), "./d1:StemKey"))

    nodename <- xml2::xml_name(nodecase)
    node_childrens <-  xml2::xml_children(nodecase)

    ws0 <- which(xml2::xml_length(node_childrens)==0)
    childrens_0 <- node_childrens[ws0]
    childrens_0_names <- xml2::xml_name(childrens_0)
    children_0_names_attr_lvc <- xml2::xml_attr(childrens_0, attr = "logVolumeCategory")
    children_0_names_attr_lvc_2 <- xml2::xml_attr(childrens_0, attr = "logVolumeCategory")
    children_0_names_attr_lvc[is.na(children_0_names_attr_lvc)] <- ""
    attrnames <- xml2::xml_attrs(childrens_0)
    logdtstring <-     sapply(attrnames, FUN = function(x) {
      ifelse(!is.na(nchar(x[1])), paste0("[",paste(paste("@", names(x), " = '", x, "'", sep = ""), collapse = " and "), "]"),
             "")
      })
    #logdtstring <- logdtstring
    attrxp <- unname(logdtstring)
    to_map <- paste(xpt1, "/d1:",childrens_0_names, attrxp, sep = "")
    to_map <- dplyr::if_else(stringr::str_detect(to_map, pattern = '@logMeasurementCategory = "Machine"')
    dt1 <-  Map(function(x) xml2::xml_text(xml2::xml_find_all(doc, x)), to_map)
    varnames <- tibble::tibble(childrens_0_names = childrens_0_names, children_0_names_attr_lvc = children_0_names_attr_lvc) %>%
      dplyr::mutate( vn = dplyr::case_when( nchar(children_0_names_attr_lvc)==0 ~ childrens_0_names, TRUE ~ children_0_names_attr_lvc)) %>%
      dplyr::pull(vn) %>%
      make.names() %>% stringr::str_remove_all(., "\\.")
    names(dt1) <- varnames
    dt1 <- list2DF(dt1) %>% utils::type.convert(as.is = TRUE)
    head(dt1)
    dt1$logkeydiff <- c(0, diff(dt1$LogKey))
    dt1$stemkeyshift <- ifelse(dt1$logkeydiff !=1, 1, 0)
    dt1$tmp_stemnumber <- cumsum(dt1$stemkeyshift)
    numlogs_per_stem <- dt1 %>% dplyr::group_by(tmp_stemnumber) %>% dplyr::summarise(n = dplyr::n())
    dt1$StemKey <- rep(StemKeys, numlogs_per_stem$n)
    dt1 <- dt1 %>% dplyr::select(-logkeydiff, -stemkeyshift, -tmp_stemnumber)
    logdt_vol <- dt1


    xpt1 <- ".//d1:Stem/d1:SingleTreeProcessedStem/d1:Log/d1:LogMeasurement[@logMeasurementCategory = 'Machine']"
    nodecase  <- xml2::xml_find_first(doc,  xpt1)
    nodename <- xml2::xml_name(nodecase)
    node_childrens <-  xml2::xml_children(nodecase)
    ws0 <- which(xml2::xml_length(node_childrens)==0)
    childrens_0 <- node_childrens[ws0]
    childrens_0_names <- xml2::xml_name(childrens_0)
    children_0_names_attr_lvc <- xml2::xml_attr(childrens_0, attr = "logDiameterCategory")
    children_0_names_attr_lvc[is.na(children_0_names_attr_lvc)] <- ""
    attrnames <- xml2::xml_attrs(childrens_0)
    attrnames <- unname(unlist(lapply(attrnames, FUN = function(x) ifelse(!is.na(nchar(x[1])), names(x)[1], NA_character_)))  )
    attrnames[is.na(attrnames)] <- ""
    attrxp <- ifelse(attrnames == "", "", paste("[@", attrnames, " = '", children_0_names_attr_lvc, "']", sep = ""))
    to_map <- paste(xpt1, "/d1:",childrens_0_names, attrxp, sep = "")
    dt1 <-  Map(function(x) xml2::xml_text(xml2::xml_find_all(doc, x)), to_map)
    varnames <- tibble::tibble(childrens_0_names = childrens_0_names, children_0_names_attr_lvc = children_0_names_attr_lvc) %>%
      dplyr::mutate( vn = dplyr::case_when( nchar(children_0_names_attr_lvc)==0 ~ childrens_0_names, TRUE ~ children_0_names_attr_lvc)) %>%
      dplyr::pull(vn) %>%
      make.names() %>% stringr::str_remove_all(., "\\.")
    names(dt1) <- varnames
    dt1<- list2DF(dt1) %>% utils::type.convert(as.is = TRUE)
    logdt_MMeasurement <- dt1
    logdt <- dplyr::bind_cols(logdt_vol, logdt_MMeasurement)


    xpt1 <- ".//d1:Stem/d1:SingleTreeProcessedStem/d1:Log/d1:Extension"
    # xpt1 <- ".//d1:Stem/d1:SingleTreeProcessedStem/d1:Log/d1:Fake"
    nodecase  <- xml2::xml_find_first(doc,  xpt1)
    nodename <- xml2::xml_name(nodecase)
    node_childrens <-  xml2::xml_children(nodecase)
    if( length(node_childrens) > 0){

      ws0 <- which(xml2::xml_length(node_childrens)==0)
      childrens_0 <- node_childrens[ws0]
      childrens_0_names <- xml2::xml_name(childrens_0)
      children_0_names_attr_lvc <- xml2::xml_attr(childrens_0, attr = "just_in_case")
      children_0_names_attr_lvc[is.na(children_0_names_attr_lvc)] <- ""
      attrnames <- xml2::xml_attrs(childrens_0)
      attrnames <- unname(unlist(lapply(attrnames, FUN = function(x) ifelse(!is.na(nchar(x[1])), names(x)[1], NA_character_)))  )
      attrnames[is.na(attrnames)] <- ""
      attrxp <- ifelse(attrnames == "", "", paste("[@", attrnames, " = '", children_0_names_attr_lvc, "']", sep = ""))
      to_map <- paste(xpt1, "/d1:",childrens_0_names, attrxp, sep = "")
      dt1 <-  Map(function(x) xml2::xml_text(xml2::xml_find_all(doc, x)), to_map)
      varnames <- tibble::tibble(childrens_0_names = childrens_0_names, children_0_names_attr_lvc = children_0_names_attr_lvc) %>%
        dplyr::mutate( vn = dplyr::case_when( nchar(children_0_names_attr_lvc)==0 ~ childrens_0_names, TRUE ~ children_0_names_attr_lvc)) %>%
        dplyr::pull(vn) %>%
        make.names() %>% stringr::str_remove_all(., "\\.")
      names(dt1) <- varnames
      dt1<- list2DF(dt1) %>% utils::type.convert(as.is = TRUE)
      logdt_extensions <- dt1
      logdt <- dplyr::bind_cols(logdt, logdt_extensions)
    }



    xpt1 <- ".//d1:Stem/d1:SingleTreeProcessedStem/d1:Log/d1:CuttingCategory"
    nodecase  <- xml2::xml_find_first(doc,  xpt1)
    nodename <- xml2::xml_name(nodecase)
    node_childrens <-  xml2::xml_children(nodecase)
    ws0 <- which(xml2::xml_length(node_childrens)==0)
    childrens_0 <- node_childrens[ws0]
    childrens_0_names <- xml2::xml_name(childrens_0)
    children_0_names_attr_lvc <- xml2::xml_attr(childrens_0, attr = "just_in_case")
    children_0_names_attr_lvc[is.na(children_0_names_attr_lvc)] <- ""
    attrnames <- xml2::xml_attrs(childrens_0)
    attrnames <- unname(unlist(lapply(attrnames, FUN = function(x) ifelse(!is.na(nchar(x[1])), names(x)[1], NA_character_)))  )
    attrnames[is.na(attrnames)] <- ""
    attrxp <- ifelse(attrnames == "", "", paste("[@", attrnames, " = '", children_0_names_attr_lvc, "']", sep = ""))
    to_map <- paste(xpt1, "/d1:",childrens_0_names, attrxp, sep = "")
    dt1 <-  Map(function(x) xml2::xml_text(xml2::xml_find_all(doc, x)), to_map)
    varnames <- tibble::tibble(childrens_0_names = childrens_0_names, children_0_names_attr_lvc = children_0_names_attr_lvc) %>%
      dplyr::mutate( vn = dplyr::case_when( nchar(children_0_names_attr_lvc)==0 ~ childrens_0_names, TRUE ~ children_0_names_attr_lvc)) %>%
      dplyr::pull(vn) %>%
      make.names() %>% stringr::str_remove_all(., "\\.")
    names(dt1) <- varnames
    dt1 <- list2DF(dt1) %>% utils::type.convert(as.is = TRUE)
    logdt_cuttingCat <- dt1
    logdt <- dplyr::bind_cols(logdt, logdt_cuttingCat)



    returnlist <- c(returnlist, list(stplogs = stplogs))

  }

  ### MTP Logs --------------
  # paste(".//d1:Stem[./d1:ProcessingCategory/text() = 'MultiTreeProcessing']/d1:MultiTreeProcessedStem/d1:"
  xpt1 <- ".//d1:Stem/d1:MultiTreeProcessedStem/d1:Log"
  # xpt1 <- ".//d1:Stem/d1:MultiTreeProcessedStem/d1:Lohjkg"
  nodecase  <- xml2::xml_find_first(doc,  xpt1)
  if(!is.na(nodecase)) {
    StemKeys <- xml2::xml_integer(xml2::xml_find_all( xml2::xml_parent(xml2::xml_parent(xml2::xml_find_all(doc, xpt1))), "./d1:StemKey"))


    nodename <- xml2::xml_name(nodecase)
    node_childrens <-  xml2::xml_children(nodecase)
    ncnames <- xml2::xml_name(node_childrens)
    xml2::xml_length(node_childrens)

    ws0 <- which(xml2::xml_length(node_childrens)==0)
    childrens_1 <- node_childrens[ws0]
    childrens_1_names <- xml2::xml_name(childrens_1)
    children_1_names_attr_lvc <- xml2::xml_attr(childrens_1, attr = "logVolumeCategory")
    children_1_names_attr_lvc[is.na(children_1_names_attr_lvc)] <- ""
    attrnames <- xml2::xml_attrs(childrens_1)
    attrnames <- unname(unlist(lapply(attrnames, FUN = function(x) ifelse(!is.na(nchar(x[1])), names(x)[1], NA_character_)))  )
    attrnames[is.na(attrnames)] <- ""

    attrxp <- ifelse(attrnames == "", "", paste("[@", attrnames, " = '", children_1_names_attr_lvc, "']", sep = ""))
    to_map <- paste(xpt1, "/d1:",childrens_1_names, attrxp, sep = "")
    dt1 <-  Map(function(x) xml2::xml_text(xml2::xml_find_all(doc, x)), to_map)
    varnames <- tibble::tibble(childrens_1_names = childrens_1_names, children_1_names_attr_lvc = children_1_names_attr_lvc) %>%
      dplyr::mutate( vn = dplyr::case_when( nchar(children_1_names_attr_lvc)==0 ~ childrens_1_names, TRUE ~ children_1_names_attr_lvc)) %>%
      dplyr::pull(vn) %>%
      make.names() %>% stringr::str_remove_all(., "\\.")
    names(dt1) <- varnames
    dt1 <- list2DF(dt1) %>% utils::type.convert(as.is = TRUE)

    dt1$logkeydiff <- c(0, diff(dt1$LogKey))
    dt1$stemkeyshift <- ifelse(dt1$logkeydiff !=1, 1, 0)
    dt1$tmp_stemnumber <- cumsum(dt1$stemkeyshift)
    numlogs_per_stem <- dt1 %>% dplyr::group_by(tmp_stemnumber) %>% dplyr::summarise(n = dplyr::n())

    dt1$StemKey <- rep(StemKeys, numlogs_per_stem$n)
    dt1 <- dt1 %>% select(-logkeydiff, -stemkeyshift, -tmp_stemnumber)
    dplyr::glimpse(dt1)

    xml2::xml_name(node_childrens)

    if("LogMeasurement" %in% ncnames){

    }



    if(nrow(dt1)){
      mtplogs <- dt1
      returnlist <- c(returnlist, mtplogs = list(mtplogs))
      dplyr::glimpse(dt1)
  }



  return(returnlist)
  }
}





