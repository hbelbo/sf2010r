

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
#' docs <- lapply(X = hprfiles, FUN = function(X){xml2::read_xml(X)})
#' getStemsAndLogs(docs[[1]]) %>% str()
#' getStemsAndLogs(docs[[2]]) %>% str()
#' getStemsAndLogs(docs[[3]]) %>% str() # MTPS and BoomPositioning
#' hqcfiles <- list.files(path =  system.file(package = "sf2010r"),
#' pattern = ".hqc", recursive = TRUE, full.names= TRUE)
#' hqcdocs <- lapply(X = hqcfiles, FUN = function(X){xml2::read_xml(X)})
#' getStemsAndLogs(hqcdocs[[1]]) %>% str()
#' getStemsAndLogs(hqcdocs[[2]]) %>% str()
#'
#' @export
getStemsAndLogs <- function(doc){

  # doc = hqcdocs[[1]]
  stemnode1  <- xml2::xml_find_first(doc,  ".//d1:Stem")
  if(!is.na(stemnode1)){
     ## getting general stem data from Stem childrens
    StemKeys <- xml2::xml_integer(xml2::xml_find_all(doc,  ".//d1:Stem/d1:StemKey"))

    node_childrens <-  xml2::xml_children(stemnode1)

    ws0 <- which(xml2::xml_length(node_childrens)==0)
    childrens_1 <- node_childrens[ws0]
    childrens_1_names <- xml2::xml_name(childrens_1)

    to_map <- paste(".//d1:Stem/d1:", childrens_1_names, sep = "")

    stmdt1 <- Map(function(x) xml2::xml_text(xml2::xml_find_all(doc, x)), to_map)
    names(stmdt1) <- stringr::str_remove(string = names(stmdt1), pattern = ".//d1:Stem/d1:")
    stmdt1 <- list2DF(stmdt1) %>% utils::type.convert(as.is = TRUE)
    stems <- stmdt1


    ##### Coordinates dataset -----------------
    # Detect all StemmCoordinate locations  (Base machine, crane tip etc)
    # First stem where stem coordinates exists. Assuming all other have similar coordinate data and settings as the first.
    xpt1 <- ".//d1:Stem/d1:StemCoordinates"

    #coord_stem1 <-  xml2::xml_parent(xml2::xml_find_first(doc, ".//d1:Stem/d1:StemCoordinates"))
    coord_stem1 <-  xml2::xml_parent(xml2::xml_find_first(doc, xpt1))
    if(!is.na(coord_stem1)){
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
        stringr::str_remove( pattern = "'\\]\\/d1:\\w+") %>%
        stringr::str_remove_all( pattern = "when |the|tree") %>%
        stringr::str_replace_all( pattern = "position felling ", replacement = "felling") %>%
        gsub(pattern = '\\b(\\pL)\\pL{2,}|.', replacement = '\\L\\1', perl = TRUE) %>% tolower()

        #stringr::str_remove(., pattern = "'\\]\\/d1:\\w+") %>%
        #stringr::str_remove_all(., pattern = "when |the|tree") %>%
        #stringr::str_replace_all(., pattern = "position felling ", replacement = "felling") %>%
        #gsub('\\b(\\pL)\\pL{2,}|.','\\L\\1', . ,perl = TRUE) %>% tolower()


      varnamesdf <- data.frame(cn = rep(tolower(coord_names), length(unique(varnameending))), vne = varnameending)
      varnames <- paste0(varnamesdf[,1], "_", varnamesdf[,2])
      # Fetch all values
      valuelist <- Map(function(x)  xml2::xml_double(xml2::xml_find_all(doc,x)), xpts_cpositions_xyz )
      coords_df <- list2DF(valuelist)
      names(coords_df) <- varnames
      coords_df$StemKey <- xml2::xml_integer(xml2::xml_find_all(xml2::xml_parent(xml2::xml_find_all(doc, ".//d1:Stem/d1:StemCoordinates")), "./d1:StemKey"))

      stems <- dplyr::left_join(stems, coords_df,  by = c("StemKey"))
    }
# cat("\n going Extension")
    xpt1 <- ".//d1:Stem/d1:Extension"  ###### Getting Extensions if present -------
    nodecase  <- xml2::xml_find_first(doc,  xpt1) # use first node as example to create dataset
    if(!is.na(nodecase)){
      nodename <- xml2::xml_name(nodecase) # is Extension
      node_childrens <-  xml2::xml_children(nodecase)
      ws0 <- which(xml2::xml_length(node_childrens)==0)
      childrens_1 <- node_childrens[ws0]
      childrens_1_names <- xml2::xml_name(childrens_1)
      childrens_1_names <- unique(childrens_1_names) # some may have multiple records, but that is not collected currently
      to_map <- paste(".//d1:Stem/d1:", nodename, "/d1:",childrens_1_names, sep = "")

      dt1 <- Map(function(x) {  # For each extension variables, create a data.frame having the extension variable and corresponding StemKey
       # print(x);
        df <- data.frame(xml2::xml_text(xml2::xml_find_all(doc, x)))
        StemKey <- xml2::xml_integer(xml2::xml_find_all(xml2::xml_parent(xml2::xml_parent(xml2::xml_find_all(doc, x))), "./d1:StemKey"))
        if(nrow(df)==length(StemKey)){ # Cant handle Extras if correspoinding StemKey is not found
          df$StemKey = StemKey
          names(df)[1] = stringr::str_extract(string = x, pattern = "\\w*$")
          return(df)
        } else {return(NULL)}

                } , to_map)
      names(dt1) <- stringr::str_extract( # Keep only last word of xpath as name
        string = names(dt1), pattern = "\\w*$")

      valids <- which(!(sapply(dt1, (is.null)))) #
      dt1 <- dt1[valids]

      if(length(dt1) > 1) {
        extensions <- dt1[[1]]
        for (i in 2:length(dt1)) {
          #i = 2
          extensions <- dplyr::full_join(extensions, dt1[[i]], by = c("StemKey"))
        }
      } else {
        extensions <- dt1[[1]]
      }

      extensions <- extensions %>% utils::type.convert(as.is = TRUE)
      stems <- dplyr::left_join(stems, extensions, by = c("StemKey"))

#       dt1 <- Map(function(x) xml2::xml_text(xml2::xml_find_all(doc, x)), to_map)
#       names(dt1) <- childrens_1_names
#       nobs <- sapply(dt1, length) #number of obs per extension variable. Not always one per node.
#       w <- which(nobs == length(nodecases)) # We keep only those having one per node
#       dt1 <- dt1[w]
#       dt1 <- list2DF(dt1) %>% utils::type.convert(as.is = TRUE)
#       ext_StemKey <- xml2::xml_integer(xml2::xml_find_all(xml2::xml_parent(xml2::xml_parent(xml2::xml_find_all(doc, to_map[1]))), "./d1:StemKey"))
#       if((length(ext_StemKey) <= length(StemKeys)) & length(ext_StemKey) == length(nodecases)){
#         dt1$StemKey <- ext_StemKey
#        extensions <- dt1
#        stems <- dplyr::left_join(stems, extensions, by = c("StemKey"))
#       }


    }


  ###### Getting Boom position when felling if present -----------
 #  cat("\n going  BoomPos")
  xpt1 <- ".//d1:Stem/d1:BoomPositioning[@boomPositioningCategory='Felling']"
  nodecase  <- xml2::xml_find_first(doc,  xpt1)

  if(!is.na(nodecase)){
    nodename <- xml2::xml_name(nodecase)
    nodecases  <- xml2::xml_find_all(doc,  xpt1) # All  nodes
    node_childrens <-  xml2::xml_children(nodecases)
    # xml2::xml_attr(stm_coords_1, attr = "boomPositioningCategory=") %>% unique() # BoomPositionCategori to be included
    ws0 <- which(xml2::xml_length(node_childrens)==0)
    childrens_1 <- node_childrens[ws0]
    childrens_1_names <- xml2::xml_name(childrens_1)


    to_map <- paste(".//d1:Stem/d1:", nodename, "[@boomPositioningCategory='Felling']/d1:",unique(childrens_1_names), sep = "")

    dt1 <- Map(function(x) xml2::xml_text(xml2::xml_find_all(doc, x)), to_map)
    childrens_1_names <- paste(childrens_1_names, "_Felling", sep = "")
    names(dt1) <- unique(childrens_1_names)
    nobs <- sapply(dt1, length)
    w <- which(nobs == length(nodecases)) # We keep only those having one per node
    dt1 <- dt1[w]
    dt1 <- list2DF(dt1) %>% utils::type.convert(as.is = TRUE)

    #dt1$StemKey <- xml2::xml_integer(xml2::xml_find_all(xml2::xml_parent(xml2::xml_find_all(doc, xpt1)), "./d1:StemKey"))
    dt1$StemKey <- xml2::xml_integer(xml2::xml_find_all(xml2::xml_parent(xml2::xml_parent(xml2::xml_find_all(doc, to_map[1]))), "./d1:StemKey"))

    Boompos <- dt1  %>% utils::type.convert(as.is = TRUE)
    stems <- dplyr::left_join(stems, Boompos, by = c("StemKey"))
  }


    ##### Single Tree Processed stems ##### ------------
 #  cat("\n going  SingleTreeProcessedStem")
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

      stemdat1 <- dt1
      #stems <- dplyr::left_join(stems, stp_stems, by = c("StemKey"))
    } else {stemdat1 = data.frame()}

    ###### Multitree processed stems ----------
  #   cat("\n going  multiTreeProcessingStem")
    xpt1 <- ".//d1:Stem/d1:MultiTreeProcessedStem"
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
      dt1 <- list2DF(dt1) %>% utils::type.convert(as.is = TRUE)

      dt1$StemKey <- xml2::xml_integer(xml2::xml_find_all(xml2::xml_parent(xml2::xml_find_all(doc, xpt1)), "./d1:StemKey"))

      stemdat2 <- dt1
      #stems <- dplyr::left_join(stems, mtp_stems, by = c("StemKey"))

    } else {stemdat2 = data.frame()}

    # Summing up -------------
    if(nrow(stemdat1)> 0 & nrow(stemdat2)>0){
      stemdat12 <- bind_rows(stemdat1, stemdat2)
    } else if(nrow(stemdat1)> 0){
      stemdat12 <- stemdat1
    } else if(nrow(stemdat2)> 0){
      stemdat12 <- stemdat2
    } else {stemdat12 = data.frame()}
    stems <- dplyr::left_join(stems, stemdat12, by = c("StemKey"))

    returnlist <- list(stems = stems)

  ##### StemGrades -----------------
    # Detect StemGrades in single tree processed stems
    xpt1 <- ".//d1:Stem/d1:SingleTreeProcessedStem/d1:StemGrade"
    grade_stem1 <-  xml2::xml_parent(xml2::xml_find_first(doc, xpt1))
    if(!is.na(grade_stem1)){
      gvs <- xml2::xml_find_all(doc, xpt1)
      p_grades_stp <-  xml2::xml_parent(xml2::xml_find_all(doc, xpt1))
      stemgrades <-  xml2::xml_integer(xml2::xml_find_all(gvs, "./d1:GradeValue"))
      StemKeys2 <- unlist(sapply(p_grades_stp, function(y) { # Create a vector of stemkeys cooresponding to each entry of StemGrade
        rep(xml2::xml_integer(  #For each stem entry having Stemgrade, repeat the corresponding stemkey for each StemGrade
          xml2::xml_find_first(
            xml2::xml_parent(y), "./d1:StemKey")),
          each = length(xml2::xml_find_all(y, xpath = "./d1:StemGrade")))
        }))

      gradestartpos <-  as.numeric( unlist(sapply(gvs, function(y) {
        xml2::xml_attr(xml2::xml_child(y), attr = "gradeStartPosition")
      })))

      stemgrades <- data.frame(StemKey = StemKeys2, GradeStartPosition = gradestartpos, Grade = stemgrades )
      returnlist <- c(returnlist, list(stemgrades = stemgrades))
    }




  #### STP Logs ######
  #  cat("\n going  STP_Logs")
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
      children_0_names_attr_lvc[is.na(children_0_names_attr_lvc)] <- ""

      children_0_names_attr_lmc <- xml2::xml_attr(childrens_0, attr = "logMeasurementCategory")
      children_0_names_attr_lmc[is.na(children_0_names_attr_lmc)] <- ""

      attrnames <- xml2::xml_attrs(childrens_0)
      logdtstring <-     sapply(attrnames, FUN = function(x) {
        nchar(attrnames[[1]][1])
        ifelse(!is.na(nchar(x[1])), paste0("[",paste(paste("@", names(x), " = '", x, "'", sep = ""), collapse = " and "), "]"),
               "")
        })



      #logdtstring <- logdtstring
      attrxp <- unname(logdtstring)
      to_map <- paste(xpt1, "/d1:",childrens_0_names, attrxp, sep = "")

      ### Test approach

      varnames <-  tibble::tibble(childrens_0_names = childrens_0_names, children_0_names_attr_lvc = children_0_names_attr_lvc, children_0_names_attr_lmc = children_0_names_attr_lmc) %>%
        dplyr::mutate( vn = dplyr::case_when( nchar(children_0_names_attr_lvc)== 0 ~ childrens_0_names,
                                              children_0_names_attr_lmc == "Machine" ~ children_0_names_attr_lvc,
                                              children_0_names_attr_lmc == "Operator" ~ paste0(children_0_names_attr_lvc, "_operator"),
                                              TRUE ~ children_0_names_attr_lvc)) %>%
        dplyr::pull("vn") %>%
        stringr::str_replace_all( "[()]", "") %>%
        make.names()


       # x = to_map[1]
       # Tedious approach because not all measurements are present for all logs.
       dt1 <- Map(function(x, xnames) {  # For each measurement variables, create a data.frame having the  variable and corresponding StemKey.
         # x = to_map[1]
         # xnames = varnames[1]
         df = data.frame(
           v1 = xml2::xml_text(xml2::xml_find_all(doc, x))
           , LogKeyII =  xml2::xml_integer(xml2::xml_find_first(xml2::xml_parent(xml2::xml_find_all(doc, x)), "./d1:LogKey"))
           #, LogKeyII =  xml2::xml_integer(xml2::xml_find_first(xml2::xml_parent(xml2::xml_parent(xml2::xml_find_all(doc, x))), "./d1:LogKey"))
           )  %>%
           dplyr::mutate(tmpstemnrchange = dplyr::if_else(((.data$LogKeyII == 1L) | .data$LogKeyII - dplyr::lag(.data$LogKeyII, default = 9)<1 ), 1, 0),
                         tmpstmnr = cumsum(.data$tmpstemnrchange))
         names(df)[1] = xnames

         #names(df)[1] = stringr::str_extract(string = x, pattern = "\\w*$")
         stmk = data.frame(StemKey = xml2::xml_integer(xml2::xml_find_all(xml2::xml_parent(xml2::xml_parent(xml2::xml_parent(xml2::xml_find_all(doc, x)))), "./d1:StemKey"))) %>%
         #stmk = data.frame(StemKey = xml2::xml_integer(xml2::xml_find_first(xml2::xml_parent(xml2::xml_parent(xml2::xml_parent(xml2::xml_parent(xml2::xml_find_all(doc, x))))), "./d1:StemKey"))) %>%
           dplyr::mutate( tmpstmnr = dplyr::row_number())
         df = df %>%
           dplyr::left_join(stmk, by = c("tmpstmnr")) %>%
           dplyr::select(-tidyselect::starts_with("tmp"))
         return(df)
       } , to_map, varnames)

       #dt1[[1]] %>% tibble::as_tibble() %>% utils::type.convert(as.is=TRUE) %>% dplyr::filter(.data$LogKey != .data$LogKeyII) %>% head()


       logdt <- dt1[[1]]
      if(length(dt1) > 1) {
        for (i in 2:length(dt1)) {
          logdt <- dplyr::full_join(logdt, dt1[[i]], by = c("StemKey", "LogKeyII"))
        }
      }
       dt_logs <- logdt %>% utils::type.convert(as.is = TRUE) %>% select(-.data$LogKeyII)




  ### Log measurements by machine; Butt.ob, Butt.ub, Mid.ob, Mid.ub, ...
      xpt1 <- ".//d1:Stem/d1:SingleTreeProcessedStem/d1:Log/d1:LogMeasurement[@logMeasurementCategory = 'Machine']"
      nodecase  <- xml2::xml_find_first(doc,  xpt1)
      nodename <- xml2::xml_name(nodecase)
      node_childrens <-  xml2::xml_children(nodecase)
      ws0 <- which(xml2::xml_length(node_childrens)==0)
      childrens_0 <- node_childrens[ws0]

      ## Then a dirty trick to avoid ControLogDiameters
      childrens_0 <- Filter(function(x) stringr::str_starts(xml2::xml_name(x), pattern = "ControlLogDiameter", negate = TRUE),  childrens_0)

      childrens_0_names <- xml2::xml_name(childrens_0)
      children_0_names_attr_ldc <- xml2::xml_attr(childrens_0, attr = "logDiameterCategory")
      children_0_names_attr_ldc[is.na(children_0_names_attr_ldc)] <- ""

      attrnames <- xml2::xml_attrs(childrens_0)
      attrnames <- unname(unlist(lapply(attrnames, FUN = function(x) ifelse(!is.na(nchar(x[1])), names(x)[1], NA_character_)))  )
      attrnames[is.na(attrnames)] <- ""
      attrxp <- ifelse(attrnames == "", "", paste("[@", attrnames, " = '", children_0_names_attr_ldc, "']", sep = ""))
      to_map <- paste(xpt1, "/d1:",childrens_0_names, attrxp, sep = "")

      varnames <- tibble::tibble(childrens_0_names = childrens_0_names, children_0_names_attr_ldc = children_0_names_attr_ldc) %>%
        dplyr::mutate(
          vn = dplyr::case_when(
            nchar(children_0_names_attr_ldc)==0 ~ childrens_0_names,
            nchar(children_0_names_attr_ldc) > 0 ~ paste0(childrens_0_names, "_", children_0_names_attr_ldc),
            TRUE ~ children_0_names_attr_ldc)) %>%
        dplyr::pull("vn") %>%
        make.names()


      # x = to_map[1]
      # Tedious approach because not all measurements are present for all logs.
      dt1 <- Map(function(x, xnames) {  # For each measurement variables, create a data.frame having the  variable and corresponding StemKey.
        # x = to_map[1]
        # xnames = varnames[1]

        df = data.frame(v1 = xml2::xml_text(xml2::xml_find_all(doc, x)),
                         LogKey =  xml2::xml_integer(xml2::xml_find_all(xml2::xml_parent(xml2::xml_parent(xml2::xml_find_all(doc, x))), "./d1:LogKey")) )
        df$tmp_LagLogKey = dplyr::lag(df$LogKey, default = 9L)
        df <- df %>%
          dplyr::mutate(tmp_stemnrchange = dplyr::if_else(((.data$LogKey == 1) | (.data$LogKey - .data$tmp_LagLogKey < 1 )), 1, 0),
                tmp_stmnr = cumsum(.data$tmp_stemnrchange))
        names(df)[1] = xnames

         #names(df)[1] = stringr::str_extract(string = x, pattern = "\\w*$")
         stmk = data.frame(StemKey = xml2::xml_integer(xml2::xml_find_all(xml2::xml_parent(xml2::xml_parent(xml2::xml_parent(xml2::xml_parent(xml2::xml_find_all(doc, x))))), "./d1:StemKey"))) %>%
                           dplyr::mutate( tmp_stmnr = dplyr::row_number())
         df = df %>% dplyr::left_join(stmk, by = c("tmp_stmnr")) %>% dplyr::select(-tidyselect::starts_with("tmp"))
         return(df)
      } , to_map, varnames)

      logmeas <- dt1[[1]]
      if(length(dt1) > 1) {
        for (i in 2:length(dt1)) {
          logmeas <- dplyr::full_join(logmeas, dt1[[i]], by = c("StemKey", "LogKey"))
        }
      }
      logmeas <- logmeas %>% utils::type.convert(as.is = TRUE)

      # Then merge log dia og length measurements with the rest of log data
      dt_logs <- dplyr::left_join(dt_logs, logmeas, by = c("StemKey", "LogKey"))

      ### Log data extension
   #   cat("\n going  Log STP extension")
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
          dplyr::pull("vn") %>%
          make.names()
        names(dt1) <- varnames
        dt1<- list2DF(dt1) %>% utils::type.convert(as.is = TRUE)
        logdt_extensions <- dt1
        dt_logs <- dplyr::bind_cols(dt_logs, logdt_extensions)
      }


      ### Log cutting category
   #   cat("\n going  STP Log CuttingCategory")
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
        dplyr::pull("vn") %>%
        make.names()
      names(dt1) <- varnames
      dt1 <- list2DF(dt1) %>% utils::type.convert(as.is = TRUE)
      dt_logs <- dplyr::bind_cols(dt_logs, dt1)



      returnlist <- c(returnlist, list(stplogs = dt_logs))

    }

  ### MTP Logs --------------
    # paste(".//d1:Stem[./d1:ProcessingCategory/text() = 'MultiTreeProcessing']/d1:MultiTreeProcessedStem/d1:"
 # cat("\n going MTPStem-Log")
    xpt1 <- ".//d1:Stem/d1:MultiTreeProcessedStem/d1:Log"
    # xpt1 <- ".//d1:Stem/d1:MultiTreeProcessedStem/d1:Lohjkg"
    nodecase  <- xml2::xml_find_first(doc,  xpt1)
    if(!is.na(nodecase)) {
      StemKeys <- xml2::xml_integer(xml2::xml_find_all( xml2::xml_parent(xml2::xml_parent(xml2::xml_find_all(doc, xpt1))), "./d1:StemKey"))
      nodename <- xml2::xml_name(nodecase)
      node_childrens <-  xml2::xml_children(nodecase)

      ws0 <- which(xml2::xml_length(node_childrens)==0)
      childrens_0 <- node_childrens[ws0]
      childrens_0_names <- xml2::xml_name(childrens_0)
      children_0_names_attr_lvc <- xml2::xml_attr(childrens_0, attr = "logVolumeCategory")
      children_0_names_attr_lvc[is.na(children_0_names_attr_lvc)] <- ""
      attrnames <- xml2::xml_attrs(childrens_0)
      x <- attrnames[3]
      logdtstring <-     sapply(attrnames, FUN = function(x) {
        ifelse(!is.na(nchar(x[1])), paste0("[",paste(paste("@", names(x), " = '", x, "'", sep = ""), collapse = " and "), "]"), "")
      })
      attrxp <- unname(logdtstring)
      to_map <- paste(xpt1, "/d1:",childrens_0_names, attrxp, sep = "")

      # For variables without attributes ----------
      slice1 <- which(!stringr::str_detect(to_map, pattern = "@"))
      to_map1 <- to_map[slice1]
      dt1 <-  Map(function(x) xml2::xml_text(xml2::xml_find_all(doc, x)), to_map1)
      varnames1 <- childrens_0_names[slice1]
      names(dt1) <- varnames1
      dt1 <- list2DF(dt1) %>% utils::type.convert(as.is = TRUE)
      # str(dt1)

      # LogVolume For logMeasurementCategory="Machine" -----------
      slice2 <-  which(stringr::str_detect(to_map, pattern = "@logMeasurementCategory = 'Machine']"))
      to_map2 <- to_map[slice2]
      dt2 <-  Map(function(x) xml2::xml_text(xml2::xml_find_all(doc, x)), to_map2)
      varnames2 <- tibble::tibble(childrens_0_names = childrens_0_names[slice2], children_0_names_attr_lvc = children_0_names_attr_lvc[slice2]) %>%
        dplyr::mutate( vn = dplyr::case_when( nchar(children_0_names_attr_lvc)==0 ~ childrens_0_names, TRUE ~ children_0_names_attr_lvc)) %>%
        dplyr::pull("vn")  %>%
        stringr::str_replace_all(pattern = "[()]", replacement = "") %>%
        make.names()
      names(dt2) <- varnames2
      dt2 <- list2DF(dt2) %>% utils::type.convert(as.is = TRUE)

      dt_logs <- dplyr::bind_cols(dt1, dt2) %>%
        dplyr::mutate(logkeydiff = .data$LogKey - dplyr::lag(.data$LogKey, 1, 1)) %>%
        dplyr::mutate(stemkeyshift = ifelse(.data$logkeydiff !=1, 1, 0),
               tmp_stemnumber = cumsum(.data$stemkeyshift))

      numlogs_per_stem <- dt_logs %>% dplyr::group_by(.data$tmp_stemnumber) %>% dplyr::summarise(n = dplyr::n())
      dt_logs$StemKey <- rep(StemKeys, numlogs_per_stem$n)
      #dt_logs <- dt_logs %>% dplyr::select(-logkeydiff, -stemkeyshift, -tmp_stemnumber)
      dt_logs <- dt_logs %>% dplyr::select(-tidyselect::all_of(c("logkeydiff", "stemkeyshift", "tmp_stemnumber")))
      #head(dt_logs)

      ### Log measurements (dia, length) by machine ----------
      xpt1 <- ".//d1:Stem/d1:MultiTreeProcessedStem/d1:Log/d1:LogMeasurement[@logMeasurementCategory = 'Machine']"
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
        dplyr::pull("vn") %>%
        make.names()
      names(dt1) <- varnames
      dt1<- list2DF(dt1) %>% utils::type.convert(as.is = TRUE)
      logdt_MMeasurement <- dt1
      dt_logs <- dplyr::bind_cols(dt_logs, logdt_MMeasurement)


      ### Log data extension ----
      # cat("\n going  MTPStem Log Extension")
      xpt1 <- ".//d1:Stem/d1:MultiTreeProcessedStem/d1:Log/d1:Extension"
      nodecase  <- xml2::xml_find_first(doc,  xpt1)
      if( length(nodecase) > 0){
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
          dplyr::pull("vn") %>%
          make.names()
        names(dt1) <- varnames
        dt1<- list2DF(dt1) %>% utils::type.convert(as.is = TRUE)
        logdt_extensions <- dt1
        dt_logs <- dplyr::bind_cols(dt_logs, logdt_extensions)
      }

      ### Log cutting category ------
      # cat("\n going  MTP Log CuttingCategory")
      xpt1 <- ".//d1:Stem/d1:MultiTreeProcessedStem/d1:Log/d1:CuttingCategory"
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
        dplyr::pull("vn") %>%
        make.names()
      names(dt1) <- varnames
      dt1 <- list2DF(dt1) %>% utils::type.convert(as.is = TRUE)
      logdt_cuttingCat <- dt1
      dt_logs <- dplyr::bind_cols(dt_logs, logdt_cuttingCat)

      returnlist <- c(returnlist, mtplogs = list(dt_logs))

    } # End MultiTreeProcessing logs

  return(returnlist)
  } # end if stemnode1 is not NA
}



#' get SingleTreeProcessed tree's diameter vector. By default not present in the hpr files.
#'
#' @param doc a hpr document (xml)
#' @return a tibble. If no diametervector is present, a message and NULL.
#' @export
#'
#' @examples
#' hprfiles <- list.files(path =  system.file(package = "sf2010r"),
#' pattern = ".hpr", recursive = TRUE, full.names= TRUE)
#' docs <- lapply(X = hprfiles, FUN = function(X){xml2::read_xml(X)})
#' getSTP_stemdiameters(docs[[1]]) %>% str()  # does not have stemdiameters
#' getSTP_stemdiameters(docs[[3]]) %>% str()  # does  have stemdiameters
getSTP_stemdiameters <- function(doc) {
  # Could probably be a lot more efficient!

  xpt1 <- ".//d1:Stem/d1:SingleTreeProcessedStem/d1:StemDiameters[@diameterCategory='Over bark']"
  nodecase  <- xml2::xml_find_first(doc,  xpt1)
  if (!is.na(nodecase)){
    StemKeys <- xml2::xml_integer(xml2::xml_find_all( xml2::xml_parent(xml2::xml_parent(xml2::xml_find_all(doc, xpt1))), "./d1:StemKey"))
    diastems <- data.frame(StemKey = StemKeys, diastemnr = 1:length(StemKeys))
    # head(diastems)

    all_nodes <- xml2::xml_find_all(doc,  xpt1)
    all_diameterpositions <-  as.integer(xml2::xml_attr(xml2::xml_find_all(doc,   paste0(xpt1, "/d1:DiameterValue")),  attr = "diameterPosition"))
    all_diameters <- xml2::xml_integer(xml2::xml_find_all(doc,   paste0(xpt1, "/d1:DiameterValue")))
    newstemindicator <- ifelse(all_diameterpositions==0, 1, 0)
    diastemnr = cumsum(newstemindicator)
    diadf <- data.frame(Position = all_diameterpositions, dia_ob = all_diameters, diastemnr = diastemnr)

    diadf <- diadf %>% dplyr::left_join(diastems, by = c("diastemnr")) %>% dplyr::select(-diastemnr)
     # head(diadf)

    return(diadf)
  } else {return( cat("No diametervectors in the hpr\n"))}

}


# # Create a function to perform inner joins on a list of data frames
# perform_full_join <- function(df_list, bykey = "key") {
#   if(length(df_list) > 1) {
#     #df_list %>% purrr::reduce(dplyr::inner_join, by = bykey)
#     result_df <- df_list[[1]]
#     for (i in 2:length(df_list)) {
#       #i = 2
#       result_df <- full_join(result_df, df_list[[i]], by = bykey)
#     }
#     return(result_df)
#   } else {
#     return(df_list[[1]])
#   }
# }
#

