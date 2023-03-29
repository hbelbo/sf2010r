pkgname <- "sf2010r"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "sf2010r-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('sf2010r')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("fTZ")
### * fTZ

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: fTZ
### Title: Get Time zone from the StanForD2010 countrycode variables.
### Aliases: fTZ

### ** Examples

fTZ("752")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("fTZ", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("getCombined.mwt")
### * getCombined.mwt

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: getCombined.mwt
### Title: Get Combined Machine Work Time for all and within a SF2010 .hpr
###   file
### Aliases: getCombined.mwt

### ** Examples

pth <- system.file(package = "sf2010r")
momf <- list.files(pth,".mom",ignore.case=TRUE,recursive=TRUE,full.names=TRUE)
momf_cmwt <- momf[which(stringr::str_detect(string=momf,pattern="combined_mwt"))]
doc <- xml2::read_xml(momf_cmwt[2])
getCombined.mwt(doc)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("getCombined.mwt", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("getDelivery")
### * getDelivery

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: getDelivery
### Title: Delivery data from one delivery definition node
### Aliases: getDelivery

### ** Examples

pth <- system.file(package = "sf2010r") 
fprfiles <- list.files(pth,".fpr",ignore.case=TRUE,recursive=TRUE,full.names=TRUE)
doc <- xml2::read_xml(fprfiles[1])
nodelist <- xml2::xml_find_all(doc, ".//d1:DeliveryDefinition")
getDelivery(nodelist[[1]]) %>% dplyr::glimpse()
plyr::ldply(nodelist[1], getDelivery)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("getDelivery", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("getLocation")
### * getLocation

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: getLocation
### Title: Location data from one Location node
### Aliases: getLocation

### ** Examples

pth <- system.file(package = "sf2010r") 
fprfiles <- list.files(pth,".fpr",ignore.case=TRUE,recursive=TRUE,full.names= TRUE)
doc <- xml2::read_xml(fprfiles[1])
locationlist <- xml2::xml_find_all(doc, ".//d1:LocationDefinition")
getLocation(locationlist[[1]]) %>% dplyr::glimpse()
plyr::ldply(locationlist[1], getLocation)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("getLocation", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("getLogs")
### * getLogs

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: getLogs
### Title: Get logsdata for all stems within a SF2010 .hpr file
### Aliases: getLogs

### ** Examples

pth <- system.file(package = "sf2010r")
hprfiles <- list.files(pth,".hpr",recursive=TRUE,full.names=TRUE)
doc <- xml2::read_xml(hprfiles[1])
getLogs(doc)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("getLogs", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("getMTPlogs")
### * getMTPlogs

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: getMTPlogs
### Title: Fetch the multi tree processed logs belonging to one multi-stem
###   entry in hpr
### Aliases: getMTPlogs

### ** Examples

pth <- system.file(package = "sf2010r")
hprfiles <- list.files(pth,".hpr",recursive=TRUE,full.names=TRUE)
doc <- xml2::read_xml(hprfiles[1])
stemlist <- xml2::xml_find_all(doc, ".//d1:Stem")
pcat <- ".//d1:ProcessingCategory"
wtch <- which(xml2::xml_text(xml2::xml_find_all(stemlist,pcat))=="MultiTreeProcessing")
getMTPlogs(stemlist[1])
if(length(wtch) > 0) { getMTPlogs(stemlist[wtch[1]])}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("getMTPlogs", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("getMachineReportHeader")
### * getMachineReportHeader

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: getMachineReportHeader
### Title: Organize the header data of the StanForD2010 report into a
###   tibble
### Aliases: getMachineReportHeader

### ** Examples

pth <- system.file(package = "sf2010r")
hprfiles <-  list.files(pth,".hpr",recursive=TRUE,full.names=TRUE)
doc <- xml2::read_xml(hprfiles[1])
getMachineReportHeader(doc)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("getMachineReportHeader", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("getMom.all")
### * getMom.all

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: getMom.all
### Title: mom-file reader function
### Aliases: getMom.all

### ** Examples

pth <- system.file(package = "sf2010r")
momfiles <- list.files(pth,".mom$",recursive=TRUE,ignore.case=TRUE,full.names= TRUE)
momtest1 <- getMom.all(momfiles[1])



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("getMom.all", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("getMom.cmwt.data")
### * getMom.cmwt.data

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: getMom.cmwt.data
### Title: Combined machine work time data from one combined machine work
###   time node
### Aliases: getMom.cmwt.data

### ** Examples

pth <- system.file(package = "sf2010r")
momf <- list.files(pth,".mom",ignore.case=TRUE,recursive=TRUE,full.names= TRUE)
momf_cmwt <- momf[which(stringr::str_detect(string=momf,pattern="combined_mwt"))]
doc <- xml2::read_xml(momf_cmwt[2])#'
cmwtlist <- xml2::xml_find_all(doc, ".//d1:CombinedMachineWorkTime")
getMom.cmwt.data(cmwtlist[[1]]) %>% dplyr::glimpse()
plyr::ldply(cmwtlist[1:2], getMom.cmwt.data)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("getMom.cmwt.data", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("getMom.imwt.activity")
### * getMom.imwt.activity

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: getMom.imwt.activity
### Title: Indivdual machine work time activity data from one indivdual
###   machine work time node
### Aliases: getMom.imwt.activity

### ** Examples

pth. <- system.file(package = "sf2010r")
momfiles <- list.files(pth.,".mom",ignore.case=TRUE,recursive=TRUE,full.names=TRUE)
momfiles_imwt <- momfiles[which(stringr::str_detect(momfiles,pattern="individual_mwt"))]
doc <- xml2::read_xml(momfiles_imwt[2])
imwtlist <- xml2::xml_find_all(doc, ".//d1:IndividualMachineWorkTime")
getMom.imwt.activity(imwtlist[[41]]) %>% dplyr::glimpse()
 plyr::ldply(imwtlist[25:42], getMom.imwt.activity)
pth. <- system.file(package = "sf2010r") 
momfiles <- list.files(pth.,".mom",ignore.case=TRUE,recursive=TRUE,full.names=TRUE)
momfiles_imwt <- momfiles[which(stringr::str_detect(string=momfiles,pattern="individual_mwt"))]
doc <- xml2::read_xml(momfiles_imwt[2])
imwtlist <- xml2::xml_find_all(doc, ".//d1:IndividualMachineWorkTime")
getMom.imwt.activity(imwtlist[[41]]) %>% dplyr::glimpse()
 plyr::ldply(imwtlist[25:42], getMom.imwt.activity)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("getMom.imwt.activity", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("getMom.imwt.production")
### * getMom.imwt.production

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: getMom.imwt.production
### Title: Indivdual machine work time production data from one individual
###   machine work time node
### Aliases: getMom.imwt.production

### ** Examples

pth <- system.file(package = "sf2010r")
momfiles <- list.files(pth,".mom",ignore.case=TRUE,recursive=TRUE,full.names=TRUE)
momfiles_imwt <- momfiles[which(stringr::str_detect(string = momfiles, pattern = "individual_mwt"))]
doc <- xml2::read_xml(momfiles_imwt[2])
imwtlist <- xml2::xml_find_all(doc, ".//d1:IndividualMachineWorkTime")
getMom.imwt.production(imwtlist[[3]]) %>% dplyr::glimpse()
plyr::ldply(imwtlist[92:94], getMom.imwt.production)
pth. <- system.file(package = "sf2010r")
momfiles <- list.files(pth.,".mom",ignore.case=TRUE,recursive=TRUE,full.names=TRUE)
momfiles_imwt <- momfiles[which(stringr::str_detect(string=momfiles,pattern="individual_mwt"))]
doc <- xml2::read_xml(momfiles_imwt[2])
imwtlist <- xml2::xml_find_all(doc, ".//d1:IndividualMachineWorkTime")
getMom.imwt.production(imwtlist[[3]]) %>% dplyr::glimpse()
plyr::ldply(imwtlist[92:94], getMom.imwt.production)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("getMom.imwt.production", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("getObjectDefinition")
### * getObjectDefinition

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: getObjectDefinition
### Title: Extracting all data defining one cut object from one
###   ObjectDefinition xml-tree
### Aliases: getObjectDefinition

### ** Examples

pth <- system.file(package = "sf2010r")
hprfiles <-  list.files(pth,".hpr",recursive=TRUE,full.names=TRUE)
doc <- xml2::read_xml(hprfiles[1])
Objects_nodes <- xml2::xml_find_all(doc, "//d1:ObjectDefinition")
getObjectDefinition(Objects_nodes[1])
plyr::ldply(Objects_nodes, getObjectDefinition)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("getObjectDefinition", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("getObjects")
### * getObjects

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: getObjects
### Title: Extracting all cut object definitions from doc
### Aliases: getObjects

### ** Examples

pth <- system.file(package = "sf2010r")
hprfiles <- list.files(pth,".hpr",recursive=TRUE,full.names=TRUE)
doc <- xml2::read_xml(hprfiles[1])
getObjects(doc)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("getObjects", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("getOperators")
### * getOperators

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: getOperators
### Title: Get the operator definitions from a StanFord2010 xml document
### Aliases: getOperators

### ** Examples

pth <- system.file(package = "sf2010r")
hprfiles <-  list.files(pth,".hpr",recursive=TRUE,full.names=TRUE)
doc <- xml2::read_xml(hprfiles[1])
getOperators(doc)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("getOperators", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("getProductDef")
### * getProductDef

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: getProductDef
### Title: Product def from product definition nodetree
### Aliases: getProductDef

### ** Examples

pth <- system.file(package = "sf2010r")
hprfiles <-  list.files(pth,".hpr",recursive=TRUE,full.names= TRUE)
doc <- xml2::read_xml(hprfiles[1])
ProductsList <- xml2::xml_find_all(doc, ".//d1:ProductDefinition" )
getProductDef(ProductsList[[1]]) %>% dplyr::glimpse()
plyr::ldply(ProductsList, getProductDef )



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("getProductDef", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("getProductDefs")
### * getProductDefs

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: getProductDefs
### Title: Get all product definitions
### Aliases: getProductDefs

### ** Examples

pth <- system.file(package = "sf2010r")
hprfiles <-  list.files(pth,".hpr",recursive=TRUE,full.names= TRUE)
doc <- xml2::read_xml(hprfiles[1])
getProductDefs(doc)
doc <- xml2::read_xml(hprfiles[2])
getProductDefs(doc)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("getProductDefs", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("getProductMatrixItems")
### * getProductMatrixItems

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: getProductMatrixItems
### Title: Get the Product Matrix items
### Aliases: getProductMatrixItems

### ** Examples

pth <- system.file(package = "sf2010r")
hprfiles <-  list.files(pth,".hpr",recursive=TRUE,full.names=TRUE)
doc <- xml2::read_xml(hprfiles[1])
Productslist <- xml2::xml_find_all(doc, ".//d1:ProductDefinition" )
getProductMatrixItems(Productslist[[1]])



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("getProductMatrixItems", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("getProductMatrixes")
### * getProductMatrixes

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: getProductMatrixes
### Title: Get all price matrixes
### Aliases: getProductMatrixes

### ** Examples

pth <- system.file(package = "sf2010r")
hprfiles <-  list.files(pth,".hpr",recursive=TRUE,full.names= TRUE)
doc <- xml2::read_xml(hprfiles[1])
pms <- getProductMatrixes(doc)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("getProductMatrixes", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("getSTP_diameters")
### * getSTP_diameters

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: getSTP_diameters
### Title: get SingleTreeProcessed tree's diametres
### Aliases: getSTP_diameters

### ** Examples

pth <- system.file(package = "sf2010r")
hprfiles <- list.files(pth,".hpr",recursive=TRUE,full.names= TRUE)
doc <- xml2::read_xml(hprfiles[1])
getSTP_diameters(doc)
doc <- xml2::read_xml(hprfiles[2])
getSTP_diameters(doc)
doc <- xml2::read_xml(hprfiles[3])
getSTP_diameters(doc)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("getSTP_diameters", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("getSTPlogs")
### * getSTPlogs

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: getSTPlogs
### Title: Fetch the single tree processed logs from one stem node tree
### Aliases: getSTPlogs

### ** Examples

pth <- system.file(package = "sf2010r")
hprfiles <- list.files(pth,".hpr",recursive=TRUE,full.names=TRUE)
doc <- xml2::read_xml(hprfiles[1])
stemlist <- xml2::xml_find_all(doc, ".//d1:Stem")
pcat <- ".//d1:ProcessingCategory"
wtch <-  which(xml2::xml_text(xml2::xml_find_all(stemlist,pcat))=="MultiTreeProcessing")
getSTPlogs(stemlist[1]) %>% dplyr::glimpse()
if(length(wtch) > 0) { getSTPlogs(stemlist[wtch[1]]) %>% dplyr::glimpse()}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("getSTPlogs", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("getSpeciesGroupDef")
### * getSpeciesGroupDef

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: getSpeciesGroupDef
### Title: Species group def from one SpeciesGroupDefinition node
### Aliases: getSpeciesGroupDef

### ** Examples

pth <- system.file(package = "sf2010r")
hprfiles <-  list.files(pth,".hpr",recursive=TRUE,full.names=TRUE)
doc <- xml2::read_xml(hprfiles[1])
SpeciesList <- xml2::xml_find_all(doc, ".//d1:SpeciesGroupDefinition" )
getSpeciesGroupDef(SpeciesList[[1]])
species <- plyr::ldply(SpeciesList, getSpeciesGroupDef)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("getSpeciesGroupDef", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("getSpeciesGroupDefinitions")
### * getSpeciesGroupDefinitions

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: getSpeciesGroupDefinitions
### Title: Get all species definitions within a SF2010 doc
### Aliases: getSpeciesGroupDefinitions

### ** Examples

pth <- system.file(package = "sf2010r")
hprfiles <-  list.files(pth,".hpr",recursive=TRUE,full.names=TRUE)
doc <- xml2::read_xml(hprfiles[1])
getSpeciesGroupDefinitions(doc)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("getSpeciesGroupDefinitions", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("getStemGrades")
### * getStemGrades

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: getStemGrades
### Title: Fetch the stem grades for each stem in hpr
### Aliases: getStemGrades

### ** Examples

pth <- system.file(package = "sf2010r")
hprfiles <- list.files(pth,".hpr",recursive = TRUE, full.names= TRUE)
doc <- xml2::read_xml(hprfiles[1])
stemlist <- xml2::xml_find_all(doc, ".//d1:Stem")
getStemGrades(stemlist[[1]])
plyr::ldply(stemlist[1:3], getStemGrades)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("getStemGrades", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("getStemTypeDefs")
### * getStemTypeDefs

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: getStemTypeDefs
### Title: get stem type definitions from one SpeciesGroupDefinition
### Aliases: getStemTypeDefs

### ** Examples

pth <- system.file(package = "sf2010r")
hprfiles <-  list.files(pth,".hpr",recursive=TRUE,full.names= TRUE)
doc <- xml2::read_xml(hprfiles[1])
SpeciesList <- xml2::xml_find_all(doc, ".//d1:SpeciesGroupDefinition" )
getStemTypeDefs(SpeciesList[[1]])



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("getStemTypeDefs", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("getStemTypes")
### * getStemTypes

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: getStemTypes
### Title: Get all stem type definitions for all SpeciesGroupDefinitions
### Aliases: getStemTypes

### ** Examples

pth <- system.file(package = "sf2010r")
hprfiles <-  list.files(pth,".hpr",recursive=TRUE,full.names=TRUE)
doc <- xml2::read_xml(hprfiles[1])
getStemTypes(doc)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("getStemTypes", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("getStemdata")
### * getStemdata

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: getStemdata
### Title: Stem data from one Stem node
### Aliases: getStemdata

### ** Examples

pth <- system.file(package = "sf2010r")
hprfiles <- list.files(pth,".hpr",recursive=TRUE,full.names=TRUE)
doc <- xml2::read_xml(hprfiles[3])
stemlist <- xml2::xml_find_all(doc, ".//d1:Stem")
getStemdata(stemlist[[1]]) %>% dplyr::glimpse()
plyr::ldply(stemlist[1:10], getStemdata)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("getStemdata", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("getStems")
### * getStems

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: getStems
### Title: Get stemdata for all stems within a SF2010 .hpr file
### Aliases: getStems

### ** Examples

pth <- system.file(package = "sf2010r")
hprfiles <- list.files(pth,".hpr",recursive=TRUE,full.names= TRUE)
doc <- xml2::read_xml(hprfiles[3])
getStems(doc)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("getStems", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("getStemsAndLogs")
### * getStemsAndLogs

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: getStemsAndLogs
### Title: Fetch all stems and all logs in hpr
### Aliases: getStemsAndLogs

### ** Examples

pth <- system.file(package = "sf2010r")
hprfiles <- list.files(pth,".hpr",recursive=TRUE,full.names=TRUE)
doc <- xml2::read_xml(hprfiles[1])
stl <- getStemsAndLogs(doc)
doc <- xml2::read_xml(hprfiles[2])
stl <- getStemsAndLogs(doc)
doc <- xml2::read_xml(hprfiles[3])
stl <- getStemsAndLogs(doc)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("getStemsAndLogs", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("getTracking.data")
### * getTracking.data

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: getTracking.data
### Title: Tracking data from mom-files#'
### Aliases: getTracking.data

### ** Examples

pth <- system.file(package = "sf2010r")
momfiles <- list.files(pth,".mom",ignore.case=TRUE,recursive=TRUE,full.names=TRUE)
doc <- xml2::read_xml(momfiles[3])#'
getTracking.data(doc) %>% dplyr::glimpse()
pth <- system.file(package = "sf2010r")
momfiles <- list.files(pth,".mom",ignore.case=TRUE,recursive=TRUE,full.names= TRUE)
doc <- xml2::read_xml(momfiles[3])#'
getTracking.data(doc) %>% dplyr::glimpse()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("getTracking.data", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_Harv_data")
### * get_Harv_data

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_Harv_data
### Title: Recursive processing of other functions in the package
### Aliases: get_Harv_data

### ** Examples

pth <- list.files(path =  system.file(package = "sf2010r"),
pattern = ".", recursive = TRUE, full.names= TRUE)
tmp <- get_Harv_data(pth[4:5])



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_Harv_data", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("hprdata")
### * hprdata

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: hprdata
### Title: Hpr-file reader function
### Aliases: hprdata

### ** Examples

hprfiles <- list.files(path =  system.file(package = "sf2010r"),
pattern = ".hpr", recursive = TRUE, full.names= TRUE)
hprtest1 <- hprdata(hprfiles[1])
hprtest2 <- hprdata(hprfiles[2])
hprtest3 <- hprdata(hprfiles[3])



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("hprdata", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("price_matr_entry_base_log_class")
### * price_matr_entry_base_log_class

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: price_matr_entry_base_log_class
### Title: finding price matrix entry for base logs
### Aliases: price_matr_entry_base_log_class

### ** Examples

pth <- system.file(package = "sf2010r")
hprfiles <-  list.files(pth,".hpr",recursive=TRUE,full.names=TRUE)
doc <- xml2::read_xml(hprfiles[1])
pricematrixes <- getProductMatrixes(doc)
price_matr_entry_base_log_class(pricematrixes)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("price_matr_entry_base_log_class", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("sf2010r_example")
### * sf2010r_example

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: sf2010r_example
### Title: Get path to Stanford2010 example files in the package
### Aliases: sf2010r_example

### ** Examples

sf2010r_example()
sf2010r_example(fileending = "hpr")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("sf2010r_example", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("xml_childs_dt")
### * xml_childs_dt

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: xml_childs_dt
### Title: Make a data table of node children values and corresponding
###   attribute values
### Aliases: xml_childs_dt

### ** Examples

pth <- system.file(package = "sf2010r")
hprfiles <-  list.files(pth,".hpr",recursive=TRUE,full.names=TRUE)
doc <- xml2::read_xml(hprfiles[1])
x <- xml2::xml_children(doc)[2] # Get one node of doc, the first is normally a header
xml_childs_dt(x[[1]]) # The function returns a data.table
x <- xml2::xml_children(x)
xml_childs_dt(x[[27]])
 x <- xml2::xml_children(x[[27]])
xml_childs_dt(x)
x %>% purrr::map_dfr( ~ xml_childs_dt(.x))  # Converted to tibble



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("xml_childs_dt", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("xml_childs_nchr")
### * xml_childs_nchr

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: xml_childs_nchr
### Title: Make a named character vector of children values
### Aliases: xml_childs_nchr

### ** Examples

pth <- system.file(package = "sf2010r")
hprfiles <-  list.files(pth,".hpr",recursive=TRUE,full.names=TRUE)
doc <- xml2::read_xml(hprfiles[1])
x <-  xml2::xml_children(doc)[1] # Get one node of doc, the first is normally a header
xml_childs_nchr(x[[1]]) # The function returns a named vector
x %>% purrr::map_dfr( ~ xml_childs_nchr(.x))  # Converted to tibble



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("xml_childs_nchr", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
