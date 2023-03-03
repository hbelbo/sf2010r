require('sf2010r')

hprfiles <- list.files(path =  system.file(package = "sf2010r"),
pattern = ".hpr", recursive = TRUE, full.names= TRUE)
hprtest1 <- hprdata(hprfiles[1])

hprt <- Map(function(x)(hprdata(x)), hprfiles[1:2])

pth <- system.file(package = "sf2010r")
momfiles <- list.files(pth,".mom$",recursive=TRUE,ignore.case=TRUE,full.names= TRUE)
momtest1 <- getMom.all(momfiles[1])




lapply(hprt[[1]], class)

do.call(rbind, hprtests1)

hprtest2 <- hprdata(hprfiles[2])
hprtest3 <- hprdata(hprfiles[3])

system.time(
hprtests <- purrr::map_dfr(hprfiles, ~tibble::tibble(filename = .x,
 hprdata = list(hprdata(.x))))
)

str(hprtests)


system.time(
)

lapply(hprtests1, tail)


system.time(
hprtests1 <- Map(function(x)tibble::tibble(hprdata(x)), hprfiles[1:2])
)

str(hprtests1)



