require('sf2010r')

hprfiles <- list.files(path =  system.file(package = "sf2010r"),
pattern = ".hpr", recursive = TRUE, full.names= TRUE)
hprtest1 <- hprdata(hprfiles[1])

hprtest2 <- hprdata(hprfiles[2])
hprtest3 <- hprdata(hprfiles[3])

system.time(
hprtests <- purrr::map_dfr(hprfiles, ~tibble::tibble(filename = .x,
 hprdata = list(hprdata(.x))))
)

str(hprtests)



system.time(
hprtests1 <- Map(function(x)tibble::tibble(hprdata(x)), hprfiles[1:2])
)

str(hprtests1)


