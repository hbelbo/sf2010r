require('sf2010r')
pth <- list.files(path =  system.file(package = "sf2010r"),
 pattern = ".", recursive = TRUE, full.names= TRUE)


get_Harv_data <- function(pth,
                          funcs = c(mom = 'getMom.all',hpr = 'hprdata')){
    casein <- paste0('(?i)', names(funcs))
    collap <- paste(casein, collapse = '|')
    selurl <- pth[grepl(collap, pth)]
    fun_groups <- Map(function(x)
        pth[grepl(x, pth)], casein)
    names(fun_groups) <- funcs
    nmfns <- sapply(fun_groups, length)
    fns2map <- Map(function(x,y)
        rep(x,y), funcs, nmfns)
    unl_nmfns <- unlist(fns2map, use.names = FALSE)
    unl_fun_groups <- unlist(fun_groups, use.names = FALSE)
    out <- Map(function(x,y)
        tryCatch(do.call(x, list(y)), error = function(e)NULL),
        unl_nmfns, unl_fun_groups)
    names(out) <- basename(unl_fun_groups)
    return(out)}

tmp <- get_Harv_data(pth[4:5])


pth
tmp <- get_Harv_data(pth)
names(tmp)
length(tmp)



hprfiles <- list.files(path =  system.file(package = "sf2010r"),
 pattern = ".hpr", recursive = TRUE, full.names= TRUE)
## hprtest1 <- hprdata(hprfiles[1])
hprfiles <- list.files(path =  system.file(package = "sf2010r"),
pattern = ".hpr$", recursive = TRUE, full.names= TRUE)
hprtest2 <- hprdata(hprfiles[1])
## hprt <- Map(function(x)(hprdata(x)), hprfiles[1:2])
pth <- system.file(package = "sf2010r")
momfiles <- list.files(pth,".mom$",recursive=TRUE,ignore.case=TRUE,full.names= TRUE)
momtest1 <- getMom.all(momfiles[1])

require('urltools')

str(url_parse(hprfiles[1L]))


pth <- hprfiles <- list.files(path =  system.file(package = "sf2010r"),
 pattern = ".", recursive = TRUE, full.names= TRUE)
get_url_ext <- function(URL)
tools::file_ext(sub("\\?.+", "", URL))
Map(function(x)
    get_url_ext(x), pth)

funcs <- c(mom = 'getMom.all',hpr = 'hprdata')
casein <- paste0('(?i)', names(funcs))
collap <- paste(casein, collapse = '|')
selurl <- pth[grepl(collap, pth)]
fun_groups <- Map(function(x)
    pth[grepl(x, pth)], casein)
names(fun_groups) <- funcs
nmfns <- sapply(fun_groups, length)
fns2map <- Map(function(x,y)
    rep(x,y), funcs, nmfns)
unl_nmfns <- unlist(fns2map, use.names = FALSE)
unl_fun_groups <- unlist(fun_groups, use.names = FALSE)
out <- Map(function(x,y)
    tryCatch(do.call(x, list(y)), error = function(e)NULL), unl_nmfns, unl_fun_groups)
names(out) <- unl_fun_groups

str(out)

    
    rep(names(x), length(x)), fun_groups)

do.call(names(fun_groups)[1L])


temp <- Map(function(x)
    do.call(fun.,x), tmp[[1]])

tmp <- fun_groups[[2L]][1L]




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



