
#' Recursive processing of other functions in the package
#' @param pth set of file paths
#' @param funcs names of other inpackage functions to iterate over
#' @export
#'
#' @examples
#' pth <- list.files(path =  system.file(package = "sf2010r"),
#' pattern = ".", recursive = TRUE, full.names= TRUE)
#' tmp <- get_Harv_data(pth[4:5])

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
