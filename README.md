
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sf2010r

<!-- badges: start -->
<!-- badges: end -->

The goal of sf2010r is to read StanForD2010 forest machine reports and
parse these to data structures suitable for R. Currently the package
provide functions to read .hpr and .mom files. The functions have been
tested only for a few example files of each category. If they fail
please send me an demail to beh at nibio.no, and please attach the file
to parse.

## Installation

You can install the development version of sf2010r from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("hbelbo/sf2010r")
```

## Example

This is a basic example which shows you how to use:

``` r
library(sf2010r)
## basic example code



# Get a list of the first three example files provided in the packagehttps://mail.google.com/mail/u/0/#search/miele/FMfcgxmSdZDpKdbMpMbsxFtgFmrxNdfh?projector=1&messagePartId=0.1
sf2010r_example()[1:3]
#> [1] "FPR_V0301_PonsseOpti4G_04761.fpr"       
#> [2] "FPR_V0303_MaxiXT_0107_20220406__1_1.fpr"
#> [3] "HPR_V0201_MaxiXplorer_0310_20170309.hpr"
# Get a list of the "hpr" example files provided in the package
hprfiles <- sf2010r_example(fileending = "hpr")
hprtest1 <- hpr_file_readr(hprfiles[1])
#> 
#> -hpr_file_readr() parsing  C:/Users/hbel/AppData/Local/Temp/RtmpIN9Ppo/temp_libpath2d706a1938b/sf2010r/extdata/HPR_V0201_MaxiXplorer_0310_20170309.hpr - 
#>  -hpr_file_readr-getSpeciesGroupDefs- 
#>  -hpr_file_readr-getProductDefs- 
#>  -hpr_file_readr-getPricematrixes 
#>  -hpr_file_readr-getStemTypes; 
#>  -hpr_file_readr-getStemsAndLogs;
```

Remenber to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()`
