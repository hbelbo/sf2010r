
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

This is a basic example which shows you how to solve a common problem:

``` r
library(sf2010r)
## basic example code



# Get a list of the first three example files provided in the package
sf2010r_example()[1:3]
#> [1] "C:/Users/hbel/AppData/Local/Temp/Rtmp0MTcyG/temp_libpath48cc379f1d56/sf2010r/extdata/bullshit.txt"                           
#> [2] "C:/Users/hbel/AppData/Local/Temp/Rtmp0MTcyG/temp_libpath48cc379f1d56/sf2010r/extdata/FPR_V0303_MaxiXT_0107_20220406__1_1.fpr"
#> [3] "C:/Users/hbel/AppData/Local/Temp/Rtmp0MTcyG/temp_libpath48cc379f1d56/sf2010r/extdata/HPR_V2_1_MaxiXplorer_3_10_20170309.hpr"
# Get a list of the "hpr" example files provided in the package
hprfiles <- sf2010r_example(fileending = "hpr")
hprtest1 <- hprdata(hprfiles[1])
#> getStemsAndLogs-getStemdata getStemsAndLogs-getSTPlogs getStemsAndLogs-getMTPlogs getStemsAndLogs-getStemGrades getStemsAndLogs-getSTP_diameters
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/v1/examples>.
