
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fdalite

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/otsegun/fdalite.svg?branch=master)](https://travis-ci.org/otsegun/fdalite)
<!-- badges: end -->

fdalite is a collection of outlier detection and visualization function
for functional data. fdalit is still at an early experimental stage.

## Installation

`r{ echo = F, eval = F} #You can install the released version of fdalite
from [CRAN](https://CRAN.R-project.org) with:
install.packages("fdalite")`

Since fdalit is still at an experimental stage, you can only install the
development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("otsegun/fdalite")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(fdalite)
## basic example code
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
up-to-date.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub\!
