
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fdalite

Outlier Detection Tools for Functional Data

<!-- badges: start -->

[![Build
Status](https://travis-ci.com/otsegun/fdalite.svg?token=is9BrWwNvqBGoneFkbEL&branch=master)](https://travis-ci.com/otsegun/fdalite)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Codecov test
coverage](https://codecov.io/gh/otsegun/fdalite/branch/master/graph/badge.svg)](https://codecov.io/gh/otsegun/fdalite?branch=master)
<!-- badges: end -->

`fdalite` is a collection of outlier detection tools for functional data
analysis. `fdalite` is still at an early experimental stage.

## Installation

Since `fdalite` is still at an experimental stage, you can only install
the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("otsegun/fdalite")
```

## Example

``` r
library(fdalite)
data(sim_data1)
# MSPLOT Dai & Genton (2018)
msplot(sim_data1$data, data_depth = "random_projection")$outliers_index
#>  [1]  4 12 17 23 40 59 70 79 82 83 84 93

sim_data1$true_outliers
#>  [1]  4 12 17 23 40 59 79 82 83 84
```

## Methods Implemented

1.  MS-Plot (Dai & Genton, 2018)
