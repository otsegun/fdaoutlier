
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fdaoutlier

Outlier Detection Tools for Functional Data Analysis

<!-- badges: start -->

[![Build
Status](https://travis-ci.com/otsegun/fdaoutlier.svg?token=is9BrWwNvqBGoneFkbEL&branch=master)](https://travis-ci.com/otsegun/fdaoutlier)
[![Codecov test
coverage](https://codecov.io/gh/otsegun/fdaoutlier/branch/master/graph/badge.svg)](https://codecov.io/gh/otsegun/fdaoutlier?branch=master&token=0c40801f-206f-4bb4-9b3b-5f493a3130c9)
[![Lifecycle:
experimental](https://lifecycle.r-lib.org/articles/figures/lifecycle-experimental.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![CRAN
status](https://www.r-pkg.org/badges/version/fdaoutlier)](https://CRAN.R-project.org/package=fdaoutlier)
[![CRAN
downloads](https://cranlogs.r-pkg.org/badges/grand-total/fdaoutlier)](https://cran.r-project.org/package=fdaoutlier)
[![Licence](https://img.shields.io/badge/licence-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)
<!-- badges: end --> `fdaoutlier` is a collection of outlier detection
tools for functional data analysis. Methods implemented include
directional outlyingness, MS-plot, total variation depth, and sequential
transformations among others.

## Installation

You can install the current version of fdaoutliers from CRAN with:

``` r
install.packages("fdaoutlier")
```

or the latest the development version from [GitHub](https://github.com/)
with:

``` r
devtools::install_github("otsegun/fdaoutlier")
```

## Example

Generate some functional data with magnitude outliers:

``` r
library(fdaoutlier)
simdata <- simulation_model1(plot = T, seed = 1)
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" style="display: block; margin: auto;" />

``` r
dim(simdata$data)
#> [1] 100  50
```

Next apply the msplot of Dai & Genton (2018)

``` r
ms <- msplot(simdata$data)
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" style="display: block; margin: auto;" />

``` r
ms$outliers
#> [1]  4  7 17 26 29 55 62 66 76
simdata$true_outliers
#> [1]  4  7 17 55 66
```

## Methods Implemented

1.  MS-Plot (Dai & Genton, 2018)
2.  TVDMSS (Huang & Sun, 2019)
3.  Extremal depth (Narisetty & Nair, 2016)
4.  Extreme rank length depth (Myllymäki et al., 2017; Dai et al., 2020)
5.  Directional quantile (Myllymäki et al., 2017; Dai et al., 2020)
6.  Fast band depth and modified band depth (Sun et al., 2012)
7.  Directional Outlyingness (Dai & Genton, 2019)
8.  Sequential transformation (Dai et al., 2020)

## Bugs and Feature Requests

Kindly open an issue using [Github
issues](https://github.com/otsegun/fdaoutlier/issues).
