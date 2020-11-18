
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fdaoutlier

Outlier Detection Tools for Functional Data

`fdaoutlier` is a collection of outlier detection tools for functional
data analysis. `fdaoutlier` is still at an early experimental stage.

## Installation

Since `fdaoutlier` is still at an experimental stage, you can only
install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("otsegun/fdaoutlier")
```

## Example

``` r
library(fdaoutlier)
data(sim_data1)
# MSPLOT Dai & Genton (2018)
msplot(sim_data1$data, data_depth = "random_projection")$outliers_index
#>  [1]  4 12 17 23 40 59 70 79 82 83 84 93
sim_data1$true_outliers
#>  [1]  4 12 17 23 40 59 79 82 83 84
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
