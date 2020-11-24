## Test environments
* local Ubuntu 20.04.1 LTS 64-bit, R version 3.6.3 (2020-02-29) -- "Holding the Windsock"
* Ubuntu 16.04.6 LTS (on travis ci), R version 4.0.2 (2020-06-22)
* win-builder (devel and release)
* Debian Linux (Rhub), R-devel, GCC ASAN/UBSAN
* Windows Server 2008 R2 SP1 (Rhub), R-devel, 32/64 bit
* Ubuntu Linux 16.04 LTS (Rhub), R-release, GCC
* Fedora Linux (Rhub), R-devel, clang, gfortran

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE (from Rhub and win-builder platforms):

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Oluwasegun Taiwo Ojo <seguntaiwoojo@gmail.com>'

  I believe this is a common note for packages submitted first time.

## Downstream dependencies
There are currently no downstream dependencies for this package
