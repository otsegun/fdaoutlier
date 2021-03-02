## Submission
This is an update to the fdaoutlier package.

## R CMD check results
There were no ERRORs or WARNINGs. 
   
There was 1 NOTE (from Rhub Fedora, Window and macOS platforms):

* checking data for non-ASCII characters ... NOTE
  Note: found 75 marked UTF-8 strings
  
  This happened because I have a data containing weather information of Spanish cities
  with UTF-8 strings as names. I believe that this is what triggered this NOTE. I hope 
  that this will not be a serious issue as I've seen the same data used in other R packages
  on CRAN e.g. fda.usc

  
## Test environments
* local Ubuntu 20.04.1 LTS 64-bit, R version 3.6.3 -- "Holding the Windsock" (Develpment PC)
* win-builder (devel)
* Fedora Linux, R-devel, clang, gfortran (Rhub)
* Windows Server 2008 R2 SP1, R-devel, 32/64 bit (Rhub)
* Ubuntu Linux 16.04 LTS, R-release, GCC (Rhub)
* macOS 10.13.6 High Sierra, R-release, CRAN's setup (Rhub)
* Debian Linux, R-devel, GCC ASAN/UBSAN (Rhub)

## Downstream dependencies
There are currently no downstream dependencies for this package
