## Resubmission
This is a resubmission after a failed installation on Solaris.  In this version, I have:

* Fixed the c++ code error that failed to compile on Solaris.
* Tested on Rhub Oracle Solaris 10, x86, 32 bit, R-release (before and after fixing error).

## R CMD check results
There were no ERRORs or WARNINGs. 
   
There was 1 NOTE (from Rhub Fedora and Solaris platforms):

* checking data for non-ASCII characters ... NOTE
  Note: found 75 marked UTF-8 strings
  
  This happened because I have a data containing weather information of Spanish cities
  with UTF-8 strings as names. I believe that this is what triggered this NOTE. I hope 
  that this will not be a serious issue as I've seen the same data used in other R packages
  on CRAN e.g. fda.usc

There was 1 NOTE from a previous CRAN check on r-devel-linux-x86_64-fedora-clang

* checking compilation flags used ... NOTE
  Compilation used the following non-portable flag(s):
  ‘-Werror=implicit-function-declaration’

  I am not sure why this happened but it seems to me the Fedora compiler/build might be
  setting a gcc flag and it shouldn't, because it uses clang. Thus, I suspect a builder 
  misconfiguration because I didn't set any flag in my package, neither do I have any 
  custom Makevars files. Kindly let me know if I am doing something wrong here as I am
  not sure. 
  
## Test environments
* local Ubuntu 20.04.1 LTS 64-bit, R version 3.6.3 (2020-02-29) -- "Holding the Windsock"
* Ubuntu 16.04.6 LTS (on travis ci), R version 4.0.2 (2020-06-22)
* win-builder (devel and release)
* Debian Linux (Rhub), R-devel, GCC ASAN/UBSAN
* Windows Server 2008 R2 SP1 (Rhub), R-devel, 32/64 bit
* Ubuntu Linux 16.04 LTS (Rhub), R-release, GCC
* Fedora Linux (Rhub), R-devel, clang, gfortran

## Downstream dependencies
There are currently no downstream dependencies for this package
