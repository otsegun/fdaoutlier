## Submission
This is an update/resubmission of the fdaoutlier package. The package was archived
on 2023-07-11 due to some issues with the recent Fedora llvm Clang 17 which were not
fixed in time.

This submission is an attempt to get the package back on CRAN. I believe the clang issue was because of including an R header before including the C++ std headers used in the C++ compiled code. This has been fixed and tests on online Clang test platforms seem to show it works.

# R CMD check results and Test Environments

## Local test

- Ubuntu 22.04.3 LTS x86_64-pc-linux-gnu (64-bit)
  - C Compiler: gcc (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0
  - C++ Compiler: g++ (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0
  - Results:  0 errors | 0 warnings | 0 notes 

## Cloud test environments

- Rhub2 Clang ASAN (via Github Actions): Ubuntu 22.04.3 LTS
  - C/C++ Compiler: Ubuntu clang version 16.0.6
  - Results:  0 errors | 0 warnings | 0 notes 

- Rhub2 Clang 17 (via Github Actions): Ubuntu 22.04.3 LTS
  - C/C++ Compiler: Ubuntu clang version 17.0.2
  - Results:  0 errors | 0 warnings | 0 notes 
  
- Rhub2 Clang 16 (via Github Actions): Ubuntu 22.04.3 LTS
  - C/C++ Compiler: ‘Ubuntu clang version 16.0.6
  - Results:  0 errors | 0 warnings | 0 notes 

- R-hub: Debian Linux, R-devel, GCC ASAN/UBSAN
  - C/C++ Compiler: gcc/g++ (Debian 12.2.0-14) 12.2.0
  - Results:  0 errors | 0 warnings | 0 notes 
  
- winbuilder: Windows Server 2022 x64 (build 20348), R version 4.3.1 (2023-06-16 ucrt)
  - C/C++ Compiler: gcc.exe/g++.exe (GCC) 12.2.0
  - Results:  0 errors | 0 warnings | 1 note
  - Explanation:
  ```
  checking CRAN incoming feasibility ... [9s] NOTE
  Maintainer: 'Oluwasegun Taiwo Ojo <seguntaiwoojo@gmail.com>'
  
  Possibly misspelled words in DESCRIPTION:
  Dai (20:3, 21:14, 24:3)
  Genton (20:11, 21:22)
  al (24:10)
  boxplot (25:56)
  et (24:7)
  outlyingness (19:43)

  CRAN repository db overrides:
    X-CRAN-Comment: Archived on 2023-07-11 as issues were not corrected
      in time.
  ```
  I suspect this note is because I am trying to resubmit/send an update on an archived
  package, rather than an indication of an actual problem with the submitted
  package. The possibly misspelt words have been checked and they are correct.
  
- R-hub: Ubuntu Linux 20.04.1 LTS, R-release, GCC
  - C/C++ Compiler: gcc/g++ (Ubuntu 9.4.0-1ubuntu1~20.04.1) 9.4.0
  - Results:  0 errors | 0 warnings | 2 notes 
  - Explanation: First note was about `incoming CRAN feasibily` mentioned above
  and the second note is about `checking HTML version of manual` which raised a 
  NOTE because of the following: 
  ```
  Skipping checking HTML validation: no command 'tidy' found
  Skipping checking math rendering: package 'V8' unavailable
  ```
  The first point is because the HTML tidy software is not installed in the test
  environment and the second note is because the package V8 is not installed
  on the environment (so a check to see if HTML math rendering via KaTeX works 
  couldn't be done).

- R-hub: Fedora Linux, R-devel, clang, gfortran
  - C/C++ Compiler: clang version 14.0.5 (Fedora 14.0.5-2.fc36)
  - Results:  0 errors | 0 warnings | 2 notes 
  - Explanation: The 2 notes are as the same as above. An `incoming CRAN feasibility`
  note and a `checking HTML version of manual` note.
  
- RProject MacOS builder: Fedora Linux, R-devel, clang, gfortran
  - C/C++ Compiler: Apple clang version 14.0.3 (clang-1403.0.22.14.1)
  - Results:  0 errors | 0 warnings | 0 notes 

## Downstream dependencies
There are currently no downstream dependencies for this package
