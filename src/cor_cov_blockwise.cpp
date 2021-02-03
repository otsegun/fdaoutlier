# include "jumpstart.h"
using namespace std;

extern "C" {
  // compute the three indices
  SEXP corCovBlock(SEXP dtScaled, SEXP meanVec, SEXP varVec, SEXP stdVec,
                    SEXP cIni, SEXP cEnd, SEXP nn, SEXP dd){
    // derefence pointers
    int cini = Rf_asInteger(cIni) - 1;
    int cend = Rf_asInteger(cEnd) - 1;
    int n = Rf_asInteger(nn);
    int d = Rf_asInteger(dd);
    const int ksize = cend - cini + 1;
    // create pointers to imput vectors
    double* dtsTV = REAL(dtScaled);
    double* meanV = REAL(meanVec);
    double* varV = REAL(varVec);
    double* stdV = REAL(stdVec);

    // setup return result
    SEXP preIndices = PROTECT(Rf_allocVector(REALSXP, ksize * 3));
    // pointer to return result
    double* kaccess = REAL(preIndices);
    // assign zeros to preindices
    for (int madrid = 0; madrid < (ksize * 3); madrid++){
      kaccess[madrid] = 0;
      }
    // compute preindices
    for(int i = 0; i < n; i++){
      for (int j = 0; j < ksize; j++){
        long double sXY = 0;
        // compute covariance between X and Y pairs
        for (int r = 0; r < d; r++){
          // look for how to access dtsTV directly
          sXY += dtsTV[r*n+i] * dtsTV[r*n+j+cini]; // sum[(X-Xbar)(Y-Ybar)]
        }
        long double aux = sXY/(d-1);
        // rolling mean
        kaccess[j*3] += (aux / stdV[i] - kaccess[j*3])/(i+1);
        kaccess[j*3+1] += (aux * (meanV[i]/varV[i]) - kaccess[j*3+1])/(i+1);
        kaccess[j*3+2] += (aux / varV[i] - kaccess[j*3+2])/(i+1);
      }
    }

    UNPROTECT(1);
    return(preIndices);

    }




}
