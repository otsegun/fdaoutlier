#include <Rinternals.h>
using namespace std;

extern "C" {
  SEXP extremeRank(SEXP dtsTRank, SEXP nn, SEXP dd){
    //derefence pointers
    int n = Rf_asInteger(nn);
    int d = Rf_asInteger(dd);

    // create pointers to input vectors
    double* dtsTV = REAL(dtsTRank);

    // set up result
    SEXP depth_return = PROTECT(Rf_allocVector(REALSXP, n));
    double* depths = REAL(depth_return);
    // write 1s in result
    for (int i = 0; i < n; i++){
      depths[i] = 1;
    }
    // compare
    for (int s = 0; s < n; s++){
     for(int t = s+1; t < n; t++){
       int dchecked = 0;
       bool tiebroken = false;
       while(tiebroken == false && dchecked < d){
         if(dtsTV[dchecked * n + t] < dtsTV[dchecked * n + s]){
           tiebroken = true;
           depths[s] += 1;
         } else if(dtsTV[dchecked * n + t] > dtsTV[dchecked * n + s]){
           tiebroken = true;
           depths[t] += 1;
         } else{
           dchecked += 1;
         }
       }
       if(!tiebroken){
         depths[t] += 0.5;
         depths[s] += 0.5;
       }
     }

    }
    UNPROTECT(1);
    return depth_return;
  }
}
