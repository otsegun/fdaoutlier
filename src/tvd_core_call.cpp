# include "jumpstart.h"
using namespace std;


extern "C" {
  //uses customMedian, customDifference
  SEXP totalVariationDepth(SEXP dtsT, SEXP dts,
                       SEXP nn, SEXP dd){
    //derefence pointers
    int n = Rf_asInteger(nn);
    int d = Rf_asInteger(dd);

    // create pointers to input vectors
    double* dtsTV = REAL(dtsT);
    double* dtsV = REAL(dts);

    // set up data
    double** dtsTMatrix = new double*[d]; // d x n
    double** dtsMatrix = new double*[n]; //n x d

    // res
    SEXP shape_var_return = PROTECT(Rf_allocVector(REALSXP, (d - 1) * n));
    SEXP differenced_data_return = PROTECT(Rf_allocVector(REALSXP, (d) * n));
    double* shapeVariation = REAL(shape_var_return);
    double* diffData = REAL(differenced_data_return);

    // allocate matrix
    for (int i = 0; i < d; i++){
      dtsTMatrix[i] = dtsTV + i*(n);
    }

    for (int i = 0; i < n; i++){
      dtsMatrix[i] = dtsV + i*(d);
    }

    // print matrix
    // for (int i = 0; i < n; i++){
    //   printf("\n Row i: %d \n", i);
    //   for(int j = 0; j < d; j++){
    //     printf("%F, ", dtsMatrix[i][j]);
    //   }
    // }

    // compute total variation
    for (int s = 1; s < d; s++){
      // find median at s
      double med;
      std::vector<double> dtsTRow(dtsTMatrix[s], dtsTMatrix[s] + n);
      customMedian(dtsTRow, med, n);

      // printf("\n column %d median is:  %F \n", s, med);
      for(int t = 0; t < n; t++){
        // recenter prev observation
        double tempPrev = dtsTMatrix[s-1][t] - (dtsTMatrix[s][t] - med);
        //printf("\n tempPrev is: %F, ", tempPrev);
        //printf("Column %d, row %d \n", t,s);

        // check how may of dtsTMatrix[s-1][i] <= tempPrev and dtsTMatrix[s][t]<= med
        int belowPrev = 0, belowPrevCurrent = 0, abovePrevBelowCurrent = 0;
        for(int i = 0; i < n; i++){
          if(i == t){
            belowPrev += 1;
            belowPrevCurrent += 1;
            // printf("i  == t \n");
            // printf("new belowPrev is: %d \n", belowPrev);
          }else{
            bool belowPrevTest = dtsTMatrix[s-1][i] <= tempPrev;
            bool belowPrevCurrentTest = dtsTMatrix[s][i] <= med;

            if(belowPrevTest){
              belowPrev += 1;
            }
            if((belowPrevTest) & (belowPrevCurrentTest)){
              belowPrevCurrent += 1;
            }
            if(!(belowPrevTest) & (belowPrevCurrentTest)) {
              abovePrevBelowCurrent += 1;
            }
          }
        }
        double P1 = (double) belowPrevCurrent *belowPrevCurrent/belowPrev;
        double P2 = 0;
        if(belowPrev != n){
          P2 = (double) abovePrevBelowCurrent * abovePrevBelowCurrent/(n - belowPrev);
        }
        shapeVariation[(s-1)*n + t] = ((P1 + P2)/n - .25)/.25;
        //int * ary = new int[nRows*nColumns] and indexing via ary[iRow*nColumns + iColumns]

      }

  }

    // compute weights
    for(int i = 0; i < n; i++){
      double* diffs = new double[d];
      std::vector<double> dtsRow(dtsMatrix[i], dtsMatrix[i] + d);
      std::adjacent_difference(dtsRow.begin(), dtsRow.end(), diffs, customDifference);
      for(int j = 0; j< d; j++){
        diffData[i*d + j] = diffs[j];
      }
      delete[] diffs;
    }
    const char *names[] = {"shape_variation", "difference_data", ""};
    SEXP reslist = PROTECT(Rf_mkNamed(VECSXP, names));  // creates a list of length 2
    SET_VECTOR_ELT(reslist, 0, shape_var_return); // x and y are arbitrary SEXPs
    SET_VECTOR_ELT(reslist, 1, differenced_data_return);

    // clean up
    delete[] dtsMatrix;
    delete[] dtsTMatrix;

    UNPROTECT(3);
    return reslist;
  }
}

