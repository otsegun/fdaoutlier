#include <R.h>
#include <Rinternals.h>
#include <vector>
#include <algorithm>
using namespace std;


extern "C" {
  void customMedian(vector<double> &rowVector, double &median, const int len){
    int position = len / 2;
    std::nth_element(rowVector.begin(), rowVector.begin() + position, rowVector.end());
    median = rowVector[position];
  }
  
  void customMAD(vector<double> &rowVector, double &mad, const double &medn,  const int len){
    /* Obtain median absolute deviation (from median) (MAD) */
    vector<double> devs(len);
    for (int i = 0; i < len; i++){
      devs[i] = std::abs(rowVector[i] - medn);
    }
    int position = len/2;
    std::nth_element(devs.begin(), devs.begin() + len/2, devs.end());
    mad = devs[position];
  }
  
  void deleteMatrix(double** mat, int numRow){
    for(int i = 0; i < numRow; i++) {
      delete[] mat[i];
    }
    delete [] mat;
  }
  
  
  
  SEXP projectionDepth(SEXP dts, SEXP dt,  SEXP direction,
                    SEXP mm, SEXP nn, SEXP dd, SEXP kk) {
    //int * ary = new int[nRows*nColumns] and indexing via
    // ary[iRow*nColumns + iColumns]
    
    
    //derefence pointers
    int n = Rf_asInteger(nn);
    int m = Rf_asInteger(mm);
    int d = Rf_asInteger(dd);
    int k = Rf_asInteger(kk);
    
    // create pointers to input vectors
    double* dtsV = REAL(dts);
    double* dtV = REAL(dt);
    double* directionV = REAL(direction);
    
    // set up data
    double** dtsMatrix = new double*[m];
    double** dtMatrix = new double*[n];
    double** directionsMatrix = new double*[k];
    
    double** dtsProjectionMatrix = new double*[k];
    double** dtProjectionMatrix = new double*[k];
    double** dtsDtProjectionMatrix = new double*[k];
    
    // allocate matrix
    for (int i = 0; i < m; i++){
      dtsMatrix[i] = dtsV + i*(d);
    }
    for (int i = 0; i < n; i++){
      dtMatrix[i] = dtV + i*(d);
    }
    for (int i = 0; i < k; i++){
      directionsMatrix[i] = directionV + i*(d);
    }
    
    // SEXP depth_return;
    // double* depths;
    // PROTECT(depth_return = NEW_NUMERIC(m));
    // depths = NUMERIC_POINTER(depth_return);
    
    
    SEXP depth_return = PROTECT(Rf_allocVector(REALSXP, m));
    double* depths = REAL(depth_return);
    
    // project directions on dt
    
    for (int s = 0; s < k; s++){
      dtProjectionMatrix[s] = new double[n];
      for (int t = 0; t < n; t++){
        double sumEntry = 0;
        for (int u = 0; u < d; u++){
          sumEntry += directionsMatrix[s][u] * dtMatrix[t][u];
        }
        dtProjectionMatrix[s][t] = sumEntry;
      }
    }
    
    // project directions on dts
    for(int s = 0; s < k; s++){
      dtsProjectionMatrix[s] = new double[m];
      for (int t = 0; t < m; t++){
        double sumEntry = 0;
        for (int u = 0; u < d; u++){
          sumEntry += directionsMatrix[s][u] * dtsMatrix[t][u];
        }
        dtsProjectionMatrix[s][t] = sumEntry;
      }
    }
    
    // compute projection depth of all direction
    for (int i = 0; i < k; i++){
      dtsDtProjectionMatrix[i] = new double[m];
      double med, mad;
      std::vector<double> dtsDtProjectionRow(dtProjectionMatrix[i], dtProjectionMatrix[i] + n);
      customMedian(dtsDtProjectionRow, med, n);
      customMAD(dtsDtProjectionRow, mad, med, n);
      for (int j = 0; j < m; j++){
        dtsDtProjectionMatrix[i][j] = (dtsProjectionMatrix[i][j] - med)/mad;
      }
    }
    
    // find column-wise minimum of maximum of dtsDtProjection
    for (int col = 0; col < m; col++){
      depths[col] = dtsDtProjectionMatrix[0][col];
    }
    
    for (int row = 1; row < k; row++){
      for (int col = 0; col < m; col++){
        if (dtsDtProjectionMatrix[row][col] > depths[col]){
          depths[col] = dtsDtProjectionMatrix[row][col];
        }
      }
    }
    
    for (int col = 0; col < m; col++){
      depths[col] = 1/(1 + depths[col]);
    }
    // clean up
    delete[] dtsMatrix;
    delete[] dtMatrix;
    delete[] directionsMatrix;
    // clean up matrix
    deleteMatrix(dtsProjectionMatrix, k);
    deleteMatrix(dtProjectionMatrix, k);
    deleteMatrix(dtsDtProjectionMatrix, k);
    
    UNPROTECT(1);
    return depth_return;
    
  }
}

