#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
void projectionDepth(const NumericMatrix& dts, const NumericMatrix& dt, const NumericMatrix& directions,
                     const int& m, const int& n, const int& d, const int& k, NumericVector& depths){
  //create projection matrix
  NumericMatrix dtProjection(k, n);
  NumericMatrix dtsProjection(k, m);
  // matrix to hold (u'x - med(u'X))/mad(u'X) for all the k directions.
  NumericMatrix dtsDtProjection(k, m);

  // project directions on dt
  for (int s = 0; s < k; s++){
    for (int t = 0; t < n; t++){
      double sum = 0;
      for (int u = 0; u < d; u++){
        sum += directions(s, u) * dt(t, u);
      }
      dtProjection(s,t) = sum;
    }
  }


  // project directions on dts
  for(int s = 0; s < k; s++){
    for (int t = 0; t < m; t++){
      double sum = 0;
      for (int u = 0; u < d; u++){
        sum += directions(s, u) * dts(t, u);
      }
      dtsProjection(s,t) = sum;
    }
  }


  // compute projection depth of all direction
  for (int i = 0; i < k; i++){
    NumericVector dtProjectionRow = dtProjection(i, _);
    double med = Rcpp::median(dtProjectionRow); // compute median
    dtProjectionRow = Rcpp::abs(dtProjectionRow - med);
    double mad = median(dtProjectionRow);
    dtsDtProjection(i, _) = (dtsProjection(i, _) - med)/mad;
    // for (int j = 0; j < m; j++){
    //   dtsDtProjection(i, j) = (dtsProjection(i, j) - med)/mad;
    // }
  }


  // find column-wise minimum of maximum of dtsDtProjection
  for (int col = 0; col < m; col++){
    depths(col) = 1/(1+ Rcpp::max(dtsDtProjection(_, col)));
  }

}
