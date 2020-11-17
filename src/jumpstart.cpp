#include "jumpstart.h"


void customMedian(std::vector<double> &rowVector, double &median, const int len){
  int position = len / 2;
  std::nth_element(rowVector.begin(), rowVector.begin() + position, rowVector.end());
  median = rowVector[position];
}

void customMAD(std::vector<double> &rowVector, double &mad, const double &medn,  const int len){
  /* Obtain median absolute deviation (from median) (MAD) */
  std::vector<double> devs(len);
  for (int i = 0; i < len; i++){
    devs[i] = std::abs(rowVector[i] - medn);
  }
  int position = len/2;
  std::nth_element(devs.begin(), devs.begin() + len/2, devs.end());
  mad = devs[position];
}

double customDifference(double x, double y){return std::abs(x - y);}

void deleteMatrix(double** mat, int numRow){
  for(int i = 0; i < numRow; i++) {
    delete[] mat[i];
  }
  delete [] mat;
}
