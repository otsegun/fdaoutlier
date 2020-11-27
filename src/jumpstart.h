#pragma once

#include <Rinternals.h>
#include <vector>
#include <algorithm>
#include <numeric>
#include <cmath>


void customMedian(std::vector<double> &rowVector, double &median, const int len);
void customMAD(std::vector<double> &rowVector, double &mad, const double &medn,  const int len);
double customDifference(double x, double y);
void deleteMatrix(double** mat, int numRow);
