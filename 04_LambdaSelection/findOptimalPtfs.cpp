#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix findOptimalPtfs(NumericVector ptfs, NumericVector rf) {
  IntegerVector dims = ptfs.attr("dim");
  int rows = dims[0]; // Time periods (T)
  int cols = dims[1]; // First dimension of portfolios
  int depth = dims[2]; // Second dimension of portfolios
  
  NumericMatrix sr_matrix(cols, depth);
  
  // Compute Sharpe Ratio for each (i, j)
  for (int j = 0; j < depth; j++) {
    for (int i = 0; i < cols; i++) {
      NumericVector excess_returns(rows);
      NumericVector asset_returns(rows);
      
      // Extract values for the (i, j) portfolio
      for (int k = 0; k < rows; k++) {
        double ptf_value = ptfs[k + i * rows + j * rows * cols];
        excess_returns[k] = ptf_value - rf[k]; // Mean based on excess returns
        asset_returns[k] = ptf_value; // Standard deviation from raw returns
      }
      
      // Compute mean(excess returns) and sd(asset returns)
      double mean_excess = mean(excess_returns);
      double sd_assets = sd(asset_returns);
      
      // Compute Sharpe ratio (avoid division by zero)
      sr_matrix(i, j) = (sd_assets > 0) ? sqrt(12) * (mean_excess / sd_assets) : NA_REAL;
    }
  }
  
  return sr_matrix;
}
