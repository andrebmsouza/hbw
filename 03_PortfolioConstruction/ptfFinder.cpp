#include <Rcpp.h>
#include <set>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector getPtf2Cpp(NumericVector top_prob, NumericVector bot_prob, IntegerVector date, NumericVector RET, double lambda_w, double lambda_l) {
  int n = top_prob.size();
  
  // Precompute constants for efficiency
  double lw_1 = lambda_w / (1 + lambda_w);
  double ll_1 = lambda_l / (1 + lambda_l);
  
  // Vectors for selection
  LogicalVector top_sel(n);
  LogicalVector bot_sel(n);
  
  // Collect all unique dates in a sorted set
  std::set<int> unique_dates(date.begin(), date.end());

  for (int i = 0; i < n; i++) {
    double top_thresh = std::max(
      lw_1, 
      (lambda_w - lambda_l) / (1 + lambda_w) + ((1 + lambda_l) / (1 + lambda_w)) * bot_prob[i]
    );
    
    double bot_thresh = std::max(
      ll_1, 
      (lambda_l - lambda_w) / (1 + lambda_l) + ((1 + lambda_w) / (1 + lambda_l)) * top_prob[i]
    );
    
    top_sel[i] = top_prob[i] > top_thresh;
    bot_sel[i] = bot_prob[i] > bot_thresh;
  }
  
  // Grouping by date and computing returns
  std::map<int, std::vector<double>> top_returns, bot_returns;
  
  for (int i = 0; i < n; i++) {
    if (top_sel[i]) top_returns[date[i]].push_back(RET[i]);
    if (bot_sel[i]) bot_returns[date[i]].push_back(RET[i]);
  }
  
  // Final output vector
  std::vector<double> out_ret;
  
  for (int d : unique_dates) {  // Now iterating in sorted order
    double top_ret = top_returns[d].empty() ? 0 : std::accumulate(top_returns[d].begin(), top_returns[d].end(), 0.0) / top_returns[d].size();
    double bot_ret = bot_returns[d].empty() ? 0 : std::accumulate(bot_returns[d].begin(), bot_returns[d].end(), 0.0) / bot_returns[d].size();
    double ret = top_ret - bot_ret;

    out_ret.push_back(ret);
  }
  
  return wrap(out_ret);
}
