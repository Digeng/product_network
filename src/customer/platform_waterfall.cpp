#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
std::vector<std::string> calculate_platform_waterfall_type_cpp(NumericVector platform, NumericVector platform_lag) {
  std::vector<std::string> waterfall_vec;
  
  long long n = platform.size(); 
  
  for (long long i = 0; i < n; i++){ 
    if (platform_lag[i] == 0 && platform[i] == 1)
      waterfall_vec.push_back("platform_cust_add");
    
    else if (platform_lag[i] == 1 && platform[i] == 1)
      waterfall_vec.push_back("platform_cust_no_change");
    
    else if (platform_lag[i] == 1 && platform[i] == 0) 
      waterfall_vec.push_back("platform_cust_loss");
    
    else if (platform_lag[i] == 0 && platform[i] == 0)
      waterfall_vec.push_back("platform_cust_null");
    
    else 
      stop("something wrong");
  }
  
  return waterfall_vec;
}



