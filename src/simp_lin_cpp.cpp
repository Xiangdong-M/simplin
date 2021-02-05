#include <Rcpp.h>
#include <Rmath.h>
using namespace Rcpp;

// [[Rcpp::export]]
List simp_lin_cpp (NumericVector x, NumericVector y){
// calculate estimates of coefficients
  double mean_x = mean(x);
  double mean_y = mean(y);
  double beta_1 = sum((x-mean_x)*(y-mean_y))/sum((x-mean_x)*(x-mean_x));
  double beta_0 = mean_y - mean_x * beta_1;
// calculate predicted values 
  NumericVector y_hat = beta_0 + beta_1 * x;
// calculate residuals 
  NumericVector residuals = y - y_hat;
// calculate SE
  int n = x.length();
  double MSE = sum(residuals * residuals)/(n-2);
  double se_beta_1 = sqrt(MSE)*sqrt(n/(n*sum(x*x)-pow(sum(x),2)));
  double se_beta_0 = sqrt(MSE)*sqrt(sum(x*x)/(n*sum(x*x)-pow(sum(x),2)));
// compute CI
  double t = R::qt( 0.975,n-2,true,false );
  double LB_beta_1 = beta_1 - se_beta_1 * t;
  double UB_beta_1 = beta_1 + se_beta_1 * t;
  double LB_beta_0 = beta_0 - se_beta_0 * t;
  double UB_beta_0 = beta_0 + se_beta_0 * t;
// change names 
  NumericVector CI = NumericVector::create(LB_beta_0,LB_beta_1,UB_beta_0,UB_beta_1);
  CI.attr("dim") = Dimension(2,2);
  NumericMatrix ci = as<NumericMatrix>(CI);
  CharacterVector rn = CharacterVector::create("beta_0","beta_1");
  CharacterVector cn = CharacterVector::create("2.5%","97.5%");
  rownames(ci) = rn;
  colnames(ci) = cn;
  NumericVector coef = NumericVector::create(Named("beta_0")=beta_0,Named("beta_1")=beta_1);
  NumericVector SE = NumericVector::create(Named("SE(beta_0)")=se_beta_0,Named("SE(beta_1)")=se_beta_1);
// store outcomes into a list
  List output = List::create(Named("coef")=coef,
                             Named("SE")=SE,
                             Named("CI")=ci,
                             Named("residuals")=residuals,
                             Named("predicts")=y_hat);
  return output;
  }


