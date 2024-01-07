#include <Rcpp.h>
using namespace Rcpp;
using namespace std;
// [[Rcpp::plugins(cpp11)]]

double taylor_solve(double x) {
  return 2 * M_PI / pow(1 + pow(x, 2), 0.5);
}

double equation(double y, double x) {
  return pow(y, 2) + 2 * x * (x + y) * (1 - cos(y)) - 4 * pow(M_PI, 2);
}
/*
double solve_equation(double x) {
  double y_solution = fsolve(equation, 0.5 * M_PI, args=(x,));
  return y_solution[0];
}
*/
// [[Rcpp::export]]
DataFrame spiral_scan_lines(double beam_divergence_angle, double compensate_angle, NumericMatrix FOU) {

  double R = std::max((FOU(1, 0) - FOU(0, 0)) / 2, (FOU(1, 1) - FOU(0, 1)) / 2);
  NumericVector theta = {0};
  double current_theta = theta[0];
  NumericVector rho = {0};
  double rho_current = rho[0];
  double step_length = beam_divergence_angle / pow(2, 0.5) - compensate_angle;
  while (rho_current <= R) {
    double dt = taylor_solve(current_theta);
    current_theta += dt;
    theta.push_back(current_theta);

    rho_current = step_length * 0.5 / M_PI * current_theta;
    rho.push_back(rho_current);
  }

  NumericVector x(theta.size()), y(theta.size());
  for (int i=0; i<theta.size(); i++) {
    double r = rho[i];
    double alpha = theta[i];
    x[i] = r * cos(alpha);
    y[i] = r * sin(alpha);
  }

  double r = beam_divergence_angle * 0.5;
  DataFrame point_locus = DataFrame::create(
    _["x"] = x,
    _["y"] = y,
    _["r"] = r
  );


  return point_locus;
}
