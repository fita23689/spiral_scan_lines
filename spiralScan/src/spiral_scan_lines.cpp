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
//'@title calculate points in the route
//'
//'@param beam_divergence_angle parameter
//'@param compensate_angle parameter
//'@param fou FOU
//'@param point point to scan
//'
//'@examples
//'fou = matrix(c(-1000, -1000, 1000, 1000), 2, 2, TRUE)
//'parameter <- list()
//'parameter$light_source = c(-1000, -1000)
//'parameter$FOU = fou
//'parameter$unit = 'μrad'
//'parameter$'scanning-time(ms)/step'= 120
//'parameter$'beam-divergence-angle' = 60
//'parameter$'compensate-angle' = 5
//'# 生成扫描点
//'point_locus = spiral_scan_lines(parameter$'beam-divergence-angle', parameter$'compensate-angle', fou)
//'point_locus = spiral_scan_lines(parameter$'beam-divergence-angle', parameter$'compensate-angle', fou, c(500,500,30))
//'
//'library(MASS)
//'n <- 10000
//'leftdown = fou[1,]
//'rightup = fou[2,]
//'mu <- c((leftdown[1] + rightup[1]) / 2, (leftdown[2] + rightup[2]) / 2)
//'R <- (rightup[1] - leftdown[1]) / 2
//'sigma <- matrix(c(1.96 * R, 0, 0, 1.96 * R), nrow = 2)
//'points <- NULL
//'while(length(points) < n * 2) {
//' random_data <- mvrnorm((n * 2 - length(points))/ 2, mu, sigma)
//' if (length(random_data) == 2){
//'  distances <- sqrt((random_data[1] - mu[1])^2 + (random_data[2] - mu[2])^2)
//'  if (distances < 1.96 * R){
//'    points <- rbind(points, valid_points)
//'     }
//'  }else{
//'     distances <- sqrt((random_data[,1] - mu[1])^2 + (random_data[,2] - mu[2])^2)
//'     valid_points <- random_data[distances < 1.96 * R, ]
//'     points <- rbind(points, valid_points)
//'  }
//'}
//'count <- 0
//'for(i in 1:n){
//'  count <- count +
//'    nrow(spiral_scan_lines(parameter$'beam-divergence-angle',
//'                           parameter$'compensate-angle',
//'                           fou,
//'                           c(points[i,], parameter$'compensate-angle'))) * parameter$'scanning-time(ms)/step'
//'}
//'print(count / 10000)
//'
//'@useDynLib spiralScan
//'@import Rcpp
//'@export
// [[Rcpp::export]]
DataFrame spiral_scan_lines(double beam_divergence_angle, double compensate_angle,
                            NumericMatrix FOU, Nullable<NumericVector> point = R_NilValue) {
  double R = max((FOU(1, 0) - FOU(0, 0)) / 2, (FOU(1, 1) - FOU(0, 1)) / 2);
  double x_, y_, r_;
  double r = beam_divergence_angle * 0.5;
  double current_theta = 0;
  double rho_current = 0;
  double step_length = beam_divergence_angle / pow(2, 0.5) - compensate_angle;

  NumericVector x, y;
  double xi(0), yi(0);
  if (point.isNotNull()) {
    NumericVector point_list = as<NumericVector>(point);
    x_ = point_list[0];
    y_ = point_list[1];
    r_ = point_list[2];
    if(pow(pow(x_ - xi, 2) + pow(y_ - yi, 2), 0.5) < r_ + r){
      DataFrame point_locus = DataFrame::create(
        _["x"] = 1,
        _["y"] = 1,
        _["r"] = 1
      );
      return point_locus;
    }
    while (pow(pow(x_ - xi, 2) + pow(y_ - yi, 2), 0.5) >= r_ + r) {
      double dt = taylor_solve(current_theta);
      current_theta += dt;
      rho_current = step_length * 0.5 / M_PI * current_theta;
      xi = rho_current * cos(current_theta);
      yi = rho_current * sin(current_theta);
      x.push_back(xi);
      y.push_back(yi);
    }
  }else{
    while (rho_current <= R) {
      double dt = taylor_solve(current_theta);
      current_theta += dt;
      rho_current = step_length * 0.5 / M_PI * current_theta;
      xi = rho_current * cos(current_theta);
      yi = rho_current * sin(current_theta);
      x.push_back(xi);
      y.push_back(yi);
    }
  }

  DataFrame point_locus = DataFrame::create(
    _["x"] = x,
    _["y"] = y,
    _["r"] = r
  );

  return point_locus;
}

