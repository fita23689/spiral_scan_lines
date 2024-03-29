# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#'@title calculate points in the route
#'
#'@param beam_divergence_angle parameter
#'@param compensate_angle parameter
#'@param fou FOU
#'@param point point to scan
#'
#'@examples
#'fou = matrix(c(-1000, -1000, 1000, 1000), 2, 2, TRUE)
#'parameter <- list()
#'parameter$light_source = c(-1000, -1000)
#'parameter$FOU = fou
#'parameter$unit = 'μrad'
#'parameter$'scanning-time(ms)/step'= 120
#'parameter$'beam-divergence-angle' = 60
#'parameter$'compensate-angle' = 5
#'# 生成扫描点
#'point_locus = spiral_scan_lines(parameter$'beam-divergence-angle', parameter$'compensate-angle', fou)
#'point_locus = spiral_scan_lines(parameter$'beam-divergence-angle', parameter$'compensate-angle', fou, c(500,500,30))
#'
#'library(MASS)
#'n <- 10000
#'leftdown = fou[1,]
#'rightup = fou[2,]
#'mu <- c((leftdown[1] + rightup[1]) / 2, (leftdown[2] + rightup[2]) / 2)
#'R <- (rightup[1] - leftdown[1]) / 2
#'sigma <- matrix(c(1.96 * R, 0, 0, 1.96 * R), nrow = 2)
#'points <- NULL
#'while(length(points) < n * 2) {
#' random_data <- mvrnorm((n * 2 - length(points))/ 2, mu, sigma)
#' if (length(random_data) == 2){
#'  distances <- sqrt((random_data[1] - mu[1])^2 + (random_data[2] - mu[2])^2)
#'  if (distances < 1.96 * R){
#'    points <- rbind(points, valid_points)
#'     }
#'  }else{
#'     distances <- sqrt((random_data[,1] - mu[1])^2 + (random_data[,2] - mu[2])^2)
#'     valid_points <- random_data[distances < 1.96 * R, ]
#'     points <- rbind(points, valid_points)
#'  }
#'}
#'count <- 0
#'for(i in 1:n){
#'  count <- count +
#'    nrow(spiral_scan_lines(parameter$'beam-divergence-angle',
#'                           parameter$'compensate-angle',
#'                           fou,
#'                           c(points[i,], parameter$'compensate-angle'))) * parameter$'scanning-time(ms)/step'
#'}
#'print(count / 10000)
#'
#'@useDynLib spiralScan
#'@import Rcpp
#'@export
spiral_scan_lines <- function(beam_divergence_angle, compensate_angle, FOU, point = NULL) {
    .Call('_spiralScan_spiral_scan_lines', PACKAGE = 'spiralScan', beam_divergence_angle, compensate_angle, FOU, point)
}

