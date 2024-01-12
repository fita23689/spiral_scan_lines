library(ggplot2)
library(cowplot)
library(progress)
library(gifski)
library(MASS)

tangent_point <- function(x, y, r, light_source = c(0, 0)) {
  rho <- sqrt((x - light_source[1])^2 + (y - light_source[2])^2)
  theta <- atan((y - light_source[2]) / (x - light_source[1]))

  alpha <- asin(r / rho)

  r_tangent <- sqrt(rho^2 - r^2)
  x1 <- light_source[1] + r_tangent * cos(theta - alpha)
  y1 <- light_source[2] + r_tangent * sin(theta - alpha)
  x2 <- light_source[1] + r_tangent * cos(theta + alpha)
  y2 <- light_source[2] + r_tangent * sin(theta + alpha)

  return(data.frame(x1 = x1, y1 = y1, x2 = x2, y2 = y2))
}

#' @title plot a gif
#'
#' @param point_locus point route
#' @param parameter super parameter
#' @param point point need to scan
#'
#' @return a gif show the process
#' @export
#'
#' @import ggplot2
#' @importFrom cowplot plot_grid
#' @importFrom progress progress_bar
#' @importFrom gifski gifski
#'
#'
#' @examples
#' fou = matrix(c(-1000, -1000, 1000, 1000), 2, 2, TRUE)
#' parameter <- list()
#' parameter$light_source = c(-1000, -1000)
#' parameter$FOU = fou
#' parameter$unit = 'μrad'
#' parameter$'scanning-time(ms)/step'= 120
#' parameter$'beam-divergence-angle' = 60
#' parameter$'compensate-angle' = 5
#' # 生成扫描点
#' point_locus = spiral_scan_lines(parameter$'beam-divergence-angle', parameter$'compensate-angle', fou)
#' # 画gif图，耗时长
#' laser_locus(point_locus, parameter)
#'
#' point_locus = spiral_scan_lines(parameter$'beam-divergence-angle', parameter$'compensate-angle', fou, c(500, 500, 30))
#' laser_locus(point_locus, parameter, c(500, 500, 30))
laser_locus <- function(point_locus, parameter, point = NULL) {
  light_source <- parameter$light_source
  FOU <- parameter$FOU
  unit <- parameter$unit
  scanning_time <- parameter$'scanning-time(ms)/step' / 1000

  time = nrow(point_locus) * parameter$`scanning-time(ms)/step` / 1000

  wd <- getwd()
  dir_save_loc <- paste(wd, 'output', sep = '/')
  unlink(dir_save_loc, recursive = TRUE)
  dir.create(dir_save_loc)
  setwd(dir_save_loc)


  leftdown = fou[1,]
  rightup = fou[2,]
  O = c((leftdown[1] + rightup[1]) / 2, (leftdown[2] + rightup[2]) / 2)
  x_r = (rightup[1] - leftdown[1]) / 2
  y_r = (rightup[2] - leftdown[2]) / 2
  theta <- seq(0, 2 * pi, length.out = 10000)
  data_big_circle <- data.frame(
    x = O[1] + x_r * cos(theta),
    y = O[2] + y_r * sin(theta)
  )

  p2 <- ggplot(data_big_circle, aes(x, y)) +
    geom_path(color = "blue", size = 0.5)+
    coord_equal()+
    theme_minimal() +
    labs(x = paste0("scanning angel/", unit), y = paste0("scanning angel/", unit),
         title = paste('Scanning Locus\n','Total:', time, 's, Avg:', scanning_time, 's', seq='')) +
    theme(axis.text = element_text(size = 8),
          axis.title = element_text(size = 10),
          legend.title = element_blank(),
          legend.text = element_text(size = 8),
          plot.title = element_text(size = 12))+
    scale_x_continuous(breaks=seq(leftdown[1], x_r, length.out=5), limits = c(leftdown[1], x_r))+
    scale_y_continuous(breaks=seq(leftdown[2], y_r, length.out=5), limits = c(leftdown[2], y_r))+
    geom_vline(xintercept = 0, linetype = "dashed", color = "lightblue", size = 1)+
    geom_hline(yintercept = 0, linetype = "dashed", color = "lightblue", size = 1)
  pc <- ggplot(data_big_circle, aes(x, y)) +
    geom_path(color = "blue", size = 0.5, show.legend = TRUE)+
    coord_equal() +
    theme_minimal() +
    labs(x = paste0("scanning angel/", unit), y = paste0("scanning angel/", unit),
         title = 'Signal-beam Scanning') +
    theme(axis.text = element_text(size = 8),
          axis.title = element_text(size = 10),
          legend.title = element_blank(),
          legend.text = element_text(size = 8),
          plot.title = element_text(size = 12))+
    scale_x_continuous(breaks=seq(leftdown[1], x_r, length.out=5), limits = c(leftdown[1], x_r))+
    scale_y_continuous(breaks=seq(leftdown[2], y_r, length.out=5), limits = c(leftdown[2], y_r))+
    geom_vline(xintercept = 0, linetype = "dashed", color = "lightblue", size = 1)+
    geom_hline(yintercept = 0, linetype = "dashed", color = "lightblue", size = 1)

  if(!is.null(point)){
    theta <- seq(0, 2 * pi, length.out = 200)
    data_small_circle <- data.frame(
      x = point[1] + point[3] * cos(theta),
      y = point[2] + point[3] * sin(theta)
    )
    p2 <- p2 + geom_path(data = data_small_circle, aes(x, y),
                   color = "lightgreen", size = 0.5)
    pc <- pc + geom_path(data = data_small_circle, aes(x, y),
                   color = "lightgreen", size = 0.5)
  }


  pb <- progress_bar$new(
    format = 'Plotting [:bar] :percent remaining time: :eta  spent :elapsed',
    total = nrow(point_locus), clear = FALSE, width = 80
  )


  print('start plotting png now')
  for (i in 1:nrow(point_locus)) {

    x <- point_locus[i, 1]
    y <- point_locus[i, 2]
    r <- point_locus[i, 3]

    # 激光点(圆)
    theta_locus <- seq(0, 2 * pi, length.out = 100)
    x_locus <- x + r * cos(theta_locus)
    y_locus <- y + r * sin(theta_locus)

    df_cricle = data.frame(x = x_locus, y = y_locus)

    p1 <- pc+
      geom_polygon(df_cricle, mapping=aes(x = x, y = y), fill = "salmon", color = "red") +
      geom_segment(aes(x = light_source[1], y = light_source[2], xend = x1, yend = y1),
                   data = tangent_point(x, y, r, light_source), color = "red", linetype = "dashed") +
      geom_segment(aes(x = light_source[1], y = light_source[2], xend = x2, yend = y2),
                   data = tangent_point(x, y, r, light_source), color = "red", linetype = "dashed")
    # 历史轨迹图
    p2 <- p2+
      geom_polygon(df_cricle, mapping=aes(x = x, y = y), fill = "salmon", color = "red")

    p <- plot_grid(p1, p2, nrow=1)
    pic_save_loc <- paste(i, '.png', sep = '')
    suppressMessages(ggsave(pic_save_loc, plot = p,
                            device = "png", bg='white'))
    pb$tick()
  }
  print('png plotting done, merge to gif now')
  png_path <- file.path(getwd(), '%01d.png')
  png_files <- sprintf(png_path, 1:nrow(point_locus))
  gif_file <- file.path(getwd(), 'spiral_scan.gif')
  #gifski(png_files, gif_file)
  gifski(png_files, gif_file, width=1700, height=1600, delay=scanning_time)
  setwd(wd)
}

spend_time <- function(point_locus, parameter){
  return(nrow(point_locus) * parameter$`scanning-time(ms)/step`)
}

if(FALSE) {
  fou = matrix(c(-1000, -1000, 1000, 1000), 2, 2, TRUE)
  parameter <- list()
  parameter$light_source = c(-1000, -1000)
  parameter$FOU = fou
  parameter$unit = 'μrad'
  parameter$'scanning-time(ms)/step'= 120
  parameter$'beam-divergence-angle' = 60
  parameter$'compensate-angle' = 5
  # 生成扫描点
  point_locus = spiral_scan_lines(parameter$'beam-divergence-angle', parameter$'compensate-angle', fou)
  # 画gif图，耗时长
  laser_locus(point_locus, parameter)
  # 模拟n个点的耗时
  n <- 10000
  leftdown = fou[1,]
  rightup = fou[2,]
  mu <- c((leftdown[1] + rightup[1]) / 2, (leftdown[2] + rightup[2]) / 2)
  R <- (rightup[1] - leftdown[1]) / 2
  sigma <- matrix(c(1.96 * R, 0, 0, 1.96 * R), nrow = 2)
  points <- NULL

  while(length(points) < n * 2) {
    random_data <- mvrnorm((n * 2 - length(points))/ 2, mu, sigma)
    if (length(random_data) == 2){
      distances <- sqrt((random_data[1] - mu[1])^2 + (random_data[2] - mu[2])^2)
      if (distances < 1.96 * R){
        points <- rbind(points, valid_points)
      }
    }else{
      distances <- sqrt((random_data[,1] - mu[1])^2 + (random_data[,2] - mu[2])^2)
      valid_points <- random_data[distances < 1.96 * R, ]
      points <- rbind(points, valid_points)
    }
  }
  count <- 0
  for(i in 1:n){
    count <- count +
      nrow(spiral_scan_lines(parameter$'beam-divergence-angle',
                             parameter$'compensate-angle',
                             fou,
                             c(points[i,], parameter$'compensate-angle'))) * parameter$'scanning-time(ms)/step'

  }
  print(count / 10000)
}
