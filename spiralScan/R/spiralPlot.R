library(ggplot2)
library(gganimate)
library(cowplot)

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

laser_locus <- function(point_locus, parameter) {
  light_source <- parameter$light_source
  FOU <- parameter$FOU
  unit <- parameter$unit
  scanning_time <- parameter$`scanning-time(ms)/step`
  wd <- getwd()
  dir_save_loc <- paste(wd, 'output', sep = '/')
  file.remove(list.files(dir_save_loc, full.names = TRUE))
  dir.create(dir_save_loc)
  setwd(dir_save_loc)


  leftdown = fou[1,]
  rightup = fou[2,]
  O = c((leftdown[1] + rightup[1]) / 2, (leftdown[2] + rightup[2]) / 2)
  x_r = (rightup[1] - leftdown[1]) / 2
  y_r = (rightup[2] - leftdown[2]) / 2
  theta <- seq(0, 2 * pi, length.out = 200)
  data_big_circle <- data.frame(
    x = O[1] + x_r * cos(theta),
    y = O[2] + y_r * sin(theta)
  )

  p2 <- ggplot(data_big_circle, aes(x, y)) +
    geom_path(color = "blue", size = 1)+
    coord_equal()+
    theme_minimal() +
    labs(x = paste0("scanning angel/", unit), y = paste0("scanning angel/", unit),
         title = 'Scanning Locus') +
    theme(axis.text = element_text(size = 18),
          axis.title = element_text(size = 24),
          legend.title = element_blank(),
          legend.text = element_text(size = 18),
          plot.title = element_text(size =30))+
    scale_x_continuous(breaks=seq(leftdown[1], x_r, length.out=5), limits = c(leftdown[1], x_r))+
    scale_y_continuous(breaks=seq(leftdown[2], y_r, length.out=5), limits = c(leftdown[2], y_r))
  pc <- ggplot(data_big_circle, aes(x, y)) +
    geom_path(color = "blue", size = 1, show.legend = TRUE)+
    coord_equal() +
    theme_minimal() +
    labs(x = paste0("scanning angel/", unit), y = paste0("scanning angel/", unit),
         title = 'Signal-beam Scanning') +
    theme(axis.text = element_text(size = 18),
          axis.title = element_text(size = 24),
          legend.title = element_blank(),
          legend.text = element_text(size = 18),
          plot.title = element_text(size =30))+
    scale_x_continuous(breaks=seq(leftdown[1], x_r, length.out=5), limits = c(leftdown[1], x_r))+
    scale_y_continuous(breaks=seq(leftdown[2], y_r, length.out=5), limits = c(leftdown[2], y_r))
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

    p = plot_grid(p1, p2, nrow=1)
    print(p)
    pic_save_loc <- paste(i, '.png', sep = '')
    dev.copy(png, filename = pic_save_loc, width = 1200, height = 600)
    dev.off()
  }
  setwd(wd)
}
