#code used to create the hex logo for the R package
library(hexSticker)
library(ggplot2)
library(colordistance)
library(SondePolishR)

#functions from hexSticker, but need to pull out due to bug that hasn't been fixed yet -------
theme_sticker <- function(size=1.2, ...) {
  center <- 1
  radius <- 1
  h <- radius
  w <- sqrt(3)/2 * radius
  m <- 1.05
  list(
    theme_transparent() +
      theme(plot.margin = margin(t=0, r=0, b=0, l=0, unit = "lines"),
            strip.text = element_blank(),
            line = element_blank(),
            text = element_blank(),
            title = element_blank(), ...),
    coord_fixed(),
    scale_y_continuous(expand = c(0, 0), limits = c(center-h*m , center+h*m )),
    scale_x_continuous(expand = c(0, 0), limits = c(center-w*m , center+w*m ))
  )
}
sticker <- function (subplot, s_x = 0.8, s_y = 0.75, s_width = 0.4, s_height = 0.5,
                     package, p_x = 1, p_y = 1.4, p_color = "#FFFFFF", p_family = "Aller_Rg",
                     p_fontface = "plain", p_size = 8, h_size = 1.2, h_fill = "#1881C2",
                     h_color = "#87B13F", spotlight = FALSE, l_x = 1, l_y = 0.5,
                     l_width = 3, l_height = 3, l_alpha = 0.4, url = "", u_x = 1,
                     u_y = 0.08, u_color = "black", u_family = "Aller_Rg", u_size = 1.5,
                     u_angle = 30, white_around_sticker = FALSE, ..., filename = paste0(package,
                                                                                        ".png"), asp = 1, dpi = 300){
  hex <- ggplot() + geom_hexagon(size = h_size, fill = h_fill,
                                 color = NA)
  if (inherits(subplot, "character")) {
    d <- data.frame(x = s_x, y = s_y, image = subplot)
    sticker <- hex + geom_image(aes(x = !!sym("x"), y = !!sym("y"),
                                    image = !!sym("image")), d, size = s_width, asp = asp)
  } else {
    sticker <- hex + geom_subview(subview = subplot, x = s_x,
                                  y = s_y, width = s_width, height = s_height)
  }
  sticker <- sticker + geom_hexagon(size = h_size, fill = NA,
                                    color = h_color)
  if (spotlight)
    sticker <- sticker + geom_subview(subview = spotlight(l_alpha),
                                      x = l_x, y = l_y, width = l_width, height = l_height)
  sticker <- sticker + geom_pkgname(package, p_x, p_y, color = p_color,
                                    family = p_family, fontface = p_fontface, size = p_size,
                                    ...)
  sticker <- sticker + geom_url(url, x = u_x, y = u_y, color = u_color,
                                family = u_family, size = u_size, angle = u_angle)
  if (white_around_sticker)
    sticker <- sticker + hexSticker:::white_around_hex(size = h_size)
  sticker <- sticker + theme_sticker(size = h_size)
  save_sticker(filename, sticker, dpi = dpi)
  class(sticker) <- c("sticker", class(sticker))
  invisible(sticker)
}

#make hex sticker ------
  plot_dat <- summarize_data(example_data, frequency = lubridate::period(5, "day"),
                             sum_method="median") %>%
   mutate(color = "normal")

plot_dat$color[23] <- "flag"

p <- ggplot(plot_dat, aes(x=DateTime_rd, y=fDOM_QSU)) +
    geom_line(color= "#ebebeb", linewidth=1) +
    geom_point(aes(color=color), size=1.5) +
    scale_color_manual(values=c("darkred","#ebebeb")) +
    theme_void() + theme(legend.position = "none") +
    theme(plot.background = element_rect(fill="#3c4d5a")) + theme_transparent()

#make with white border
sticker(p, package="SondePolishR", h_fill="#3c4d5a", h_color="#1d3040",
        p_color = "#ebebeb",
        p_size=18, s_x=1.01, s_y=0.83, s_width=1.6, s_height=0.9,
        p_y=1.4,p_family = "sans",p_fontface = "bold",
        white_around_sticker = FALSE,
        filename="inst/figures/SondePolishR.png")
