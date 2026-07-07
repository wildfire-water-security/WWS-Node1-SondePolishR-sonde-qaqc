get_plotly_snap <- function(plot_obj){
  x <- plotly::plotly_build(plot_obj)$x

  list(layout = x$layout,
       names = sapply(x$data, names),
       ntraces = length(x$data),
       x_rng = lapply(x$data, function(x){summary(x$x)}),
       y_rng = lapply(x$data, function(x){summary(x$y)}))

}
