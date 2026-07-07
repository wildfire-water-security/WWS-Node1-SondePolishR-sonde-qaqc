## testing methods of identifying potential outliers within a dataset

library(pracma)
library(zoo)

## inputs to module
proj <- example_sondeproj
y_var <- "fDOM_QSU"

#UI choices
filter_type <- "hampel"

data <- proj$data
x <- data[[y_var]] #needed by everything

# interpolate to temp fill gaps so filter will work
x_fill <- zoo::na.approx(x, na.rm = FALSE)
x_fill <- zoo::na.locf(x_fill, na.rm = FALSE)        # forward fill
x_fill <- zoo::na.locf(x_fill, fromLast = TRUE)      # backward fill

  #all methods should return logic vector of flagged points that could be passed to edit
#step 1: hampel filter -----
  #UI for this option
  k <- 8
  t <- 2.5
if(filter_type == "hampel"){
  #show UI options for k (window size) and t (threshold)

  hampel_out <- pracma::hampel(x_fill, k, t)

  outlier <- rep(FALSE, length(x))
  outlier[hampel_out$ind] <- TRUE

  outlier

}

  #UI for this option
  t <- 5 #percent
  k <- 9 #must be odd here
if(filter_type == "relative_change"){
  #show UI options for threshold
  rel_change_lead <- abs((x_fill - x_lead)) / rollmedian(x_fill, k, fill= NA, align = "right") * 100
  rel_change_lag <- abs((x_fill - x_lag)) / rollmedian(x_fill, k, fill= NA, align = "left") * 100

  outlier <- rel_change_lead >= t & rel_change_lag >= t
  outlier[is.na(outlier)] <- FALSE #deal with ending/starting NA

  outlier

}


#step x: plot data with outlier -----
  out_data <- data[outlier,]

  p <- plot_sonde(data, y_var)
  p <- p + geom_point(data= out_data, color="darkred")
  p

  ggplotly(p)
