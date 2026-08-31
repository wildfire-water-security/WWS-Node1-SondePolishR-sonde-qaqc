## testing methods of identifying potential outliers within a dataset

library(pracma)
library(zoo)

## inputs to module
proj <- example_sondeproj
y_var <- "ODO_mg_L"

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
  k <- 7
  t <- 7




if(filter_type == "hampel"){
  #show UI options for k (window size) and t (threshold)

  outlier <- hampel_robust(x_fill, k, t)
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

#step x: test some new methods -------
  t <- 10 #threshold
  k <- 5 #window

  #hampel filter
  roll_mad <- zoo::rollapply(x, k, fill = NA, mad,na.rm = TRUE, align = "center")
  roll_med <- zoo::rollmedian(example_data$fDOM_QSU, k, fill= NA, align = "center")
  outlier <- ifelse(abs(x - roll_med) > (roll_mad * t), TRUE, FALSE)

  #regions of high variability
  # diffs <- c(NA, abs(diff(x)))
  # roll_mad <- zoo::rollapply(diffs, k, fill = NA, mad,na.rm = TRUE, align = "center")
  # outlier <- ifelse(roll_mad > t, TRUE, FALSE)

  #regions of high variability (works expect grabs rising limbs)
  smooth <- zoo::rollmedian(x, k, fill = NA, align = "center") #get expected median
  resid <- x - smooth #see difference between smoothed and observed
  roll_mad <- zoo::rollapply(resid, k, fill = NA, mad,na.rm = TRUE, align = "center") #get median dev of residuals
  typical_mad <- mean(roll_mad, na.rm = TRUE) #mean not median since it is likely 0
  outlier <- roll_mad > typical_mad * t #is above threshold?

  #check
  test <- example_data %>% mutate(outlier = outlier)
  #ggplotly(ggplot(test, aes(x=DateTime_rd, y=fDOM_QSU)) + geom_line() + geom_point(aes(color=median)))
  ggplotly(ggplot(test, aes(x=DateTime_rd, y=ODO_mg_L))  + geom_line() + geom_point(aes(color=outlier)))

  ggplot(test, aes(x=DateTime_rd)) + geom_line(aes(y=fDOM_QSU)) + geom_line(aes(y=median), color="red")
