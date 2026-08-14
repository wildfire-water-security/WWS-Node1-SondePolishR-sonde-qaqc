## testing out potential smoothing methods for when there's optical interferance causing bounciness

#read data that has this issue:
path <- "../WWS-Node1-SONDE-postfire-sonde-network/data/02_raw-downloads/Staley-Creek-above/20260717_STA.csv"

#load data and zoom into region that needs smoothing
dat <- read_sonde(path) %>% filter(Date > as.Date("2026-04-25") & Date < as.Date("2026-05-20"))

#check out raw data
ggplot(dat %>% filter(Turbidity_FNU < 10), aes(x=DateTime_rd, y=Turbidity_FNU)) + geom_line()

#trying six methods base on this article: https://medium.com/@dmitriy.bolotov/six-approaches-to-time-series-smoothing-cc3ea9d6b64f
  #also see this cool interactive tester: https://timeseriessmoothing.streamlit.app/

library(plotly)

# method 1: rolling mean -------
  library(zoo)

  filled <- lapply(c(1,5,11), function(x){
    data.frame(DateTime_rd = dat$DateTime_rd,
                       Turbidity_FNU = rollmean(dat$Turbidity_FNU, na.pad=TRUE, k=x),
                       k_val = as.character(x))
  }) %>% bind_rows()

  plot_dat <- dat %>% select(DateTime_rd, Turbidity_FNU) %>% mutate(k_val = "obs") %>%
    bind_rows(filled)

  p <- ggplot(plot_dat, aes(x=DateTime_rd, y=Turbidity_FNU, color=k_val)) + geom_line()
  ggplotly(p)

#method 2: Savitzky–Golay Filter ------
  library(pracma)

  filled <- lapply(c(5,15,25,41), function(x){
    data.frame(DateTime_rd = dat$DateTime_rd,
               Turbidity_FNU = savgol(dat$Turbidity_FNU, x, forder = 4, dorder = 0),
               k_val = as.character(x))
  }) %>% bind_rows()

  plot_dat <- dat %>% select(DateTime_rd, Turbidity_FNU) %>% mutate(k_val = "obs") %>%
    bind_rows(filled)

  p <- ggplot(plot_dat, aes(x=DateTime_rd, y=Turbidity_FNU, color=k_val)) + geom_line()
  ggplotly(p)

#method 3: Kalman Filter ------
  library(dlm)

  filled <- lapply(c(500, 2000,5000), function(x){
    data.frame(DateTime_rd = dat$DateTime_rd,
               Turbidity_FNU = dlmSmooth(dat$Turbidity_FNU, dlmModPoly(1, dV = x, dW = 1000))$s[-1],
               k_val = as.character(x))
  }) %>% bind_rows()

  plot_dat <- dat %>% select(DateTime_rd, Turbidity_FNU) %>% mutate(k_val = "obs") %>%
    bind_rows(filled)

  p <- ggplot(plot_dat, aes(x=DateTime_rd, y=Turbidity_FNU, color=k_val)) + geom_line()
  ggplotly(p)


  filled <- lapply(c(100,500,2000), function(x){
    data.frame(DateTime_rd = dat$DateTime_rd,
               Turbidity_FNU = dlmSmooth(dat$Turbidity_FNU, dlmModPoly(1, dV = 2000, dW = x))$s[-1],
               k_val = as.character(x))
  }) %>% bind_rows()

  plot_dat <- dat %>% select(DateTime_rd, Turbidity_FNU) %>% mutate(k_val = "obs") %>%
    bind_rows(filled)

  p <- ggplot(plot_dat, aes(x=DateTime_rd, y=Turbidity_FNU, color=k_val)) + geom_line()
  ggplotly(p)

#create a little function to smooth selection ------
  data <- dat
  y_var <- "Turbidity_FNU"
  method <- c("rollmean", "rollmedian", "savgol", "kalman")
  parms <- list(k = 8)
  range <- as.POSIXct(c("2026-05-03 13:15", "2026-05-07 10:45"), tz=tz(data$DateTime))

  smooth_data <- function(data, y_var, method, parms=list(k=7,fl=15,dV=10), range){
    stopifnot(method %in% c("rollmean", "rollmedian", "savgol", "kalman"))

   #get data interval
    int <- get_interval(data)

   #pull out just data to smooth with a little extra for starting and ending
    #get indices of values to smooth
    index <- which(data$DateTime_rd >= range[1] & data$DateTime_rd <= range[2])
    wide_index <- seq(from=(min(index)-4), to = (max(index)+4), by=1)
    vals <- data[[y_var]][wide_index]

  #apply smoothing method
   if(method == "rollmean"){
     filled <- rollmean(vals, na.pad=TRUE, k=parms$k)
   }

   if(method == "rollmedian"){
      filled <- rollmedian(vals, na.pad=TRUE, k=parms$k)
   }

  if(method == "savgol"){
    filled <- savgol(vals, parms$fl, forder = 4, dorder = 0)
  }

  if(method == "kalman"){
    filled <- dlmSmooth(vals, dlmModPoly(1, dV = 10, dW = parms$dW))$s[-1]
  }

  #replace data with smoothed data
   filled <- filled[-c(1:4, (length(filled)-3):length(filled))]
   data[[y_var]][index] <- filled

  #return smoothed data
   return(data)
  }

  #do some testing
  smooth_dat <- smooth_data(dat, "Turbidity_FNU", method="kalman", parms=list(dW=10),
                            range = as.POSIXct(c("2026-05-03 13:15", "2026-05-07 10:45"), tz=tz(data$DateTime)))

  p <- ggplot(dat, aes(x=DateTime_rd, y=Turbidity_FNU)) + geom_line(color="black", linewidth=0.8) +
    geom_line(data=smooth_dat, color="darkred")

  ggplotly(p)
