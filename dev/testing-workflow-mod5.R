## testing shifting values -----
#inputs in the module
proj <- example_sondeproj #the project that's getting passed around the app
y_var <- "ODO_mg_L"

#user will select a set of points and set slope and intercept for shift
select <- 5:7
slope <- 0
int <- 0

#guess and update the shift values
guess <- guess_shift(proj$data, y_var, select)
slope <- guess$slope
int <- guess$int

#apply shift to points
newdata <- shift_points(proj$data, y_var, select, shift_val = list(slope=slope, int=int))

newdata[[y_var]][select]

#check guesser
proj$data[[y_var]][select] / 0.8

proj$data[[y_var]][4]
proj$data[[y_var]][8]

vals <- proj$data[[y_var]]
start <- 5
end <- 7
#get values of data to shift and good data
y1 <- vals[start]
y2 <- vals[end]

t1 <- vals[start - 1]
t2 <- vals[end + 1]

# amount needed at boundaries
d1 <- t1 - y1
d2 <- t2 - y2

# slope across selected region
b <- (d2 - d1) / (length(select) + 1)
a <- d1 + b

add <- (b * (seq_along(proj$data[[y_var]][select])-1)) + a
proj$data[[y_var]][select] + add


## simplifying the example
shiftdat <- c(7.072, 7.056, 7.040)
realdat <- shiftdat / 0.8

add_goal <- realdat - shiftdat

#current shifted values
y1 <- shiftdat[1]
y2 <- shiftdat[3]
r_y1 <- realdat[1]
r_y2 <- realdat[3]

t1 <- 8.86
t2 <- 8.78

#slope between known points
b <- (t2-t1) / (length(select) + 1) #gets correct difference between points
a <- t1 - y1 + b

add <- (b * (seq_along(proj$data[[y_var]][select])-1)) + a




##figure out what to do with the cal checks ------
  #get cal check info and data
  data <- example_sondeproj$data
  calcheck <- example_sondeproj$calcheck
  oow_data <- get_oow(example_sondeproj$fieldform)
  y_var <- "fDOM_QSU"

  #use ff data to determine when cal data likely was
  mean_visit <- oow_data %>% rowwise() %>%
    mutate(avg_time = mean(c(.data$start, .data$end)),date = as.Date(.data$avg_time))

  cal_data <- calcheck %>%
    dplyr::left_join(mean_visit %>% select("date", "avg_time"),
                     join_by("Date" == "date")) %>%
    filter(.data$Parameter == y_var)

  #figure out the logic for correcting for cal checks
    #from when sonde is first put in water to first cal check, we assume first point is "perfect cal" so apply shift of
    #data from first point to first cal check
  apply_drift_shift <- function(x, check, resident){

    n <- length(x)

    # amount needed at final point (using paired check, resident)
    d <- check - resident

    # increasing additive correction
    add <- d * ((seq_len(n) - 1) / (n - 1))

    x + add
  }

    first_period <- data %>% filter(.data$DateTime_rd <= cal_data$avg_time[1])

    test_vals <- c(first_period$fDOM_QSU)

    #correct data
    test_df <- data.frame(datetime = c(first_period$DateTime_rd),
                          org_val = test_vals,
                          new_val = apply_drift_shift(test_vals, cal_data$Check_Value[1], cal_data$Resident_Value[1]))
    #check shift
    ggplot(test_df, aes(x=datetime)) + geom_line(aes(y=org_val), color="red") + geom_line(aes(y=new_val), color="darkgreen")


  ## read in sonde data and get probe serials to match with calcheck
    files <- c("inst/extdata/example-csv-data1.csv", "inst/extdata/example-csv-data2.csv",
               "inst/extdata/example-csv-data3.csv")
    tz <- "Etc/GMT+8"
    data_merge <- lapply(files, read_sonde, tz = tz, return="list")
    serials <- lapply(data_merge, "[[", 1) %>% bind_rows()
    data_merge <- lapply(data_merge, "[[", 2)%>% dplyr::bind_rows() %>%
      dplyr::mutate(Index = 1:n())

    switch_df <- serials %>% pivot_longer(-date, names_to = "parameter", values_to = "serial") %>%
      dplyr::group_by(parameter) %>%
      dplyr::mutate(switched = serial != dplyr::lag(serial, default = first(serial)))

