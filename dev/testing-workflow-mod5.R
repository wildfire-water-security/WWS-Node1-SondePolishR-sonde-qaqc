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
    example_sondeproj$calcheck  #we need the extra file to know there was a shift on 10/23 so maybe add to example....
    data <- example_sondeproj$data

    #for dates with a switch, apply drift correction to that period [don't apply for periods when we don't know if there was a shift]
    to_cor <- example_sondeproj$calcheck %>% filter(Parameter == y_var)

    #apply shift (start with just set known and make more flexible)
    shift_period <- data$DateTime_rd <= to_cor$Est_Time[2]

    newdata <- data
    newdata[[y_var]][shift_period] <- apply_drift_shift(newdata[[y_var]][shift_period], to_cor$Check_Value[2], to_cor$Resident_Value[2])

    #check
    p <- ggplot() + geom_line(data=newdata, aes(x=DateTime_rd, y=fDOM_QSU), color="red", alpha=0.5) +
      geom_line(data=data, aes(x=DateTime_rd, y=fDOM_QSU), color="green", alpha=0.5)

    ggplotly(p)

  #make more flexible
     #pretend we had a switch in first one
     to_cor <- example_sondeproj$calcheck %>%  filter(Parameter == y_var) %>% mutate(Probe_Switch = c(TRUE, TRUE, NA))

     adj <- which(ifelse(is.na(to_cor$Probe_Switch), FALSE, to_cor$Probe_Switch))
     data <- example_sondeproj$data

     for(x in adj){
       if(x == 1){
         lines <- data$DateTime <= to_cor$Est_Time[x]
       }else{
         lines <- data$DateTime <= to_cor$Est_Time[x] & data$DateTime >= to_cor$Est_Time[x-1]
       }
       print(sum(lines))
       data[[y_var]][lines] <- apply_drift_shift(data[[y_var]][lines], to_cor$Check_Value[x], to_cor$Resident_Value[x])
     }

  olddata<- example_sondeproj$data
     #check
     p <- ggplot() + geom_line(data=olddata, aes(x=DateTime_rd, y=fDOM_QSU), color="red", alpha=0.5) +
       geom_line(data=data, aes(x=DateTime_rd, y=fDOM_QSU), color="green", alpha=0.5)

     ggplotly(p)


  ## try to work the workflow how it'd work in the app
  correct_drift <- TRUE #a button to click
  apply_edits <- TRUE  #the flagging module to confirm changes
  y_var <- "fDOM_QSU"
  sondeproj <- example_sondeproj
  sondeproj$calcheck$Probe_Switch <- c(TRUE, TRUE, NA) #this is for my testing, shouldn't be carried over


  #TODO: how do we prevent correct drift from being run multiple times, or if we add data and want to rerun ,we don't want things to be corrected
    #multiple times -> store if it's be adjusted in calcheck?? <- probabaly this, check if it looks like it's been corrected??
  if(correct_drift){
    #get just calcheck rows with the parameter we're focused on
      checkrows <- sondeproj$calcheck %>% filter(Parameter == y_var)

    #get the rows in check rows where sonde probes were switched for that parameter
      adj_periods <- which(ifelse(is.na(checkrows$Probe_Switch), FALSE, checkrows$Probe_Switch))

    #for each period needing correction, apply correction and stick back in data
    plot_data <- sondeproj$data

    for(x in adj){
        if(x == 1){
          adj_rows <- plot_data$DateTime <= checkrows$Est_Time[x]
        }else{
          adj_rows <- plot_data$DateTime <= checkrows$Est_Time[x] & plot_data$DateTime >= checkrows$Est_Time[x-1]
        }
          plot_data[[y_var]][adj_rows] <- apply_drift_shift(plot_data[[y_var]][adj_rows], checkrows$Check_Value[x], checkrows$Resident_Value[x])
      }

    p <- plot_sonde(sondeproj$data, y_var) + geom_line(data = plot_data, aes(x=.data$DateTime_rd, y = .data[[y_var]]), color="darkred")
  }

# scratched ideas -----
    #we could also look for gaps between datafiles and guess that it's a gap that could be corrected
    #identify periods where file is switching (sonde could be switched, cleaned, would need correction) --> I don't know if this makes sense
      #it could just be change in time sonde was OOW
      file_switch <- data %>% group_by(FileName) %>% summarise(f1 = last(DateTime),
                                                               f2 = first(DateTime)) %>%
        mutate(f2 = lead(f2))

      #get essentially a correction from the data itself (next val super low, do after cleaning)
       data_cor <- file_switch %>% select(-FileName) %>% pivot_longer(f1:f2, names_to = "file", values_to = "DateTime") %>%
         na.omit() %>%
         left_join(data, join_by("DateTime"))


# actual workflow plan ------
 #will get passed to module
    sondeproj <- example_sondeproj
    y_var <- "fDOM_QSU"

 #will be UI buttons
  file <- unique(sondeproj$data$FileName)[1]  #selectize with file names
  uncorrected <- NA #updatable number input, need better names
  corrected <- NA #updatable number input, need better names
  save_changes <- FALSE #button to flag

#server code
  #step 1 identify the shift values (either from cal check or guess from data)
    data <- sondeproj$data
    rows <- data$FileName == file

    #get potential calcheck data
    par_calcheck <- sondeproj$calcheck %>%
      filter(.data$Parameter == y_var & Date == as.Date(max(data$DateTime[rows])))

    if(nrow(par_calcheck) == 1){
      ##update uncorrected and corrected value in UI to resident and check values
      uncorrected <- par_calcheck$Resident_Value
      corrected <- par_calcheck$Check_Value
    }else{
      #we guess from data
        #get median 5 points before file ends
        endvals <- data[[y_var]][(max(rows)-4):max(rows)]
        newvals <- data[[y_var]][(max(rows)+1):(max(rows)+5)]

        uncorrected <- median(endvals, na.rm = TRUE)
        corrected <- median(newvals, na.rm = TRUE)
    }

  #step 2 perform shift and preliminary update data so we can plot
    data[[y_var]] <- apply_drift_shift(data[[y_var]], rows, corrected, uncorrected)

  #step 3: visualize the difference
    p <- plot_sonde(sondeproj$data, y_var) + geom_line(data = data[rows,], aes(x=.data$DateTime_rd, y = .data[[y_var]]), color="darkred")
    ggplotly(p)

