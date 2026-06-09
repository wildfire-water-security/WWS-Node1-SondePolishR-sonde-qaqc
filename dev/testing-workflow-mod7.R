## testing the workflow/code to perform interpolation steps:
    #TO DO -> dups ->> force dealing with first??

  #borrowed from zoo package for consistent maxgap detection
  .fill_short_gaps <- function(x, fill, maxgap) {
    if (maxgap <= 0)
      return(x)
    if (maxgap >= length(x))
      return(fill)
    naruns <- rle(is.na(x))
    naruns$values[naruns$lengths > maxgap] <- FALSE
    naok <- inverse.rle(naruns)
    x[naok] <- fill[naok]
    return(x)
  }

## things passed to module
  proj <- readRDS(file.path(test_path(), "testdata", "example-sondeproj-messy.RDS"))
  y_var <- "fDOM_QSU"

#UI things
max_length <- 24 #hours
method <- "ts_interp"
if(method == "ts_interp"){frequency = 96}

# things that will be the same no matter the method ------
#get data from project
data <- proj$data

#determine interval of data for gap length
interval <- get_interval(data)

#stuff to fill in missing correctly
tz <- tz(data$DateTime_rd)
name <- unique(data$Site_Name)

par_names <- get_parms(data)

#get the dataset to interpolate (still may have dupes)
data_fill <- data %>%
  complete(DateTime_rd = seq(min(.data$DateTime_rd), max(.data$DateTime_rd),
                             by = paste(interval, "min"))) %>%
  arrange(.data$DateTime_rd) %>% #want to arrange in time order for filling
  mutate(Index = 1:n(),
         DupNum = ifelse(is.na(.data$DupNum), 1, .data$DupNum),
         FileName = ifelse(is.na(.data$FileName), "interpolated", .data$FileName),
         Date = if_else(is.na(.data$Date), as.Date(.data$DateTime_rd, tz = tz), .data$Date),
         Time_HH_mm_ss = if_else(is.na(.data$Time_HH_mm_ss), strftime(.data$DateTime_rd, "%H:%M:%S"), .data$Time_HH_mm_ss),
         DateTime = if_else(is.na(.data$DateTime), .data$DateTime_rd, .data$DateTime),
         Site_Name = name)

#get df with a single stamp per row (conflicting duplicates are set to NA)
  #determine which sets of dups are conflicting
    conflict <- data %>%
      pivot_longer(any_of(par_names), names_to = "param", values_to = "value") %>%
      group_by(DateTime_rd, param) %>%
      summarise(count = n(),sd = sd(value),.groups = "drop_last") %>%
      filter(.data$count > 1 & .data$sd != 0)

  #set those parameters/datetimes to NA
    conflict_list <- split(conflict$DateTime_rd, conflict$param)

    data_interp <- data_fill %>%
      mutate(across(all_of(names(conflict_list)),~ replace(.x,
                    DateTime_rd %in% conflict_list[[cur_column()]],NA)))

  #fill and summarize to a single obs per datetime
    data_interp <- data_interp %>% arrange(DateTime_rd, DupNum) %>%
      group_by(DateTime_rd) %>%
      tidyr::fill(any_of(par_names), .direction = "downup") %>%
      slice(1) %>% ungroup()

  #pass to interpolation functions
    if(method == "linear"){
      data_interp[[y_var]] <- zoo::na.approx(data_interp[[y_var]],
                                             maxgap = max_length*(60/interval))
    }

    if(method == "spline"){
      data_interp[[y_var]] <- zoo::na.spline(data_interp[[y_var]],
                                             maxgap = max_length*(60/interval))
    }

    if(method == "random_forest"){
      filled <- data_interp %>%
        select(-any_of(c("DateTime_rd", "FileName", "Date", "Time_HH_mm_ss",
                         "DateTime", "Site_Name", "DupNum"))) %>%
        as.data.frame() %>% missForest::missForest()

      #only fill to max gap
      x_fill <- .fill_short_gaps(data_interp[[y_var]], filled$ximp[[y_var]],
                                 maxgap = max_length*(60/interval))
      data_interp[[y_var]] <- x_fill
    }

    if(method == "ts_interp"){
      #make time series
      var_ts <- ts(data_interp[[y_var]], frequency = frequency)
      #only fill to max gap
      x_fill <- .fill_short_gaps(data_interp[[y_var]],
                                 as.numeric(forecast::na.interp(var_ts)),
                                 maxgap = max_length*(60/interval))
      data_interp[[y_var]] <- x_fill
    }

#map filled values back to filled df

#get diff and flags

#plot

#update project


### start with simple baby data for testing -----
  data <- proj$data
  data <- data %>% filter(DateTime_rd <= "2024-08-02 12:00:00")
  data[34:45, 10] <- NA #make a gap of removed data (for 1 analyte)
  data <- data[-(20:23),] #make a gap of fully missing data (no data collected)
  data$fDOM_QSU[data$DupNum == 2] <- data$fDOM_QSU[data$DupNum == 2] * 1.2
  #data <- data %>% filter(DupNum == 1) #keep duplicates out for right now
  #data <- data[-c(3040:3090),] # full remove a section
  #data$fDOM_QSU <- ifelse(data$fDOM_QSU < 6, NA, data$fDOM_QSU) #remove negatives so we have something to fill

#always complete so we can try and fill gaps if no data was collected (missing datetime)

  #fill in missing lines
  data_fill <- data %>% complete(DateTime_rd = seq(min(DateTime_rd), max(DateTime_rd), by = paste(interval, "min"))) %>%
    arrange(.data$DateTime_rd) %>% #want to arrange in time order for filling
    mutate(Index = 1:n(),
           DupNum = ifelse(is.na(.data$DupNum), 1, .data$DupNum),
           FileName = ifelse(is.na(.data$FileName), "interpolated", .data$FileName),
           Date = if_else(is.na(.data$Date), as.Date(.data$DateTime_rd, tz = tz), .data$Date),
           Time_HH_mm_ss = if_else(is.na(.data$Time_HH_mm_ss), strftime(.data$DateTime_rd, "%H:%M:%S"), .data$Time_HH_mm_ss),
           DateTime = if_else(is.na(.data$DateTime), .data$DateTime_rd, .data$DateTime),
           Site_Name = name)

  data_prefill <- data_fill

#option 1: linear interpolation -----
if(method == "linear"){
  data_fill[[y_var]] <- zoo::na.approx(data_fill[[y_var]], maxgap = max_length*(60/interval))
}

if(method == "spline"){
  data_fill[[y_var]] <- zoo::na.spline(data_fill[[y_var]], maxgap = max_length*(60/interval))
}

if(method == "random_forest"){
  rf_data <- data_fill %>% select(-any_of(c("DateTime_rd", "FileName", "Date", "Time_HH_mm_ss", "DateTime", "Site_Name", "DupNum")))
  rf_data <- as.data.frame(rf_data)
  filled <- missForest::missForest(rf_data)

  #only fill to max gap
  x_fill <- .fill_short_gaps(data_fill[[y_var]], filled$ximp[[y_var]], maxgap = max_length*(60/interval))
  data_fill[[y_var]] <- x_fill
}

if(method == "ts_interp"){
  #make time series
  var_ts <- ts(data_fill[[y_var]], frequency = frequency)
  #only fill to max gap
  x_fill <- .fill_short_gaps(data_fill[[y_var]], as.numeric(forecast::na.interp(var_ts)), maxgap = max_length*(60/interval))
  data_fill[[y_var]] <- x_fill
}

#plot for now to check, probably want plotting module in this as well
ggplot() +
  geom_line(data= data, aes(x=DateTime_rd, y=.data[[y_var]]), color="darkgreen") +
  geom_line(data = data_fill, aes(x=DateTime_rd, y=.data[[y_var]]), color="darkred") +
  geom_line(data = data_prefill, aes(x=DateTime_rd, y=.data[[y_var]]), color="gray30")

ggplotly(p)

#filter out parts from complete that weren't filled
#set as new data
#add flags
#update project
#
