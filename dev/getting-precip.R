##testing methods of getting precip to match and plot with the data

library(nasapower)

#user supplied
lat <- 43.96775
long <- -122.63012
y_var <- "Turbidity_FNU"

data <- example_data
#server code
dates <- data %>% select(DateTime_rd) %>% mutate(DateTime_UTC = lubridate::with_tz(.data$DateTime_rd, tz="UTC"),
                                                 Dates = as.Date(DateTime_UTC)) %>% summarise(min=min(Dates),
                                                                                              max = max(Dates))

#get precip from NASA Power
precip  <- get_power(community = "ag",lonlat = c(long, lat),
  pars = c("PRECTOTCORR"),dates = c(dates$min, dates$max),temporal_api = "hourly",
  time_standard = "UTC")

#clean precip to match our data (change back to correct tz, get a datetime)
  precip_clean <- precip %>% mutate(DateTime_UTC = as.POSIXct(paste(.data$YEAR, .data$MO, .data$DY, .data$HR, sep="-"), format="%Y-%m-%d-%H"),
                                    DateTime = lubridate::with_tz(.data$DateTime_UTC, tz=tz(data$DateTime_rd))) %>%
                        filter(.data$DateTime %in% data$DateTime_rd) %>% rename("Precip_mm_hr" = "PRECTOTCORR") %>%
                        select("DateTime", "Precip_mm_hr")

#check see how it looks and how to plot
  # scale precip into y_var range
  precip_scale <- diff(range(data[[y_var]], na.rm = TRUE)) /
    diff(range(precip_clean$Precip_mm_hr, na.rm = TRUE))

  precip_offset <- min(data[[y_var]], na.rm = TRUE) -
    min(precip_clean$Precip_mm_hr, na.rm = TRUE) * precip_scale

  precip_clean <- precip_clean %>% mutate(xmax = .data$DateTime - lubridate::minutes(30),
                                          xmin = .data$DateTime + lubridate::minutes(30),
                                          ymax = .data$Precip_mm_hr * precip_scale + precip_offset,
                                          ymin = min(data[[y_var]], na.rm=TRUE))

  p<- ggplot() +
    geom_segment(data = precip_clean,aes(x=.data$DateTime, xend=.data$DateTime, y=ymin, yend=ymax),alpha = 0.3, color="gray40") +
     geom_line(data= data, aes(DateTime_rd, .data[[y_var]]), na.rm = TRUE)

  ggplotly(p)
