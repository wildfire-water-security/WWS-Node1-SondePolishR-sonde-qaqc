#' Get hourly precipitation at data site
#'
#' Precipitation is downloaded from [NASA Power](https://power.larc.nasa.gov/) at an hourly scale based on the provided
#' coordinates.
#'
#' @param data the data to get matching precipitation data for
#' @param lat the latitude to get precipitation data at
#' @param long the longitude to get precipitation data at
#' @md
#' @returns a data.frame with two columns:
#' - DateTime: The datetime (`POSIXct`) in the same timezone as the data, at an hourly resolution.
#' - Precip_mm_hr: Average MERRA-2 bias corrected total precipitation at the surface of the earth in mm per hour.
#' @export
#'
#' @examples
#' data <- example_data %>% filter(Date == "2024-08-01")
#' get_precip(data, 43.96775, -122.63012)
get_precip <- function(data, lat, long){
  stopifnot("DateTime_rd" %in% colnames(data), is.numeric(lat), is.numeric(long))

  dates <- data %>% select(DateTime_rd) %>% mutate(DateTime_UTC = lubridate::with_tz(.data$DateTime_rd, tz="UTC"),
                                                   Dates = as.Date(DateTime_UTC)) %>% summarise(min=min(Dates),
                                                                                                max = max(Dates))
  #get precip from NASA Power
  precip  <- nasapower::get_power(community = "ag",lonlat = c(long, lat),
                       pars = c("PRECTOTCORR"),dates = c(dates$min, dates$max),temporal_api = "hourly",
                       time_standard = "UTC")

  #clean precip to match our data (change back to correct tz, get a datetime)
  precip_clean <- precip %>% mutate(DateTime_UTC = as.POSIXct(paste(.data$YEAR, .data$MO, .data$DY, .data$HR, sep="-"), format="%Y-%m-%d-%H"),
                                    DateTime = lubridate::with_tz(.data$DateTime_UTC, tz=tz(data$DateTime_rd))) %>%
    filter(.data$DateTime %in% data$DateTime_rd) %>% rename("Precip_mm_hr" = "PRECTOTCORR") %>%
    select("DateTime", "Precip_mm_hr")

  return(precip_clean)

}
