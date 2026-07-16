#' Get hourly precipitation at data site
#'
#' Precipitation is downloaded from [NASA Power](https://power.larc.nasa.gov/) at an hourly scale based on the provided
#' coordinates.
#'
#' @param data the data to get matching precipitation data for
#' @param lat the latitude to get precipitation data at
#' @param long the longitude to get precipitation data at
#' @param method method used to get data either "merra-2" or "nldas"
#' @param token only required for nldas method. see details for how to obtain this token.
#' @md
#' @returns a data.frame with two columns:
#' - DateTime: The datetime (`POSIXct`) in the same timezone as the data, at an hourly resolution.
#' - Precip_mm_hr: Average precipitation at the requested point in mm per hour.
#' @export
#'
#' @examples
#' data <- example_data[example_data$Date == "2024-11-13",]
#' precip <- get_precip(data, 43.96775, -122.63012)
get_precip <- function(data, lat, long, method, token=NULL){
  stopifnot("DateTime_rd" %in% colnames(data), is.numeric(lat), is.numeric(long), method %in% c("merra-2", "nldas"))

  dates <- data %>% select("DateTime_rd") %>% mutate(DateTime_UTC = lubridate::with_tz(.data$DateTime_rd, tz="UTC"),
                                                   Dates = as.Date(.data$DateTime_UTC)) %>% summarise(min=min(.data$Dates),
                                                                                                max = max(.data$Dates))
  if(method == "merra-2"){
    #get precip from NASA Power
    precip  <- nasapower::get_power(community = "ag",lonlat = c(long, lat),
                                    pars = c("PRECTOTCORR"),dates = c(dates$min, dates$max),temporal_api = "hourly",
                                    time_standard = "UTC") %>%
      mutate(DateTime = as.POSIXct(paste(.data$YEAR, .data$MO, .data$DY, .data$HR, sep="-"),
                                                                format="%Y-%m-%d-%H", tz = "UTC")) %>%
      rename("Precip_mm_hr" = "PRECTOTCORR") %>% select("DateTime", "Precip_mm_hr") %>%
      mutate(Precip_mm_hr = .data$Precip_mm_hr/24) #despite saying it's in mm/hr appears to be mm/day
  }

  if(method == "nldas"){
    precip <- get_nldas(token = token, lat=lat, long=long, start=dates$min, end=dates$max)
  }

  #clean precip to match our data (change back to correct tz, get a datetime)
  precip_clean <- precip %>% mutate(DateTime = lubridate::with_tz(.data$DateTime, tz=tz(data$DateTime_rd))) %>%
    filter(.data$DateTime %in% data$DateTime_rd)

  return(precip_clean)

}

get_nldas <- function(token, lat, long, start, end){

  time <- paste(format(c(start, end), "%Y-%m-%dT%H:%M:%S"),collapse = "/")

  url <- request("https://api.giovanni.earthdata.nasa.gov/timeseries") %>%
    req_url_query(
      data = "NLDAS_FORA0125_H_2_0_Rainf",
      location = paste0("[", lat, ",", long, "]"),
      time = time,
      version = "5.12.4") %>%
    req_headers(Authorization = paste("Bearer", token))

  resp <- url %>%
    req_retry(
      max_tries = 10,
      is_transient = \(resp) resp_status(resp) %in% c(429, 500, 503),
      backoff = ~ runif(1, 1, 5)) %>%
    req_perform()

  txt <- resp %>%
    resp_body_string()

  if (grepl("message", txt) || nchar(txt) < 100) {
    stop("Fetching precipitation failed: ", txt)
  }

  dat <- txt %>%
    I() %>%
    textConnection() %>%
    read.csv(skip = 14) %>%
    dplyr::rename("Precip_mm_hr" = "Data", "DateTime"= "Timestamp..UTC.")

  return(dat)
}
