##testing methods of getting precip to match and plot with the data

#NASA power (easy to get and global, but doesn't seem the most accurate ------
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
  precip_clean <- precip %>% mutate(DateTime_UTC = as.POSIXct(paste(.data$YEAR, .data$MO, .data$DY, .data$HR, sep="-"), format="%Y-%m-%d-%H", tz="UTC"),
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

  #units seem weird... testing with published data to compare
  precip  <- get_power(community = "ag",lonlat = c(-81.875, 30),
                       pars = c("PRECTOTCORR"),dates = c("2003-06-18", "2003-06-25"),temporal_api = "hourly",
                       time_standard = "UTC")
  precip_clean <- precip %>% mutate(DateTime_UTC = as.POSIXct(paste(.data$YEAR, .data$MO, .data$DY, .data$HR, sep="-"), format="%Y-%m-%d-%H", tz="UTC"))

  ggplot(precip_clean, aes(x=DateTime_UTC, y=PRECTOTCORR/24)) + geom_line()

  #if I divide by 24 and plot in UTC the data exactly matches the figure for MERRA-2 in figure 9 here: https://journals.ametsoc.org/view/journals/clim/30/5/jcli-d-16-0570.1.xml

  #so I think that it is for sure in UTC and dividing by 24 seems to be accurate for getting it to mm/hr
  #however, it still looks funky compared to reality so I'd still like to get the other method to work


# using giovanni to get NLDAS data over time (hopefully)------
  library(earthdatalogin)
  edl_netrc() #set credentials
  edl_unset_netrc()
  url <- "https://data.lpdaac.earthdatacloud.nasa.gov/lp-prod-protected/HLSL30.020/HLS.L30.T56JKT.2023246T235950.v2.0/HLS.L30.T56JKT.2023246T235950.v2.0.SAA.tif"

  terra::rast(url, vsi=TRUE)
"~"

#manually create files
urs <- "urs.earthdata.nasa.gov"

home_dir <- path.expand("~")

# Create .urs_cookies
urs_cookies <- file.path(home_dir, ".urs_cookies")
file.create(urs_cookies)

# Create .dodsrc
dodsrc <- file.path(home_dir, ".dodsrc")
writeLines(
  c(
    sprintf("HTTP.COOKIEJAR=%s", urs_cookies),
    sprintf("HTTP.NETRC=%s", file.path(home_dir, ".netrc"))
  ),
  dodsrc
)

cat("Saved .urs_cookies and .dodsrc to:", home_dir, "\n")

# Copy .dodsrc to the working directory on Windows
if (.Platform$OS.type == "windows") {
  file.copy(
    from = dodsrc,
    to = getwd(),
    overwrite = TRUE
  )
  cat("Copied .dodsrc to:", getwd(), "\n")
}


earthdata_login <- function(username = NULL,
                            password = NULL,
                            overwrite = FALSE) {

  if (!requireNamespace("getPass", quietly = TRUE))
    stop("Please install the 'getPass' package.")

  if (is.null(username))
    username <- readline("Earthdata username: ")

  if (is.null(password))
    password <- getPass::getPass("Earthdata password: ")

  home <- path.expand("~")

  netrc_name <- ".netrc"

  netrc <- file.path(home, netrc_name)
  dodsrc <- file.path(home, ".dodsrc")
  cookies <- file.path(home, ".urs_cookies")

  if (!overwrite && file.exists(netrc))
    stop(netrc, " already exists.")

  writeLines(
    sprintf(
      "machine urs.earthdata.nasa.gov login %s password %s",
      username,
      password
    ),
    netrc
  )

  if (!file.exists(cookies))
    file.create(cookies)

  writeLines(
    c(
      sprintf("HTTP.COOKIEJAR=%s", cookies),
      sprintf("HTTP.NETRC=%s", netrc)
    ),
    dodsrc
  )

  if (.Platform$OS.type != "windows") {
    Sys.chmod(netrc, mode = "600")
  } else {
    file.copy(dodsrc, getwd(), overwrite = TRUE)
  }

  invisible(
    list(
      netrc = netrc,
      dodsrc = dodsrc,
      cookies = cookies
    )
  )
}


call_time_series <- function(lat,
                             lon,
                             time_start,
                             time_end,
                             data,
                             token) {

  request(time_series_url) |>
    req_url_query(
      data = data,
      location = sprintf("[%s,%s]", lat, lon),
      time = sprintf("%s/%s", time_start, time_end)
    ) |>
    req_headers(
      Authorization = paste("Bearer", token)
    ) |>
    req_perform() |>
    resp_body_string() |>
    read.csv(text = _)
}


earthdata_login(overwrite=TRUE)

#trying to get to work with python code straight
library(reticulate)

### got the url version of api.giovanni to work can we run it via code....? ------
#works on browser (with authentication files in C)
url <-"https://api.giovanni.earthdata.nasa.gov/proxy-timeseries?data=NLDAS_FORA0125_H_2_0_Rainf&location=[43.15,-123.36]&time=2024-06-01T00:00:00/2024-06-01T07:30:00"
dat <- read.csv(url)


library(httr2)

browseURL(url)



url <- request("https://api.giovanni.earthdata.nasa.gov/timeseries") |>
  req_url_query(
    data = "NLDAS_FORA0125_H_2_0_Rainf",
    location = "[43.15,-123.36]",
    time = "2024-06-01T00:00:00/2024-06-01T07:30:00",
    version = "5.12.4"
  ) %>%
  req_headers(
    Authorization = paste("Bearer", token)
  )

resp <- url |>
  req_perform()

dat <- resp |>
  resp_body_string() |>
  I() |>
  read_csv()

#will want to make sure start and end are in UTC
start <- as.POSIXct("2024-07-31 00:00:00", tz = "UTC")
end <- as.POSIXct("2024-08-16 00:00:00", tz = "UTC")
get_precip <- function(token=token, lat=43.15, long=-123.36, start=start, end = end){
  start <- lubridate::with_tz(start, tz="UTC")
  end <- lubridate::with_tz(end, tz="UTC")
  time <- paste(
    format(c(start, end), "%Y-%m-%dT%H:%M:%S"),
    collapse = "/"
  )

  url <- request("https://api.giovanni.earthdata.nasa.gov/timeseries") %>%
    req_url_query(
      data = "NLDAS_FORA0125_H_2_0_Rainf",
      location = paste0("[", lat, ",", long, "]"),
      time = time,
      version = "5.12.4"
    ) %>%
    req_headers(
      Authorization = paste("Bearer", token)
    )

  resp <- url %>%
    req_retry(
      max_tries = 10,
      is_transient = \(resp) resp_status(resp) %in% c(429, 500, 503),
      backoff = ~ runif(1, 1, 5)
    ) %>%
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

test <- get_precip(token, 43.15, -123.36, start, end)
