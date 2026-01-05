#' Get USGS Sonde Parameter Limits
#'
#' Pulls daily water quality records from USGS for stations within the specified ecoregion and returns
#' the distribution of the minimum and maximum values for use in sonde QA-QC.
#'
#' @note
#' You are likely to run into API limits with this function unless you request an [API](https://api.waterdata.usgs.gov/signup/) from USGS
#'
#' If a ecoregion returns more than 300 sites for a particular parameter, it will randomly sample 500 of the sites to
#' determine the limits, to conserve computing power and USGS resources.
#'
#' @param ecoregion Name for Level III North America Ecoregion (NA_L3NAME)
#' @param parameter USGS parameter code(s) for the parameters you'd like to get limits of
#' @md
#'
#' @export
#' @returns
#' A data.frame for each parameter code that has two rows and 10 columns:
#' - statistic_id: 01 is max daily value, 02 is min daily value
#' - max: the maximum of the daily min or max values
#' - q999: the 99.9th quantile of the daily min or max values
#' - q99: the 99th quantile of the daily min or max values
#' - q95: the 95th quantile of the daily min or max values
#' - q05: the 5th quantile of the daily min or max values
#' - q01: the 1st quantile of the daily min or max values
#' - q001: the 0.1th quantile of the daily min or max values
#' - min: the minimum of the daily min or max values
#' - n: the number of daily values used to calculate metrics
#' - par: the USGS parameter code
#' - stat_n: the number of USGS stations used to calculate metrics
#'
#' If multiple parameters are requested, it will return a list of data.frames
#'
#' @details
#' Potential parameter code common for USGS continuous water quality:
#' - 00010: Temperature, water, degrees Celsius
#' - 00095: Specific conductance, water, unfiltered, microsiemens per centimeter at 25 degrees Celsius
#' - 00300: Dissolved oxygen, water, unfiltered, milligrams per liter
#' - 00301: Dissolved oxygen, water, unfiltered, percent of saturation
#' - 00400: pH, water, unfiltered, field, standard units
#' - 00480: Salinity, water, unfiltered, parts per thousand
#' - 32295: Dissolved organic matter fluorescence (fDOM), water, in situ, concentration estimated from reference material, micrograms per liter as quinine sulfate equivalents (QSE)
#' - 32322: Dissolved organic matter relative fluorescence (fDOM), water, in situ, relative fluorescence units (RFU)
#' - 63680: Turbidity, water, unfiltered, monochrome near infra-red LED light, 780-900 nm, detection angle 90 +-2.5 degrees, formazin nephelometric units (FNU)
#' - 72147: Depth of sensor below water surface, feet
#' - 99409: Suspended sediment concentration, water, unfiltered, estimated by regression equation, milligrams per liter
#'
#' @examples
#' get_eco_limits("Blue Mountains", "32295")
get_eco_limits <- function(ecoregion, parameter){

  stopifnot(ecoregion %in% SondePolishR::ecoregions$NA_L3NAME, all(is.character(parameter)))

  #get par codes
  pars <- parameter

  #get stations in ecoregion
  ecoregions <- SondePolishR::ecoregions
  eco <- ecoregions[ecoregions$NA_L3NAME == ecoregion,]
  eco <- sf::st_transform(eco, crs="EPSG:4326") #transform to corret crs

  bbox <- terra::ext(eco)
  bbox <- c(bbox[1], bbox[3], bbox[2], bbox[4])

  #identify stations in ecoregion
  stats <- dataRetrieval::read_waterdata_monitoring_location(
    agency_code = "USGS",
    site_type = "Stream",
    bbox = bbox,
    skipGeometry = TRUE,
    properties = c("monitoring_location_id",
                   "site_type", "state_name"))

  #get stations with data
  #split to not exceed limits on API
  chunk_size <- 300
  group_factor <- ceiling(seq_len(nrow(stats)) / chunk_size)
  stats_split <- split(stats, group_factor)

  stats_useful <- purrr::map(stats_split, ~ dataRetrieval::read_waterdata_ts_meta(
    monitoring_location_id = .x$monitoring_location_id,
    parameter_code = pars,
    statistic_id = c("00001", "00002"),
    properties = c("monitoring_location_id",
                   "parameter_code",
                   "begin",
                   "end",
                   "time_series_id",
                   "statistic_id"),
    skipGeometry = TRUE
  ))

  #output1

  #drop any lists without any stations
  empty <- sapply(stats_useful, function(y){any(apply(y, 2, function(x) all(is.na(x))))})

  if(sum(empty) > 0){
    stats_useful <- stats_useful[!empty]
  }

  #stop here if no data
  if(length(stats_useful) == 0){
    message("No stations for parameter code(s): ", pars, " were found for ecoregion: ", eco$US_L3NAME)
    return(NA)
  }

  #otherwise keep going
  stats_useful <- stats_useful %>% dplyr::bind_rows() %>% dplyr::arrange(stats_useful$parameter_code) %>% tidyr::drop_na() %>%
    dplyr::filter(.data$end > as.POSIXct(Sys.Date() - 10*360)) %>%
    dplyr::select(-c("statistic_id", "time_series_id", "begin", "end")) %>%
    dplyr::distinct()

  pars <- pars[pars %in%  unique(stats_useful$parameter_code)]

  if(length(pars) > 0){
    par_limit <- list()

    #get data
    for(par in pars){
      cat(paste("working on parameter", par))
      stats_par <- stats_useful[stats_useful$parameter_code == par,]

      # we don't need 1000's of records for a par, 300 random should give us a good idea of the limits
      set.seed(9)
      if(nrow(stats_par) > 300){
        stats_par <- stats_par[stats_par$monitoring_location_id %in% sample(stats_par$monitoring_location_id, 300, replace=FALSE),]
      }

      #split to not exceed limits on API
        if(par == "00010"){
          time <- "P13Y"
        }else{time <- "P30Y"}

      chunk_size <- 10
      if(chunk_size < nrow(stats_par)){
        group_factor <- ceiling(seq_len(nrow(stats_par)) / chunk_size)
        stats_split <- split(stats_useful, group_factor)
      }else{
        stats_split <- list(stats_par)
      }


      stats_data <- suppressWarnings(purrr::map(stats_split, ~ dataRetrieval::read_waterdata_daily(
        monitoring_location_id = .x$monitoring_location_id,
        statistic_id = c("00001", "00002"),
        time = time,
        parameter_code = par,

        skipGeometry = TRUE
      )))


      stats_merge <- stats_data %>% dplyr::bind_rows() %>% dplyr::group_by(.data$statistic_id) %>%
        dplyr::filter(.data$approval_status == "Approved")

      if(nrow(stats_merge) > 0){
        par_lim <- stats_merge %>%
          dplyr::summarise(max= max(.data$value),
                           q999 = stats::quantile(.data$value, 0.999),
                           q99 = stats::quantile(.data$value, 0.99),
                           q95 = stats::quantile(.data$value, 0.95),
                           q05 = stats::quantile(.data$value, 0.05),
                           q01 = stats::quantile(.data$value, 0.01),
                           q001 = stats::quantile(.data$value, 0.001),
                           min = min(.data$value),
                           n = dplyr::n()) %>% mutate(par = par,
                                                      n_stats = length(unique(stats_par$monitoring_location_id)))

      }else{
       par_lim <- NA
      }

    par_limit[[par]] <- par_lim

    }}

  if(length(par_limit) == 1){par_limit <- par_limit[[1]]}

  return(par_limit)

  }
