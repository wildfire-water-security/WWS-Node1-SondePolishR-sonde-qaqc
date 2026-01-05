#' US EPA Level III Ecoregions
#'
#' Designed to serve as a spatial framework for the research, assessment, and
#' monitoring of ecosystems and ecosystem components, ecoregions denote areas of
#' similarity in the mosaic of biotic, abiotic, terrestrial, and aquatic ecosystem
#' components with humans being considered as part of the biota.
#'
#' @md
#' @format An sf object with 87 rows and 9 columns:
#'  - **US_L3CODE**: Code for Level III Ecoregion (US)
#'  - **US_L3NAME**: Name for Level III Ecoregion (US)
#'  - **NA_L3CODE**: Code for Level III Ecoregion (North America/CEC)
#'  - **NA_L3NAME**: Name for Level III Ecoregion (North America/CEC)
#'  - **NA_L2CODE**: Code for Level II Ecoregion (North America/CEC)
#'  - **NA_L2NAME**: Name for Level II Ecoregion (North America/CEC)
#'  - **NA_L1CODE**: Code for Level I Ecoregion (North America/CEC)
#'  - **NA_L1NAME**: Name for Level I Ecoregion (North America/CEC)
#'  - **geometry**: Spatial data associated with the polygon
#' @source US Environmental Protection Agency. (2013). Level III Ecoregions of the Conterminous United States [Data set](ftp://ftp.epa.gov/wed/ecoregions/us/us_eco_l3.zip).
#' U.S. EPA Office of Research and Development (ORD) - National Health and Environmental Effects Research Laboratory (NHEERL).

"ecoregions"


#' Physical Limits for Sonde Parameters
#'
#' Physical minimum and maximum limits used to perform preliminary filtering of sonde data.
#' Determined by ecoregion using USGS water quality data within each ecoregion.
#'
#' @details
#' - Minimum was determined as the minimum daily value observed across any site within the ecoregion.
#' - Maximum was determined as the highest value between the maximum of the daily minimum or the 99.9th percentile of the daily maximums.
#'
#' Only accepted data values were used in the calculations. See DATASET.R for exact process used to derived minimums and maximums.
#'
#' @md
#' @format An data.frame object with 29 rows and 5 columns:
#'  - **parameter**: USGS parameter code
#'  - **ecoregion**: Name for Level III Ecoregion (US)
#'  - **max**: Code for Level III Ecoregion (North America/CEC)
#'  - **min**: Name for Level III Ecoregion (North America/CEC)
#'  - **metric**: Sonde column name for parameter

"phys_limits"

#' Sample Sonde Data
#'
#' An example of the output from \link[SondePolishR]{read_sonde} function.
#'
#' @md
#' @format An data.frame object with 2209 rows and 23 columns:
#'  - **Date_MM_DD_YYYY**: Date of measurement
#'  - **Time_HH_mm_ss**: Time of measurement
#'  - **DateTime**: Date and time of measurement as a POSIXct
#'  - **Time_Fract_Sec**: Time in fractions of a second (usually not used)
#'  - **Site_Name**: Name of the site
#'  -**Cond_/u00B5S_cm** through **Cable_Pwr_V**: Sonde measurement variables
#' @source Forest Ecohydrology and Watershed Science Lab (2024) Fall Creek: 2024-04-22 to 2024-05-15. Data set.

"raw_sonde"

#' Sample Sonde Project
#'
#' An example of a sonde project, which saves the versions and changes made to the raw data.
#'
#' @md
#' @format A list where the names indicate the version name and each list item is a version of the dataset. The
#' first two items are special:
#' - **log**: the change log
#' - **raw**: the sonde data before any changes have been made

"example_project"

#' Sample Data Versioning
#'
#' An example of the data versioning object saved to the package environment as `data_ver`.
#'
#' @md
#' @format A list where the names indicate the version name and each list item is a version of the dataset. The
#' first item is special:
#' - **raw**: the sonde data before any changes have been made

"example_data_ver"

#' Sample Data Change Log
#'
#' An example of the change log object saved to the package environment as `log`. This tracks the changes made to the dataset.
#'
#' @md
#' @format An data.frame object with 2 rows and 6 columns:
#'  - **datetime**: The date and time the change was made
#'  - **parameter**: The parameter that was changed
#'  - **step**: The name of the correction step performed
#'  - **n_changed**: The number of data points modified
#'  - **user**: The name of the person who made the change
#'  - **version**: the version name indicated by a hash code
"example_log"

