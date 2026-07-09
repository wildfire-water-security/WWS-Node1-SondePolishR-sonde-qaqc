#' Read sonde field form
#'
#' Loads a `.csv` file with field visit metadata. Ensures correct column classes and cleans up
#' extra notes.
#'
#' @param file File path to field form data (.csv).
#' @param tz Timezone for the times in the form.
#'
#' @returns a `data.frame` with the following columns. The `file` to read in should have the same structure:
#' - **Date**: The date of the site visit.
#' - **Site_Code**: The site name or site code.
#' - **Time**: The start time of the field visit (used to guess out of water periods when data is missing).
#' - **Start_Sonde_Serial**: The serial number for the sonde in the water at the start of the visit.
#' - **Start_Sonde_Name**: The sonde name for the sonde in the water at the start of the visit.
#' - **End_Sonde_Serial**: The serial number for the sonde in the water at the end of the visit.
#' - **End_Sonde_Name**: The sonde name for the sonde in the water at the end of the visit.
#' - **Removal_Time**: The time the sonde was removed from the water.
#' - **Return_Time**: The time the sonde was returned to the water.
#' - **Next_Timepoint**: The next timepoint that data will be collected.
#'  This should be the next "good" time point after the sonde has been returned to the water, but is sometimes the timepoint
#'  when the wiper is checked and the sonde is out of the water.
#' - **Data_Download**: Logical, was data downloaded at this visit?
#' - **Download_Device**: The name of the device data was downloaded to.
#' - **Remove_Period**: Logical, is there a data disruption that merits removing data during the out of water period. Used to skip periods
#' where we have coarse out of water periods (missing times), but the data appears uninterrupted to prevent removing excess data.
#' - **Crew**: The names or initials of the people performing the field visit.
#' - **Weather**: A description of the weather during the field visit.
#' - **Notes**: Notes associated with the field visit.
#' - **DataEntry_Notes**: Additional notes from data entry. Can have any name that includes "Notes"
#'
#' @export
#' @md
#' @examples
#' file <- file.path(fs::path_package("extdata", package = "SondePolishR"), "example-fieldform.csv")
#' fieldform <- read_ff(file, tz="Etc/GMT+8")

read_ff <- function(file, tz){
  stopifnot(tools::file_ext(file) == "csv")

  #read in csv
  df <- read.csv(file, colClasses = c(Start_Sonde_Serial = "character",
                                    End_Sonde_Serial = "character"))

  #check that file looks correct
    #rename cols with PST (OSU specific)
    df <- df %>% rename_with(~ gsub("_PST", "", .x))

  if(!(all(c("Date", "Time", "Removal_Time", "Return_Time", "Next_Timepoint", "Remove_Period") %in% colnames(df)))){
    stop("Unexpected column names. Please see help(example_fieldform) for details on structure.")
  }

  #get correct date format
  dateform <- lubridate::parse_date_time(df$Date, orders = c("mdY", "mdy", "Ymd"), tz=tz) %>% as.Date()

  #ensure things have the right class
  df <- df %>% dplyr::mutate(Date = dateform,
                      dplyr::across(c("Site_Code":"Next_Timepoint", "Download_Device", "Crew":"Notes"), ~as.character(.x)),
                      dplyr::across(c("Data_Download", "Remove_Period"), ~as.logical(.x)))

  #rename extra notes
  extra_notes <- grep("Notes.+|.+Notes", colnames(df), ignore.case = TRUE)
  if(length(extra_notes) > 0){
    colnames(df)[extra_notes] <- "DataEntry_Notes"

  }

  #replace blanks with NA
  df[df == ""] <- NA

  #remove any full NA rows
  df <- df[rowSums(is.na(df)) < ncol(df), ]

  df <- df %>% arrange(.data$Date)
  return(df)
}

#' Read sonde calibration check file
#'
#' Loads a `.csv` file with calibration checks performed during field visits.
#' Ensures correct column classes and converts NA to R recognized NA values.
#'
#' @param file File path to calibration check data (.csv).
#' @param tz Timezone for the times in the form.
#'
#' @returns a `data.frame` with the following columns. The `file` to read in should have the same structure:
#' - **Date**: The date of the site visit.
#' - **Site_Code**: The site name or site code.
#' - **Parameter**: The name of the parameter, should match column names from read_sonde.
#' - **Resident_Probe_Serial**: The serial number for the sonde located at the site.
#' - **Resident_Value**: The measured value for the parameter on the resident sonde.
#' - **Check_Probe_Serial**: The serial number for the sonde used to check the calibration. Should be freshly calibrated.
#' - **Check_Value**: The measured value for the parameter on the check sonde.
#' - **Notes**: Notes associated with the calibration check or data entry.
#'
#' @export
#' @md
#' @examples
#' file <- file.path(fs::path_package("extdata", package = "SondePolishR"), "example-calcheck.csv")
#' calcheck <- read_cal(file, tz="Etc/GMT+8")

read_cal <- function(file, tz){
  stopifnot(tools::file_ext(file) == "csv")

  #read in csv
  df <- read.csv(file)

  #check that file looks correct
  if(!(all(c("Date", "Parameter", "Resident_Value", "Check_Value") %in% colnames(df)))){
    stop("Unexpected column names. Please see help(example_calcheck) for details on structure.")
  }

  #make sure the parameter names match exactly
  df <- df %>% dplyr::mutate(Parameter = dplyr::case_when(grepl("Temp", .data$Parameter, ignore.case = TRUE) ~ "Temp_C",
                                                          grepl("Cond", .data$Parameter, ignore.case = TRUE) ~ "SpCond_uS_cm",
                                                          grepl("ODO|Oxygen", .data$Parameter, ignore.case = TRUE) ~ "ODO_mg_L",
                                                          grepl("pH$", .data$Parameter, ignore.case = TRUE) ~ "pH",
                                                          grepl("Turb", .data$Parameter, ignore.case = TRUE) ~ "Turbidity_FNU",
                                                          grepl("fDOM", .data$Parameter, ignore.case = TRUE) ~ "fDOM_QSU",
                                                          .default = .data$Parameter))

  #replace blanks and "N/A" with NA
  df[df == ""] <- NA
  df[df == "N/A"] <- NA

  #remove any full NA rows
  df <- df[rowSums(is.na(df)) < ncol(df), ]

  #get correct date format
    dateform <- lubridate::parse_date_time(df$Date, orders = c("mdY", "mdy", "Ymd"), tz=tz) %>% as.Date()

  #ensure things have the right class
  df <- df %>% dplyr::mutate(Date = dateform,
                             dplyr::across("Site_Code":"Resident_Probe_Serial", ~as.character(.x)),
                             dplyr::across(c("Check_Probe_Serial", "Notes"), ~as.character(.x)),
                             dplyr::across(c("Resident_Value", "Check_Value"), ~as.numeric(.x))) %>%
    arrange(.data$Date)

  return(df)


}


#' Convert field form data to out of water periods
#'
#' Use the information contained in the project field form and use the removal and return times to determine
#' the times the sonde was out of the water (OOW) for plotting and data QA/QC. Determines the start and end times
#' of when the sonde was out of the water. If a time is missing it will try to guess using the visit time, next timepoint,
#' or when no other options will remove the full day. If `Remove_Period` is FALSE it will ignore that period as an OOW period.
#'
#'
#' @param ff Field form `data.frame`.
#' @param tz Timezone for the times in the form.
#' @param interval Time (in minutes) between measurements, removes the measurement before and after OOW period for safety.
#'
#' @returns
#' A `data.frame`with three columns:
#' - **site_code**: The site code associated with the OOW period.
#' - **start**: The start of the OOW period stored as a `POSIXct`.
#' - **end**: The end of the OOW period, stored as a `POSIXct`
#' @export
#'
#' @examples
#' get_oow(example_sondeproj$fieldform, tz="Etc/GMT+8", interval=15)
get_oow <- function(ff, tz, interval){
  stopifnot(all(c("Site_Code", "Date", "Time", "Removal_Time", "Return_Time", "Next_Timepoint", "Remove_Period") %in% colnames(ff)))

  #make some new columns to manipulate
  ff_adj <- ff %>% dplyr::select("Site_Code", "Date", "Time", "Removal_Time", "Return_Time", "Next_Timepoint",
                                 "Data_Download", "Remove_Period") %>%
    dplyr::mutate(Remove_Period = ifelse(is.na(.data$Removal_Time) & is.na(.data$Return_Time) &
                                           is.na(.data$Next_Timepoint) & (is.na(.data$Data_Download) |
                                                                                !.data$Data_Download), FALSE, .data$Remove_Period)) %>%
    dplyr::filter(.data$Remove_Period) %>%
    dplyr::mutate(Date = as.Date(.data$Date, format = "%m/%d/%Y", tz=tz),
           remove_date = .data$Date,
           remove_time = ifelse(is.na(.data$Removal_Time),
                                ifelse(is.na(.data$Time), "00:00", .data$Time), .data$Removal_Time),
           return_date = .data$Date,
           return_time = ifelse(is.na(.data$Return_Time),
                                ifelse(is.na(.data$Next_Timepoint), "23:59", .data$Next_Timepoint), .data$Return_Time))


  #update dates/times if sonde removed and not returned till later
  long_oow <- which(!is.na(ff_adj$Removal_Time) & is.na(ff_adj$Return_Time) &
                      is.na(lead(ff_adj$Removal_Time)) & !is.na(lead(ff_adj$Return_Time)))

  if(length(long_oow) >0){
    ff_adj$return_date[long_oow] <- ff_adj$return_date[long_oow + 1]
    ff_adj$return_time[long_oow] <- ff_adj$return_time[long_oow + 1]
    ff_adj <- ff_adj[-(long_oow + 1),]
  }

  #convert to date times, give 15 minute buffer on either side to ensure we've cut enough
  #sometimes the next_timepoint is used to check the wiper and is bad, so we ideally want to use the return_time for the next good point
  ff_adj <- ff_adj %>%  dplyr::mutate(start = lubridate::floor_date(as.POSIXct(paste(.data$remove_date, .data$remove_time),
                                                                               tz=tz), paste(interval, "mins")) - lubridate::minutes(interval),
                               end = lubridate::ceiling_date(as.POSIXct(paste(.data$return_date, .data$return_time),
                                                                        tz=tz), paste(interval, "mins")) + lubridate::minutes(interval))

  #return the site code and data/time of OOW
  oow <- ff_adj %>% dplyr::select("Site_Code", "start", "end") %>% dplyr::rename("site_code" = "Site_Code")

  return(oow)

}
