#' Read sonde field form
#'
#' Loads a `.csv` file with field visit metadata. Ensures correct column classes and cleans up
#' extra notes.
#'
#' @param file File path to field form data (.csv).
#'
#' @returns a `data.frame` with the following columns. The `file` to read in should have the same structure:
#' - **Date**: The date of the site visit.
#' - **Site_Code**: The site name or site code.
#' - **Time_PST**: The start time of the field visit (used to guess out of water periods when data is missing).
#' - **Start_Sonde_Serial**: The serial number for the sonde in the water at the start of the visit.
#' - **Start_Sonde_Name**: The sonde name for the sonde in the water at the start of the visit.
#' - **End_Sonde_Serial**: The serial number for the sonde in the water at the end of the visit.
#' - **End_Sonde_Name**: The sonde name for the sonde in the water at the end of the visit.
#' - **Removal_Time_PST**: The time the sonde was removed from the water.
#' - **Return_Time_PST**: The time the sonde was returned to the water.
#' - **Next_Timepoint_PST**: The next timepoint that data will be collected.
#'  This should be the next "good" time point after the sonde has been returned to the water, but is sometimes the timepoint
#'  when the wiper is checked and the sonde is out of the water.
#' - **Data_Download**: Logical, was data downloaded at this visit?
#' - **Download_Device**: The name of the device data was downloaded to.
#' - **Remove_Period**: Logical, is there a data disruption that merits removing data during the out of water period. Used to skip periods
#' where we have coarse out of water periods (missing times), but the data appears uninterupted to prevent removing excess data.
#' - **Crew**: The names or initials of the people performing the field visit.
#' - **Weather**: A description of the weather during the field visit.
#' - **Notes**: Notes associated with the field visit.
#' - **DataEntry_Notes**: Additional notes from data entry. Can have any name that includes "Notes"
#'
#' @export
#' @md
#' @examples
#' file <- file.path(fs::path_package("extdata", package = "SondePolishR"), "example-fieldform.csv")
#' fieldform <- read_ff(file)

read_ff <- function(file){
  stopifnot(tools::file_ext(file) == "csv")

  #read in csv
  df <- read.csv(file, colClasses = c(Start_Sonde_Serial = "character",
                                    End_Sonde_Serial = "character"))

  #check that file looks correct
  if(!(all(c("Date", "Time_PST", "Removal_Time_PST", "Return_Time_PST", "Next_Timepoint_PST", "Remove_Period") %in% colnames(df)))){
    stop("Unexpected column names. Please see help(example_fieldform) for details on structure.")
  }

  #get correct date format
    #add option for two digit year (only for function)
    anytime::addFormats("%m/%d/%y")
    dateform <- anytime::anydate(df$Date)
    anytime::removeFormats("%m/%d/%y")

  #ensure things have the right class
  df <- df %>% dplyr::mutate(Date = dateform,
                      dplyr::across(c("Site_Code":"Next_Timepoint_PST", "Download_Device", "Crew":"Notes"), ~as.character(.x)),
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
#' calcheck <- read_cal(file)

read_cal <- function(file){
  stopifnot(tools::file_ext(file) == "csv")

  #read in csv
  df <- read.csv(file)

  #check that file looks correct
  if(!(all(c("Date", "Parameter", "Resident_Value", "Check_Value") %in% colnames(df)))){
    stop("Unexpected column names. Please see help(example_calcheck) for details on structure.")
  }

  #replace blanks and "N/A" with NA
  df[df == ""] <- NA
  df[df == "N/A"] <- NA

  #remove any full NA rows
  df <- df[rowSums(is.na(df)) < ncol(df), ]

  #get correct date format
    #add option for two digit year (only for function)
    anytime::addFormats("%m/%d/%y")
    dateform <- anytime::anydate(df$Date)
    anytime::removeFormats("%m/%d/%y")

  dateform <- anytime::anydate(df$Date)

  #ensure things have the right class
  df <- df %>% dplyr::mutate(Date = dateform,
                             dplyr::across("Site_Code":"Resident_Probe_Serial", ~as.character(.x)),
                             dplyr::across(c("Check_Probe_Serial", "Notes"), ~as.character(.x)),
                             dplyr::across(c("Resident_Value", "Check_Value"), ~as.numeric(.x))) %>%
    arrange(.data$Date)

  return(df)


}
