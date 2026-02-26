#' Try to guess csv file encoding
#'
#' Turns out it's actually quite challenging to load in `.csv` file when it has a weird encoding. This will use \link[readr]{guess_encoding}
#' first to see if it can figure it out, if not, it will test the two likely encoding: UTF-16LE or Windows-1252.
#'
#' @param file file path to sonde data
#' @md
#' @returns
#' A character with the file encoding
#' @export
#'
#' @examples
#' file <- file.path(fs::path_package("extdata", package = "SondePolishR"), "sonde-example.csv")
#' get_encoding(file)
get_encoding <- function(file){
  #check if data looks right
  file_check <- function(data){
    if(ncol(data) > 2){
      return(TRUE)
    }else{return(FALSE)}
  }

  #find encoding
  enc_guess <- readr::guess_encoding(file, n_max=100, threshold = 0.95)

  #if encoding guess is good, use that
  if(nrow(enc_guess) > 0){
    data <- read.csv(file, fileEncoding = enc_guess$encoding[1], header = FALSE, skip=9)

    if(file_check(data)){
      return(enc_guess$encoding[1])
    }
  }

  #otherwise check Windows-1252
  data <- read.csv(file, fileEncoding = "Windows-1252")
  if(file_check(data)){return("Windows-1252")}

  #then check UTF-16LE
  data <- read.csv(file, fileEncoding = "UTF-16LE")
  if(file_check(data)){return("UTF-16LE")}

  #otherwise print message
  stop("Could not identify file encoding, please put in Notepad++ and look in bottom right corner to identify encoding")

}

#' Guess the number of rows to skip to get correct headers
#'
#' @param file file path to sonde data (.xlsx or .csv)
#' @param encoding the file encoding if file is a .csv, will guess if NULL
#'
#' @returns
#' a numeric indicating the number of rows to skip
#' @export
#'
#' @examples
#' file <- file.path(fs::path_package("extdata", package = "SondePolishR"), "sonde-example.csv")
#' get_skip(file)
#'
#' file <- file.path(fs::path_package("extdata", package = "SondePolishR"), "sonde-usb-example.csv")
#' get_skip(file)
get_skip <- function(file, encoding=NULL){
  if(usb_export(file)){
    if(is.null(encoding)){encoding <- get_encoding(file)}
    data <- suppressWarnings(readr::read_csv(file,
                                           locale = readr::locale(encoding = "UTF-16LE"),
                          col_names=FALSE,show_col_types = FALSE))
    skip <- grep("^Date", data[[1]]) + 5

  }else{
    if(is.null(encoding)){encoding <- get_encoding(file)}
    data <- read.csv(file, fileEncoding = encoding, header=FALSE)
    skip <- grep("^Date", data[,1]) -1
    if(encoding == "UTF-16LE"){skip <- skip + 3}
  }

  return(skip)
}

#' Detect if file was downloaded via a USB or not
#'
#' Sonde data files differ based on how the data is downloaded, this is used to detect so
#' data can be processed correctly.
#'
#' @param file file path to sonde data (.xlsx or .csv)
#'
#' @returns
#' Logical, if TRUE file has USB download structure, if FALSE file does not.
#' @export
#'
#' @examples
#' file <- file.path(fs::path_package("extdata", package = "SondePolishR"), "sonde-example.csv")
#' usb_export(file)
#'
#' file <- file.path(fs::path_package("extdata", package = "SondePolishR"), "sonde-usb-example.csv")
#' usb_export(file)
usb_export <- function(file){
  encoding <- get_encoding(file)
  data <- readLines(file, encoding = encoding, skipNul = TRUE)

  if(any(grepl("Model, Submodel", data[2:6], fixed=TRUE))){
    return(TRUE)
  }else{
    return(FALSE)
  }


}

#' Make nice timezones for selecting
#'
#' Uses \link[base]{OlsonNames} to extract potential timezones for the .csv file.
#'
#' @export
#'
#' @returns
#' a vector of time zones with names that are human readable
#'
#' @examples
#' head(nice_tz())

nice_tz <- function(){
  #getting timezones and making nice
  tz <- OlsonNames()[grepl("America|GMT", OlsonNames())]
  tz_nice <- gsub("GMT", "UTC", gsub("Etc/", "", gsub("[_]", " ", tz)))
  plus_flip <- grepl("\\+", tz_nice)
  minus_flip <- grepl("\\-", tz_nice)
  tz_nice[plus_flip] <- gsub("\\+", "\\-", tz_nice[plus_flip])
  tz_nice[minus_flip] <- gsub("\\-", "\\+", tz_nice[minus_flip])

  names(tz) <- tz_nice
  tz <- tz[!(names(tz) %in% c("UTC-0", "UTC0", "UTC"))]
  tz <- tz[order(tz, decreasing=TRUE)]

  return(tz)
}

#' Reformats column names to be human readable
#'
#' @param data the dataframe you want to column names from
#'
#' @returns a named vector where the names are the human readable names and the values are the column names
#'
nice_yvar <- function(data){
  #remove any variables that are totally 0 or NA
  empty <- sapply(data, function(x){
    if(is.list(x)){
      return(FALSE)
    }else{
      all(is.na(x) | x == 0)
    }
  })

  if(sum(empty) > 0){
    data <- data[-empty]
  }

  #remove non numeric
  y_var <- colnames(data)[sapply(data, is.numeric)]

  #remove variables that aren't needed
  y_var <- y_var[!(y_var %in% c("Index", "Time_Fract_Sec", "Wiper_Position_volt", "Cable_Pwr_V", "Battery_V"))]

  #give nice names
  nice_names <- c("Cond_uS_cm"= "Conductivity (\u03BCS/cm)",
                  "fDOM_QSU" = "fDOM (QSU)",
                  "fDOM_RFU" = "fDOM (RFU)",
                  "nLF_Cond_uS_cm" = "nLF Conductivity (\u03BCS/cm)",
                  "ODO_%_sat"  = "Dissolved Oxygen (% Saturation)",
                  "ODO_%_CB" = "Dissolved Oxygen (% Calibrated Barometer)",
                  "ODO_mg_L" = "Dissolved Oyxgen (mg/L)",
                  "Sal_psu"= "Salinity (ppm)",
                  "SpCond_uS_cm" = "Specific Conductance (\u03BCS/cm)",
                  "TDS_mg_L" = "Total Dissolved Solids (mg/L)",
                  "Turbidity_FNU" = "Turbidity (FNU)",
                  "TSS_mg_L" = "Total Suspended Solids (mg/L)",
                  "pH"  = "pH",
                  "pH_mV" = "pH (mV)",
                  "Temp_C" = "Temperature (\u00B0C)",
                  "Pressure_psi_a" = "Pressure (PSI)",
                  "Depth_m" = "Depth (m)",
                  "Vertical_Position_m" = "Vertical Position (m)")

  names(y_var) <- ifelse(
    y_var %in% names(nice_names),
    nice_names[y_var],
    y_var)

  return(y_var)
}

