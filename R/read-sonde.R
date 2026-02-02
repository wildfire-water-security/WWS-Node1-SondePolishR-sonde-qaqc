#' Upload raw sonde data
#'
#' Loads a `.csv` file with raw sonde data. Cleans column names, adds a `DateTime` column,
#' and optionally adds a `flag` column.
#'
#' @param file file path to sonde data (.csv)
#' @param encoding the file encoding, will guess if NULL
#' @param skip the number of rows to skip, assumes the first row is a header,  will guess if NULL
#' @param flags logical, if TRUE a flag column will be added for each parameter
#' @param tz the time zone for the dataset. If not specified will use user timezone. See details for more information
#'
#' @importFrom magrittr %>%

#' @details
#' Daylights saving time can cause issues for continuous datasets, so data is often collected in standard time.
#' If this is the case use Etc/GMT offsets. See \link[base]{OlsonNames} for all available timezones. Note that
#' Etc/GMT signs are **reverse** of UTC signs, so for example UTC-8 would be Etc/GMT+8,
#' signifying Pacific Standard Time (PST).
#'
#' @md
#'
#' @returns
#' a data.frame of sonde data
#' @export
#'
#' @examples
#' file <- file.path(fs::path_package("extdata", package = "SondePolishR"), "sonde-example.csv")
#' df <- read_sonde(file)
#'
#' file <- file.path(fs::path_package("extdata", package = "SondePolishR"), "sonde-usb-example.csv")
#' df <- read_sonde(file)
read_sonde <- function(file, encoding = NULL, flags=TRUE, skip=NULL, tz="Etc/GMT+8"){
  stopifnot(tools::file_ext(file) == "csv", file.exists(file))

  #guess timezone
  if(is.null(tz)){tz <- Sys.timezone(location = TRUE)}

  #read file in
    #guess encoding
    if(is.null(encoding)){encoding <- get_encoding(file)}

    #read file
    text <- readLines(file, skipNul = TRUE, encoding = encoding)
    text <- utf8::as_utf8(text)

    #remove empty lines
    text <- text[text != ""]

    #determine if usb export
    usb_export <- any(grepl("Model, Submodel", text[2:6], fixed=TRUE))

    if(is.null(skip)){
      skip <- ifelse(usb_export, grep("^Date", text) + 3, grep("^Date", text))
    }

  #get column names
    cols <- text[grep("^Date", text)]
    cols <- iconv(cols, "UTF-8", "ASCII//TRANSLIT") #remove non ASCII characters
    cols <- gsub(" |[(]|[)]|[/]|[:]|[-]|[.]", "_", cols)
    cols <- unlist(strsplit(cols, ","))
    cols <- gsub("_$", "", gsub("_{1,}", "_", cols))
    cols <- gsub("[?]S", "uS", cols)

  #split into nice df
    df <- text[-c(1:skip)] %>% as.data.frame() %>% tidyr::separate_wider_delim(cols='.', delim=",", names_sep="")
    df[df == ""] <- NA     #replace "" with NA
    colnames(df) <- cols

  #drop any NA col names
    df <- df[,!is.na(colnames(df))]

  #save site name
    if("Site_Name" %in% colnames(df)){site <- df$Site_Name[1]
    }else{
      site <- ifelse(usb_export, stringr::str_split_i(text[grep("Site:", text)], ",", 2), stop("site row not determined"))
    }

  #rename col names
    lookup <- c(Date_MM_DD_YYYY = "Date", Time_HH_mm_ss = "Time", Temp_C="?C",
                Temp_C = "Temp_?C",
                ODO_sat = "DO_%", ODO_mg_L = "DO_mg_L",
                SpCond_uS_cm = "SPC_uS_cm",
                Turbidity_FNU = "NTU", Battery_V ="Batt_V")
    df <- df %>% dplyr::rename(any_of(lookup))

  #some data cleaning
    #drop all NA columns
    df <- df[, !apply(df, 2, function(x) all(is.na(x)))]

    #drop columns that don't change
    if(nrow(df) > 1){
      df <- df[, !apply(df, 2, function(x) length(unique(x)) == 1)]
    }

    #add site column
      df <- df %>% dplyr::mutate(Site_Name = site, .after="Time_HH_mm_ss")


    #make date and time back to character to match csv
    df$Time_HH_mm_ss <- as.character(df$Time_HH_mm_ss)
    df$Time_HH_mm_ss <- sub("^([0-9]):", "0\\1:", df$Time_HH_mm_ss)

  #add obs index for tracking easier
  df <- df %>% dplyr::mutate(Index = 1:dplyr::n(), .before="Date_MM_DD_YYYY") %>% dplyr::select(!any_of("Time_Fract_Sec"))

  #remove any duplicated header rows
  extra_header <- c(grep("^Date", as.character(df$Date_MM_DD_YYYY)), which(as.character(df$Date_MM_DD_YYYY) == ""))
  if(length(extra_header) >0){
    extra <- which(is.na(df$Date_MM_DD_YYYY[1:extra_header]))
    df <- df[-c(extra, extra_header),]
    }

  #make date time into a column set to correct tz
  df <- df %>% dplyr::mutate(DateTime = anytime::anytime(paste(df$Date_MM_DD_YYYY, df$Time_HH_mm_ss),
                                                  asUTC=TRUE, tz="UTC"),
                      .after="Time_HH_mm_ss") %>%
    dplyr::mutate(Date_MM_DD_YYYY = anytime::anydate(df$Date_MM_DD_YYYY, asUTC = TRUE, tz="UTC"))

  #set time zone
    df$Date_MM_DD_YYYY <- lubridate::force_tz(df$Date_MM_DD_YYYY, tzone=tz)
    df$DateTime <- lubridate::force_tz(df$DateTime, tzone=tz)

    #round to nearest minute
    df$DateTime <- as.POSIXct(round.POSIXt(df$DateTime, units="mins"))

  #make sure things that look numeric are
    numeric <- colnames(df)[!colnames(df) %in% c("Date_MM_DD_YYYY", "Time_HH_mm_ss", "DateTime", "Site_Name")]

  #make numeric and remove time fract sec
    df <- df %>% dplyr::mutate(dplyr::across(all_of(numeric), as.numeric))

 #add flags
  if(flags){
    #guess pars
    pars <- paste(c("Cond", "fDOM", "ODO", "Sal", "TDS", "Turbidity","TSS","pH","Temp", "Depth"), collapse="|")
    par_names <- grep(pars, names(df), value = TRUE)

    #add spot for flags for each parameter
    for(x in par_names){
      df <- df %>% mutate(!!paste0(x, "_flag") := NA, .after=tidyselect::all_of(x))
    }
  }

  #clear the log and dataframe
    clear_log()
    clear_data()

    write_log("All", "Initial Load", n = 0, version = "raw", env=.pkgenv)
    write_data(df, "raw")

  return(df)

}
