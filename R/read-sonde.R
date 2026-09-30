#' Upload raw sonde data
#'
#' Loads a `.csv` file with raw sonde data. Turns into a `sonde` object.
#' Cleans column names, adds a `DateTime` column, and optionally adds a `flag` column.
#'
#' @param file File path to sonde data (.csv).
#' @param return What format should data be returned in? Either `df` or `list`.
#' @param encoding File encoding, will guess if `NULL`.
#' @param skip The number of rows to skip, assumes the first row is a header,  will guess if `NULL`.
#' @param flags Logical, if `TRUE` a flag column will be added for each parameter.
#' @param tz Time zone for the file. If not specified will use user timezone. See details for more information.
#'
#' @importFrom magrittr %>%
#' @details
#' Daylights saving time can cause issues for continuous datasets, so data is often collected in standard time.
#' If this is the case use Etc/GMT offsets. See \link[base]{OlsonNames} for all available timezones. Note that
#' Etc/GMT signs are **reverse** of UTC signs. For example UTC-8 would be Etc/GMT+8,
#' signifying Pacific Standard Time (PST).
#'
#' @md
#'
#' @returns
#' If `return` is `df`:
#' **A `data.frame` containing the date, time, site name, and the "core" measurements (when available):**
#' - Specific conductivity measured in µS/cm.
#' - Fluorescent dissolved organic matter (fDOM) measured in Quinine Sulfate Units (QSU).
#' - Dissolved oxygen measured in mg/L.
#' - Turbidity measured in Formazin Nephelometric Units (FNU).
#' - pH measured in pH units.
#' - Temperature measured in degrees C.
#' - Battery voltage measured in volts.
#'
#' If `return` is `list`:
#' **A list containing:**
#' - **serials**: the probe serial numbers associated with each measurement.
#' - **data**: the sonde data formatted as described above.
#'
#' @export
#'
#' @examples
#' file <- file.path(fs::path_package("extdata", package = "SondePolishR"), "example-csv-data1.csv")
#' data <- read_sonde(file)
#'
#' file <- file.path(fs::path_package("extdata", package = "SondePolishR"), "sonde-usb-example.csv")
#' data <- read_sonde(file, return = "list")
read_sonde <- function(file, return="df", encoding = NULL, flags=FALSE, skip=NULL, tz="Etc/GMT+8"){
  stopifnot(tools::file_ext(file) == "csv", file.exists(file), return %in% c("df", "list"))
  #guess timezone
  if(is.null(tz)){tz <- Sys.timezone(location = TRUE)}

  #read file in
    #guess encoding
      if(is.null(encoding)){encoding <- get_encoding(file)}

    #read file to get skip
      con <- file(file, open = "r", encoding = encoding)
      text <- readLines(con, n = 20)
      close(con)

    if(is.null(skip)){skip <- min(grep("[0-9]{1,2}/[0-9]{2}/[0-9]{4}[,]",text))-1}

  #read in file
    data <- readr::read_csv(file, locale = readr::locale(encoding = encoding), show_col_types = FALSE, col_names = FALSE,
                                             skip = skip)
  #get column names
    usb_export <- ifelse(any(grepl("Model", text[1:skip], ignore.case = TRUE)), TRUE, FALSE)
    col_skip <- ifelse(usb_export, skip-4, skip-1)
    cols <- readr::read_csv(file, locale = readr::locale(encoding = encoding), show_col_types = FALSE, col_names = FALSE,
                            skip = col_skip, n_max=1)

    cols <- as.character(cols)
    cols <- gsub(" |[(]|[)]|[/]|[:]|[-]|[.]", "_", cols) #replace spaces with underscores
    cols <- gsub("_$", "", gsub("_{1,}", "_", cols)) #remove underscores at the end
    colnames(data) <- cols

  #drop any NA col names
    data <- data[,!is.na(colnames(data))]

  #rename col names
    lookup <- c(
      Date = "Date_MM_DD_YYYY|Date_m_d_yyyy|Date",
      Time_HH_mm_ss = "Time_h_mm_ss_tt|Time_HH_mm_ss|^Time$",
      Temp_C = "\\?C|\\^0C|Temp_\\?C|Temp_\\^0C|Temp_C|Temp_\u00B0C|\u00B0C",
      Turbidity_FNU = "FNU|NTU|Turbidity_FNU",
      ODO_sat = "DO_%$|ODO_sat|ODO_%_SAT",
      ODO_mg_L = "DO_mg_L|ODO_mg_L",
      SpCond_uS_cm = "SPC_uS_cm|SpCond_uS_cm|SpCond_\U03BCS_cm|SpCond_\U00B5S_cm",
      Battery_V = "Batt_V|Battery_V",
      Depth_m = "DEP_m|Depth_m",
      fDOM_QSU = "fDOM_QSU",
      pH = "pH$",
      Site_Name = "SITE_NAME"
    )

    for(new_name in names(lookup)) {
      matches <- grepl(
        lookup[[new_name]],
        names(data),
        ignore.case = TRUE)
      names(data)[matches] <- new_name
    }

  #get serial numbers (needs to be here because it calls to colname of original data)
    if(!usb_export){
      serial <- readr::read_csv(file, locale = readr::locale(encoding = encoding), show_col_types = FALSE, col_names = FALSE,
                                skip = skip-2, n_max=1)
      colnames(serial) <- colnames(data)
      serials <- serial %>% select(any_of(c("SpCond_uS_cm","fDOM_QSU","ODO_mg_L", "Turbidity_FNU","pH","Temp_C","Battery_V")))
    }else{
      serial <- readr::read_csv(file, locale = readr::locale(encoding = encoding), show_col_types = FALSE, col_names = TRUE,
                                skip = 2, n_max=skip-7-2) #7 for extra rows, two for top

      #remove any empty rows/cols
      serial$Model <- gsub("[0-9]P Sonde", "Battery_V", serial$Model)
      add_c <- serial[which(serial$Model == "CT"),]
      add_c$Model <- "Temp_C"
      serial <- rbind(serial, add_c) %>% dplyr::mutate(Model = .data$Model %>%
                                                         dplyr::recode_values("Turbidity" ~ "Turbidity_FNU",
                                                                              "CT" ~ "SpCond_uS_cm",
                                                                              "ODO" ~ "ODO_mg_L",
                                                                              "fDOM" ~ "fDOM_QSU",
                                                                              default = .data$Model))
      serials <- serial %>% dplyr::rename(measure = "Model", serial = "S/N") %>% select("measure", "serial") %>%
        mutate(serial = trimws(serial)) %>% tidyr::pivot_wider(names_from="measure", values_from="serial")
    }

  #some data cleaning
    #remove the not directly measured analytes (this clutters and you can calculate them after the fact)
    data <- data %>% select(any_of(c("Date","Time_HH_mm_ss","Site_Name","SpCond_uS_cm","fDOM_QSU","ODO_mg_L",
                                   "Turbidity_FNU","pH","Temp_C","Battery_V", "Depth_m")))

    #remove any duplicated header rows
    extra_header <- c(grep("Date", as.character(data$Date), ignore.case = TRUE), which(as.character(data$Date) == ""))
    if(length(extra_header) >0){
      for(x in extra_header){
        extra <- which(is.na(data$Date[1:x]))
        data <- data[-c(extra, extra_header),]
      }}

    #save site name
    if("Site_Name" %in% colnames(data)){
      if(length(unique(data$Site_Name)) > 1){
        stop(paste0("multiple sites detected in file: ", basename(file)))}
      site <- data$Site_Name[1]
    }else{
      site <- ifelse(usb_export, stringr::str_split_i(text[grep("Site:", text)], ",", 2), stop("site row not determined"))
    }


    #drop all NA columns
    data <- data[, !apply(data, 2, function(x) all(is.na(x)))]

    #drop columns that don't change
    if(nrow(data) > 1){
      data <- data[, !apply(data, 2, function(x) length(unique(x)) == 1)]
    }

    #make date and time back to character to match csv
    data$Time_HH_mm_ss <- as.character(data$Time_HH_mm_ss)
    data$Time_HH_mm_ss <- sub("^([0-9]):", "0\\1:", data$Time_HH_mm_ss)

    #add obs index for tracking easier
    data <- data %>% dplyr::select(!any_of("Time_Fract_Sec"))

    #add site column
    data <- data %>% dplyr::mutate(Site_Name = site, .after="Time_HH_mm_ss")

  #make date time into a column set to correct tz
  data <- data %>% dplyr::mutate(Date = as.Date(anytime::anydate(data$Date, asUTC = TRUE, tz="UTC"))) %>%
    dplyr::mutate(DateTime = anytime::anytime(paste(.data$Date, gsub("AM|PM", "", .data$Time_HH_mm_ss, ignore.case = TRUE)),
                                                  asUTC=TRUE, tz="UTC"),
                      .after="Time_HH_mm_ss")

  #set time zone
    data$DateTime <- lubridate::force_tz(data$DateTime, tzone=tz)

    #round to nearest minute
    data$DateTime <- as.POSIXct(round.POSIXt(data$DateTime, units="mins"))

  #make sure things that look numeric are
    numeric <- colnames(data)[!colnames(data) %in% c("Date", "Time_HH_mm_ss", "DateTime", "Site_Name")]

  #make numeric and remove time fract sec
    data <- data %>% dplyr::mutate(dplyr::across(all_of(numeric), as.numeric))

  #create rounded datetime column for checking dups, gaps
    interval <- get_interval(data)

    data <- data %>%
      dplyr::mutate(DateTime_rd = lubridate::round_date(.data$DateTime, paste0(interval, " mins")), .after = "DateTime")

  #add file name
    data <- data %>% dplyr::mutate(FileName = basename(file))

  #organize order and make a regular df to be consistent
    data <- data %>% dplyr::select(dplyr::any_of(c("Index", "FileName", "Date", "Time_HH_mm_ss", "DateTime", "DateTime_rd", "Site_Name", "Battery_V",
                                   "Depth_m", "fDOM_QSU", "ODO_mg_L", "pH", "SpCond_uS_cm", "Temp_C", "Turbidity_FNU"))) %>% as.data.frame() %>%
      arrange(.data$DateTime) %>% dplyr::mutate(Index = 1:dplyr::n(), .before="FileName")

 #add flags
  if(flags){
    #guess pars
    par_names <- get_parms(data)

    #add spot for flags for each parameter
    for(x in par_names){
      data <- data %>% mutate(!!paste0(x, "_flag") := I(list(NA)), .after=tidyselect::all_of(x))
    }
  }

  #add date to serials
    serials <- serials %>% mutate(Date = min(data$Date))

  #turn in sonde object
    obj <- list(serials = serials, data = data)

  #return what is requested
  if(return == "df"){
    return(obj$data)
  }else{
    return(obj)
  }

}
