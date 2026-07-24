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

  #split into nice data
    data <- text[-c(1:skip)] %>% as.data.frame() %>% tidyr::separate_wider_delim(cols='.', delim=",", names_sep="")
    data[data == ""] <- NA     #replace "" with NA
    colnames(data) <- cols

  #drop any NA col names
    data <- data[,!is.na(colnames(data))]

  #rename col names
    lookup <- c(Date = "Date_MM_DD_YYYY", Time_HH_mm_ss = "Time", Temp_C="?C",
                Temp_C = "Temp_?C", Turbidity_FNU = "FNU",
                ODO_sat = "DO_%", ODO_mg_L = "DO_mg_L",
                SpCond_uS_cm = "SPC_uS_cm",
                Turbidity_FNU = "NTU", Battery_V ="Batt_V",
                Depth_m = "DEP_m")
    data <- data %>% dplyr::rename(any_of(lookup))

  #get serial numbers (needs to be here because it calls to colname of original data)
    if(!usb_export){
      serial <- unlist(strsplit(text[skip-1], ","))
      serials <- data.frame(measure = colnames(data)[-(1:4)], serial = serial[-(1:4)]) %>%
        filter(.data$measure %in% c("SpCond_uS_cm","fDOM_QSU","ODO_mg_L", "Turbidity_FNU","pH","Temp_C","Battery_V"))%>%
        tidyr::pivot_wider(names_from="measure", values_from="serial")
    }else{
      serial <- text[4:(skip-4)] %>% as.data.frame() %>% tidyr::separate_wider_delim(cols='.', delim=",", names_sep="") %>% as.data.frame()
      colnames(serial) <- unlist(strsplit(text[3], ","))
      serial$Model <- gsub("[0-9]P Sonde", "Battery_V", serial$Model)
      add_c <- serial[which(serial$Model == "CT"),]
      add_c$Model <- "Temp_C"
      serial <- rbind(serial, add_c) %>% dplyr::mutate(Model = .data$Model %>%
                                                         dplyr::recode_values("Turbidity" ~ "Turbidity_FNU",
                                                                              "CT" ~ "SpCond_uS_cm",
                                                                              "ODO" ~ "ODO_mg_L",
                                                                              "fDOM" ~ "fDOM_QSU",
                                                                              default = .data$Model))
      serials <- serial %>% dplyr::rename(measure = "Model", serial = " S/N") %>% select("measure", "serial") %>%
        mutate(serial = trimws(serial)) %>% tidyr::pivot_wider(names_from="measure", values_from="serial")
    }

  #some data cleaning
    #remove the not directly measured analytes (this clutters and you can calculate them after the fact)
    data <- data %>% select(any_of(c("Date","Time_HH_mm_ss","Site_Name","SpCond_uS_cm","fDOM_QSU","ODO_mg_L",
                                   "Turbidity_FNU","pH","Temp_C","Battery_V", "Depth_m")))

    #remove any duplicated header rows
    extra_header <- c(grep("^Date", as.character(data$Date)), which(as.character(data$Date) == ""))
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
    data <- data %>% dplyr::mutate(Index = 1:dplyr::n(), .before="Date") %>% dplyr::select(!any_of("Time_Fract_Sec"))

    #add site column
    data <- data %>% dplyr::mutate(Site_Name = site, .after="Time_HH_mm_ss")

  #make date time into a column set to correct tz
  data <- data %>% dplyr::mutate(DateTime = anytime::anytime(paste(.data$Date, .data$Time_HH_mm_ss),
                                                  asUTC=TRUE, tz="UTC"),
                      .after="Time_HH_mm_ss") %>%
    dplyr::mutate(Date = as.Date(anytime::anydate(data$Date, asUTC = TRUE, tz="UTC")))

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
      arrange(.data$DateTime)

 #add flags
  if(flags){
    #guess pars
    par_names <- get_parms(data)

    #add spot for flags for each parameter
    for(x in par_names){
      data <- data %>% mutate(!!paste0(x, "_flag") := NA, .after=tidyselect::all_of(x))
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
