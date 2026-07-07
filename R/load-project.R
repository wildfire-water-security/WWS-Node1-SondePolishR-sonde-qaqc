#' Load and combine sonde project components
#'
#' Uses provided filepaths and to read in, clean/format, and merge together components of a `sondeproj`.
#'
#' @param csv_path character vector with the filepaths to sonde data files,
#' can be more than one.
#' @param csv_files names of the csv files, primarily used for shiny input which
#' doesn't store filenames with paths.
#' @param prj_path character vector with the filepath to a sonde project.
#' @param ff_path character vector with the filepath to a field form file.
#' @param cc_path character vector with the filepath to a calibration check file.
#' @param tz the timezone for the sonde data
#' @param site the site name or site code.
#' @param update_pb takes a function used to update a progress bar in a shiny
#' interface.
#'
#' @returns a `sondeproj` object. For more details on structure
#' see `example_sondeproj`
#' @export
#' @md
#' @examples
#' file <- file.path(fs::path_package("extdata", package = "SondePolishR"), "example-csv-data1.csv")
#' proj <- load_project(csv_path = file, csv_files = "example_file1")
load_project <- function(csv_path=NULL, csv_files=NULL, prj_path=NULL,
                         ff_path=NULL, cc_path=NULL, tz="Etc/GMT+8",
                         site = NULL,
                         update_pb = NULL){
  #set csv merge as NULL if not loaded to prevent errors in creating obj
    csv_merge <- NULL

  #if csv projected, load files
  if(!is.null(csv_path)){
    data_merge <- list()
    for(x in csv_path){
      num <- which(x == csv_path)
      dat <- read_sonde(x, tz = tz, return="list")
      dat$data$FileName <- basename(csv_files[num]) #in shiny the default filename is nothing
      if(is.function(update_pb)){setProgress(value = num)} #update shiny progress bar
      data_merge <- c(data_merge, list(dat))
    }

    #combine things from import
    serials <- lapply(data_merge, "[[", 1) %>% bind_rows()
    csv_merge <- lapply(data_merge, "[[", 2)%>% dplyr::bind_rows() %>%
        dplyr::mutate(Index = 1:n()) %>% group_by(.data$DateTime_rd) %>%
        mutate(DupNum = row_number(), .after="Index") %>% ungroup()
  }

  #load existing project
    if(!is.null(prj_path)){
      obj <- readRDS(prj_path)
    }else{
      #create new project if one isn't loaded
      changelog <- write_log(NULL, "all", "initial load", n = nrow(csv_merge), diff_name = "raw")

      #create sonde object
      obj <- list(meta = list(site = site, tz= tz, coords = c(NA, NA)),
                  data = csv_merge,
                  flags = NULL,
                  precip = NULL,
                  fieldform = NULL,
                  calcheck = NULL,
                  diffs = list(),
                  changelog = changelog,
                  duplicates = NULL,
                  data_gaps = NULL)

      class(obj) <- "sondeproj"

      obj <- add_flags(obj, csv_merge)
    }
  #read in ff and cal file (these cover the entire period and we don't need to merge, just update)
    if(!is.null(ff_path)){
      fieldform <- read_ff(ff_path, tz)
      obj$fieldform <- fieldform
    }

    if(!is.null(cc_path)){
      calcheck <- read_cal(cc_path, tz)

      #link with serial records to know when probes were changed (only if new data is added)
      if(!is.null(csv_path)){
        switch_df <- serials %>% pivot_longer(-"Date", names_to = "Parameter", values_to = "serial") %>%
          dplyr::group_by(.data$Parameter) %>%
          dplyr::mutate(Probe_Switch = .data$serial != dplyr::lag(.data$serial, default = dplyr::first(.data$serial))) %>%
          select(-"serial")

        calcheck <- calcheck %>% left_join(switch_df, by = join_by("Date", "Parameter"))

      }

      if(!is.null(ff_path)){
        #use ff data to determine when cal data likely was
        mean_visit <- get_oow(fieldform, tz, get_interval(obj$data)) %>% rowwise() %>%
          mutate(Est_Time = mean(c(.data$start, .data$end)),
                 Date = as.Date(.data$Est_Time))

        calcheck <- calcheck %>%
          dplyr::left_join(mean_visit %>% select("Date", "Est_Time"),
                           dplyr::join_by("Date"))
      }

      obj$calcheck <- calcheck
    }

  #if project and csv loaded, merge together (everything: data, flags, diffs, replace ff and cal)
    merge_flag <- !is.null(prj_path) && !is.null(csv_path)

    if(merge_flag){
      #store previous nrow so can see how many we actually added
      prev_lines <- nrow(obj$data)

      #merge data and flags
      #we want to keep the modified data if datetimes are the same
      all_data <- obj$data %>% mutate(source = "sondeproj") %>% bind_rows(csv_merge %>% mutate(source = "csv"))

      data_merge <- all_data %>%
        dplyr::arrange(dplyr::desc(source == "sondeproj")) %>%
        dplyr::group_by(.data$Date, .data$DateTime, .data$DateTime_rd) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup() %>% dplyr::select(-"source") %>%
        dplyr::mutate(Index = 1:n())

      if(nrow(data_merge) > prev_lines){
        #document data addition
        obj <- write_log(obj, "all", "adding new data", n = (nrow(data_merge) - prev_lines), diff_name = diff_version(obj), return="sondeproj")

        #store diff
        diff <- list(get_diff(obj$data, data_merge))
        names(diff) <- diff_version(obj)
        obj$diffs <- append(obj$diffs, diff)

        #update precip if lat and long available
        if(all(!is.na(obj$meta$coords))){
          new_precip <- get_precip(data_merge, obj$meta$coords[1], obj$meta$coords[2])
          obj$precip <- new_precip
        }

      }
      obj$data <- data_merge

      #keep existing flags
      obj <- add_flags(obj, obj$data)

      #check that flags match data
      stopifnot(all(sapply(obj$flags, nrow) == nrow(obj$data)))

    }

  return(obj)
}
