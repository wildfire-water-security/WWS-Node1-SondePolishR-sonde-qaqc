## testing the logic when files are uploaded

raw_files <- list.files("../WWS-Node1-SONDE-postfire-sonde-network/data/02_raw-downloads/Fall-Creek", full.names=TRUE)[4:5]
ff <- list.files("../WWS-Node1-SONDE-postfire-sonde-network/data/01_site-visit-metadata/Fall-Creek", pattern = "Field-Form", full.names = TRUE)
cal <- list.files("../WWS-Node1-SONDE-postfire-sonde-network/data/01_site-visit-metadata/Fall-Creek", pattern = "Calibration", full.names = TRUE)
prj <- list.files("inst/extdata/", pattern="example-sonde-project", full.names=TRUE) #come back to merging with sample project

#what we'd get from the fileinput function -------
  csv_files <- data.frame(name=basename(raw_files),
                          size = 0,
                          type = NA,
                          datapath = raw_files)

  ff_file <- data.frame(name=basename(ff),
                        size = 0,
                        type = NA,
                        datapath = ff)

  cc_file <- data.frame(name=basename(cal),
                        size = 0,
                        type = NA,
                        datapath = cal)

  pj_file <- data.frame(name=basename(prj),
                        size = 0,
                        type = NA,
                        datapath = prj)
  tz <- "Etc/GMT+8"

#what we want for the server code ------
  #read in csv files, merge as needed
    if(exists("csv_files")){
      csv_merge <- lapply(csv_files$datapath, read_sonde, tz = tz, ) %>% dplyr::bind_rows()
    }

  #read in project if it exists
    if(exists("pj_file")){
      sonde_obj <- readRDS(pj_file$datapath)
    }else{
      #create new project if one isn't loaded
      changelog <- write_log(NULL, "all", "initial load", n = 0, diff_name = "raw")

      #create sonde object
      sonde_obj <- list(prj_path = save_file,
                        data = csv_merge,
                        flags = list(
                          flag_rm = empty_flags,
                          flag_chg = empty_flags,
                          flag_add = empty_flags),
                        fieldform = fieldform,
                        calcheck = calcheck,
                        diffs = list(),
                        changelog = changelog)

      class(sonde_obj) <- "sondeproj"
    }

  #flag if we need to merge things together (so we don't have to keep repeating this code)
    merge_flag <- ifelse(exists("pj_file") & exists("csv_files"), TRUE, FALSE)

  #read in ff and cal file (these cover the entire period and we don't need to merge, just update)
    fieldform <- read_ff(ff_file$datapath)
    calcheck <- read_cal(cc_file$datapath)

  #if project and csv loaded, merge together (everything: data, flags, diffs, replace ff and cal)
    if(merge_flag){
      #replace ff and cal check
      sonde_obj$fieldform <- fieldform
      sonde_obj$calcheck <- calcheck

      #document data addition (can't currently do diff because lines are different)
      sonde_obj <- write_log(sonde_obj, "all", "adding new data", n = nrow(csv_merge), diff_name = "data_upload", return="sondeproj")

      #merge data and flags
      sonde_obj$data <- sonde_obj$data %>% dplyr::bind_rows(csv_merge) %>% distinct(across(-Index)) %>%
        arrange(DateTime) %>% mutate(Index = 1:n(), .before=Date)

      #create flag tables for new data
        empty_flags <- add_flags(sonde_obj$data)

        #keep existing flags
        ext_flags <- sonde_obj$flags
        new_flags <- empty_flags %>% filter(!(DateTime %in% ext_flags$flag_rm$DateTime))

        sonde_obj$flags <- lapply(sonde_obj$flags, function(x){
          x %>% dplyr::bind_rows(new_flags) %>%
            arrange(DateTime) %>% mutate(Index = 1:n(), .before=DateTime)
        })

        #check that flags match data
        stopifnot(all(sapply(sonde_obj$flags, nrow) == nrow(sonde_obj$data)))

    }

## TODO: tests to write
  #loading just project, just csv, project and csv
  #loading when duplicated data
