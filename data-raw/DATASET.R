# Includes sourcing of the raw-data and included data in the package so I know where it came from

#create an example project with example objects ------
  ## testing the logic when files are uploaded
    raw_files <- list.files("../WWS-Node1-SONDE-postfire-sonde-network/data/02_raw-downloads/Fall-Creek", full.names=TRUE)[3:4]
    ff <- list.files("../WWS-Node1-SONDE-postfire-sonde-network/data/01_site-visit-metadata/Fall-Creek", pattern = "Field-Form", full.names = TRUE)
    cal <- list.files("../WWS-Node1-SONDE-postfire-sonde-network/data/01_site-visit-metadata/Fall-Creek", pattern = "Calibration", full.names = TRUE)

    path <- list(type="package", path="extdata/example-sonde-project.RDS")

  #what we'd get from the fileinput function
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
    tz <- "Etc/GMT+8"

  #what we want for the server code
    #read in csv files, merge as needed
      data_merge <- lapply(csv_files$datapath, read_sonde, tz = tz, ) %>% dplyr::bind_rows()

  #if project and csv loaded, merge together (everything: data, flags, diffs, replace ff and cal)
      data <- data_merge

  #read in ff and cal file (these cover the entire period and we don't need to merge, just update)
    fieldform <- read_ff(ff_file$datapath)
    calcheck <- read_cal(cc_file$datapath)

    #clip data down for a smaller example and clear some actual data
      fieldform <- fieldform %>% filter(Date >= min(data_merge$Date) & Date <= max(data_merge$Date)) %>%
        mutate(Crew = "JS", Weather = "Cloudy with a chance of meatballs",
               Notes = c("Replacing previously removed sonde.", NA, NA),
               Start_Sonde_Serial = "23K139551",
               End_Sonde_Serial = c(NA, NA, "23K597634"))

      calcheck <- calcheck %>% filter(Date > min(data_merge$Date) & Date <= max(data_merge$Date)) %>% mutate(Notes=NA)

  #create flag tables
    empty_flags <- add_flags(data)

  #create log if not read in from existing project
    changelog <- write_log(NULL, "all", "initial load", n = 0, diff_name = "raw")
    changelog$user <- "smith"

   #create sonde object
    sonde_obj <- list(prj_path = path,
                      data = data,
                      flags = list(
                        flag_rm = empty_flags,
                        flag_chg = empty_flags,
                        flag_add = empty_flags),
                      fieldform = fieldform,
                      calcheck = calcheck,
                      diffs = list(),
                      changelog = changelog)

    class(sonde_obj) <- "sondeproj"

  #add some changes
    #make some changes and save
    data2 <- sonde_obj$data
    data2$fDOM_QSU[1:4] <- NA
    dd1 <- list(commit_diff(sonde_obj$data, data2)) #commit difference
    names(dd1) <- "dd1"
    sonde_obj$flags$flag_rm$fDOM_QSU_flag[1:4] <- "RM01" #add flag
    sonde_obj <- write_log(sonde_obj, "fDOM_QSU", "removing first four points", n = 4, diff_name = "dd1", return = "sondeproj") #write log
    sonde_obj$diffs <- append(sonde_obj$diffs, dd1)
    sonde_obj$data <- data2

    #make more changes
    data2 <- sonde_obj$data
    data2$ODO_mg_L[5:7] <- data2$ODO_mg_L[5:7] * 0.8
    dd2 <- list(commit_diff(sonde_obj$data, data2)) #commit difference
    names(dd2) <- "dd2"
    sonde_obj$flags$flag_chg$ODO_mg_L[5:7] <- "AD01" #add flag
    sonde_obj <- write_log(sonde_obj, "ODO_mg_L", "applying shift correction", n = 3, diff_name = "dd2", return = "sondeproj") #write log
    sonde_obj$diffs <- append(sonde_obj$diffs, dd2)
    sonde_obj$data <- data2

  #remove my username from log
    sonde_obj$changelog$user <- "smith"

  #save as an example
    saveRDS(sonde_obj, "inst/extdata/example-sonde-project.RDS")

#write objects for example data
  example_sondeproj <- sonde_obj
  use_data(example_sondeproj, overwrite = TRUE)

  example_data <- data_merge
  use_data(example_data, overwrite = TRUE)

  example_fieldform <- fieldform
  use_data(example_fieldform, overwrite= TRUE)

  example_calcheck <- calcheck
  use_data(example_calcheck, overwrite= TRUE)
