# Includes sourcing of the raw-data and included data in the package so I know where it came from

#create an example project with example objects ------
  ## testing the logic when files are uploaded
    raw_files <- list.files("../WWS-Node1-SONDE-postfire-sonde-network/data/02_raw-downloads/Fall-Creek", full.names=TRUE)[3:5]
    ff <- list.files("../WWS-Node1-SONDE-postfire-sonde-network/data/01_site-visit-metadata/Fall-Creek", pattern = "Field-Form", full.names = TRUE)
    cal <- list.files("../WWS-Node1-SONDE-postfire-sonde-network/data/01_site-visit-metadata/Fall-Creek", pattern = "Calibration", full.names = TRUE)

    proj <- load_project(csv_path = raw_files, csv_files = paste0("example-csv-data", 1:3, ".csv"),
                       ff_path = ff, cc_path = cal, tz = "Etc/GMT+8", site="FAL")

    #add precip
    proj$precip <- get_precip(proj$data, 43.96, -122.63)
    proj$meta$coords <- c(43.96,-122.63)

    #clip data down for a smaller example and clear some actual data
     proj$fieldform <- proj$fieldform %>% filter(Date >= min(proj$data$Date) & Date <= max(proj$data$Date)) %>%
        mutate(Crew = "JS", Weather = "Cloudy with a chance of meatballs",
               Notes = c("Replacing previously removed sonde.", NA, NA, NA),
               Start_Sonde_Serial = "23K139551",
               End_Sonde_Serial = c(NA, NA, "23K597634", "23K597634"))


     proj$calcheck <- proj$calcheck %>% filter(Date > min(proj$data$Date) & Date <= max(proj$data$Date)) %>% mutate(Notes=NA)


  #create log if not read in from existing project
    proj$changelog$user <- "smith"

  data <- proj$data #save unedited for later

  #add some changes
    #make some changes and save
    data2 <- proj$data
    data2$fDOM_QSU[1:4] <- NA
    dd1 <- list(get_diff(proj$data, data2, id=c("DateTime_rd", "DupNum"))) #commit difference
    names(dd1) <- "dd1"
    proj$flags$flag_rm$fDOM_QSU[1:4] <- "RM01" #add flag
    proj <- write_log(proj, "fDOM_QSU", "removing first four points", n = 4, diff_name = "dd1", return = "sondeproj") #write log
    proj$diffs <- append(proj$diffs, dd1)
    proj$data <- data2

    #make more changes
    data2 <- proj$data
    data2$ODO_mg_L[5:7] <- data2$ODO_mg_L[5:7] * 0.8
    dd2 <- list(get_diff(proj$data, data2,id=c("DateTime_rd", "DupNum"))) #commit difference
    names(dd2) <- "dd2"
    proj$flags$flag_chg$ODO_mg_L[5:7] <- "CH01" #add flag
    proj <- write_log(proj, "ODO_mg_L", "applying shift correction", n = 3, diff_name = "dd2", return = "sondeproj") #write log
    proj$diffs <- append(proj$diffs, dd2)
    proj$data <- data2

    #make more changes
    data2 <- proj$data
    data2$Temp_C[52:90] <- NA
    dd3 <- list(get_diff(proj$data, data2, id=c("DateTime_rd", "DupNum"))) #commit difference
    names(dd3) <- "dd3"
    proj$flags$flag_rm$Temp_C[52:90] <- "RM02" #add flag
    proj <- write_log(proj, "Temp_C", "removing a bunch of points", n = 39, diff_name = "dd3", return = "sondeproj") #write log
    proj$diffs <- append(proj$diffs, dd3)
    proj$data <- data2

    #make more changes
    data2 <- proj$data
    data2$Temp_C[52:90] <- mean(c(data2$Temp_C[51],data2$Temp_C[91]))
    dd4 <- list(get_diff(proj$data, data2, id=c("DateTime_rd", "DupNum"))) #commit difference
    names(dd4) <- "dd4"
    proj$flags$flag_add$Temp_C[52:60] <- "AD02" #add flag
    proj <- write_log(proj, "Temp_C", "linear interpolation", n = 39, diff_name = "dd4", return = "sondeproj") #write log
    proj$diffs <- append(proj$diffs, dd4)
    proj$data <- data2


  #remove my username from log
    proj$changelog$user <- "smith"

  #save as an example
    saveRDS(proj, "inst/extdata/example-sonde-project.RDS")

    #also copy over the example csv's for testing
    file.copy(raw_files, c("inst/extdata/example-csv-data1.csv",
                           "inst/extdata/example-csv-data2.csv","inst/extdata/example-csv-data3.csv"), overwrite = TRUE)

    #and ff and cal file
    write.csv(proj$fieldform, "inst/extdata/example-fieldform.csv", row.names = FALSE)
    write.csv(proj$calcheck, "inst/extdata/example-calcheck.csv", row.names = FALSE)
    write.csv(proj$precip, "inst/extdata/example-precip.csv", row.names = FALSE)

#write objects for example data
  example_sondeproj <- proj
  use_data(example_sondeproj, overwrite = TRUE)

  example_data <- data
  use_data(example_data, overwrite = TRUE)

  example_fieldform <- proj$fieldform
  use_data(example_fieldform, overwrite= TRUE)

  example_calcheck <- proj$calcheck
  use_data(example_calcheck, overwrite= TRUE)

  example_precip <- proj$precip
  use_data(example_precip, overwrite= TRUE)

#move files from ext data to testdata for testing too
  testfiles <- list.files("inst/extdata")
  file.copy(file.path("inst/extdata", testfiles), file.path("tests/testthat/testdata", testfiles), overwrite = TRUE)

#make a "messy" project with dups and gaps for testing (may move to main example??)
 #two types of duplicates
  data_messy <- rbind(data, data[1:14,]) #single file dup
  data_messy <- rbind(data_messy, data[251:264,] %>% mutate(FileName = "dupfile2.csv"))
  data_messy[data_messy$FileName == "dupfile2.csv", 10:15] <- data_messy[data_messy$FileName == "dupfile2.csv", 10:15] * 1.1


 #add a gap (missing observations)
  data_messy <- data_messy[-(500:580),]

  data_messy <- data_messy %>% dplyr::mutate(Index = 1:n()) %>% group_by(.data$DateTime_rd) %>%
    mutate(DupNum = row_number(), .after="Index") %>% ungroup() #redo index and dupnum

  #create log if not read in from existing project
  changelog <- write_log(NULL, "all", "initial load", n = nrow(data_messy), diff_name = "raw")
  changelog$user <- "smith"

  #create sonde object
  proj_messy <- list(meta = proj$meta,
                    data = data_messy,
                    flags = NULL,
                    precip = proj$precip,
                    fieldform = proj$fieldform,
                    calcheck = proj$calcheck,
                    diffs = list(),
                    changelog = changelog,
                    duplicates = NULL,
                    data_gaps = NULL)

  class(proj_messy) <- "sondeproj"

  proj_messy <- add_flags(proj_messy, data_messy)

  #remove my username from log
  proj_messy$changelog$user <- "smith"

  saveRDS(proj_messy, "tests/testthat/testdata/example-sondeproj-messy.RDS")
  saveRDS(proj_messy, "inst/extdata/example-sondeproj-messy.RDS")
