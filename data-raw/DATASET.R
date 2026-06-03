# Includes sourcing of the raw-data and included data in the package so I know where it came from

#create an example project with example objects ------
  ## testing the logic when files are uploaded
    raw_files <- list.files("../WWS-Node1-SONDE-postfire-sonde-network/data/02_raw-downloads/Fall-Creek", full.names=TRUE)[3:5]
    ff <- list.files("../WWS-Node1-SONDE-postfire-sonde-network/data/01_site-visit-metadata/Fall-Creek", pattern = "Field-Form", full.names = TRUE)
    cal <- list.files("../WWS-Node1-SONDE-postfire-sonde-network/data/01_site-visit-metadata/Fall-Creek", pattern = "Calibration", full.names = TRUE)


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
      data_merge <- lapply(csv_files$datapath, read_sonde, tz = tz, return="list")
      serials <- lapply(data_merge, "[[", 1) %>% bind_rows()
      data_merge <- lapply(data_merge, "[[", 2)%>% dplyr::bind_rows() %>%
        dplyr::mutate(Index = 1:n())

  #if project and csv loaded, merge together (everything: data, flags, diffs, replace ff and cal)
  #read in ff and cal file (these cover the entire period and we don't need to merge, just update)
    fieldform <- read_ff(ff_file$datapath)
    calcheck <- read_cal(cc_file$datapath)

    #add serial switches to calcheck
    #link with serial records to know when probes were changed
    switch_df <- serials %>% tidyr::pivot_longer(-Date, names_to = "Parameter", values_to = "serial") %>%
      dplyr::group_by(Parameter) %>%
      dplyr::mutate(Probe_Switch = serial != dplyr::lag(serial, default = first(serial))) %>%
      select(-serial)

    calcheck <- calcheck %>% left_join(switch_df, by = join_by(Date, Parameter))


    #clip data down for a smaller example and clear some actual data
      fieldform <- fieldform %>% filter(Date >= min(data_merge$Date) & Date <= max(data_merge$Date)) %>%
        mutate(Crew = "JS", Weather = "Cloudy with a chance of meatballs",
               Notes = c("Replacing previously removed sonde.", NA, NA, NA),
               Start_Sonde_Serial = "23K139551",
               End_Sonde_Serial = c(NA, NA, "23K597634", "23K597634"))

      calcheck <- calcheck %>% filter(Date > min(data_merge$Date) & Date <= max(data_merge$Date)) %>% mutate(Notes=NA)

      data_merge$FileName[data_merge$FileName == "20240820_FAL.csv"] <- "example-csv-data1.csv"
      data_merge$FileName[data_merge$FileName == "20241023_FAL.csv"] <- "example-csv-data2.csv"
      data_merge$FileName[data_merge$FileName == "20241229_FAL.csv"] <- "example-csv-data3.csv"

      #use ff data to determine when cal data likely was
      mean_visit <- get_oow(fieldform) %>% rowwise() %>%
        mutate(Est_Time = mean(c(.data$start, .data$end)),
               Date = as.Date(.data$Est_Time))

      calcheck <- calcheck %>%
        dplyr::left_join(mean_visit %>% select("Date", "Est_Time"),
                         dplyr::join_by("Date"))

  #create flag tables
    empty_flags <- add_flags(data_merge)

  #create log if not read in from existing project
    changelog <- write_log(NULL, "all", "initial load", n = nrow(data_merge), diff_name = "raw")
    changelog$user <- "smith"

   #create sonde object
    sonde_obj <- list(data = data_merge,
                      flags = list(
                        flag_rm = empty_flags,
                        flag_chg = empty_flags,
                        flag_add = empty_flags),
                      fieldform = fieldform,
                      calcheck = calcheck,
                      diffs = list(),
                      changelog = changelog,
                      duplicates = NULL,
                      data_gaps = NULL)

    class(sonde_obj) <- "sondeproj"

  #add some changes
    #make some changes and save
    data2 <- sonde_obj$data
    data2$fDOM_QSU[1:4] <- NA
    dd1 <- list(get_diff(sonde_obj$data, data2)) #commit difference
    names(dd1) <- "dd1"
    sonde_obj$flags$flag_rm$fDOM_QSU[1:4] <- "RM01" #add flag
    sonde_obj <- write_log(sonde_obj, "fDOM_QSU", "removing first four points", n = 4, diff_name = "dd1", return = "sondeproj") #write log
    sonde_obj$diffs <- append(sonde_obj$diffs, dd1)
    sonde_obj$data <- data2

    #make more changes
    data2 <- sonde_obj$data
    data2$ODO_mg_L[5:7] <- data2$ODO_mg_L[5:7] * 0.8
    dd2 <- list(get_diff(sonde_obj$data, data2)) #commit difference
    names(dd2) <- "dd2"
    sonde_obj$flags$flag_chg$ODO_mg_L[5:7] <- "AD01" #add flag
    sonde_obj <- write_log(sonde_obj, "ODO_mg_L", "applying shift correction", n = 3, diff_name = "dd2", return = "sondeproj") #write log
    sonde_obj$diffs <- append(sonde_obj$diffs, dd2)
    sonde_obj$data <- data2

    #make more changes
    data2 <- sonde_obj$data
    data2$Temp_C[52:90] <- NA
    dd3 <- list(get_diff(sonde_obj$data, data2)) #commit difference
    names(dd3) <- "dd3"
    sonde_obj$flags$flag_rm$Temp_C[52:90] <- "RM02" #add flag
    sonde_obj <- write_log(sonde_obj, "Temp_C", "removing a bunch of points", n = 39, diff_name = "dd3", return = "sondeproj") #write log
    sonde_obj$diffs <- append(sonde_obj$diffs, dd3)
    sonde_obj$data <- data2

    #make more changes
    data2 <- sonde_obj$data
    data2$Temp_C[52:90] <- mean(c(data2$Temp_C[51],data2$Temp_C[91]))
    dd4 <- list(get_diff(sonde_obj$data, data2)) #commit difference
    names(dd4) <- "dd4"
    sonde_obj$flags$flag_add$Temp_C[52:60] <- "AD02" #add flag
    sonde_obj <- write_log(sonde_obj, "Temp_C", "linear interpolation", n = 39, diff_name = "dd4", return = "sondeproj") #write log
    sonde_obj$diffs <- append(sonde_obj$diffs, dd4)
    sonde_obj$data <- data2


  #remove my username from log
    sonde_obj$changelog$user <- "smith"

  #save as an example
    saveRDS(sonde_obj, "inst/extdata/example-sonde-project.RDS")

    #also copy over the example csv's for testing
    file.copy(raw_files, c("inst/extdata/example-csv-data1.csv",
                           "inst/extdata/example-csv-data2.csv","inst/extdata/example-csv-data3.csv"), overwrite = TRUE)

    #and ff and cal file
    write.csv(fieldform, "inst/extdata/example-fieldform.csv", row.names = FALSE)
    write.csv(calcheck, "inst/extdata/example-calcheck.csv", row.names = FALSE)

#write objects for example data
  example_sondeproj <- sonde_obj
  use_data(example_sondeproj, overwrite = TRUE)

  example_data <- data_merge
  use_data(example_data, overwrite = TRUE)

  example_fieldform <- fieldform
  use_data(example_fieldform, overwrite= TRUE)

  example_calcheck <- calcheck
  use_data(example_calcheck, overwrite= TRUE)

#move files from ext data to testdata for testing too
  testfiles <- list.files("inst/extdata")
  file.copy(file.path("inst/extdata", testfiles), file.path("tests/testthat/testdata", testfiles), overwrite = TRUE)

#make a "messy" project with dups and gaps for testing (may move to main example??)
 #two types of duplicates
  data_messy <- rbind(data_merge, data_merge[1:14,]) #single file dup
  data_messy <- rbind(data_messy, data_merge[251:264,] %>% mutate(FileName = "dupfile2.csv"))

 #add a gap (missing observations)
  data_messy <- data_messy[-(500:580),]

  data_messy <- data_messy %>% dplyr::mutate(Index = 1:n()) #redo index

  empty_flags <- add_flags(data_messy)

  #create log if not read in from existing project
  changelog <- write_log(NULL, "all", "initial load", n = nrow(data_messy), diff_name = "raw")
  changelog$user <- "smith"

  #create sonde object
  sonde_obj_messy <- list(data = data_messy,
                    flags = list(
                      flag_rm = empty_flags,
                      flag_chg = empty_flags,
                      flag_add = empty_flags),
                    fieldform = fieldform,
                    calcheck = calcheck,
                    diffs = list(),
                    changelog = changelog,
                    duplicates = NULL,
                    data_gaps = NULL)

  class(sonde_obj_messy) <- "sondeproj"
  #remove my username from log
  sonde_obj_messy$changelog$user <- "smith"

  saveRDS(sonde_obj_messy, "tests/testthat/testdata/example-sondeproj-messy.RDS")
