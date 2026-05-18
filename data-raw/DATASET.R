# Includes sourcing of the raw-data and included data in the package so I know where it came from

#copy FAL data and clip so it's smaller --------
  data <- readRDS("../WWS-Node1-SONDE-postfire-sonde-network/data/03_merged-data/raw-sonde-data.rds") %>%
    filter(site_code == "FAL") %>% filter(Date_MM_DD_YYYY >= "2024-06-01" &  Date_MM_DD_YYYY <= "2024-09-01")


#save copy of sonde data for use in examples without needing to load ------
  clear_log()
  clear_data()

  file <- file.path(fs::path_package("extdata", package = "SondePolishR"), "sonde-example.csv")
  raw_sonde <- read_sonde(file, flags = FALSE)


  use_data(raw_sonde, overwrite = TRUE)

# create example versioning -------
  path <- list(type="package", path="extdata/example-sonde-project.RDS")

  data <- raw_sonde
  write_data(data, "raw")

  #a change
  data <- flag_data(data, par = "Cond_uS_cm", index = 1:4, flag_name = "test step", note = "we want to remove these points", prj_path = path, makeNA = TRUE)

  #make another change
  data$Cond_uS_cm[500:600] <- data$Cond_uS_cm[500:600] + 7.5
  data <- flag_data(data, par = "Cond_uS_cm", index = 500:600, flag_name = "test step2", note = "these points were modified", prj_path = path)

  #get vals
  log <- get_log()
  data_ver <- get_data()

  #save
  log$user <- "smith"

  #save as data objects
  example_log <- log
  example_data_ver <- data_ver

  #rewrite username
  example_project <- readRDS(resolve_path(path))
  example_project$change_log$user <- "smith"
  saveRDS(example_project, resolve_path(path))
  saveRDS(example_project, file.path("tests/testthat/testdata/example-sonde-project.RDS"))

  use_data(example_log, overwrite = TRUE)
  use_data(example_data_ver, overwrite = TRUE)
  use_data(example_project, overwrite= TRUE)
