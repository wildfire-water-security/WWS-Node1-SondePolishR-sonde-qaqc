## code to prepare `DATASET` dataset goes here

#pull together
  files <- list.files("data-raw/usgs-limits", pattern = "usgs-limit", full.names = TRUE)

  #use 0.999 for max of max of min if higher
    for(x in files){
      df <- read.csv(x)

      min <- df$min[df$statistic_id == 2]
      max <- ifelse(df$max[df$statistic_id == 2] > df$q999[df$statistic_id ==1], df$max[df$statistic_id == 2],df$q999[df$statistic_id ==1])
      par <- stringr::str_split_i(basename(x), "-",3)
      eco <- gsub("-", " ", gsub(".csv$", "", gsub("usgs-limit-[0-9]{5}-", "", basename(x))))

      #replace missing values with NA
      min <- ifelse(length(min) ==0 , NA, min)
      max <- ifelse(length(max) ==0 , NA, max)

      lim <- data.frame(ecoregion = eco, parameter = par, max = max, min=min)

      if(x == files[1]){
        limits <- lim
      }else{limits <- rbind(limits, lim)}
    }

  #input into physical limits
  codes <- read.csv("data-raw/usgs-sonde-codes.csv")
  codes$usgs_code <- stringr::str_pad(codes$usgs_code, 5, side="left", pad="0") #ensure codes are formatted correctly

  #merge together
  phys_limits <- merge(limits, codes, by.x="parameter", by.y="usgs_code")

  usethis::use_data(phys_limits, overwrite = TRUE)


#save copy of sonde data for use in examples without needing to load
  file <- file.path(fs::path_package("extdata", package = "SondePolishR"), "sonde-example.csv")
  raw_sonde <- read_sonde(file, flags = FALSE)


  use_data(raw_sonde, overwrite = TRUE)

# create example versioning
  clear_log()
  clear_data()

  data <- raw_sonde
  write_data(data, "raw")

  #a change
  row_change <- 1:4
  data$Cond_uS_cm[row_change] <- NA

  #log change and save value
  version <- digest::digest(data)
  change <- write_log("Cond_uS_cm", "test step", length(row_change), version, user="smith")
  write_data(data, version)

  #make another change
  row_change <- 500:600
  data$Cond_uS_cm[row_change] <- NA

  version <- digest::digest(data)
  change <- write_log("Cond_uS_cm", "test step", length(row_change), version, user="smith")
  write_data(data, version)

  #get vals
  log <- get_log()
  data_ver <- get_data()

  #save
  save_project(data_ver, log, "inst/extdata/example-sonde-project.RDS")

  #save as data objects
  example_log <- log
  example_data_ver <- data_ver
  example_project <- append(list(log), data_ver)
  names(example_project)[1] <- "log"


  use_data(example_log, overwrite = TRUE)
  use_data(example_data_ver, overwrite = TRUE)
  use_data(example_project, overwrite= TRUE)
