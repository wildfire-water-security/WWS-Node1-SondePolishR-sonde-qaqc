## code to prepare `DATASET` dataset goes here

#save EPA ecoregions for use -----
  tmpfile <- tempfile(fileext=".zip")
  download.file("https://dmap-prod-oms-edc.s3.us-east-1.amazonaws.com/ORD/Ecoregions/us/us_eco_l3_state_boundaries.zip", mode="wb",
                destfile = tmpfile)

  #load file
  unzip(tempfile, exdir = tempdir())
  ecoregions <- sf::read_sf(file.path(tempdir(), "us_eco_l3_state_boundaries.shp"))

  ecoregions$NA_L2NAME <- gsub(" (?)", "",ecoregions$NA_L2NAME, fixed=TRUE)

  #merge into each ecoregion
  ecoregions <-  ecoregions %>% dplyr::group_by(across(-c(geometry, STATE_NAME, EPA_REGION, L3_KEY, L2_KEY, L1_KEY))) %>%
    dplyr::summarise(do_union=TRUE)

  #simplify geometry so it's not so big
  ecoregions <- sf::st_simplify(ecoregions, preserveTopology = FALSE, dTolerance = 500)

  usethis::use_data(ecoregions, overwrite = TRUE, compress="xz")


#check parameter codes (don't need to rerun) -----
  eco <- get_ecoregion(site)
  eco <- sf::st_transform(eco, crs="EPSG:4326") #transform to corret crs

  bbox <- terra::ext(eco)
  bbox <- c(bbox[1], bbox[3], bbox[2], bbox[4])

  #identify stations in ecoregion
  stats <- dataRetrieval::read_waterdata_monitoring_location(
    agency_code = "USGS",
    site_type = "Stream",
    bbox = bbox,
    skipGeometry = TRUE,
    properties = c("monitoring_location_id",
                   "site_type", "state_name"))


  #get stations with data
  #split to not exceed limits on API
  chunk_size <- 300
  group_factor <- ceiling(seq_len(nrow(stats)) / chunk_size)
  stats_split <- split(stats, group_factor)

  params <- vector()
  for(x in 1:length(stats_split)){
    stats_useful <- dataRetrieval::read_waterdata_ts_meta(monitoring_location_id = stats_split[[x]]$monitoring_location_id,
                                                          statistic_id = c("00001", "00002"),
                                                          properties = c("monitoring_location_id",
                                                                         "parameter_code",
                                                                         "begin",
                                                                         "end",
                                                                         "time_series_id",
                                                                         "statistic_id"),
                                                          skipGeometry = TRUE)
    params <- c(params, unique(stats_useful$parameter_code))

  }

  params <- unique(params)

#get data for stations in ecoregion with data -------
  ecos <- c("Cascades", "Klamath Mountains","North Cascades", "Blue Mountains", "Columbia Plateau")
  parms <- c("00010","00095", "00300", "00301", "00400", "00480", "32295","32322","63680","72147","99409")

for(x in ecos){
  cat("working on ecoregion ", x , "\n")
  for(y in parms){
  cat("working on parameter ", y , "\n")
  data <- get_eco_limits(x,y)

  if(!any(is.na(data))){
    write.csv(data,
              file.path("data-raw/usgs-limits", paste0("usgs-limit-", y,
                                            "-", gsub(" ", "-", x), ".csv")),
              quote=FALSE, row.names=FALSE)
  }}}

#pull together
  files <- list.files("data-raw/usgs-limits", pattern = "usgs-limit", full.names = TRUE)

  #use 0.999 for max of max of min if higher
    for(x in files){
      data <- read.csv(x)

      min <- data$min[data$statistic_id == 2]
      max <- ifelse(data$max[data$statistic_id == 2] > data$q999[data$statistic_id ==1], data$max[data$statistic_id == 2],data$q999[data$statistic_id ==1])
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
  clear_log()
  clear_data()

  file <- file.path(fs::path_package("extdata", package = "SondePolishR"), "sonde-example.csv")
  raw_sonde <- read_sonde(file, flags = FALSE)


  use_data(raw_sonde, overwrite = TRUE)

# create example versioning
  path <- list(type="project", path="inst/extdata/example-sonde-project.RDS")

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
