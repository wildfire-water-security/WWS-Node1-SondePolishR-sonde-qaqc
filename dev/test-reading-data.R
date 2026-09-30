## code to test the reading in csv script across all our data files to ensure it doesn't randomly fail

  files <- list.files(file.path("../WWS-Node1-SONDE-postfire-sonde-network/data/02_raw-downloads/"),
                      pattern=paste0("[0-9]{8}_", site, ".csv"), recursive = TRUE, full.names = TRUE)

    #read files and get the days with data
    # dat <- lapply(files, read_sonde) %>% bind_rows() %>%
    #   dplyr::select(Date) %>% distinct() %>% mutate(site = site)
    dat <- lapply(files, function(x){print(basename(x))
      read_sonde(x)})

    #will try to read all files and print file names so you know where it failed
