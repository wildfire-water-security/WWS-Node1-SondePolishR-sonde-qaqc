#get path to all raw data
path <- file.path(fs::path_home(), "Documents/Projects/WWS-Node1-SONDE-postfire-sonde-network/data/02_raw-downloads")
files <- list.files(path, recursive = TRUE, pattern = "[0-9]{8}")

#random testing to make sure file looks like what it should when checked manually
x <- sample(files, 1)
file <- file.path(path, x)
df <- read_sonde(file.path(path, x), flags = FALSE)
cat(paste("start date:", min(df$DateTime), "\n"),
    paste("nobs:", nrow(df), "\n"))

shell.exec(file)
