#get path to all raw data
path <- file.path(fs::path_home(), "Documents/Projects/WWS-Node1-SONDE-postfire-sonde-network/data/02_raw-downloads")
files <- list.files(path, recursive = TRUE, pattern = "[0-9]{8}")

x <- files[3]
file <- file.path(path, x)
df <- read_csv_robust(file.path(path, x))

problem <- c(3,6)
for(x in 1:length(files)){
  print(x)
  if(!(x %in% problem)){
    df <- read_csv_robust(file.path(path, files[x]))
  }
}


file <- file.path(fs::path_package("extdata", package = "SondePolishR"), "sonde-example.csv")
test <- utils::read.csv(file, fileEncoding = encoding, skip=skip, header = TRUE)
test <- readLines(file,encoding = encoding)
