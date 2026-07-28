## DO sensors

## level loggers

library(fs)
library(XML)
library(ecoflux)

path <- file.path(path_home(), "Documents/Projects/WWS-Node1-SONDE-postfire-sonde-network/data/02_raw-downloads/Bacon-Creek/barologger-Bacon-Creek")

baro_files <- list.files(path, pattern="xle", full.names = TRUE)


file <- xmlParse(baro_files,  encoding = "ISO-8859-1")

readLines(baro_files, encoding = "UTF-8")

test <- read_xle(baro_files)
