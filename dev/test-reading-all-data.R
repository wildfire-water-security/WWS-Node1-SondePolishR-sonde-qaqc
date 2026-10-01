## code to test the reading in csv script across all our data files to ensure it doesn't randomly fail
library(pbapply)
  files <- list.files("../WWS-Node1-SONDE-postfire-sonde-network/data/02_raw-downloads", recursive = TRUE,
                      pattern=paste0("[0-9]{8}_[A-Z]{3,}.csv"), full.names = TRUE)

    read_nicely <- function(x){
      file <- basename(x)
      print(file)
      tryCatch({
        dat <- read_sonde(x)
      },
      error = function(e){
        message("An error occurred: ", conditionMessage(e))
        return(file)
      },
      warning = function(w){
        return(file)
      },
      finally= {
        return(NULL)
      })
    }

    #will try to read all files and return any files that have issues
    prob_files <- sapply(files, read_nicely)



file <- grep("20241022_DEP.csv", files, value=TRUE)

test <- read_sonde(file)
