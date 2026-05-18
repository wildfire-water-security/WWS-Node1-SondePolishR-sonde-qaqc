## testing out other methods of saving differences so maybe we can just store the changes

library(daff)

prj <- readRDS("inst/extdata/example-sonde-project.RDS")

data1 <- as.data.frame(prj$`01a99a344d087970ebac7ea06eeb2a5c`)
data2 <- as.data.frame(prj$`3d5abbbac2555f6258e2808b8c421520`)

dd <- diff_data(data1, data2, ids="Index")

data2_check <- patch_data(data1, dd)

library(daff)
x <- iris
x[1,1] <- 10
diff_data(x, iris)

dd <- diff_data(x, iris)
#write_diff(dd, "diff.csv")
summary(dd)


#testing removing data
olddata <- raw_sonde
newdata <- data1

commit_diff(data1, data2)

newdata$fDOM_QSU[1:4] <- NA

dd <- commit_diff(data1, data2)

check <- apply_diff(data1, dd)

#testing storing flags
flags1 <- raw_sonde %>% mutate(SpCond_uS_cm = NA,
                               fDOM_QSU = NA,
                               ODO_mg_L = NA,
                               Turbidity_FNU =NA,
                               pH = NA,
                               Temp_C = NA)

flags2 <- flags1
flags2$fDOM_QSU[1:4] <- "LIM01"

dd <- commit_diff(flags1, flags2)

check <- apply_diff(flags1, dd)

#

#create wrapper for diff_data and patch data to remove warnings and losing date/time
commit_diff <- function(olddata, newdata){
  #check that dates/datetime are exactly the same because we can't check these
    if(any(olddata$Date != newdata$Date)){
      stop("Dates are different between the two datasets, can't determine differences.")
    }

    if(any(olddata$DateTime != newdata$DateTime)){
      stop("Datetimes are different between the two datasets, can't determine differences.")
    }

  #rm date and datetime so we don't get warning
  dates <- olddata %>% select(Date, DateTime)
  olddata <- olddata %>% select(-c(Date, DateTime))
  newdata <- newdata %>% select(-c(Date, DateTime))

  #get diff
  dd <- diff_data(olddata, newdata)

  return(dd)
}

#wrapper for patch data to apply changes without losing date/time
apply_diff <- function(olddata, diff){
  #pull out dates
  dates <- olddata %>% select(Date, DateTime)
  olddata <- olddata %>% select(-c(Date, DateTime))

  #apply patch
  suppressWarnings(newdata <- patch_data(olddata, diff))

  #put back in datetimes
  newdata <- newdata %>% mutate(Date = dates$Date, .before=Time_HH_mm_ss) %>%
                         mutate(DateTime = dates$DateTime, .after=Time_HH_mm_ss)

  return(newdata)
}


#testing workflow
  library(pbapply)
  files <- list.files("../WWS-Node1-SONDE-postfire-sonde-network/data/02_raw-downloads/Bacon-Creek/",
                      full.names=TRUE, recursive = TRUE,pattern="[0-9]{8}_[A-Z]{2,}.csv$")

  #load add data minus one file
  data1 <- pblapply(files[-length(files)], function(f) {
    tryCatch({
      dat <- read_sonde(f, flags = FALSE)
      dat$site_code <- gsub("[0-9]{8}_|\\.csv$", "", basename(f))
      dat$file <- basename(f)
      dat
    }, error = function(e) {
      message("FAILED: ", f)
      message("  ", e$message)
      NULL
    }, warning = function(w){
      message("FAILED: ", f)
      message("  ", w$message)
      NULL
    })
  }) %>% bind_rows()

  #make some changes and save
  data2 <- data1
  data2$fDOM_QSU[1:4] <- NA
  dd1 <- commit_diff(data1, data2)

  #make more changes
  data3 <- data2
  data3$ODO_mg_L[5:7] <- data3$ODO_mg_L[5:7] * 0.8
  dd2 <- commit_diff(data2, data3)

  #more changes
  data4 <- data3
  data4$Temp_C[1:100] <- NA
  dd3 <- commit_diff(data3, data4)

  #get to whatever level we want from raw
  diffs <- list(dd1,dd2,dd3)

  data <- data1
  for(x in diffs[1:2]){
    data <- apply_diff(data, x)
  }

# now add the new file, how will that work?
  data1 <- pblapply(files, function(f) {
    tryCatch({
      dat <- read_sonde(f, flags = FALSE)
      dat$site_code <- gsub("[0-9]{8}_|\\.csv$", "", basename(f))
      dat$file <- basename(f)
      dat
    }, error = function(e) {
      message("FAILED: ", f)
      message("  ", e$message)
      NULL
    }, warning = function(w){
      message("FAILED: ", f)
      message("  ", w$message)
      NULL
    })
  }) %>% bind_rows() %>% mutate(Index = 1:n())

  data <- data1
  for(x in diffs){
    data <- apply_diff(data, x)
  }
