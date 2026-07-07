##figuring out how the flagging should work
  #flags stored in matching structured df

#workflow would be altering the df and then hitting a button so this is what would happen when button is hit
y_var <- "fDOM_QSU"
ymin <- 2 #user selected lower limit
ymax <- 50 #user selected upper limit
flag <- "RM02"
note <- paste0("Data removed based on absolute limits of ", ymin, " and ", ymax)

proj <- example_sondeproj #the project that's getting passed around the app
data1 <- proj$data #the data before the most recent alteration
data2 <- data1 #the data after we make a change

#make a change #if NA don't need to re-remove
  setna <- data2[[y_var]] < ymin | data2[[y_var]] > ymax
  setna[is.na(setna)] <- FALSE
  data2[[y_var]][setna] <- NA

#note differences
  dif <- list(get_diff(data1, data2))
  names(dif) <- diff_version(proj)

#add flags
  proj$flags$flag_rm[[y_var]][setna] <- flag

#create log entry
  proj <- write_log(proj, y_var, "absolute limits", n=sum(setna, na.rm=TRUE), note = note, diff_name = names(dif), return = "sondeproj")

#add in new df and diff
  proj$data <- data2
  proj$diffs <- c(proj$diffs, list(dif))

#check that it worked as expected
  nrow(proj$changelog) == 6
  proj$changelog$step[6] == "absolute limits"

  sum(proj$data$fDOM_QSU > 50, na.rm=TRUE) == 0
  sum(proj$data$fDOM_QSU < 2, na.rm=TRUE) == 0

  sum(proj$flags$flag_rm$fDOM_QSU == "RM02", na.rm=TRUE) == 67
