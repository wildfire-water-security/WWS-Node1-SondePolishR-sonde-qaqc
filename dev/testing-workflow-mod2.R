#goals of the module 2: general visual plotting of the data, visualize the calibration and OOW period data

## input to function will be sondeproj

sondeproj <- example_sondeproj

#UI options for plotting
  y_var <- "Temp_C"
  points <- TRUE
  line <- TRUE
  oow <- TRUE
  calcheck <- TRUE
  files <- TRUE
  date_start <- min(sondeproj$data$Date)
  date_end <- max(sondeproj$data$Date)

#create plot
plot_data <- sondeproj$data %>% filter(Date >= date_start & Date <= date_end)
p <- ggplot(plot_data, aes(x=.data$DateTime_rd, y=.data[[y_var]]))

#plot points by filename
if(points & files){
  p <- p + geom_point(na.rm = TRUE, aes(color=.data$FileName)) + labs(color = "file name")
}

#plot points without color
if(points & !files){
  p <- p + geom_point(na.rm = TRUE)
}

#add line
if(line){
  p <- p + geom_line(na.rm = TRUE)
}

#add oow periods
if(oow & !is.null(sondeproj$fieldform)){
  #convert ff to OOW periods
   oow_data <- get_oow(sondeproj$fieldform)

   p <- p +  geom_rect(data=oow_data,
                       aes(xmin =.data$start,xmax=.data$end,ymin=-Inf, ymax=Inf),
                       inherit.aes = FALSE, fill="darkred", color="darkred",
                       alpha =0.5)
  }

#add cal check data
if(calcheck & !is.null(sondeproj$calcheck)){
  #pull times from ff
  mean_visit <- oow_data %>% dplyr::rowwise() %>%
    mutate(avg_time = mean(c(start, end)),
           date = as.Date(avg_time))
  cal_data <- sondeproj$calcheck %>% dplyr::left_join(mean_visit %>% dplyr::select("date", "avg_time"), dplyr::join_by("Date" == "date")) %>%
    dplyr::filter(Parameter == y_var) %>% tidyr::pivot_longer(c("Resident_Value", "Check_Value"), names_to = "type",
                                                              values_to = "value")

  p <- p + geom_point(data=cal_data,
                        aes(x=.data$avg_time, y =.data$value, fill=type), shape=24, stroke =1) +
      labs(fill="Calibration Check") + scale_fill_manual(values=c("#39FF14", "#FFD700"))

}

## testing how to display different versions based on a table selection
  #user selection via changlelog table row
  row <- 2

  proj <- example_sondeproj
  log <- proj$changelog

  #we want to see what the data looks like before altering the ODO_mg_L points
  #zoom in to better check
  cur <- ggplot(proj$data[1:100,], aes(x=DateTime, y=ODO_mg_L)) + geom_line()

  #get the diffs to apply
  diff_list <- log$diff_name[1:2]
  diff_list <- diff_list[grepl("^dd", diff_list)]
  diffs <- proj$diffs[names(proj$diffs) %in% diff_list]

#testing storing both diffs
  data1 <- example_data
  data2 <- data1
  data2$fDOM_QSU[1:4] <- NA
  dd1 <- list(commit_diff(data1, data2))

  #make more changes
  data3 <- data2
  data3$ODO_mg_L[5:7] <- data3$ODO_mg_L[5:7] * 0.8
  dd2 <- list(fw=commit_diff(data2, data3),
              rv=commit_diff(data3, data2))

  #more changes
  data4 <- data3
  data4$Temp_C[1:100] <- NA
  dd3 <- list(fw=commit_diff(data3, data4),
              rv=commit_diff(data4, data3))

  #test 1: go between one set of changes
    test1 <- apply_diff(data3, dd3$fw)
    test2 <- apply_diff(data4, dd3$rv)

  #test2: go from data to data 4
    diffs <- list(dd1, dd2, dd3)
    data_fw <- data1
    for(x in diffs[1:3]){
      data_fw <- apply_diff(data_fw, x$fw)
    }

    all.equal(data4, data_fw)

  #test3: go from data4 to data
    diffs <- list(dd1, dd2, dd3)
    data_fw <- data4
    for(x in diffs[1:3]){
      data_rv <- apply_diff(data_rv, x$rv)
    }

    all.equal(data1, data_rv)

#try just storing a unaltered copy of the data (as we add data, we would add to it)
  ogdata <- example_data

  editdata <- example_sondeproj$data

  #test getting to the first change
  row <- 2

  proj <- example_sondeproj
  proj$ogdata <- ogdata
  log <- proj$changelog

  #we want to see what the data looks like before altering the ODO_mg_L points
  #zoom in to better check
  cur <- ggplot(proj$data[1:100,], aes(x=DateTime, y=ODO_mg_L)) + geom_line()

  #get the diffs to apply
  diff_list <- log$diff_name[1:2]
  diff_list <- diff_list[grepl("^dd", diff_list)]
  diffs <- proj$diffs[names(proj$diffs) %in% diff_list]

  #apply changes up to what we want
  data_fw <- apply_diff(proj$ogdata, dd1)

  datastop <- apply_diff(proj$ogdata, dd1)
  stop_plot <- ggplot(datastop[1:100,], aes(x=DateTime, y=ODO_mg_L)) + geom_line()

#add some data
  proj$data <- proj$data %>% bind_rows(read_sonde("inst/extdata/example-csv-data3.csv")) %>%
    arrange(DateTime) %>% mutate(Index = 1:n())
  proj$ogdata <- proj$ogdata %>% bind_rows(read_sonde("inst/extdata/example-csv-data3.csv")) %>%
    arrange(DateTime) %>% mutate(Index = 1:n())

  #reapply the changes
  datastop2 <- apply_diff(proj$ogdata, dd1)
  ggplot(datastop, aes(x=DateTime, y=ODO_mg_L)) + geom_line()
  ggplot(proj$data, aes(x=DateTime, y=ODO_mg_L)) + geom_line()
