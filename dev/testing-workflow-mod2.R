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

