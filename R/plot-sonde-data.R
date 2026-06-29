#function used to plot sonde data

#' Create ggplot of sonde project
#'
#' Allows options for plotting sonde project data for different variables, used within modules to
#' simplify code for plotting
#'
#' @param data A `data.frame` with the sonde data to plot.
#' @param y_var Y-variable to plot on the y-axis.
#' @param opts A list of options for plotting:
#' - points: should points be plotted?
#' - line: should line be plotted?
#' -files: should points be colored by file?
#' -oow: should out of water periods be plotted?
#' -calcheck: should cal check be plotted?
#' -precip: should precip data be plotted?
#' @param fieldform Field form from the `sondeproj`.
#' @param calcheck Calibration check from the `sondeproj`.
#' @param precip Precip data from the `sondeproj`.
#'
#' @returns a `ggplot2` object
#' @export
#'
#' @examples
#' plot_sonde(example_data, "Temp_C")
plot_sonde <- function(data, y_var,
                       opts=list(points=TRUE,
                                 line=TRUE,
                                 files=FALSE,
                                 oow=FALSE,
                                 calcheck=FALSE,
                                 precip=FALSE),
                       fieldform=NULL,
                       calcheck=NULL,
                       precip = NULL){
  #get data from field form for determining cal check (oow periods)
  if(!is.null(fieldform)){oow_data <- get_oow(fieldform)}
  #get cal data
  if(opts$calcheck & !is.null(calcheck) && "Est_Time" %in% colnames(calcheck)){
    cal_data <- calcheck %>%
      filter(.data$Parameter == y_var) %>%
      pivot_longer(c("Resident_Value", "Check_Value"),names_to = "type",values_to = "value")}

  #nice name for y axis
    nice_names <- c("fDOM_QSU" = "fDOM (QSU)",
                    "ODO_mg_L" = "Dissolved Oyxgen (mg/L)",
                    "SpCond_uS_cm" = "Specific Conductance (\u03BCS/cm)",
                    "Turbidity_FNU" = "Turbidity (FNU)",
                    "pH"  = "pH",
                    "Temp_C" = "Temperature (\u00B0C)")

    y_var_nice <- ifelse(y_var %in% names(nice_names),nice_names[y_var],y_var)

  #get date range to clip OOW and calcheck periods
    date_rg <- range(as.Date(data$DateTime_rd))

  #create plot based on options
    #base plot
    p <- ggplot(data, aes(x = .data$DateTime_rd,y = .data[[y_var]])) +
      labs(x="Date", y=y_var_nice)

    #add precip data
    if(opts$precip){
      #get scale so it's scaled for all the data
      precip_scale <- diff(range(data[[y_var]], na.rm = TRUE)) / diff(range(precip$Precip_mm_hr, na.rm = TRUE))

      precip <- precip %>% filter(.data$DateTime >= min(date_rg) & .data$DateTime <= max(date_rg)) %>%
        mutate(Precip_mm_hr = ifelse(.data$Precip_mm_hr == 0, 0.001, .data$Precip_mm_hr))

      #get offset to plot
      precip_offset <- min(data[[y_var]], na.rm = TRUE) - min(precip$Precip_mm_hr, na.rm = TRUE) * precip_scale

      precip <- precip %>% mutate(ymax = .data$Precip_mm_hr * precip_scale + precip_offset,
                                              ymin = min(data[[y_var]], na.rm=TRUE))

      p <- p + geom_segment(data = precip,aes(x=.data$DateTime, xend=.data$DateTime,
                                                    y=.data$ymin, yend=.data$ymax),alpha = 0.3)
    }

    #add points (colored or not)
    if(opts$points && opts$files){
      p <- p +  geom_point(aes(color = .data$FileName),na.rm = TRUE, size = 1.5) +
        labs(color = "File Name")
    }

    if(opts$points && !opts$files){
      p <- p + geom_point(na.rm = TRUE, size = 1.5)
    }

    #add a line
    if(opts$line){
      p <- p + geom_line(na.rm = TRUE, linewidth = 0.3)
    }

    #plot oow periods
    if(opts$oow && !is.null(fieldform)){
      oow_data_clip <- oow_data %>% filter(as.Date(.data$end) >= min(date_rg) & as.Date(.data$start) <= max(date_rg))
      if(nrow(oow_data_clip) > 0){
        p <- p + geom_rect(data = oow_data_clip,
                           aes(xmin = .data$start, xmax = .data$end,
                               ymin = min(data[[y_var]], na.rm = TRUE),
                               ymax = max(data[[y_var]], na.rm = TRUE)),
                           inherit.aes = FALSE,fill = "darkred",color = "darkred",
                           alpha = 0.4)
      }

    }

    #plot cal check
    if(opts$calcheck && !is.null(calcheck)){
      cal_data_clip <- cal_data %>% filter(as.Date(.data$Est_Time) >= min(date_rg) & as.Date(.data$Est_Time) <= max(date_rg))

    if(nrow(cal_data_clip) > 0){
      #plot one at at time because color scales are a poop
      p <- p + geom_point(data = cal_data_clip,
                          aes(x = .data$Est_Time, y = .data$value, shape = .data$type),
                          color = "darkred", size = 2) +
        scale_shape_manual(values = c("Resident_Value" = 17,
                                      "Check_Value" = 15),name = "Calibration Check")
    }

    }

    return(p)


}
