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
#' @param fieldform Field form from the `sondeproj`.
#' @param calcheck Calibration check from the `sondeproj`.
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
                                 calcheck=FALSE),
                       fieldform=NULL,
                       calcheck=NULL){
  #get data from field form for determining cal check (oow periods)
  if(!is.null(fieldform)){oow_data <- get_oow(fieldform)}
  #get cal data
  if(opts$calcheck & !is.null(fieldform) & !is.null(calcheck)){
    #use ff data to determine when cal data likely was
    mean_visit <- oow_data %>% rowwise() %>%
      mutate(avg_time = mean(c(.data$start, .data$end)),date = as.Date(.data$avg_time))

    cal_data <- calcheck %>%
      dplyr::left_join(mean_visit %>% select("date", "avg_time"),
                       join_by("Date" == "date")) %>%
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

  #create plot based on options
    #base plot
    p <- ggplot(data, aes(x = .data$DateTime_rd,y = .data[[y_var]])) +
      labs(x="Date", y=y_var_nice)

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
      p <- p + geom_rect(data = oow_data,
                         aes(xmin = .data$start, xmax = .data$end,
                             ymin = min(data[[y_var]], na.rm = TRUE),
                             ymax = max(data[[y_var]], na.rm = TRUE)),
                         inherit.aes = FALSE,fill = "darkred",color = "darkred",
                         alpha = 0.4)
    }

    #plot cal check
    if(opts$calcheck && !is.null(calcheck)){
      #plot one at at time because color scales are a poop
      p <- p + geom_point(data = cal_data,
                          aes(x = .data$avg_time, y = .data$value, shape = .data$type),
                          color = "darkred", size = 2) +
        scale_shape_manual(values = c("Resident_Value" = 17,
                                      "Check_Value" = 15),name = "Calibration Check")
    }

    return(p)


}
