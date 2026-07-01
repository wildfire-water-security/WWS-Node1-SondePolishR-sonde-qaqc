#' Create plotly object of requested sonde data
#'
#' Allows options for interative plotting sonde project data for different variables,
#' used within modules to simplify code for plotting.
#'
#' @param data A `data.frame` with the sonde data to plot.
#' @param y_var Y-variable to plot on the y-axis.
#' @param sec_y_var A second, optional y-variable to plot on a second axis.
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
#' @param source A character specifiying the plot name for shiny reactivity.
#'
#' @returns a `plotly` object
#' @export
#'
#' @examples
#' plot_sonde(example_data, "Temp_C")
plot_sonde <- function(data, y_var, sec_y_var,
                       opts=list(points=TRUE,
                                 line=TRUE,
                                 files=FALSE,
                                 oow=FALSE,
                                 calcheck=FALSE,
                                 precip=FALSE),
                       fieldform=NULL,
                       calcheck=NULL,
                       precip = NULL,
                       source = "plot"){
  #get data from field form for determining cal check (oow periods)
  if(!is.null(fieldform)){oow_data <- get_oow(fieldform)}
  #get cal data
  if(opts$calcheck & !is.null(calcheck) && "Est_Time" %in% colnames(calcheck)){
    cal_data <- calcheck %>%
      filter(.data$Parameter == y_var) %>%
      pivot_longer(c("Resident_Value", "Check_Value"),names_to = "type",values_to = "value")}

  #nice name for y axis
    y_var_nice <- get_yvar(y_var)

  #sort data so line looks correct
    data <- data %>% arrange(.data$DateTime_rd)

  #get date range to clip OOW and calcheck periods
    date_rg <- range(as.Date(data$DateTime_rd))

  #create plot based on options
    #base plot
      mode <- case_when(
        opts$points & opts$line ~ "lines+markers",
        opts$points & !opts$line ~ "markers",
        !opts$points & opts$line ~ "lines")

      p <- plot_ly(source = source) %>%
        layout(paper_bgcolor = "#3c4d5a", plot_bgcolor = "#475763", font = list(color = "#ebebeb", family="sans-serif"),
               xaxis = list(title = "<b>Date</b>"),
               yaxis=list(gridcolor = "#3c4d5a", zeroline = FALSE,title = paste0("<b>", y_var_nice, "</b>")),
               yaxis2=list(gridcolor = "#3c4d5a",zeroline = FALSE,side = "right",
                           overlaying = "y", title = "<b>Precipitation (mm hr\U207B\U00B9)</b>"))

      #add precip data
      if(opts$precip){
        precip <- precip %>% filter(.data$DateTime >= min(date_rg) & .data$DateTime <= max(date_rg)) %>%
          arrange(.data$DateTime)

        #add second axis
        p <- p %>% add_trace(data= precip, x=~DateTime, y=~Precip_mm_hr, type="scatter", yaxis="y2", mode="lines",
                             name = "Precipitation",
                             line = list(color = "#1d3040"))
      }

      if(!opts$files){
        p <- p %>% add_trace(data = data, x = ~DateTime_rd,y = as.formula(paste0("~`", y_var, "`")),
                             mode=mode, type="scatter", name=y_var_nice, yaxis="y")

          if(opts$line){p <- p %>% style(line = list(color = "#ebebeb"), traces =ifelse(opts$precip, 2,1))}
          if(opts$points){p <- p %>% style(marker = list(color = "#ebebeb"), traces =ifelse(opts$precip, 2,1))}
      }else{
        p <- p %>% add_trace(data = data, x = ~DateTime_rd,y = as.formula(paste0("~`", y_var, "`")),
                     mode=mode, type="scatter", color = ~FileName, yaxis="y")
      }



    #plot oow periods
    if(opts$oow && !is.null(fieldform)){
      oow_data_clip <- oow_data %>% filter(as.Date(.data$end) >= min(date_rg) & as.Date(.data$start) <= max(date_rg))
      if(nrow(oow_data_clip) > 0){
        p <- p %>%
          layout(
            shapes = lapply(seq_len(nrow(oow_data_clip)), function(i) {
              list(
                type = "rect",xref= "x",yref = "paper",
                x0 = oow_data_clip$start[i],x1 = oow_data_clip$end[i],
                y0 = 0,y1 = 1,fillcolor = "darkred",line = list(color = "darkred"),
                opacity = 0.4)}))
    } }

    #plot cal check
    if(opts$calcheck && !is.null(calcheck)){
      cal_data_clip <- cal_data %>% filter(as.Date(.data$Est_Time) >= min(date_rg) & as.Date(.data$Est_Time) <= max(date_rg))

    if(nrow(cal_data_clip) > 0){
      #plot one at at time because color scales are a poop
      p <- p %>% add_trace(data = cal_data_clip, x = ~Est_Time,y = ~value,
                           mode="markers", type="scatter", color = ~type, symbol = I("triangle-up"), yaxis="y",
                           inherit = FALSE, marker = list(size = 12))
    }

    }

    return(p)


}
