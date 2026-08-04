#' Create plotly object of requested sonde data
#'
#' Allows options for iterative plotting sonde project data for different variables,
#' used within modules to simplify code for plotting.
#'
#' @param data A `data.frame` with the sonde data to plot.
#' @param proj A `sondeproj` object with additional metadata to plot,
#' only required if opts includes oow, calcheck, quality_flag or `y2_var` is "precip" or "raw".
#' @param y_var Y-variable to plot on the y-axis.
#' @param y2_var A second, optional y-variable to plot on a second axis.
#' @param opts A list of options for plotting:
#' - points: should points be plotted?
#' - line: should line be plotted?
#' -files: should points be colored by file?
#' -oow: should out of water periods be plotted?
#' -calcheck: should cal check be plotted?
#' -quality_flag: should questionable points be plotted?
#' @param source A character specifying the plot name for shiny reactivity.
#'
#' @returns a `plotly` object
#' @export
#'
#' @examples
#' plot_sonde(example_data, y_var = "Temp_C")
#' plot_sonde(example_data, y_var = "Temp_C", y2_var = "ODO_mg_L") #adding a second axis
#' plot_sonde(example_data, proj = example_sondeproj,
#' y_var ="Temp_C", y2_var = "precip")
plot_sonde <- function(data, y_var, y2_var=NULL,
                       proj=NULL,
                       opts=list(points=TRUE,
                                 line=TRUE,
                                 files=FALSE,
                                 oow=FALSE,
                                 calcheck=FALSE,
                                 qualflag=FALSE),
                       source = "plot"){
  stopifnot(is.null(proj) || inherits(proj, "sondeproj"))

  #pull things from project
    if(!is.null(proj) && opts$oow | opts$calcheck){
      fieldform <- proj$fieldform
      calcheck <- proj$calcheck
    }else{
      fieldform <- NULL
      calcheck <- NULL
    }

    if(!is.null(y2_var) && y2_var == "precip"){
      precip <- proj$precip
    }else{precip <- NULL}

  #nice name for y axes
    y_var_nice <- get_yvar(y_var)
    y2_var_nice <- ifelse(!is.null(y2_var), get_yvar(y2_var), "")

  #sort data so line looks correct and remove NA values to prevent warnings
    data <- data %>% arrange(.data$DateTime_rd) %>% filter(!is.na(.data[[y_var]]))

  #get date range to clip OOW and calcheck periods
    date_rg <- range(as.Date(data$DateTime_rd))

  #get cal data
    if(opts$calcheck & !is.null(proj) && "Est_Time" %in% colnames(calcheck)){
      cal_data <- calcheck %>%
        filter(.data$Parameter == y_var) %>%
        pivot_longer(c("Resident_Value", "Check_Value"),names_to = "type",values_to = "value")}

  #set up the plot
    mode <- case_when(
      opts$points & opts$line ~ "lines+markers",
      opts$points & !opts$line ~ "markers",
      !opts$points & opts$line ~ "lines")

    p <- plot_ly(source = source) %>%
      layout(paper_bgcolor = "#3c4d5a", plot_bgcolor = "#475763", font = list(color = "#ebebeb", family="sans-serif"),
             xaxis = list(title = "<b>Date</b>"),
             yaxis2=list(gridcolor = "#3c4d5a", zeroline = FALSE,title = paste0("<b>", y_var_nice, "</b>"),
                         overlaying = "y", side = "left"),
             yaxis=list(gridcolor = "#3c4d5a",zeroline = FALSE,side = "right",
                        title = paste0("<b>", y2_var_nice, "</b>")))

  #add second axis (in the back)
    if(!is.null(y2_var)){
      if(y2_var == "precip" & !is.null(precip)){
        precip <- precip %>% filter(.data$DateTime >= min(date_rg) & .data$DateTime <= max(date_rg)) %>%
          arrange(.data$DateTime)

        #add second axis
        p <- p %>% add_trace(data= precip, x=~DateTime, y=~Precip_mm_hr, type="scatter", yaxis="y", mode="lines",
                             name = y2_var_nice,
                             line = list(color = "#1d3040"))
      }else if(y2_var == "raw"){
        raw_data <- get_raw_data(proj)
        p <- p %>% add_trace(data= raw_data, x=~DateTime_rd, y=as.formula(paste0("~`", y_var, "`")),
                             type="scatter", yaxis="y", mode="lines",
                             name = "Raw Data",
                             line = list(color = "#1d3040"))
      }else if(y2_var != "precip"){
        p <- p %>% add_trace(data= data, x=~DateTime_rd, y=as.formula(paste0("~`", y2_var, "`")),
                             type="scatter", yaxis="y", mode="lines",
                             name = y2_var_nice,
                             line = list(color = "#1d3040"))
      }
    }

  #add main data
    if(!opts$files){
      p <- p %>% add_trace(data = data, x = ~DateTime_rd,y =as.formula(paste0("~`", y_var, "`")),
                           mode=mode, type="scatter", name=y_var_nice, yaxis="y2", inherit=FALSE)
    }else{
      pal <- colorRampPalette(c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854" ,"#FFD92F" ,"#E5C494", "#B3B3B3")) #from Set 2 color brewer
      files <- sort(unique(data$FileName))
      cols <- setNames(pal(length(files)), files)

      #add unique per file (prevents color bugs)
      for (f in files) {
        df <- data %>% filter(.data$FileName == f)
        p <- p %>% add_trace(data = df, x = ~DateTime_rd, y = as.formula(paste0("~`", y_var, "`")),
                             name = f,line = list(color = cols[[f]]),marker = list(color = cols[[f]]),
                             mode=mode, type="scatter", yaxis="y2", inherit = FALSE)}

    }

  #get traces and color if needed
    if(!opts$files){
      built_plot <- plotly_build(p)
      trace_name <- sapply(built_plot$x$data, function(trace) trace$name == y_var_nice)
      trace_n <- which(trace_name)
      if(opts$line){p <- p %>% style(line = list(color = "#ebebeb"), traces=trace_n)}
      if(opts$points){p <- p %>% style(marker = list(color = "#ebebeb"), traces=trace_n)}
    }

  #plot oow periods
    if(opts$oow && !is.null(fieldform)){
      #get data from field form for determining cal check (oow periods)
      if(!is.null(fieldform)){oow_data <- get_oow(fieldform, tz=proj$meta$tz,interval=get_interval(proj$data))}

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
        plot_info <- list(Resident_Value = list(shape="triangle-up", name = "Resident Value"),
                          Check_Value = list(shape="square", name = "Check Value"))

        #add unique per file (prevents color bugs)
        for (f in unique(cal_data_clip$type)){
          df <- cal_data_clip %>% filter(.data$type == f)
          p <- p %>% add_trace(data = df, x = ~Est_Time, y = ~value,
                               name = plot_info[[f]]$name,
                               marker = list(color = "black", size = 12, symbol=plot_info[[f]]$shape),
                               type="scatter", yaxis="y2", inherit = FALSE, mode="markers")}
      }}

  #plot questionable points
    if(opts$qualflag){
      questionable <- data %>% arrange(.data$Index) %>% mutate(qual_flags = get_qual_flags(.data, y_var)) %>%
        filter(!is.na(.data$qual_flags)) %>% arrange(.data$DateTime)

      qual_col <- c("Bad" = "darkred", "Questionable" = "orange")

      if(nrow(questionable) > 0){

        for(f in unique(questionable$qual_flags)){
          df <- questionable %>% filter(.data$qual_flags == f)
          p <- p %>% add_trace(data = df, x = ~DateTime_rd, y = as.formula(paste0("~`", y_var, "`")),
                               name = f,type="scatter", mode="markers", yaxis="y2",
                               marker = list(color = qual_col[[f]]), inherit = FALSE)}

      }}


      return(p)


  }
