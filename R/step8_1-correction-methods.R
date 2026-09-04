# additive correction -----
#' Apply an additive shift to data
#'
#' Used to adjust data either by a single value or apply a linear correction.
#'
#' @param id the shiny ID of the module
#' @param sondeproj A `reactiveVal` holding the current dataset.
#' @param y_var A `reactiveVal` holding the y-variable being plotted.
#' @param plot A `reactiveVal` holding a basic plot before things were added.
#' @param plot_data A `reactiveVal` holding the current data being plotted.
#' @param currplot A `reactiveVal` holding the current main plot.
#' @param curredit A `reactiveVal` holding the current edit object.
#' @param currmethod A `reactiveVal` holding the current correction method.
#' @param index A `reactiveVal` holding the indices of the selected points within the full dataset.
#'
#' @returns Updates the values of `currplot` and `curredit` to be current with the method.
#' @rdname additive-method
#' @export
#' @keywords internal
#'
#'
additive_UI <- function(id){
  ns <- NS(id)
  tagList(
    div(style="margin-bottom: 8px; font-size:14px", "Adjust the slope and intercept to shift the selected data:"),

    fluidRow(numericInput(ns("slope"),"Slope",value = 0,step=0.001),
             numericInput(ns("int"),"Intercept",value = 0,step=0.01)))
  }


#' @rdname additive-method
#' @export
additive_server <- function(id, sondeproj, y_var, plot, plot_data, currplot, curredit, currmethod, index){
  moduleServer(id, function(input, output, session){

  #clear index if currmethod changes
    observeEvent(list(currmethod()), {
      req(index())
      index(NULL)
    })

  #reset slope and intercept when data updates
    observeEvent(sondeproj(),{
      req(currmethod() == "additive")
      updateNumericInput(session,"slope", value =0)
      updateNumericInput(session,"int", value = 0)
    })

  #update guesses based on selected points
    observeEvent(index(),{
      req(currmethod() == "additive")

      guess <- guess_shift(sondeproj()$data, y_var(), index())
      updateNumericInput(session,"slope", value = guess$slope)
      updateNumericInput(session,"int", value = guess$int)
    })

  #create edit object
   observe({
     req(currmethod() == "additive", sondeproj(), y_var(), index())
     newdata <- sondeproj()$data
     indexn <- index()
        #get updated data
        newdata <- shift_points(newdata, y_var(), indexn, shift_val = list(slope=input$slope, int=input$int))
        note <- paste0("shift with slope ", input$slope," and intercept ", input$int)
        step <- "additive shifts"
        flag <- "CHG01"

      #make edit list
      curredit(list(data = newdata,rows = indexn,y_var = y_var(),
        step = step,note = note,flag = flag))
    })

  #update plot
   observe({
     req(currmethod() == "additive")

    p <- plot()
    if(!is.null(index()) &&
       !is.null(input$slope) &&
       !is.null(input$int)){
        data <- sondeproj()$data
        plot_range <- range(plot_data()$Date)

        flag_data <- shift_points(data, y_var(), index(),
                                shift_val = list(slope=input$slope, int=input$int))
        flag_data <- flag_data[data$Index %in% index(),] %>%
        filter(!is.na(.data[[y_var()]])) %>%
          dplyr::filter(.data$Date >= plot_range[1], .data$Date <= plot_range[2])

      if(nrow(flag_data) > 0){
        p <- p %>% add_trace(data= flag_data, x=~DateTime_rd, y=as.formula(paste0("~`", y_var(), "`")), type="scatter", mode="markers",
                             name = "Changed", marker = list(color = "darkred"), yaxis="y2", inherit=FALSE)
      }

    }
    currplot(p)

  })

  })

}

# drift correction ------
#' Apply a drift correction to a data file
#'
#' Used to account for instrument drift by applying a linear correction to a data file. Uses differences between
#' check and resident sonde measurements when available as the default correction amount.
#'
#' @param id the shiny ID of the module
#' @param sondeproj A `reactiveVal` holding the current dataset.
#' @param y_var A `reactiveVal` holding the y-variable being plotted.
#' @param plot A `reactiveVal` holding a basic plot before things were added.
#' @param plot_data A `reactiveVal` holding the current data being plotted.
#' @param currplot A `reactiveVal` holding the current main plot.
#' @param curredit A `reactiveVal` holding the current edit object.
#' @param currmethod A `reactiveVal` holding the current correction method.
#'
#' @returns Updates the values of `currplot` and `curredit` to be current with the method.
#' @rdname drift-method
#' @export
#' @keywords internal
#'
#'
drift_UI <- function(id, sondeproj){
  ns <- NS(id)
  opts <- unique(sondeproj()$data$FileName)
  tagList(selectInput(ns("file"),label = "File to Drift Correct:",
                      choices = opts[opts != "interpolated"], selectize=TRUE),
          fluidRow(div(style="margin-bottom: 8px; font-size:14px",
                       "Adjust the corrected and uncorrected values to account for drift:"),
                   numericInput(ns("uncorrect"),"Uncorrected",value = 0,step=0.01),
                   numericInput(ns("correct"),"Corrected",value = 0,step=0.01)))
}


#' @rdname drift-method
#' @export
drift_server <- function(id, sondeproj, y_var,plot, plot_data, currplot, curredit, currmethod){
  moduleServer(id, function(input, output, session){

  #update the drift correction values once a file/variable has been chosen
    observeEvent(
      list(input$file, y_var()),{
        req(sondeproj(),input$file, y_var(), currmethod() == "drift")
        vals <- guess_drift(sondeproj()$data, sondeproj()$calcheck, y_var(), input$file)

        updateNumericInput(session,"uncorrect",
                           value = round(vals$uncorrect, 3))

        updateNumericInput(session,"correct",
                           value = round(vals$correct, 3))
      })

  #create edit object
    observe({
      req(input$file, currmethod() == "drift")
      newdata <- sondeproj()$data

      #get updated data
      indexn <- newdata %>% filter(.data$FileName == input$file) %>% pull(.data$Index)
      rows <- newdata$Index %in% indexn #T/F
      newdata[[y_var()]] <- apply_drift_shift(newdata[[y_var()]], rows, input$correct, input$uncorrect)
      note <- paste0("drift correction based on an uncorrected value of ", input$uncorrect," and corrected value of ", input$correct,
                     " for file ", input$file)
      step <- "drift correction"
      flag <- "CHG02"

      #make edit list
      curredit(list(data = newdata,rows = indexn,y_var = y_var(),
           step = step,note = note,flag = flag))
    })

    #update plot (try to do without edit call!!!)
   observe({
     req(currmethod() == "drift", plot_data(), y_var(), plot(), input$file)
      p <- plot()
      plot_range <- range(plot_data()$Date)
      data <- sondeproj()$data
      data[[y_var()]] <- apply_drift_shift(data[[y_var()]], data$FileName == input$file, input$correct, input$uncorrect)
      dat <- data[data$FileName == input$file,] %>% dplyr::filter(.data$Date >= plot_range[1], .data$Date <= plot_range[2]) %>%
        arrange(.data$DateTime_rd) %>% filter(!is.na(.data[[y_var()]]))

      if(nrow(dat) > 0){
        p <- p %>% add_trace(data= dat, x=~DateTime_rd, y=as.formula(paste0("~`", y_var(), "`")), type="scatter", mode="lines",
                             name = "Changed", line = list(color = "darkred"), yaxis="y2", inherit = FALSE)
      }

      currplot(p)

      })

  })

}

#smoothing correction ------
#' Apply a smoothing correction to a data file
#'
#' Used to smooth out messy data based on optical interference.
#'
#' @param id the shiny ID of the module
#' @param sondeproj A `reactiveVal` holding the current dataset.
#' @param y_var A `reactiveVal` holding the y-variable being plotted.
#' @param plot A `reactiveVal` holding a basic plot before things were added.
#' @param plot_data A `reactiveVal` holding the current data being plotted.
#' @param currplot A `reactiveVal` holding the current main plot.
#' @param curredit A `reactiveVal` holding the current edit object.
#' @param currmethod A `reactiveVal` holding the current correction method.
#' @param index A `reactiveVal` holding the indices of the selected points within the full dataset.
#'
#' @returns Updates the values of `currplot` and `curredit` to be current with the method.
#' @rdname smooth-method
#' @export
#' @keywords internal
#'
#'
smooth_UI <- function(id){
  ns <- NS(id)
  tagList(
    div(style="margin-bottom: 8px; font-size:14px", "Apply smoothing correction to selected data:"),

    fluidRow(selectInput(ns("method"),label = "Smoothing Method:",
                      choices = c("Rolling Mean" = "rollmean",
                                  "Rolling Median" = "rollmedian",
                                  "Kalman Filter" = "kalman"), selectize=TRUE, width="60%"),
          numericInput(ns("smooth_fact"),"Smoothing Factor:",value = 7,step=2, min=1, width="40%"))

  )}


#'
#' @rdname smooth-method
#' @export
smooth_server <- function(id, sondeproj, y_var, plot, plot_data, currplot, curredit, currmethod, index){
  moduleServer(id, function(input, output, session){

  #clear index if currmethod changes or we change plotted data (then index no longer accurate)
    observeEvent(list(currmethod()), {
      req(index())
      index(NULL)
    })

 #update the default smoothing factor based on method
  observeEvent(list(y_var(), input$method),{
        req(input$method, y_var(), currmethod() == "smooth")

        default <- switch(input$method,
                          "rollmean" = 7 ,
                          "rollmedian" = 7,
                          "savgol" = 15,
                          "kalman" = 10)
        updateNumericInput(session,"smooth_fact",value = default)
      })

    observeEvent(input$smooth_fact, {
      req(input$method, input$smooth_fact, currmethod() == "smooth")
      if(input$method %in% c("rollmedian","savgol") && input$smooth_fact %% 2 == 0){
        updateNumericInput(session,"smooth_fact",value = input$smooth_fact + 1)
      }
    })

  #create edit object
   observe({
      req(sondeproj(), y_var(), currmethod() == "smooth")
      newdata <- sondeproj()$data
      #get updated data
      indexn <- index() #convert from row numbers to T/F
      req(input$method, input$smooth_fact)
      nice_methods <- switch(input$method,
                             "rollmean" = "Rolling Mean",
                             "rollmedian" = "Rolling Median",
                             "kalman" = "Kalman Filter")
      newdata <- apply_smoothing(newdata, y_var(), input$method, indexn, k=input$smooth_fact)
      note <- paste0("smoothing correction using ", nice_methods," using a smoothing factor of ", input$smooth_fact)
      step <- "smoothing correction"
      flag <- "CHG05"

      #make edit list
      curredit(list(data = newdata,rows = indexn, y_var = y_var(),
           step = step,note = note,flag = flag))
    })

    #update plot
    observe({
      req(plot(), y_var(), currmethod() == "smooth", input$smooth_fact)
      p <- plot()
      plot_range <- range(plot_data()$Date)
      data <- sondeproj()$data

      if(!is.null(index())){
        smooth_data <- apply_smoothing(data, y_var(), input$method, index(), k=input$smooth_fact)
        smooth_data <- smooth_data[index(),] %>% arrange(.data$DateTime_rd) %>% filter(!is.na(.data[[y_var()]])) %>%
          dplyr::filter(.data$Date >= plot_range[1], .data$Date <= plot_range[2])

        if(nrow(smooth_data) > 0){
          p <- p %>% add_trace(data= smooth_data, x=~DateTime_rd, y=as.formula(paste0("~`", y_var(), "`")), type="scatter", mode="lines+markers",
                               name = "Smoothed", line = list(color = "darkred"),
                               marker = list(color = "darkred"), yaxis="y2", inherit = FALSE)
        }

      }

      currplot(p)
    })

  })

}
