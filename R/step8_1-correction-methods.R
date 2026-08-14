# additive correction -----
#' Apply an additive shift to data
#'
#' Used to adjust data either by a single value or apply a linear correction.
#'
#' @param id the shiny ID of the module
#' @param sondeproj A `reactiveVal` holding the current dataset.
#' @param data_ver A `reactiveVal` holding a number used to track when new data is added to trigger resets.
#' @param dates The date range to view the data.
#' @param period_view Should data be viewed by period?
#' @param p_length The length of the period to view.
#'
#' @returns a reactive of length two with the min and max dates.
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
additive_server <- function(id, sondeproj, y_var, plot, plot_data, index){
  moduleServer(id, function(input, output, session){

  #reset slope and intercept when data updates
    observeEvent(sondeproj(),{
      updateNumericInput(session,"slope", value =0)
      updateNumericInput(session,"int", value = 0)
    })

  #update guesses based on selected points
    reactive({
      guess <- guess_shift(sondeproj()$data, y_var(), index())
      updateNumericInput(session,"slope", value = guess$slope)
      updateNumericInput(session,"int", value = guess$int)
    })

  #create edit object
    edit <- reactive({
      newdata <- sondeproj()$data
        #get updated data
        newdata <- shift_points(newdata, y_var(), index(), shift_val = list(slope=input$slope, int=input$int))
        rows <- newdata$Index %in% index() #convert from row numbers to T/F
        note <- paste0("shift with slope ", input$slope," and intercept ", input$int)
        step <- "additive shifts"
        flag <- "CHG01"

      #make edit list
      list(data = newdata,rows = rows,y_var = y_var(),
        step = step,note = note,flag = flag)
    })

  #update plot
  p <- reactive({
    p <- plot()
    if(!is.null(index()) &&
       !is.null(input$slope) &&
       !is.null(input$int)){
      flag_data <- plot_data()[plot_data()$Index %in% index(),] %>% filter(!is.na(.data[[y_var()]]))
      p <- p %>% add_trace(data= flag_data, x=~DateTime_rd, y=as.formula(paste0("~`", y_var(), "`")), type="scatter", mode="markers",
                                 name = "Changed", marker = list(color = "darkred"), yaxis="y2", inherit=FALSE)
    }
      p

  })

    return(list(edit=edit, plot=p))
  })

}

# drift correction ------
#' Apply a drift correction to a data file
#'
#' Used to account for instrument drift by applying a linear correction to a data file. Uses differences between
#' check and resident sonde measurments when available as the default correction amount.
#'
#' @param id the shiny ID of the module
#' @param sondeproj A `reactiveVal` holding the current dataset.
#' @param data_ver A `reactiveVal` holding a number used to track when new data is added to trigger resets.
#' @param dates The date range to view the data.
#' @param period_view Should data be viewed by period?
#' @param p_length The length of the period to view.
#'
#' @returns a reactive of length two with the min and max dates.
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


#' @rdname additive-method
#' @export
drift_server <- function(id, sondeproj, y_var,plot, plot_data){
  moduleServer(id, function(input, output, session){

  #update the drift correction values once a file/variable has been chosen
    observeEvent(
      list(input$file, y_var()),{
        req(sondeproj(),input$file, y_var())
        vals <- guess_drift(sondeproj()$data, sondeproj()$calcheck, y_var(), input$file)

        updateNumericInput(session,"uncorrect",
                           value = round(vals$uncorrect, 3))

        updateNumericInput(session,"correct",
                           value = round(vals$correct, 3))
      })

  #create edit object
    edit <- reactive({
      req(input$file)
      newdata <- sondeproj()$data
      #get updated data
      rows <- newdata$FileName == input$file #T/F
      newdata[[y_var()]] <- apply_drift_shift(newdata[[y_var()]], rows, input$correct, input$uncorrect)
      note <- paste0("drift correction based on an uncorrected value of ", input$uncorrect," and corrected value of ", input$correct,
                     " for file ", input$file)
      step <- "drift correction"
      flag <- "CHG02"

      #make edit list
      list(data = newdata,rows = rows,y_var = y_var(),
           step = step,note = note,flag = flag)
    })

    #update plot
    p <- reactive({
      p <- plot()
      plot_range <- range(plot_data()$Date)
      dat <- edit()$data[edit()$rows,] %>% dplyr::filter(.data$Date >= plot_range[1], .data$Date <= plot_range[2]) %>%
        arrange(.data$DateTime_rd) %>% filter(!is.na(.data[[y_var()]]))

      if(nrow(dat) > 0){
        p <- p %>% add_trace(data= dat, x=~DateTime_rd, y=as.formula(paste0("~`", y_var(), "`")), type="scatter", mode="lines",
                             name = "Changed", line = list(color = "darkred"), yaxis="y2", inherit = FALSE)
      }

      p
      })


    return(list(edit=edit, plot=p))

  })

}

#smoothing correction ------
#' Apply a smoothing correction to a data file
#'
#' Used to smooth out messy data based on optical interference.
#'
#' @param id the shiny ID of the module
#' @param sondeproj A `reactiveVal` holding the current dataset.
#' @param data_ver A `reactiveVal` holding a number used to track when new data is added to trigger resets.
#' @param dates The date range to view the data.
#' @param period_view Should data be viewed by period?
#' @param p_length The length of the period to view.
#'
#' @returns a reactive of length two with the min and max dates.
#' @rdname smooth-method
#' @export
#' @keywords internal
#'
#'
smooth_UI <- function(id){
  ns <- NS(id)
  tagList(
    div(style="margin-bottom: 8px; font-size:14px", "Apply smoothing correction to selected data:"),

    fluidRow(selectInput(inputId=ns("method"),label = "Smoothing Method:",
                      choices = c("Rolling Mean" = "rollmean",
                                  "Rolling Median" = "rollmedian",
                                  "Savitzky–Golay Filter" = "savgol",
                                  "Kalman Filter" = "kalman"), selectize=TRUE, width="60%"),
          numericInput(ns("smooth_fact"),"Smoothing Factor:",value = 7,step=1, width="40%"))

  )}


#' @rdname additive-method
#' @export
smooth_server <- function(id, sondeproj, y_var, plot, plot_data, index){
  moduleServer(id, function(input, output, session){

 #update the default smoothing factor based on method
  observeEvent(list(y_var(), input$method),{
        req(input$method, y_var())

        default <- switch(input$method,
                          "rollmean" = 7 ,
                          "rollmedian" = 7,
                          "savgol" = 15,
                          "kalman" = 10)
        updateNumericInput(session,"smooth_fact",value = default)
      })


  #create edit object
    edit <- reactive({
      newdata <- sondeproj()$data
      #get updated data
      rows <- newdata$Index %in% index() #convert from row numbers to T/F
      nice_methods <- switch(input$method,
                             "rollmean" = "Rolling Mean",
                             "rollmedian" = "Rolling Median",
                             "savgol" = "Savitzky–Golay Filter",
                             "kalman" = "Kalman Filter")
      newdata[[y_var()]] <- apply_smoothing(newdata, y_var(), input$method, index(), k=input$smooth_fact)
      note <- paste0("smoothing correction based using ", nice_methods," using a smoothing factor of ", input$smooth_fact)
      step <- "smoothing correction"
      flag <- "CHG05"

      #make edit list
      list(data = newdata,rows = rows,y_var = y_var(),
           step = step,note = note,flag = flag)
    })

    #update plot
    p <- reactive({
      p <- plot()
      plot_range <- range(plot_data()$Date)
      dat <- edit()$data[edit()$rows,] %>% dplyr::filter(.data$Date >= plot_range[1], .data$Date <= plot_range[2]) %>%
        arrange(.data$DateTime_rd) %>% filter(!is.na(.data[[y_var()]]))

      if(nrow(dat) > 0){
        p <- p %>% add_trace(data= dat, x=~DateTime_rd, y=as.formula(paste0("~`", y_var(), "`")), type="scatter", mode="lines",
                                   name = "Smoothed", line = list(color = "darkred"), yaxis="y2", inherit = FALSE)
      }

      p
    })

    return(list(edit=edit, plot=p))

  })

}
