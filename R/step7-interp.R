
#' @export
#' @rdname interp
interp_UI <- function(id){
  ns <- NS(id) #line to make module work
  tagList(
    sidebarLayout(
      sidebarPanel(
        update_parms_UI(ns("update_parms")),
        update_parms_UI(ns("update_parms"), input_id = "y2_var", text = "Select Second Parameter to Plot:"),

        HTML("<hr>"),
      #select physical limits
        tags$h5("Fill Missing Observations"),
                selectInput(ns("method"),
                  "Select Interpolation Method:",
                  choices = c("Linear" = "linear",
                              "Linear (seasonally adjusted)" = "ts_interp",
                              "Spline" = "spline",
                              "Random Forest" = "random_forest")),
                  fluidRow(
                    numericInput(ns("max_length"),"Max Fill Window (hr)",value =8,step=1, min=0),
                    conditionalPanel(condition = sprintf("input['%s'] == 'ts_interp'",ns("method")),
                      numericInput(ns("freq"), "Season Period (days)", value = 1,step = 1, min = 0))),
        HTML("<hr>"),

        apply_edit_UI(ns("apply_limits"), note=""),
        HTML("<hr>"),

        #date options
        weekly_range_sidebar_UI(ns("date_nav")),

        HTML("<hr>"),

      #plotting options
        plot_options_UI(ns("plot_opts")),

      ),
      mainPanel(
        plotlyOutput(ns("interp_plot"), height="60%"),
        #add buttons to navigate date
        weekly_range_buttons_UI(ns("date_nav")),
      ))


  )}


#' Fill in data gaps using interpolation
#'
#' Attempts to fill gaps less than the max length using the specified interpolation method.
#'
#' @keywords internal
#'
#' @param id An ID string passed to shiny::NS(), used for namespacing UI inputs/outputs.
#' @param sondeproj A `reactiveVal` holding the current dataset.
#' @param data_ver A `reactiveVal` holding a number used to track when new data is added to trigger resets.
#' @param y_var Y-variable to plot on the y-axis.
#'
#' @export
#' @rdname interp
interp_server <- function(id, sondeproj, data_ver, y_var){
  moduleServer(id, function(input, output, session){
  #keep track of second y_variable
    y2_var <- reactiveVal()

  #get column names after file upload (dynamic)
    update_parms_server("update_parms", sondeproj, data_ver, y_var, choices_fun = nice_yvar)
    update_parms_server("update_parms", sondeproj, data_ver, y2_var, input_id= "y2_var", choices_fun = nice_yvar)

  #get what to plot via user options
    plot_opts <- plot_options_server("plot_opts")

  #dynamic UI for frequency
  #   ns = session$ns #needed to make updating UI work
  # output$freq <- renderUI({
  #   if(input$method == "ts_interp"){
  #      numericInput(ns("freq"),"Season Period (days)",value = 1,step=1, min =0)
  #     }
  # })

  #keep track of dates
    dates <- weekly_range_server(
      "date_nav",
      min_date = reactive({req(sondeproj())
        min(sondeproj()$data$Date, na.rm = TRUE)}),
      max_date = reactive({req(sondeproj())
        max(sondeproj()$data$Date, na.rm = TRUE)}))

  #get data to fill and interpolation df as list
   data_fill_list <- reactive({
     req(sondeproj())
     withProgress(message = "Preparing data", value = 0, {
     prep_interp(sondeproj())
     })
   })

  #interpolate
  data_interp <- reactive({
    withProgress(message = "Interpolating data", value = 0, {
      run_interp(data_fill_list()$interp, y_var(), input$method, input$freq)
    })
  })

  #fill data
  data_fill <- reactive({
    req(data_fill_list(), data_interp(), y_var())
    withProgress(message = "Interpolating data", value = 0.5, {
      apply_interp(data_fill_list()$fill, data_interp(), y_var(), input$max_length)
    })
    })

  #filter data to plot
    plot_data <- reactive({
      req(sondeproj(), dates(), data_fill())

      data_fill() %>% dplyr::filter(.data$Date >= dates()[1], .data$Date <= dates()[2])

    })

  #create plotly plot
    plot_obj <- reactive({
      req(y_var(),y2_var(), plot_data())
      if(y2_var() == "none"){y2 <- NULL}else{y2 <- y2_var()}

      #use function to plot sonde data
      p <- plot_sonde(data = plot_data()%>% filter(!.data$fill_flag), y_var=y_var(), y2_var= y2, proj = sondeproj(), opts=plot_opts())
      #add interpolated data (show as green points)
      interp_points <- plot_data() %>% filter(.data$fill_flag) %>% filter(!is.na(.data[[y_var()]]))
      if(nrow(interp_points) > 0){
        y <- y_var()
        p <- p  %>% add_trace(data= interp_points, x=~DateTime_rd, y=as.formula(paste0("~`", y, "`")), type="scatter", mode="markers",
                                  name = "Flagged", marker = list(color = "#2ECC71"), inherit = FALSE)
      }


      #return plot
      p
    })

    output$interp_plot <- plotly::renderPlotly({
      validate(
        need(nrow(plot_data()) > 0,
             "No data available for the selected date range."))

      # convert to plotly
      p <- plot_obj()
      toWebGL(p)
    })

    observeEvent(input$modules, {
      req(input$modules == "step-7")

      plotlyProxy("interp_plot", session) %>%
        plotlyProxyInvoke("resize")
    })

  #create edit object
    edit <- reactive({
      req(data_fill(), y_var())
      newdata <- data_fill()
      rows <- newdata$fill_flag
      newdata <- newdata %>% select(-"fill_flag")

      #nice names of methods
      label_name <- switch(input$method,
                           "linear" = "linear interpolation",
                           "spline" = "spline interpolation",
                           "random_forest" = "a random forest",
                           "ts_interp" = "seasonally adjusted linear interpolation")

      #get diff and flags
      edit <- list(
        data = newdata,
        rows = rows,
        y_var = y_var(),
        step = "data interpolation",
        note = paste0("Data interpolated using ", label_name, " with a maximum gap size of ", input$max_length, " hours."),
        flag = "AD01",
        changetype = "flag_add")

      edit
    })

  #flagging module
    apply_edit_server("apply_limits", sondeproj, edit)

  #export plot so we can check it
    exportTestValues(
      plot_obj = plot_obj(),
      changelog = sondeproj()$changelog)

  })}
