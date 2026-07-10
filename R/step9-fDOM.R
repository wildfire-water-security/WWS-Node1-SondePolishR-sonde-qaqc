
#' @export
#' @rdname fdom
fdom_UI <- function(id){
  ns <- NS(id) #line to make module work
  tagList(
    withMathJax(), #allows equations to work

    sidebarLayout(
      sidebarPanel(

      #temperature correction
      tags$h5("Temperature Corrections"),
      fluidRow(column(width = 6,p("\\[fDOM_{T} = \\frac{fDOM}{1 + \\rho (T - 25)}\\]")),
               column(width = 6,numericInput(ns("rho"), tags$span("\U03C1", style = "font-size: 20px;"), value = -0.011, step=0.001))),
      tags$small("Source: Watras et al. 2011"),


      HTML("<hr>"),

      #turbidity correction
        tags$h5("Turbidity Corrections"),
        selectInput(ns("method"),
                  "Select Correction Equation:",
                  choices = c("None" = "none", "Inverse Polynomial" = "inverse_poly", "Exponential (1-parameter)" = "1p_exponential",
                              "Exponential (2-parameter)" = "2p_exponential","Exponential (5-parameter)" = "5p_exponential"),
                  selected = "inverse_poly",
                  multiple = FALSE),
        uiOutput(ns("equation")),
        tags$small(textOutput(ns("source"))),
        uiOutput(ns("coef_inputs")),


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
        plotlyOutput(ns("fdom_plot"), height="60%"),
        #add buttons to navigate date
        weekly_range_buttons_UI(ns("date_nav")),
      ))


  )}


#' Correct fDOM data for temperature and turbidity
#'
#' The fDOM signal can be affected but sediment and temperature, but these effects can be corrected for.
#'
#' @keywords internal
#'
#' @param id An ID string passed to shiny::NS(), used for namespacing UI inputs/outputs.
#' @param sondeproj A `reactiveVal` holding the current dataset.
#' @param data_ver A `reactiveVal` holding a number used to track when new data is added to trigger resets.
#' @param y_var Y-variable to plot on the y-axis.
#' @param dates The date range to view the data.
#' @param period_view Should data be viewed by period?
#' @param p_length The length of the period to view.
#' @export
#' @rdname fdom
fdom_server <- function(id, sondeproj, data_ver, y_var, period_view, dates, p_length){
  moduleServer(id, function(input, output, session){

  #get info on equation
    eq_info <- reactive({
      req(input$method)
      get_equation(input$method)
    })

  #show equation in UI based on method
    output$equation <- renderUI({
      req(input$method)
      withMathJax(HTML(eq_info()$equation))
    })

    output$source <- renderText({
      req(input$method)
      paste("Source:", eq_info()$source)
    })

 #show options for parameters
  ns = session$ns
  output$coef_inputs <- renderUI({
      req(input$method)
      params <- eq_info()$params

      tagList(
        fluidRow(
        lapply(names(params), function(par) {
          column(width = floor(12 / length(params)),
          numericInput(session$ns(par),
                       label = tags$span(par, style = "font-size: 20px;"),
                       value = params[[par]]$value,
                       step = params[[par]]$step %||% 0.01))})
      ))
    })

  #keep track of user parameter values to pass to corr_fun
  coef_vals <- reactive({
    req(input$method)
    params <- names(eq_info()$params)
    vals <- setNames(lapply(params, function(x) input[[x]]),params)
    # wait until new UI inputs exist
    req(all(vapply(vals, Negate(is.null), logical(1))))
    vals
  })

  #get what to plot via user options
    plot_opts <- plot_options_server("plot_opts")

  #keep track of dates
    plot_dates <- weekly_range_server("date_nav", sondeproj, period_view, dates, p_length, data_ver)

  #filter data to plot
    plot_data <- reactive({
      req(sondeproj(), plot_dates())
      sondeproj()$data %>% dplyr::filter(.data$Date >= plot_dates()[1], .data$Date <= plot_dates()[2])
    })

    corr_data <- reactive({
      req(sondeproj(), plot_dates(), eq_info(), coef_vals())
      corr_fun <- eq_info()$fun

      sondeproj()$data %>% dplyr::filter(.data$Date >= plot_dates()[1], .data$Date <= plot_dates()[2]) %>%
        mutate(fDOM_QSU_T = .data$fDOM_QSU / (1 + input$rho*(.data$Temp_C - 25)),
               fDOM_QSU_Tt = corr_fun(.data$fDOM_QSU_T, .data$Turbidity_FNU, coef_vals()))
    })

  #create plotly plot
    plot_obj <- reactive({
      req(plot_data())

      #use function to plot sonde data
      p <- plot_sonde(data = plot_data(), y_var="fDOM_QSU", proj = sondeproj(), opts=plot_opts())
      #add corrected fDOM
      dat <- corr_data() %>% arrange(.data$DateTime_rd)
      p <- p %>% add_trace(data= dat, x=~DateTime_rd, y=~fDOM_QSU_Tt, type="scatter", mode="lines",
                               name = "Changed", line = list(color = "darkred"), yaxis="y", inherit = FALSE)

      #return plot
      p
    })

    #save to export
    output$fdom_plot <- plotly::renderPlotly({
      validate(
        need(nrow(plot_data()) > 0,
             "No data available for the selected date range."))

      # convert to plotly
      p <- plot_obj()
      toWebGL(p)
    })

    observeEvent(input$modules, {
      req(input$modules == "step-9")

      plotlyProxy("fdom_plot", session) %>%
        plotlyProxyInvoke("resize")
    })

  #create edit object
    edit <- reactive({
      newdata <- sondeproj()$data

      #replace with fDOM corrected data
      corr_fun <- eq_info()$fun

      newdata <- newdata %>%
        mutate(fDOM_QSU_T = .data$fDOM_QSU / (1 + input$rho*(.data$Temp_C - 25)),
               fDOM_QSU = corr_fun(.data$fDOM_QSU_T, .data$Turbidity_FNU, coef_vals())) %>%
          select(-"fDOM_QSU_T")

      #create note
      nice_method <- switch(input$method,
                           "inverse_poly" = "Inverse Polynomial",
                           "1p_exponential" ="Exponential (1-parameter)",
                           "2p_exponential" = "Exponential (2-parameter)",
                           "5p_exponential" = "Exponential (5-parameter)")
      nice_coeff <- paste(paste0(names(coef_vals()), " = ", coef_vals()), collapse = ", ")
      method_note <- ifelse(input$method == "none", "", paste0(" and turbidity using the ", nice_method, " method (", nice_coeff, ")"))

      #make edit list
      list(
        data = newdata,
        rows = rep(TRUE, nrow(newdata)),
        y_var = "fDOM_QSU",
        step = "fDOM correction",
        note = paste0("fDOM corrected for temperature (\U03C1 = ", input$rho, ")", method_note),
        flag = "CHG03",
        changetype = "flag_chg"
      )

    })

  #flagging module
    apply_edit_server("apply_limits", sondeproj, edit)

  #export plot so we can check it
    exportTestValues(
      plot_obj = plot_obj(),
      changelog = sondeproj()$changelog)

  })}
