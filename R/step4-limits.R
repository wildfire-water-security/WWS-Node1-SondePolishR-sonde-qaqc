##TODO: could specify limits from instrument as default like Jake does (ask Jake for limits he uses)

#' @export
#' @rdname limits
limits_UI <- function(id){
  ns <- NS(id) #line to make module work
  tagList(
    sidebarLayout(
      sidebarPanel(
        update_parms_UI(ns("update_parms")),

      #select physical limits
        tags$h5("Set Physical Limits"),
      div(style="margin-bottom: 8px; font-size:10px",
          "Default Limits based on YSI EXO Ranges"),
            numericInput(ns("max"),
              HTML("<b>Maximum</b> Physical Limit"), value = NULL),
            numericInput(ns("min"),
                   HTML("<b>Minimum</b> Physical Limit"),value = NULL),

            input_switch(ns("rm_flags"), "Hide Flagged Data"),
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
        plotlyOutput(ns("limit_plot"), height="60%"),
        #add buttons to navigate date
        weekly_range_buttons_UI(ns("date_nav")),
      ))


  )}


#' Flagging data the is outside specified limits
#'
#' There are certain thresholds for some of the sonde parameters that aren't physical possible (i.e, water temperature above 100 deg C).
#' This module visualizes those limits and flags data outside specified limits.
#'
#' @keywords internal
#'
#' @param id An ID string passed to shiny::NS(), used for namespacing UI inputs/outputs.
#' @param sondeproj A `reactiveVal` holding the current dataset.
#' @param data_ver A `reactiveVal` holding a number used to track when new data is added to trigger resets.
#' @param y_var Y-variable to plot on the y-axis.
#'
#' @export
#' @rdname limits
limits_server <- function(id, sondeproj, data_ver, y_var){
  moduleServer(id, function(input, output, session){

  #get column names after file upload (dynamic)
    update_parms_server("update_parms", sondeproj, data_ver, y_var, choices_fun = nice_yvar)

  #update limits in UI
    observeEvent(y_var(), {
      req(y_var())

      #update default limits based on manufacturer specifications
      rng <- switch(y_var(),
                    "fDOM_QSU" = c(0,300),
                    "ODO_mg_L" = c(0,50),
                    "pH"= c(0, 14),
                    "SpCond_uS_cm" = c(0, 3000),
                    "Temp_C" = c(-5, 50),
                    "Turbidity_FNU" = c(0, 4000))

      updateNumericInput(session,"min", value = rng[1])
      updateNumericInput(session,"max", value = rng[2])
    })

  #get what to plot via user options
    plot_opts <- plot_options_server("plot_opts")

  #keep track of dates
    dates <- weekly_range_server(
      "date_nav",
      min_date = reactive({req(sondeproj())
        min(sondeproj()$data$Date, na.rm = TRUE)}),
      max_date = reactive({req(sondeproj())
        max(sondeproj()$data$Date, na.rm = TRUE)}))


  #filter data to plot
    plot_data <- reactive({
      req(sondeproj(), dates())

      sondeproj()$data %>% dplyr::filter(.data$Date >= dates()[1], .data$Date <= dates()[2])

    })

  #create plotly plot
    plot_obj <- reactive({
      req(y_var(), plot_data())

      #if we want to filter out flagged points, filter before plotting
      if(input$rm_flags){
        filter_data <- plot_data() %>% dplyr::filter(.data[[y_var()]] >= input$min & .data[[y_var()]] <= input$max)
      }else{
        filter_data <- plot_data()
        flag_data <- plot_data() %>% dplyr::filter(.data[[y_var()]] < input$min | .data[[y_var()]] > input$max)
      }

      #use function to plot sonde data
      p <- plot_sonde(filter_data, y_var(), plot_opts(),sondeproj()$fieldform, sondeproj()$calcheck)

      #add limits
      p <- p + ggplot2::geom_hline(yintercept = input$min, color="darkred") +
        ggplot2::geom_hline(yintercept = input$max, color="darkred")

      #color points outside limits as red
      if(!input$rm_flags){
        p <- p + ggplot2::geom_point(data=flag_data, aes(x = .data$DateTime_rd,y = .data[[y_var()]]), color = "darkred")
      }

      #return plot
      p
    })

    #save to export
    output$limit_plot <- plotly::renderPlotly({
      validate(
        need(
          nrow(plot_data()) > 0,
          "No data available for the selected date range."
        )
      )

      # convert to plotly
      plotly::ggplotly(plot_obj(), dynamicTicks = TRUE)
    })


  #create edit object
    edit <- reactive({
      newdata <- sondeproj()$data

      #get filtered data
      setna <- newdata[[y_var()]] < input$min | newdata[[y_var()]] > input$max
      setna[is.na(setna)] <- FALSE #if NA, will return NA, we want to make FALSE
      newdata[[y_var()]][setna] <- NA

      #make edit list
      list(
        data = newdata,
        rows = setna,
        y_var = y_var(),
        step = "absolute limits",
        note = paste0("Data removed based on absolute limits of ", input$min, " and ", input$max),
        flag = "RM02",
        changetype = "flag_rm"
      )

    })

  #flagging module
    apply_edit_server("apply_limits", sondeproj, edit)

  #export plot so we can check it
    exportTestValues(
      plot_obj = plot_obj(),
      changelog = sondeproj()$changelog)

  })}
