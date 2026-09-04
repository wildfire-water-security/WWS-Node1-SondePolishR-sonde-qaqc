##TODO: could specify limits from instrument as default like Jake does (ask Jake for limits he uses)

#' @export
#' @rdname limits
limits_UI <- function(id){
  ns <- NS(id) #line to make module work
  tagList(
    sidebarLayout(
        sidebarPanel(
          accordion(
            open = c("Select Parameters", "Set Physical Limits"),
            accordion_panel(
              "Select Parameters",
              update_parms_UI(ns("update_parms")),
              update_parms_UI(ns("update_parms"), input_id = "y2_var", text = "Select Second Parameter to Plot:")
            ),
            accordion_panel(
              "Set Physical Limits",
              div(style="margin-bottom: 8px; font-size:10px",
                  "Default Limits based on YSI EXO Ranges"),
              fluidRow(numericInput(ns("max"),
                                    HTML("<b>Maximum</b> Physical Limit"), value = NULL),
                       numericInput(ns("min"),
                                    HTML("<b>Minimum</b> Physical Limit"),value = NULL)),

              input_switch(ns("rm_flags"), "Hide Flagged Data")
            ),
            accordion_panel(
              "Save Edits",
              apply_edit_UI(ns("apply_limits"), edit_type = "remove", note="Highlighted points will be removed")
            ),
            accordion_panel(
              "Date Ranges",
              weekly_range_sidebar_UI(ns("date_nav")),
            ),
            accordion_panel(
              "Plotting Options",
              plot_options_UI(ns("plot_opts"))
            ))
      ),
      mainPanel(
        main_plot_UI(ns("limit_plot")),

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
#' @param id An ID string passed to shiny::NS(), used for namespacing UI inputs/outputs.
#' @param sondeproj A `reactiveVal` holding the current dataset.
#' @param data_ver A `reactiveVal` holding a number used to track when new data is added to trigger resets.
#' @param y_var Y-variable to plot on the y-axis.
#' @param view_state A `reactiveVal` holding a list of items specifying the view state:
#'  - abs_dates: The absolute range of dates within the dataset
#'  - dates: The range of dates being viewed via the date selector
#'  - period_view: Logical if the period view is being used
#'  - period_length: Length of period view
#'  - period_n: The period number to view.
#' @param username A `reactiveVal` holding the name of the analyst for the changelog
#' @export
#' @rdname limits
limits_server <- function(id, sondeproj, data_ver, y_var,view_state, username){
  moduleServer(id, function(input, output, session){
    #keep track of second y_variable
    y2_var <- reactiveVal()
    plot_exist <- reactiveVal() #keeps warning about missing plot

  #get column names after file upload (dynamic)
    update_parms_server("update_parms", sondeproj, data_ver, y_var, choices_fun = nice_yvar)
    update_parms_server("update_parms", sondeproj, data_ver, y2_var, input_id= "y2_var", choices_fun = nice_yvar)

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
    plot_dates <- weekly_range_server("date_nav", sondeproj, data_ver, view_state)

  #filter data to plot
    plot_data <- reactive({
      req(sondeproj(), plot_dates())

      sondeproj()$data %>% dplyr::filter(.data$Date >= plot_dates()[1], .data$Date <= plot_dates()[2])

    })

  #create plotly plot
    plot_obj <- reactive({
      req(y_var(),y2_var(), plot_data())
      if(y2_var() == "none"){y2 <- NULL}else{y2 <- y2_var()}

      #if we want to filter out flagged points, filter before plotting
      if(input$rm_flags){
        filter_data <- plot_data() %>% dplyr::filter(.data[[y_var()]] >= input$min & .data[[y_var()]] <= input$max)
      }else{
        filter_data <- plot_data()
        flag_data <- plot_data() %>% dplyr::filter(.data[[y_var()]] < input$min | .data[[y_var()]] > input$max)
      }


      #use function to plot sonde data
      p <- plot_sonde(data = filter_data, y_var=y_var(), y2_var = y2, proj = sondeproj(), opts=plot_opts(),
                      source = "limit_plot")
      #color points outside limits as red
      if(!input$rm_flags){
        y <- y_var()
        p <- p %>% add_trace(data= flag_data, x=~DateTime_rd, y=as.formula(paste0("~`", y, "`")), type="scatter", mode="markers",
                             name = "Flagged", marker = list(color = "darkred"), yaxis="y2", inherit = FALSE)
      }

      #add limits (guarded if OOW periods are also plotted)
      new_shapes <- lapply(c(input$min, input$max), function(x){
        list(type = "line",
             x0 = 0, x1 = 1, xref = "paper", # Spans the full width of the plot
             y0 = x, y1 = x, yref = "y2",
             line = list(color = "darkred", width = 2, dash = "dash"))
      })

        trace <- which(sapply(p$x$layoutAttrs, names) == "shapes")
        if(length(trace) > 0){
          exist_shapes <- p$x$layoutAttrs[[trace]]$shapes
          p$x$layoutAttrs[[trace]]$shapes <- c(exist_shapes, new_shapes)
        }else{
          p <- p %>% layout(shapes = new_shapes)
        }

      #return plot
      p
    })

    #save to export
    main_plot_server("limit_plot", data_ver, sondeproj, plot_obj, plot_data, y_var, plot_exist=plot_exist,
                     startmin=reactive(input$min), startmax=reactive(input$max))

    #redraw when back on module to prevent weird drawing issues
    observeEvent(input$modules, {
      req(input$modules == "step-5")

      plotlyProxy("limit_plot", session) %>%
        plotlyProxyInvoke("resize")
    })

  #create edit object
    edit <- reactive({
      newdata <- sondeproj()$data

      #get filtered data
      index <- newdata %>% filter(.data[[y_var()]] < input$min | .data[[y_var()]] > input$max) %>%
        dplyr::filter(.data$Date >= plot_dates()[1], .data$Date <= plot_dates()[2]) %>% pull(.data$Index)
      newdata[[y_var()]][newdata$Index %in% index] <- NA

      #make edit list
      list(
        data = newdata,
        rows = index,
        y_var = y_var(),
        step = "absolute limits",
        note = paste0("Data removed based on absolute limits of ", input$min, " and ", input$max),
        flag = "RM02"
      )

    })

  #flagging module
    apply_edit_server("apply_limits", sondeproj, edit, username)

  #export plot so we can check it
    exportTestValues(
      plot_obj = plot_obj(),
      changelog = sondeproj()$changelog)

  })}
