##step 2 of the app: exploratory plotting with plotly

# UI Function
#' @export
#' @rdname explore-data
explore_data_UI <- function(id){
  ns <- NS(id) #line to make module work

  tagList(
    sidebarLayout(
      sidebarPanel(
        #give directions
        tags$h4("To Explore Data:"),
        HTML("1. Click and drag to zoom in<br>
                   2. Double click to zoom out<br>
                   3. Hover to see values<br><hr>"),

        #allow moving between data versions
        # tags$h4("Select Data Version"),
        # selectInput(NS(id, "version"), "Select Data Version",
        #                choices = NULL),

        #adjust plotting to explore data
        tags$h4("Adjust Plotting"),

        #select parameter to plot
        SondePolishR::update_parms_UI(ns("update_parms")),

        #select date range to view with week selectors
        dateRangeInput(NS(id, "date_range"), label="Select Data Date Range", start = "9999-12-31", end= "9999-12-31"),

        #add switch for weekly mode
        column(4,input_switch(NS(id, "week_view"), "View Data Weekly")),

        #add checkboxes to remove flagged data by flag name
        checkboxGroupInput(NS(id, "rm_flags"), tags$h6("Remove the Following Flagged Data:"))

      ),

      mainPanel(
        #add plot
        plotlyOutput(NS(id,"exp_plot")),

        fluidRow(column(2,actionButton(NS(id,"prev_week"), "Previous Week", disabled = TRUE)),
                 column(8, HTML("")),
                 column(2,actionButton(NS(id,"next_week"), "Next Week", disabled = TRUE))),
        #adding some space after buttons
        HTML("<br><br>"),
        #visualize data log
        DT::DTOutput(NS(id, "log_table"))
      ))
  )
}

# Server Function
#' Visualize dataset by analyte
#'
#' Plots loaded dataset with options to explore full dataset or view the data in weekly sections.
#' Plots are created using `plotly` so they are interactive. Module also allows the user to view
#' the changes over the dataset versions via row selection in a table via the `log`.
#'
#' @param id An ID string passed to shiny::NS(), used for namespacing UI inputs/outputs.
#' @param sdata A `reactiveVal` holding the current dataset.
#' @param log A `reactiveVal` holding the change log.
#' @md
#' @keywords internal
#' @export
#' @rdname explore-data
#' @returns Invisible NULL
#'
explore_data_server <- function(id, sdata, log){
  moduleServer(id, function(input, output, session){

  #get column names after file upload (dynamic)
    y_var <- SondePolishR::update_parms_server("update_parms", sdata, choices_fun = nice_yvar)

  #create log table
    output$log_table <- DT::renderDT({
      req(sdata())

      DT::datatable(
        log(),
        selection = list(mode = "single")
      )})

  #get initial data for plotting
    base_data <- reactive({
      req(sdata())

      if (!is.null(input$log_table_rows_selected) &&
          length(input$log_table_rows_selected) == 1) {

        get_data()[[input$log_table_rows_selected]]

      } else {
        sdata()
      }
    })

  #adjust for hidden flags
    plot_data <- reactive({
      dat <- base_data()

      if (!is.null(input$rm_flags) && length(input$rm_flags) > 0) {
        dat <- remove_flagged(dat, input$rm_flags)
      }

      dat
    })

  #update flags to potentially remove
    observeEvent(list(sdata(), log()), {
      #update flags to remove
      flags <- unique(log()$step[log()$n_changed > 0])
      updateCheckboxGroupInput(session, "rm_flags", choices = flags)
    })

  #create plot
    plot_obj <- reactive({
      req(plot_data(), y_var())
      ggplot2::ggplot(plot_data() %>% dplyr::filter(.data$Date_MM_DD_YYYY >= input$date_range[1] & .data$Date_MM_DD_YYYY <= input$date_range[2]),
                      ggplot2::aes(x=.data$DateTime, y=.data[[y_var()]])) +
        ggplot2::geom_point(na.rm = TRUE)
    })
    output$exp_plot <- renderPlotly({
      plot_obj()
    })

  #dealing with dates
    #get absolute min and max of dates
    date_bounds <- reactive({
      req(plot_data())
      rng <- as.Date(range(plot_data()$Date_MM_DD_YYYY, na.rm = TRUE), tz=attr(plot_data()$Date_MM_DD_YYYY, "tz"))
      list(min = rng[1], max = rng[2])
    })

    #set initial range
    observeEvent(sdata(),{
      req(date_bounds())
      updateDateRangeInput(inputId = "date_range", start = date_bounds()$min, end = date_bounds()$max)
    })

    #change default dates when week_view flipped on or off
    observeEvent(input$week_view, {
      req(plot_data(), date_bounds())

      if(input$week_view) {
        start <- date_bounds()$min
        end   <- start + 7

        updateDateRangeInput(inputId = "date_range", start = start, end = end)

        updateActionButton(inputId = "next_week", disabled = FALSE)
        updateActionButton(inputId = "prev_week", disabled = FALSE)
      }else{
        updateDateRangeInput(inputId = "date_range", start = date_bounds()$min, end = date_bounds()$max)

        updateActionButton(inputId = "next_week", disabled = TRUE)
        updateActionButton(inputId = "prev_week", disabled = TRUE)
      }
    })

    #update date range when buttons for next and previous clicked
    shift_week <- function(direction = c("prev", "next")) {
      req(input$date_range, date_bounds())

      step <- if (direction == "prev") -7 else 7

      start <- input$date_range[1] + step
      start <- max(start, date_bounds()$min)
      start <- min(start, date_bounds()$max - 7)

      end <- start + 7
      updateDateRangeInput(inputId = "date_range", start = start, end = end)

    }

    observeEvent(input$prev_week, {
      req(sdata())
      shift_week("prev")
    })

    observeEvent(input$next_week, {
      req(sdata())
      shift_week("next")
    })

  #export plot so we can check it
    exportTestValues(
      plot_obj = plot_obj(),
      log_table = get_log()
    )

  })
}
