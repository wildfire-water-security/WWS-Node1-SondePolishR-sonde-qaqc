##step 2 of the app: exploratory plotting with plotly

# UI Function
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
        tags$h4("Select Data Version"),
        selectInput(NS(id, "version"), "Select Data Version",
                       choices = NULL),

        #adjust plotting to explore data
        tags$h4("Adjust Plotting"),

        #select parameter to plot
        update_parms_UI(ns("update_parms")),

        #select date range to view with week selectors
        dateRangeInput(NS(id, "date_range"), label="Select Data Date Range"),

        fluidRow(
          column(4,input_switch(NS(id, "week_view"), "View Data Weekly")),
          column(4,actionButton(NS(id,"prev_week"), "Previous")),
          column(4,actionButton(NS(id,"next_week"), "Next"))
          )

      ),

      mainPanel(
        #add plot
        plotlyOutput(NS(id,"exp_plot")),

        #visualize data log
        DT::DTOutput(NS(id, "log_table"))
      ))
  )
}

# Server Function
explore_data_server <- function(id, df){
  moduleServer(id, function(input, output, session){
    #get column names after file upload (dynamic)
    y_var <- update_parms_server("update_parms", df, choices_fun = nice_yvar)


  #set df to hold the thing that's being plotted
    plot_df <- reactiveVal()

    # initialize
    observeEvent(df(), {plot_df(df())}, once = TRUE)

    observeEvent(input$log_table_rows_selected, {
      req(input$log_table_rows_selected)

      new_data <- get_data()[[input$log_table_rows_selected]]
      plot_df(new_data)
    })

  #dealing with dates
    #update date range
    observeEvent(plot_df(),{
      req(df())
      updateDateRangeInput(inputId = "date_range", start = min(plot_df()$Date_MM_DD_YYYY), end=max(plot_df()$Date_MM_DD_YYYY))

      # updateSelectInput(
      #   session,
      #   "version",
      #   choices = list(),
      #   selected = choices[[1]]
      # )
    })

    #switch to the first week if week view is flipped
    observeEvent(input$week_view,
                 if(input$week_view){
                   req(df())

                   start <- min(plot_df()$Date_MM_DD_YYYY)
                   end <- start + lubridate::weeks(1)
                   updateDateRangeInput(inputId = "date_range", start = start, end=end)
                 }else{
                   req(df())

                   updateDateRangeInput(inputId = "date_range", start = min(plot_df()$Date_MM_DD_YYYY), end=max(plot_df()$Date_MM_DD_YYYY))
                 })

    #if prev week is hit, go to previous week
      observeEvent(input$prev_week,{
        req(df())

        rng <- input$date_range
        start <- if(rng[1] <= min(plot_df()$Date_MM_DD_YYYY)){as.Date(min(plot_df()$Date_MM_DD_YYYY))}else{rng[1] - 7} #don't change if already at start
        end <- start + 7
        updateDateRangeInput(inputId = "date_range", start = start, end=end)
      })


    #if next week is hit, go to next week
      observeEvent(input$next_week,{
        req(df())

        rng <- input$date_range
        start <- if(rng[2] >= max(plot_df()$Date_MM_DD_YYYY)){rng[1]}else{rng[1] + 7}        # don't change if already at end
        end <- start + 7
        updateDateRangeInput(inputId = "date_range", start = start, end=end)
      })


    #create plot
    output$exp_plot <- renderPlotly({
      req(df(), y_var())
      ggplot2::ggplot(plot_df(), ggplot2::aes(x=.data$DateTime, y=.data[[y_var()]])) +
        ggplot2::geom_point() + ggplot2::scale_x_datetime(limits = as.POSIXct(input$date_range))
    })

    #create log table
    output$log_table <- DT::renderDT({
      req(df())

      DT::datatable(
        get_log(),
        selection = list(mode = "single")
      )})

  })
}
