##step 2 of the app: exploratory plotting with plotly

# UI Function
#' @export
#' @rdname explore-data
explore_data_UI <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(

        SondePolishR::update_parms_UI(ns("update_parms")),

        HTML("<hr>"),
        #date options
        dateRangeInput(ns("dates"),"Date Range"),
        input_switch(ns("week_view"), "View Data Weekly", value=FALSE),

        HTML("<hr>"),

        #plotting options
        tags$h4("Plot Options"),
        checkboxInput( ns("points"), "Plot points",value = TRUE),

        checkboxInput( ns("line"), "Plot line",value = TRUE),

        checkboxInput( ns("files"),"Color points by file",value = FALSE),

        checkboxInput(ns("oow"),"Show out-of-water periods",value = FALSE),

        checkboxInput(ns("calcheck"),"Show calibration checks",value = FALSE),

        HTML("<hr>"),
        tags$h4("Table Options"),

        selectInput(
          ns("table_opt"),
          "Select Table to View:",
          choices = c("Change Log", "Field Form", "Calibration Check")
        )



      ),

      mainPanel(
        #add plot
        plotlyOutput(NS(id,"plot")),

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
#' @param sondeproj A `reactiveVal` holding the current dataset.
#'
#' @md
#' @keywords internal
#' @export
#' @rdname explore-data
#' @returns Invisible NULL
#'
explore_data_server <- function(id, sondeproj){
  moduleServer(id, function(input, output, session){

  #create log table
  tab <- reactive({
    req(sondeproj())

    if(input$table_opt == "Field Form"){
      req(sondeproj()$fieldform)
      sondeproj()$fieldform %>% dplyr::select("Date", "Removal_Time_PST",
                                                     "Return_Time_PST",
                                                     "Remove_Period", "Notes")
    }
    if(input$table_opt == "Change Log"){
      req(sondeproj()$changelog)
      sondeproj()$changelog
    }
    if(input$table_opt == "Calibration Check"){
      req(sondeproj()$calcheck, y_var())
      sondeproj()$calcheck %>%
        dplyr::select(-c("Resident_Probe_Serial","Check_Probe_Serial", "Site_Code")) %>%
        filter(.data$Parameter == y_var())
    }
  })

    output$log_table <- DT::renderDT({
      DT::datatable(
       tab()
      )})

    #set up dates
    date_bounds <- reactiveVal(NULL)

    #get column names after file upload (dynamic)
    y_var <- SondePolishR::update_parms_server("update_parms", sondeproj, choices_fun = nice_yvar)

    #update date UI when project changes
    observe({
      req(sondeproj())
      dat <- sondeproj()$data

      date_bounds(list(
        min = min(dat$Date, na.rm = TRUE),
        max = max(dat$Date, na.rm = TRUE)
      ))

      updateDateRangeInput(
        session,"dates",
        start = date_bounds()$min,
        end   = date_bounds()$max
      )
    })

    #adjust date bounds
    observeEvent(input$week_view, {
      req(date_bounds())

      if(input$week_view) {

        start <- date_bounds()$min
        end   <- start + 7

        updateDateRangeInput(
          session,"dates",
          start = start, end = end)

        updateActionButton(inputId = "next_week", disabled = FALSE)
        updateActionButton(inputId = "prev_week", disabled = FALSE)
      }else{
        updateDateRangeInput(
          session,"dates",
          start = date_bounds()$min,
          end   = date_bounds()$max)

        updateActionButton(inputId = "next_week", disabled = TRUE)
        updateActionButton(inputId = "prev_week", disabled = TRUE)
      }
    })

    #update date range when buttons for next and previous clicked
    shift_week <- function(direction = c("prev", "next")) {
      direction <- match.arg(direction)
      req(input$dates, date_bounds())

      step <- if(direction == "prev") -7 else 7
      start <- input$dates[1] + step

      start <- max(start, date_bounds()$min)
      start <- min(start, date_bounds()$max - 7)

      end <- start + 7

      updateDateRangeInput(session,"dates",
                           start = start,end = end)

    }

    observeEvent(input$next_week, shift_week("next"))
    observeEvent(input$prev_week, shift_week("prev"))

    #filter data
    plot_data <- reactive({
      req(sondeproj())
      req(input$dates)

      sondeproj()$data %>% dplyr::filter(.data$Date >= input$dates[1], .data$Date <= input$dates[2])
    })

    #oow periods
    oow_data <- reactive({
      req(sondeproj())
      req(sondeproj()$fieldform)
      get_oow(sondeproj()$fieldform)
    })

    #get cal data
    cal_data <- reactive({
      req(input$calcheck)
      req(sondeproj())
      req(sondeproj()$calcheck)

      #use ff data to determine when cal data likely was
      mean_visit <- oow_data() %>% rowwise() %>%
        mutate(avg_time = mean(c(.data$start, .data$end)),date = as.Date(.data$avg_time))

      sondeproj()$calcheck %>%
        dplyr::left_join(mean_visit %>% select("date", "avg_time"),
                         join_by("Date" == "date")) %>%
        filter(.data$Parameter == y_var()) %>%
        pivot_longer(c("Resident_Value", "Check_Value"),names_to = "type",values_to = "value")
    })

    #create plotly plot
    plot_obj <- reactive({
      req(y_var(), plot_data())

      #base plot
      p <- ggplot(plot_data(), aes(x = .data$DateTime_rd,y = .data[[y_var()]])) +
        labs(x="Date", y=y_var())

      #add points (colored or not)
      if(input$points && input$files){
        p <- p +  geom_point(aes(color = .data$FileName),na.rm = TRUE, size = 1.5) +
          labs(color = "File Name")
      }

      if(input$points && !input$files){
        p <- p + geom_point(na.rm = TRUE, size = 1.5)
      }

      #add a line
      if(input$line){
        p <- p + geom_line(na.rm = TRUE, linewidth = 0.3)
      }

      #plot oow periods
      if(input$oow && !is.null(sondeproj()$fieldform)){
        p <- p + geom_rect(data = oow_data(),
                           aes(xmin = .data$start, xmax = .data$end,
                               ymin = min(plot_data()[[y_var()]], na.rm = TRUE),
                               ymax = max(plot_data()[[y_var()]], na.rm = TRUE)),
                           inherit.aes = FALSE,fill = "darkred",color = "darkred",
                           alpha = 0.4)
      }

      #plot cal check
      if(input$calcheck && !is.null(sondeproj()$calcheck)){
        #plot one at at time because color scales are a poop
        p <- p + geom_point(data = cal_data(),
                            aes(x = .data$avg_time, y = .data$value, shape = .data$type),
                            color = "darkred", size = 2) +
          scale_shape_manual(values = c("Resident_Value" = 17,
                                        "Check_Value" = 15),name = "Calibration Check")
      }

      p
    })

    #save to export
    output$plot <- plotly::renderPlotly({
      # convert to plotly
      plotly::ggplotly(plot_obj(), dynamicTicks = TRUE)
    })

    #export plot so we can check it
    exportTestValues(
      plot_obj = plot_obj(),
      table = tab())

  })
}
