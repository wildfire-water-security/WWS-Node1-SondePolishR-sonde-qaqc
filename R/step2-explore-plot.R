##step 2 of the app: exploratory plotting with plotly

# UI Function
#' @export
#' @rdname explore-data
explore_data_UI <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(

        update_parms_UI(ns("update_parms")),

        HTML("<hr>"),
        #date options
        weekly_range_sidebar_UI(ns("weekly_range")),

        HTML("<hr>"),

        #plotting options
        plot_options_UI(ns("plot_opts")),

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
        #add buttons to navigate date
        weekly_range_buttons_UI(ns("weekly_range")),
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
    }else if(input$table_opt == "Change Log"){
      req(sondeproj()$changelog)
      sondeproj()$changelog %>% mutate(parameter = factor(.data$parameter, levels=unique(.data$parameter)))
    }else if(input$table_opt == "Calibration Check"){
      req(sondeproj()$calcheck, y_var())
      sondeproj()$calcheck %>%
        dplyr::select(-c("Resident_Probe_Serial","Check_Probe_Serial", "Site_Code")) %>%
        filter(.data$Parameter == y_var())
    }
  })

    output$log_table <- DT::renderDT({
      DT::datatable(
       tab(),
       selection = list(mode = "single"),
       filter = "top"
      )})

  #get what to plot via user options
    plot_opts <- plot_options_server("plot_opts")

  #get column names after file upload (dynamic)
    y_var <- update_parms_server("update_parms", sondeproj, choices_fun = nice_yvar)

  #keep track of dates
    dates <- weekly_range_server(
      "weekly_range",
      min_date = reactive({req(sondeproj())
                          min(sondeproj()$data$Date, na.rm = TRUE)}),
      max_date = reactive({req(sondeproj())
                          max(sondeproj()$data$Date, na.rm = TRUE)}))

    #filter data
    plot_data <- reactive({
      req(sondeproj(), dates())

      #alter data if change log rows are selected
      if(!is.null(input$log_table_rows_selected)){
        #get the diffs to apply
          row <- input$log_table_rows_selected
          #only get diffs if not current data
          if(row < nrow(sondeproj()$changelog)){
            diff_list <- sondeproj()$changelog$diff_name[(row+1):nrow(sondeproj()$changelog)]
            diff_list <- diff_list[grepl("^dd", diff_list)]
            diffs <- sondeproj()$diffs[names(sondeproj()$diffs) %in% diff_list]

            data_ver <- apply_diff(sondeproj()$data, diffs, invert=TRUE)
          }else{
            data_ver <- sondeproj()$data

          }

      }else{
        data_ver <- sondeproj()$data
      }

       data_ver %>% dplyr::filter(.data$Date >= dates()[1], .data$Date <= dates()[2])
    })

    #create plotly plot
    plot_obj <- reactive({
      req(y_var(), plot_data())

      #use function to plot sonde data
      plot_sonde(plot_data(), y_var(), plot_opts(),sondeproj()$fieldform, sondeproj()$calcheck)
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
