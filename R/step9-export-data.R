
#' @export
#' @rdname export-data
export_UI <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    bslib::page_fluid(
      bslib::layout_columns(
        col_widths = c(7, 5),
       #exporting data
        bslib::card(
          height = "700px",
          bslib::card_header("Export Data"),
          bslib::layout_columns(col_widths = c(3,9),
                                div(
                                  style = "padding-right: 1rem;",
                                update_parms_UI(ns("update_parms"), text="Select Parameter to View:"),
                                dateRangeInput(ns("dates"),"Date Range"),
                                radioButtons(ns("frequency"),"Export Frequency",
                                             choices = c("Hourly" = "hour","Daily" = "day",
                                                         "Weekly (7-day)" = "week","Monthly" = "month","Annual" = "year")),
                                conditionalPanel(
                                  condition = sprintf("input['%s'] != 'interval'",ns("frequency")),
                                  radioButtons(ns("summary_method"),"Summary Method",
                                               choices = c("Mean" = "mean","Median" = "median","Maximum" = "max","Minimum" = "min")))
                                ),
                                plotlyOutput(ns("export_plot"))),
          tags$br(),
          save_path_UI(ns("save_data"),filetype = ".csv", button_label = "Export Data")
        ),

      #side part with metadata and exporting
        bslib::layout_column_wrap(
          width = 1,
          bslib::card(
            bslib::card_header("Export Metadata"),
            bslib::card_body(
            class = "d-flex flex-column justify-content-center",

            radioButtons(
              ns("meta_opts"),NULL,choices = c(
                "Duplicate Notes" = "dups",
                "Missing Data Notes" = "gaps",
                "Change Log" = "changelog",
                "Precipitation" = "precip")),

            save_path_UI(ns("save_meta"),filetype = ".csv", button_label = "Export Metadata"))

          ),
          bslib::card(
            bslib::card_header("Save Sonde Project"),
            bslib::card_body(
              class = "d-flex flex-column justify-content-center",
              save_path_UI(ns("save_proj"),filetype = ".RDS", button_label = "Export Project"))
          )
        )
      )
    )
  )
}


#' Export data and metadata
#'
#' Save to file the corrected data and metadata including summaries of the data.
#'
#' @keywords internal
#'
#' @param id An ID string passed to shiny::NS(), used for namespacing UI inputs/outputs.
#' @param sondeproj A `reactiveVal` holding the current dataset.
#' @param data_ver A `reactiveVal` holding a number used to track when new data is added to trigger resets.
#' @param y_var Y-variable to plot on the y-axis.
#'
#' @export
#' @rdname export-data
export_server <- function(id, sondeproj, data_ver, y_var){
  moduleServer(id, function(input, output, session){

  #selecting parameter to view
  update_parms_server("update_parms", sondeproj, data_ver, y_var, choices_fun = nice_yvar)

  #update range to view when data loaded
    observeEvent(sondeproj(), {
      updateDateRangeInput(session, "dates", start = min(sondeproj()$data$Date, na.rm = TRUE),
                           end = max(sondeproj()$data$Date, na.rm = TRUE))})

  ## EXPORTING DATA TO CSV -----
    #create plot
      #filter data by requested range
        plot_data <- reactive({
          req(sum_data())
          sum_data() %>% mutate(Date = as.Date(.data$DateTime_rd)) %>% dplyr::filter(.data$Date >= input$dates[1], .data$Date <= input$dates[2])
        })

      #summarized data with flags added for export and plotting
      sum_data <- reactive({
        req(sondeproj())

        export_data <- combine_flags(sondeproj())

        #summarize
        if(input$frequency != "interval"){
          frequency <- switch(input$frequency,
                              "hour" = lubridate::period(1, "hour"),
                              "day" = lubridate::period(1, "day"),
                              "week" = lubridate::period(7, "day"),
                              "month" = lubridate::period(1, "month"),
                              "year" = lubridate::period(1, "year"))
          summarize_data(export_data, frequency, input$summary_method)
        }else{
          export_data
        }

      })

      #create plot
      plot_obj <- reactive({
        req(y_var(), plot_data())

        #use function to plot sonde data
        plot_sonde(data=plot_data(), y_var = y_var(), y2_var = NULL, opts=list(points=FALSE,line=TRUE,files=FALSE,
                                              oow=FALSE,calcheck=FALSE))
      })

      #save to export
      output$export_plot <- plotly::renderPlotly({
        validate(
          need(nrow(plot_data()) > 0,
               "No data available for the selected date range."))

        # convert to plotly
        p <- plot_obj()
        #toWebGL(p)
      })

    #when data loaded get interval of data
      observeEvent(data_ver(),{
        req(sondeproj())
        interval <- paste0(get_interval(sondeproj()$data), "-minutes")
        updateRadioButtons(session,"frequency",
                           choices=c(setNames("interval", interval), "Hourly" = "hour","Daily" = "day", "Weekly (7-day)" = "week",
                                               "Monthly" = "month", "Annual" = "year"))
      })

    #data save path and saving data
      data_path <- save_path_server("save_data", sum_data)


 ## EXPORTING METADATA ------
    metadata <- reactive({
      req(input$meta_opts)

      switch(input$meta_opts,
             "dups" = sondeproj()$duplicates,
             "gaps" = sondeproj()$data_gaps,
             "changelog" = sondeproj()$changelog,
             "precip" = sondeproj()$precip)
    })

    meta_path <- save_path_server("save_meta", metadata)

## EXPORTING PROJECT -----
    proj_path <- save_path_server("save_proj", sondeproj)

 ## STUFF FOR TESTING ------

  #export plot so we can check it
    exportTestValues(
      plot_obj = plot_obj())

  })}
