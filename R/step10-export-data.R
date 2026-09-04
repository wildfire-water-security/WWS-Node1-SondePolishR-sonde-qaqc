
#' @export
#' @rdname export-data
export_UI <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    bslib::page_fluid(
      bslib::layout_columns(
        col_widths = c(8, 4),
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

                                  checkboxGroupInput(ns("summary_method"),"Summary Method",
                                               choices = c("Mean" = "mean","Median" = "median","Maximum" = "max","Minimum" = "min"),selected ="mean")),
                                plotlyOutput(ns("export_plot"))),
          tags$br(),
          save_path_UI(ns("save_data"), button_label = "Export Data")
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

            save_path_UI(ns("save_meta"),button_label = "Export Metadata"))

          ),
          bslib::card(
            bslib::card_header("Save Sonde Project"),
            bslib::card_body(
              class = "d-flex flex-column justify-content-center",
              save_path_UI(ns("save_proj"), button_label = "Export Project"))
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
#' @param current_mod The name of the current module being viewed.
#' @export
#' @rdname export-data
export_server <- function(id, sondeproj, data_ver, y_var, current_mod){
  moduleServer(id, function(input, output, session){

  #selecting parameter to view
  update_parms_server("update_parms", sondeproj, data_ver, y_var, choices_fun = nice_yvar)

  #update range to view when data loaded
    observeEvent(sondeproj(), {
      updateDateRangeInput(session, "dates", start = min(sondeproj()$data$Date, na.rm = TRUE),
                           end = max(sondeproj()$data$Date, na.rm = TRUE))})

  #starting filenames for export file
    datastartname <- reactive({
      if(is.null(sondeproj()) | length(input$summary_method) == 0){
        "data"
      }else if(input$frequency == "interval"){
        make_filename(sondeproj()$meta$site, paste0(get_interval(sondeproj()$data), "min"))
      }else{
        make_filename(sondeproj()$meta$site, input$frequency, input$summary_method)
      }
    })
    projstartname <- reactive({
      if(is.null(sondeproj()) || is.na(sondeproj()$meta$site)){
        "sondeproj"
      }else{
        paste0(sondeproj()$meta$site, "_sondeproj")
      }
    })
    metastartname <- reactive({
      if(is.null(sondeproj()) || is.na(sondeproj()$meta$site)){
        paste0("sonde_", input$meta_opts)
      }else{
        paste0(sondeproj()$meta$site,"_", input$meta_opts)
      }
    })

  ## EXPORTING DATA TO CSV -----
    #create plot
      #filter data by requested range
        plot_data <- reactive({
          req(sum_data(), current_mod() == "step-10")
          sum_data() %>% mutate(Date = as.Date(.data$DateTime_rd)) %>% dplyr::filter(.data$Date >= input$dates[1], .data$Date <= input$dates[2])
        })

      #summarized data with flags added for export and plotting
      sum_data <- reactive({
        req(sondeproj(), (length(input$summary_method) > 0), current_mod() == "step-10")
        if(input$frequency %in% c("interval", "hour")){
          show_modal_spinner(text = "Summarizing data...", spin="fading-circle")
          on.exit(remove_modal_spinner(), add = TRUE)
        }

        export_data <- sondeproj()$data

        #summarize
          frequency <- switch(input$frequency,
                              "interval" = lubridate::period(get_interval(export_data), "minute"),
                              "hour" = lubridate::period(1, "hour"),
                              "day" = lubridate::period(1, "day"),
                              "week" = lubridate::period(7, "day"),
                              "month" = lubridate::period(1, "month"),
                              "year" = lubridate::period(1, "year"))
          summarize_data(export_data, frequency, input$summary_method) %>%
            mutate(Site = sondeproj()$meta$site, .after="DateTime_rd")

      })

    #format data little more before exporting
      export_data <- reactive({
        req(sum_data())

        sum_data() %>% rename("DateTime" = "DateTime_rd") %>%
          mutate(DateTime = format(.data$DateTime, "%Y-%m-%d %H:%M:%S"))
      })

      #create plot
      plot_obj <- reactive({
        req(y_var(), plot_data())

        #guard so doesn't crash if no method is selected
        if(length(input$summary_method) > 0){
          #pull out plotting so we can make a plot for each summary method
          y_var_nice <- get_yvar(y_var())

          #sort data so line looks correct and remove NA values to prevent warnings
          data <- plot_data() %>% arrange(.data$DateTime_rd)
          p <- plot_ly(source = "export_plot") %>%
            layout(paper_bgcolor = "#3c4d5a", plot_bgcolor = "#475763", font = list(color = "#ebebeb", family="sans-serif"),
                   xaxis = list(title = "<b>Date</b>"),
                   yaxis2=list(gridcolor = "#3c4d5a", zeroline = FALSE,title = paste0("<b>", y_var_nice, "</b>"),
                               overlaying = "y", side = "left"))

            pal <- colorRampPalette(c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854" ,"#FFD92F" ,"#E5C494", "#B3B3B3")) #from Set 2 color brewer
            colors <- pal(length(input$summary_method))
            for(x in input$summary_method){
              y_sum <- paste(y_var(), x, sep="_")
              subdat <- data %>% filter(!is.na(.data[[y_sum]]))
              p <- p %>% add_trace(data = subdat, x = ~DateTime_rd, y = as.formula(paste0("~`", y_sum, "`")),
                                   name = x,line = list(color = colors[which(input$summary_method == x)]),
                                   mode="lines", type="scatter", yaxis="y2", inherit = FALSE)}

          p
        }


      })

      #save to export
      output$export_plot <- plotly::renderPlotly({
        validate(
          need(nrow(plot_data()) > 0,
               "No data available for the selected date range."))

        # convert to plotly
        p <- plot_obj()
        toWebGL(p)
      })

      observeEvent(input$modules, {
        req(input$modules == "step-10")

        plotlyProxy("export_plot", session) %>%
          plotlyProxyInvoke("resize")
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
      data_path <- save_path_server("save_data", export_data, startname=datastartname, data_ver=data_ver)


 ## EXPORTING METADATA ------
    metadata <- reactive({
      req(input$meta_opts)
      switch(input$meta_opts,
             "dups" = sondeproj()$duplicates,
             "gaps" = sondeproj()$data_gaps,
             "changelog" = sondeproj()$changelog %>% mutate(datetime = format(.data$datetime, "%Y-%m-%d %H:%M:%S")),
             "precip" = sondeproj()$precip %>% mutate(DateTime = format(.data$DateTime, "%Y-%m-%d %H:%M:%S")))

      #make sure any commas are changed to ; to not break csv
    })

    meta_path <- save_path_server("save_meta", metadata, startname = metastartname, filetype = ".csv", data_ver=data_ver)

## EXPORTING PROJECT -----
    proj_path <- save_path_server("save_proj", sondeproj, startname=projstartname, filetype = ".RDS", data_ver=data_ver)

 ## STUFF FOR TESTING ------

  #export plot so we can check it
    exportTestValues(
      plot_obj = plot_obj())

  })}
