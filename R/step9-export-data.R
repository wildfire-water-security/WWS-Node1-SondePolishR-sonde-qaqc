
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
          height = "650px",
          bslib::card_header("Export Data"),
          bslib::layout_columns(col_widths = c(3,9),
                                div(
                                  style = "padding-right: 1rem;",
                                update_parms_UI(ns("update_parms"), "Select Parameter to View:"),
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
          save_path_UI(ns("save_data"),filetype = ".csv")
        ),

      #side part with metadata and exporting
        bslib::layout_column_wrap(
          width = 1,
          bslib::card(
            bslib::card_header("Metadata"),
          #   checkboxGroupInput(
          #     ns("meta_opts"),NULL,choices = c(
          #       "Duplicate Notes" = "dups",
          #       "Missing Data Notes" = "gaps",
          #       "Change Log" = "changelog")),
          #   tags$hr(),
          #   tags$h6("Save Location"),
          #   div(class = "d-flex gap-3 align-items-center",
          #       shinyFiles::shinySaveButton(
          #         ns("save_meta"),label = "Choose Location", title = "Select export path",filetype = ".csv"),
          #       uiOutput(ns("meta_path_text"))),
          #   tags$hr(),
          #   div(
          #     class = "text-center",
          #     actionButton(ns("data_save_loc"),"Export Metadata", width = "220px"))
          ),
          bslib::card(
            bslib::card_header("Save Sonde Project"),
          #   div(class = "d-flex gap-3 align-items-center",
          #       shinyFiles::shinySaveButton(
          #         ns("save_meta"),label = "Choose Location", title = "Select export path",filetype = ".csv"),
          #       uiOutput(ns("meta_path_text"))),
          #   tags$hr(),
          #   div(
          #     class = "text-center",
          #     actionButton(ns("proj_save_loc"),"Export Sonde Project",width = "220px"))
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

  #create plot
    #filter data by requested range
      plot_data <- reactive({
        req(sum_data())
        sum_data() %>% mutate(Date = as.Date(.data$DateTime_rd)) %>% dplyr::filter(.data$Date >= input$dates[1], .data$Date <= input$dates[2])
      })

    #summarized data with flags added for export and plotting
    sum_data <- reactive({
      req(sondeproj())
      data <- sondeproj()$data

    #get flags and combine into a single column per par
      parms <- get_parms(data)
      flags <- sondeproj()$flags %>% bind_rows() %>% group_by(.data$Index, .data$DupNum, .data$DateTime, .data$DateTime_rd) %>%
        summarise(across(all_of(parms),function(x) paste(x[!is.na(x)], collapse = ";")),
                  .groups = "drop") %>% mutate(across(all_of(parms), ~ ifelse(.x == "", NA, .x))) %>%
        rename_with(~ paste0(.x, "_flag"), .cols=all_of(parms))

      #link to data
      export_data <- data %>% left_join(flags, by = join_by("Index", "DupNum", "DateTime", "DateTime_rd"))
      export_data <- export_data %>%
        select("Index":"Battery_V",sort(setdiff(names(export_data), names(export_data %>% select("Index":"Battery_V")))))

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
      plot_sonde(plot_data(), y_var(), list(points=FALSE,line=TRUE,files=FALSE,
                                            oow=FALSE,calcheck=FALSE))
    })

    #save to export
    output$export_plot <- plotly::renderPlotly({
      validate(need(nrow(plot_data()) > 0,
          "No data available for the selected date range."))

      # convert to plotly
        p <- plotly::ggplotly(plot_obj(), dynamicTicks = TRUE, height = 450)
        p <- strip_hoveron(p)
        toWebGL(p)
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


 #  #get info on equation
 #    eq_info <- reactive({
 #      req(input$method)
 #      get_equation(input$method)
 #    })
 #
 #  #show equation in UI based on method
 #    output$equation <- renderUI({
 #      req(input$method)
 #      withMathJax(HTML(eq_info()$equation))
 #    })
 #
 #    output$source <- renderText({
 #      req(input$method)
 #      paste("Source:", eq_info()$source)
 #    })
 #
 # #show options for parameters
 #  ns = session$ns
 #  output$coef_inputs <- renderUI({
 #      req(input$method)
 #      params <- eq_info()$params
 #
 #      tagList(
 #        fluidRow(
 #        lapply(names(params), function(par) {
 #          column(width = floor(12 / length(params)),
 #          numericInput(session$ns(par),
 #                       label = tags$span(par, style = "font-size: 20px;"),
 #                       value = params[[par]]$value,
 #                       step = params[[par]]$step %||% 0.01))})
 #      ))
 #    })
 #
 #  #keep track of user parameter values to pass to corr_fun
 #  coef_vals <- reactive({
 #    req(input$method)
 #    params <- names(eq_info()$params)
 #    vals <- setNames(lapply(params, function(x) input[[x]]),params)
 #    # wait until new UI inputs exist
 #    req(all(vapply(vals, Negate(is.null), logical(1))))
 #    vals
 #  })
 #
 #  #get what to plot via user options
 #    plot_opts <- plot_options_server("plot_opts")
 #
 #  #keep track of dates
 #    dates <- weekly_range_server(
 #      "date_nav",
 #      min_date = reactive({req(sondeproj())
 #        min(sondeproj()$data$Date, na.rm = TRUE)}),
 #      max_date = reactive({req(sondeproj())
 #        max(sondeproj()$data$Date, na.rm = TRUE)}))
 #
 #
 #  #filter data to plot
 #    plot_data <- reactive({
 #      req(sondeproj(), dates())
 #      sondeproj()$data %>% dplyr::filter(.data$Date >= dates()[1], .data$Date <= dates()[2])
 #    })
 #
 #    corr_data <- reactive({
 #      req(sondeproj(), dates(), eq_info(), coef_vals())
 #      corr_fun <- eq_info()$fun
 #
 #      sondeproj()$data %>% dplyr::filter(.data$Date >= dates()[1], .data$Date <= dates()[2]) %>%
 #        mutate(fDOM_QSU_T = .data$fDOM_QSU / (1 + input$rho*(.data$Temp_C - 25)),
 #               fDOM_QSU_Tt = corr_fun(.data$fDOM_QSU_T, .data$Turbidity_FNU, coef_vals()))
 #    })
 #
 #  #create plotly plot
 #    plot_obj <- reactive({
 #      req(plot_data())
 #
 #      #use function to plot sonde data
 #      p <- plot_sonde(plot_data(), "fDOM_QSU", plot_opts(),sondeproj()$fieldform, sondeproj()$calcheck)
 #
 #      #add corrected fDOM
 #      p <- p + ggplot2::geom_line(data=corr_data(), aes(x=.data$DateTime_rd, y=.data$fDOM_QSU_Tt), color="darkred", na.rm=TRUE)
 #
 #      #return plot
 #      p
 #    })
 #
 #    #save to export
 #    output$fdom_plot <- plotly::renderPlotly({
 #      validate(
 #        need(
 #          nrow(plot_data()) > 0,
 #          "No data available for the selected date range."
 #        )
 #      )
 #
 #      # convert to plotly
 #      p <- plotly::ggplotly(plot_obj(), dynamicTicks = TRUE)
 #      p <- strip_hoveron(p)
 #      toWebGL(p)
 #    })
 #
 #
 #  #create edit object
 #    edit <- reactive({
 #      newdata <- sondeproj()$data
 #
 #      #replace with fDOM corrected data
 #      corr_fun <- eq_info()$fun
 #
 #      newdata <- newdata %>%
 #        mutate(fDOM_QSU_T = .data$fDOM_QSU / (1 + input$rho*(.data$Temp_C - 25)),
 #               fDOM_QSU = corr_fun(.data$fDOM_QSU_T, .data$Turbidity_FNU, coef_vals())) %>%
 #          select(-"fDOM_QSU_T")
 #
 #      #create note
 #      nice_method <- switch(input$method,
 #                           "inverse_poly" = "Inverse Polynomial",
 #                           "1p_exponential" ="Exponential (1-parameter)",
 #                           "2p_exponential" = "Exponential (2-parameter)",
 #                           "5p_exponential" = "Exponential (5-parameter)")
 #      nice_coeff <- paste(paste0(names(coef_vals()), " = ", coef_vals()), collapse = ", ")
 #      method_note <- ifelse(input$method == "none", "", paste0(" and turbidity using the ", nice_method, " method (", nice_coeff, ")"))
 #
 #      #make edit list
 #      list(
 #        data = newdata,
 #        rows = rep(TRUE, nrow(newdata)),
 #        y_var = "fDOM_QSU",
 #        step = "fDOM correction",
 #        note = paste0("fDOM corrected for temperature (\U03C1 = ", input$rho, ")", method_note),
 #        flag = "CHG03",
 #        changetype = "flag_chg"
 #      )
 #
 #    })
 #
 #  #flagging module
 #    apply_edit_server("apply_limits", sondeproj, edit)
 #
 #  #export plot so we can check it
 #    exportTestValues(
 #      plot_obj = plot_obj(),
 #      changelog = sondeproj()$changelog)

  })}
