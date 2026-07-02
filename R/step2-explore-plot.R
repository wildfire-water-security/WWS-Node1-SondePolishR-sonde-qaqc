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

        update_parms_UI(ns("update_parms"), input_id = "y2_var", text = "Select Second Parameter to Plot:"),

        HTML("<hr>"),

        tags$h5("Remove Out of Water Periods"),
        actionButton(ns("remove_oow"), "Flag OOW Periods"),
        HTML("<hr>"),

        #date options
        weekly_range_sidebar_UI(ns("weekly_range")),

        HTML("<hr>"),

        #plotting options
        plot_options_UI(ns("plot_opts")),

        HTML("<hr>"),
        tags$h5("Table Options"),

        selectInput(
          ns("table_opt"),
          "Select Table to View:",
          choices = c("Change Log", "Field Form", "Calibration Check", "Data Summary")
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
#' @param data_ver A `reactiveVal` holding a number used to track when new data is added to trigger resets.
#' @param y_var Y-variable to plot on the y-axis.

#' @md
#' @keywords internal
#' @export
#' @rdname explore-data
#' @returns Invisible NULL
#'
explore_data_server <- function(id, sondeproj, data_ver, y_var){
  moduleServer(id, function(input, output, session){

  #keep track of second y_variable
    y2_var <- reactiveVal()

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
    }else if(input$table_opt == "Data Summary"){
      data <- sondeproj()$data %>% dplyr::filter(.data$Date >= dates()[1], .data$Date <= dates()[2])
      describe_data(data)
    }
  })



    output$log_table <- DT::renderDT({
      #column names
      df_cols <- switch(
        input$table_opt,
        "Change Log" = c("DateTime" = "datetime", "Parameter"= "parameter", "Step" = "step",
                         "Values Changed (#)" = "n_changed", "Note" = "note", "User" = "user", "Version Name" =  "diff_name"),
        "Calibration Check" = c("Date" = "Date", "Parameter" = "Parameter", "Resident Value" = "Resident_Value",
                                "Check Value" = "Check_Value", "Notes" ="Notes", "Probe Switched?" =  "Probe_Switch",
                                "Estimated Switch Time"  ="Est_Time"),
        "Field Form" = c("Date" = "Date", "Removal Time" = "Removal_Time_PST","Return Time" =  "Return_Time_PST",
                          "Remove OOW Period?" = "Remove_Period", "Notes" ="Notes"),
        "Data Summary" = c("Parameter" = "Parameter", "Mean" = "Mean", "Median" = "Median", "Maximum" = "Maximum",
                           "Minimum" = "Minimum", "Std. Deviation" = "Std_Deviation", "1st Quantile" = "Quantile_1st",
                           "3rd Quantile" = "Quantile_3rd", "Number of NA's" = "Number_NAs")
      )


       df <- tab()
      #making datetime nice
      if(input$table_opt == "Change Log"){
        df$datetime <- format(df$datetime, "%Y-%m-%d  %H:%M")
      }else if(input$table_opt == "Calibration Check"){
        df$Est_Time <- format(df$Est_Time, "%Y-%m-%d  %H:%M")
      }

      DT::datatable(
       df,
       selection = list(mode = "single"),
       filter = "top",
       colnames = df_cols
      )})

  #get what to plot via user options
    plot_opts <- plot_options_server("plot_opts")

  #get column names after file upload (dynamic)
    update_parms_server("update_parms", sondeproj, data_ver, y_var, choices_fun = nice_yvar)

    update_parms_server("update_parms", sondeproj, data_ver, y2_var, input_id= "y2_var", choices_fun = nice_yvar)

  #remove OOW periods
    observeEvent(input$remove_oow, {
      req(sondeproj(), sondeproj()$fieldform)

      data <- sondeproj()$data
      #get OOW periods
      oow <- get_oow(sondeproj()$fieldform)

      #remove those periods from data
      rm_index <- data %>%
        rowwise() %>%
        filter(any(.data$DateTime_rd >= oow$start & .data$DateTime_rd <= oow$end)) %>% pull(.data$Index) %>% unique()
      setna <- data$Index %in% rm_index

      data_filter <- data %>% mutate(filter = setna) %>%
        mutate(across(-("Index":"Battery_V"), ~ if_else(filter, NA, .x))) %>%
        select(-"filter")

      #flag these changes were made
      edit <- list(
        data = data_filter,
        rows = setna,
        y_var = "all",
        step = "removing oow",
        note = paste0("OOW periods removed based on information from the field form."),
        flag = "RM04",
        changetype = "flag_rm"
      )


      #log edits
      proj <- apply_edit(sondeproj(), edit)

      #update sondeproj
      sondeproj(proj)

    })

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

            data_ver <- apply_diff(sondeproj()$data, diffs, id=c("DateTime_rd", "DupNum"), invert=TRUE)
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
      req(y_var(),y2_var(), plot_data())
      if(y2_var() == "none"){y2 <- NULL}else{y2 <- y2_var()}

      #use function to plot sonde data
      plot_sonde(data = plot_data(), y_var=y_var(), y2_var= y2, opts=plot_opts(),fieldform=sondeproj()$fieldform,
                 calcheck =sondeproj()$calcheck, precip=sondeproj()$precip)
    })

    #save to export
    output$plot <- plotly::renderPlotly({
      validate(
        need(nrow(plot_data()) > 0,
          "No data available for the selected date range."))

      # convert to plotly
      p <- plot_obj()
      #toWebGL(p)
    })

    #export plot so we can check it
    exportTestValues(
      plot_obj = plot_obj(),
      table = tab())

  })
}
