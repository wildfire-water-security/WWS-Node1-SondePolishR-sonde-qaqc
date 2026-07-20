##step 2 of the app: exploratory plotting with plotly

# UI Function
#' @export
#' @rdname explore-data
explore_data_UI <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        accordion(
          open = c("Select Parameters", "Remove Out of Water Periods", "Undo Data Edits", "Table Options"),

          accordion_panel(
            "Select Parameters",
            update_parms_UI(ns("update_parms")),
            update_parms_UI(ns("update_parms"), input_id = "y2_var", text = "Select Second Parameter to Plot:")
          ),
          accordion_panel(
            "Remove Out of Water Periods",
            actionButton(ns("remove_oow"), "Flag OOW Periods"),
          ),
          accordion_panel(
            "Undo Data Edits",
            actionButton(ns("undo_changes"), "Restore Selected Version"),
          ),
          accordion_panel(
            "Table Options",
            selectInput(
              ns("table_opt"),
              "Select Table to View:",
              choices = c("Change Log", "Field Form", "Calibration Check", "Data Summary"))
          ),
          accordion_panel(
            "Date Ranges",
            weekly_range_sidebar_UI(ns("date_nav")),
          ),
          accordion_panel(
            "Plotting Options",
            plot_options_UI(ns("plot_opts"))
          )
        )),

      mainPanel(
        #add plot
        plotlyOutput(NS(id,"plot")),
        #add buttons to navigate date
        weekly_range_buttons_UI(ns("date_nav")),
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
#' @param dates The date range to view the data.
#' @param period_view Should data be viewed by period?
#' @param p_length The length of the period to view.

#' @md
#' @keywords internal
#' @export
#' @rdname explore-data
#' @returns Invisible NULL
#'
explore_data_server <- function(id, sondeproj, data_ver, y_var,period_view, dates, p_length){
  moduleServer(id, function(input, output, session){
    ns <- NS(id) #line to make module work

  #keep track of second y_variable
    y2_var <- reactiveVal()
    undo_ver <- reactiveVal() #keep track of data version

  #create log table
  tab <- reactive({
    req(sondeproj())

    if(input$table_opt == "Field Form"){
      req(sondeproj()$fieldform)
      sondeproj()$fieldform %>% dplyr::select("Date", "Removal_Time",
                                                     "Return_Time",
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
      precip <- sondeproj()$precip %>% dplyr::filter(.data$DateTime >= dates()[1], .data$DateTime <= dates()[2])
      describe_data(data, precip)
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
        "Field Form" = c("Date" = "Date", "Removal Time" = "Removal_Time","Return Time" =  "Return_Time",
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
      oow <- get_oow(sondeproj()$fieldform, tz=sondeproj()$meta$tz,interval=get_interval(data))

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
    plot_dates <- weekly_range_server("date_nav", sondeproj, period_view, dates, p_length, data_ver)

  #keep track of selected data version
    undo_ver <- reactive({
      req(sondeproj())

      row <- input$log_table_rows_selected

      if (is.null(row)) {
        return(NULL)
      }

      if (row == nrow(sondeproj()$changelog)) {
        return(sondeproj()$data)
      }

      diff_list <- sondeproj()$changelog$diff_name[(row + 1):nrow(sondeproj()$changelog)]
      diff_list <- diff_list[grepl("^dd", diff_list)]
      diffs <- sondeproj()$diffs[names(sondeproj()$diffs) %in% diff_list]

      apply_diff(sondeproj()$data, diffs,
                 id = c("DateTime_rd", "DupNum"),
                 invert = TRUE)
    })

  #keep track of if we are okay restoring changes
   observeEvent(input$undo_changes,{
     #only undo changes if something is selected
     if(!is.null(input$log_table_rows_selected)){
       ##confirmation here
       shinyalert::shinyalert(title = "Confirm restoring past data version",
                              text = "If you continue you will lose any edits made after the selected version.",
                              type = "warning",
                              showCancelButton = TRUE,
                              inputId = "conf")
     }else{
       shinyalert::shinyalert(title = "Select a Version to Restore",
                              text = "Select a row in the table to restore to.",
                              type = "info")}
       })

  #reset version selected when project
  #restore a version
   observeEvent(input$conf,{
     if(input$conf == TRUE){
       proj <- sondeproj()

       #update dataset
       proj$data <- undo_ver()

       #remove extra diffs
       #get the diffs to apply
       row <- input$log_table_rows_selected
       #only get diffs if not current data
       if(row < nrow(sondeproj()$changelog)){
         diff_list <- sondeproj()$changelog$diff_name[1:row]
         diff_list <- diff_list[grepl("^dd", diff_list)]
         diffs <- sondeproj()$diffs[names(sondeproj()$diffs) %in% diff_list]}
       proj$diffs <- diffs

       #roll back changelog
       proj$changelog <- proj$changelog[1:row,]

       #set as sondeproj
       sondeproj(proj)
     }
   })

    #filter data
    plot_data <- reactive({
      req(sondeproj(), plot_dates())
      if(is.null(undo_ver())){
        data <- sondeproj()$data
      }else{
        data <- undo_ver()
      }
      data %>% dplyr::filter(.data$Date >= plot_dates()[1], .data$Date <= plot_dates()[2])
    })

    #create plotly plot
    plot_obj <- reactive({
      req(y_var(),y2_var(), plot_data())
      if(y2_var() == "none"){y2 <- NULL}else{y2 <- y2_var()}

      #use function to plot sonde data
      plot_sonde(data = plot_data(), y_var=y_var(), y2_var= y2, proj = sondeproj(), opts=plot_opts())
    })

    #save to export
    output$plot <- plotly::renderPlotly({
      validate(
        need(nrow(plot_data()) > 0,
          "No data available for the selected date range."))

      # convert to plotly
      p <- plot_obj()
      toWebGL(p)
    })

    #redraw when back on module to prevent weird drawing issues
    observeEvent(input$modules, {
      req(input$modules == "step-2")

      plotlyProxy("plot", session) %>%
        plotlyProxyInvoke("resize")
    })

    #export plot so we can check it
    exportTestValues(
      plot_obj = plot_obj(),
      table = tab())

  })
}
