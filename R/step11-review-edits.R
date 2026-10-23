##step 10 of the app: review data edits and okay or flag or checking

# UI Function
#' @export
#' @rdname review-edits
review_edits_UI <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        accordion(
          open = c("Select Parameters","Review Data Edits"),

          accordion_panel(
            "Select Parameters",
            update_parms_UI(ns("update_parms")),
            update_parms_UI(ns("update_parms"), input_id = "y2_var", text = "Select Second Parameter to Plot:")
          ),
          accordion_panel(
            "Review Data Edits",
            data_version_UI(ns("data_version"))
          ),
          accordion_panel(
            "Date Ranges",
            weekly_range_sidebar_UI(ns("date_nav")),
          ),
          accordion_panel(
            "Plotting Options",
            plot_options_UI(ns("plot_opts"), start_val = c(TRUE,TRUE,FALSE,FALSE,FALSE,FALSE))
          )
        )),

      mainPanel(
        main_plot_UI(ns("plot")),

        #add buttons to navigate date
        weekly_range_buttons_UI(ns("date_nav")),
        #adding some space after buttons
        HTML("<br><br>"),
        #visualize data log
        DT::DTOutput(NS(id, "log_table"))
      ))
  )
}

#' Review data corrections
#'
#' Allows a reviewer to view the corrections made, accept or reject them, and make comments.
#'
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
#' @param webgl_supported A `reactiveVal` indicating if webgl is support in the current browser.


#' @md
#' @keywords internal
#' @export
#' @rdname review-edits
#' @returns Invisible NULL
#'
review_edits_server <- function(id, sondeproj, data_ver, y_var, view_state, username,webgl_supported){
  moduleServer(id, function(input, output, session){
    ns <- NS(id) #line to make module work

    #keep track of second y_variable
    y2_var <- reactiveVal()
    undo_ver <- reactiveVal() #keep track of data version
    plot_exist <- reactiveVal() #keeps warning about missing plot
    sel_row <- reactiveVal() #keep track of row selected in table

    #create log table
    tab <- reactive({
      req(sondeproj())
      sondeproj()$changelog %>% mutate(parameter = factor(.data$parameter, levels=unique(.data$parameter))) %>%
        filter(.data$parameter %in% c("all", y_var()))
    })

    output$log_table <- DT::renderDT({
      #column names
      df_cols <- c("DateTime" = "datetime", "Parameter"= "parameter", "Step" = "step")

      df <- tab()
      #making datetime nice
        df$datetime <- format(df$datetime, "%Y-%m-%d  %H:%M")

      DT::datatable(
        df,
        selection = list(mode = "single"),
        filter = "top",
        colnames = df_cols,
        editable = FALSE,
        escape = FALSE,
        options = list(
          stateSave = TRUE,
          stateDuration = -1,
          pageLength = 5,
          lengthMenu = list(
            c(3, 5, 10, 20, -1),
            c('3', '5', '10', '20', 'All'))
        )
      )
    })

  #sets sel row to diff name
    observeEvent(input$log_table_rows_selected,{
      sel_row(tab()$diff_name[input$log_table_rows_selected])
    })

    version_data <- data_version_server("data_version",sondeproj = sondeproj,y_var = y_var, dd=sel_row)

   #get what to plot via user options
    plot_opts <- plot_options_server("plot_opts")

    #get column names after file upload (dynamic)
    update_parms_server("update_parms", sondeproj, data_ver, y_var, choices_fun = nice_yvar)

    update_parms_server("update_parms", sondeproj, data_ver, y2_var, input_id= "y2_var", choices_fun = nice_yvar)

    #keep track of dates
    plot_dates <- weekly_range_server("date_nav", sondeproj, data_ver, view_state)

    #filter data
    plot_data <- reactive({
      req(version_data$current(), plot_dates())

      data <- version_data$current()

      data %>% dplyr::filter(.data$Date >= plot_dates()[1], .data$Date <= plot_dates()[2])
    })

    #create plotly plot
    plot_obj <- reactive({
      req(y_var(),y2_var(), plot_data(), data_check(plot_data(), y_var()))
      if(y2_var() == "none"){y2 <- NULL}else{y2 <- y2_var()}

      #use function to plot sonde data
      p <- plot_sonde(data = plot_data(), y_var=y_var(), y2_var= y2, proj = sondeproj(), opts=plot_opts())

      #add previous data if triggered
      if(!is.null(version_data$changed())){
        #plot as a line
        prev <- version_data$previous() %>% dplyr::filter(.data$Date >= plot_dates()[1], .data$Date <= plot_dates()[2])
        p <- p %>% add_trace(data = prev, x = ~DateTime_rd, y = as.formula(paste0("~`", y_var(), "`")),
                             name = "Pre-Correction",type="scatter", mode="lines", yaxis="y2",
                             line = list(color = "#1d3040"), inherit = FALSE)

        prev_data <- version_data$changed() %>% filter(!is.na(.data$old)) %>% dplyr::filter(.data$DateTime_rd >= plot_dates()[1], .data$DateTime_rd <= plot_dates()[2])

        op_col <- c("data_added" = "#2ECC71", "data_removed" = "darkred", "data_changed" = "orange", "data_merge" = "darkgreen")

        #change color based on change
        for(f in unique(prev_data$op_type)){
          nice_f <- stringr::str_to_title(gsub("_", " ", f))
          df <- prev_data %>% filter(.data$op_type == f)
          p <- p %>% add_trace(data = df, x = ~DateTime_rd, y = ~old,
                               name = nice_f,type="scatter", mode="markers", yaxis="y2",
                               marker = list(color = op_col[[f]]), inherit = FALSE)}

        #stick the new line in the back
        p <- plotly::plotly_build(p)
        trace_names <- vapply(p$x$data, function(trace){
        if(is.null(trace$name)) "" else trace$name}, character(1))
        prev_idx <- which(trace_names == "Pre-Correction")
        if(length(prev_idx) == 1){
          p$x$data <- append(
            p$x$data[-prev_idx],
            p$x$data[prev_idx],
            after = 0)}
      }

      p

    })

    #keep track of y-limits
    startmax <- reactive({
      req(version_data, y_var())
      curr <- version_data$current()
      prev <- version_data$previous()
      vals <- c(curr[[y_var()]], prev[[y_var()]])

      if(all(is.null(vals))){return(NULL)}
      ceiling(max(vals, na.rm=TRUE)*1.05)
      })

    startmin <- reactive({
      req(startmax(), y_var())
      curr <- version_data$current()
      prev <- version_data$previous()
      vals <- c(curr[[y_var()]], prev[[y_var()]])
      floor(min(vals, na.rm=TRUE) - (startmax()*0.05))

    })

    #save to export
    main_plot_server("plot", data_ver, sondeproj, plot_obj, plot_data, y_var, plot_exist=plot_exist, startmin=startmin, startmax=startmax,
                     webgl_supported=webgl_supported)

    #redraw when back on module to prevent weird drawing issues
    observeEvent(input$modules, {
      req(input$modules == "step-11")

      plotlyProxy("plot", session) %>%
        plotlyProxyInvoke("resize")
    })

    #export plot so we can check it
    exportTestValues(
      plot_obj = plot_obj(),
      table = tab())

  })
}
