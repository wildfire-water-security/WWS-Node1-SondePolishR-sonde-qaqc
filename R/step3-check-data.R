##step 2 of the app: exploratory plotting with plotly

# UI Function
#' @export
#' @rdname check-data
check_data_UI <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        width = 2,

        tags$h5("Table Options"),

        selectInput(
          ns("table_opt"),
          "Select Table to View:",
          choices = c("Duplicates", "Gaps")
        ),

      ),

      mainPanel(
        width =10,
        #visualize data log
        DT::DTOutput(NS(id, "change_table")),

        conditionalPanel(
          condition = sprintf(
            "input['%s'] == 'Duplicates'",
            ns("table_opt")
          ),

          plotlyOutput(ns("dup_plot"), height = "300px"),

          uiOutput(ns("keep_ui")),

          textInput(ns("flag_notes"), "Analyst Notes (optional):",
                  value = "",
                  placeholder = "Enter text..."),

          actionButton(ns("apply_dup"),"Resolve Duplicate")
        )
      ))
  )
}

#' Check data for duplicates, gaps, remove OOW periods
#'
#' @param id An ID string passed to shiny::NS(), used for namespacing UI inputs/outputs.
#' @param sondeproj A `reactiveVal` holding the current dataset.
#' @param data_ver A `reactiveVal` holding a number used to track when new data is added to trigger resets.
#' @param y_var Y-variable to plot on the y-axis.

#' @md
#' @keywords internal
#' @export
#' @rdname check-data
#' @returns Invisible NULL
#'
check_data_server <- function(id, sondeproj, data_ver, y_var){
  moduleServer(id, function(input, output, session){

  #when data loaded, get dups and gaps
  observeEvent(sondeproj(),{
    req(sondeproj())

    proj <- sondeproj()
    proj <- refresh_checks(proj)

    #replace sondeproj with updated
    sondeproj(proj)

  })

  #create log table
    tab <- reactive({
      req(sondeproj(), input$table_opt)

      df <- switch(
        input$table_opt,
        "Duplicates" = sondeproj()$duplicates,
        "Gaps"       = sondeproj()$data_gaps
      )

    })

    output$change_table <- DT::renderDT({
        df <- tab()

        validate(
          need(nrow(df) > 0,
               paste("No", tolower(input$table_opt), "found."))
        )


      if(nrow(df) > 0){
        df$start <- format(df$start, "%Y-%m-%d  %H:%M")
        df$end   <- format(df$end,   "%Y-%m-%d  %H:%M")
      }

      #column options
        colopt <- switch(
          input$table_opt,
          "Duplicates" = list(
            list(targets = "_all", className = "dt-center"),
            list(targets = c(7), width = "250px", className = "dt-nowrap"),
            list(targets = c(1, 2, 3, 7), width = "120px"),
            list(targets = c(4,5,6), width = "60px"),
            list(targets = c(8), width = "350px")),
          "Gaps"       = list(
            list(targets = "_all", className = "dt-center"),
            list(targets = c(1, 2), width = "120px"),
            list(targets = c(3), width = "60px"),
            list(targets = c(4), width = "350px"))
        )

        DT::datatable(
          df,
          selection = "single",
          filter = "top",
          editable = "cell",
          options = list(
            autoWidth = TRUE,
            scrollX = TRUE,
            columnDefs = colopt
            )
        )
      })

  #keep track of edits
    observeEvent(input$change_table_cell_edit, {
      info <- input$change_table_cell_edit
      df <- tab()
      df[info$row, info$col] <- info$value
      proj <- sondeproj()

      if (input$table_opt == "Duplicates") {
        proj$duplicates <- df
      } else if (input$table_opt == "Gaps") {
        proj$data_gaps <- df
      }

      sondeproj(proj)
    })

  #keep track of which dup is selected
    selected_dup <- reactive({
      req(input$table_opt == "Duplicates")

      rows <- input$change_table_rows_selected
      req(length(rows) == 1)

      sondeproj()$duplicates[rows, ]
    })

  #plot selected dup
    dup_plot_data <- reactive({
      req(selected_dup())
      data <- sondeproj()$data
      dupdata <- data %>% filter(.data$DateTime_rd >= selected_dup()$start,.data$DateTime_rd <= selected_dup()$end) %>%
      mutate(color_labs = if(selected_dup()$duptype == "multiple files"){.data$FileName}else{paste0("Set ", .data$DupNum)})

      #get data for before and after dup period
      prepost <- data %>% filter((.data$DateTime_rd >= (selected_dup()$start - lubridate::hours(8)) & .data$DateTime_rd < selected_dup()$start) |
                                 (.data$DateTime_rd > selected_dup()$end & .data$DateTime_rd <= selected_dup()$end + lubridate::hours(8))) %>%
        mutate(color_labs = "non-duplicated data")

      rbind(dupdata, prepost)

    })

    plot_obj <- reactive({
      dat <- dup_plot_data()

      y <- y_var()
      y_var_nice <- get_yvar(y)

      plot_ly() %>%
        layout(paper_bgcolor = "#3c4d5a", plot_bgcolor = "#475763", font = list(color = "#ebebeb", family="sans-serif"),
               xaxis = list(title = "<b>Date</b>"),
               yaxis=list(gridcolor = "#3c4d5a", zeroline = FALSE,title = paste0("<b>", y_var_nice, "</b>"))) %>%
                 add_trace(data = dat, x = ~DateTime_rd,y = as.formula(paste0("~`", y, "`")),
                           mode="lines+markers", type="scatter", color = ~color_labs)

    })

    output$dup_plot <- renderPlotly({
      toWebGL(plot_obj())
    })

  #update options for which version to keep
    output$keep_ui <- renderUI({
     # browser()
      dat <- dup_plot_data() %>% filter(.data$color_labs != "non-duplicated data")
      opts <- c(unique(dat$color_labs), "use_mean", "remove_both")

      if(selected_dup()$duptype == "multiple files"){
        names(opts) <- c(unique(dat$FileName),"Use Mean", "Remove Both")
      }else{
        names(opts) <- c(unique(dat$color_labs),"Use Mean","Remove Both")
       }

      radioButtons(session$ns("keep_opt"),"Select Which Duplicate Set to Keep",choices = opts)
    })

  #apply duplicate edits
    observeEvent(input$apply_dup,{
      req(selected_dup())
      req(input$keep_opt)

      proj <- sondeproj()
      proj <- apply_dup_edits(proj,selected_dup(),input$keep_opt,input$flag_notes)
      proj <- refresh_checks(proj)

      sondeproj(proj) #update project
    })

  #export plot so we can check it
    exportTestValues(
      table = tab(),
      plot_obj = plot_obj())

  })
}
