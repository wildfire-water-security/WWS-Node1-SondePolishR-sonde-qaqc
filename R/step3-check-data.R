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
        DT::DTOutput(NS(id, "change_table"))
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
  observeEvent(data_ver(),{
    req(sondeproj())

    proj <- sondeproj()
  #get duplicates
    dup_check <- identify_dups(proj$data)

    #put in the project (merge user notes to preserve)
    if(!is.null(proj$duplicates)){
      old_dups <- proj$duplicates

      merge_dups <- dup_check %>% select(-"user_note") %>% left_join(old_dups %>% select("start", "end", "user_note"), join_by("start", "end"))
    }else{
      merge_dups <- dup_check
    }

    proj$duplicates <- merge_dups

  #get gaps
    missing <- identify_gaps(proj$data)

    #put in the project (merge user notes to preserve)
    if(!is.null(proj$data_gaps)){
      old_gap <- proj$data_gaps

      merge_gap <- missing %>% select(-"user_note") %>% left_join(old_gap %>% select("start", "end", "user_note"), join_by("start", "end"))
    }else{
      merge_gap <- missing
    }

    proj$data_gaps <- merge_gap

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

      #colops
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
          selection = list(mode = "single"),
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

      print("edit logged")
      info <- input$change_table_cell_edit

      print(info)
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

    #export plot so we can check it
    exportTestValues(
      table = tab())

  })
}
