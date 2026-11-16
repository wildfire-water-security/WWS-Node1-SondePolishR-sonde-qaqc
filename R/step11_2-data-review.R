#' Allow interactive review of data changes
#'
#' Creates UI and backend to accept/reject changes, including adding notes.
#'
#' @param id the shiny ID of the module
#' @param sondeproj A `reactiveVal` holding the current dataset.
#' @param mode A `reactiveVal` with the review mode (analyst, reviewer, everything)
#' @param y_var A `reactiveVal` holding the Y-variable to plot on the y-axis.
#' @param username A `reactiveVal` holding the name of the analyst for the changelog
#'
#' @returns  the diff name that's selected for showing the changed points
#' @rdname data-review
#' @md
#' @export
#' @keywords internal
#'

data_review_side <- function(id) {
  ns <- NS(id)
  tagList(
    #simple table to select change
    DT::DTOutput(ns("review_table"))
  )
}

#' @rdname data-review
#' @export
data_review_bottom <- function(id) {
  ns <- NS(id)
  tagList(
    #return the pretty changelog info
    bslib::card(
      bslib::card_header(textOutput(ns("change_header"))),
      bslib::card_body(
        uiOutput(ns("change_details")),
        textAreaInput(ns("note"), tags$strong("Note: "), updateOn = "blur", width="80%")),
        uiOutput(ns("buttons")),
      )
  )
}

#' @rdname data-review
#' @export
data_review_server <- function(id, sondeproj, mode, y_var, username){
  moduleServer(id, function(input, output, session){
    ns <- session$ns  #line to make module work

    dd <- reactiveVal() #keep track of row selected in table
    note_update <- reactiveVal("") #notes to add

    #create review table
      review_tab <- reactive({
        req(sondeproj(), mode(), y_var())
        log <- sondeproj()$changelog #get log

        #get rows to show
        statuses <- switch(mode(),
                           "analyst" = c("returned"),
                           "reviewer" = c("pending", "justified"),
                           "everything" = c("pending", "approved", "returned", "justified", "undone"))

        log <- log %>% filter(.data$review_status %in% statuses & .data$parameter == y_var())

        if(mode() == "analyst"){
          log <- log %>% arrange(desc(.data$datetime))
        }else{
          log <- log %>% arrange(.data$datetime)

        }
        log
      })

    #output pretty review table
      output$review_table <- DT::renderDT({
        #column names
        df_cols <- c("DateTime" = "datetime", "Step" = "step", "Status" = "review_status")

        df <- review_tab()

        validate(
          need(nrow(df) > 0,
               "No new changes to Review."))

        #making datetime nice
        df<- df %>% select(any_of(df_cols))
        df$DateTime <- format(df$DateTime, "%Y-%m-%d  %H:%M")

        #sort (for reviewer to old to new but analyst new to old for undoing)
        if(mode() == "analyst"){
          df <- df %>% arrange(desc(.data$DateTime))
        }

        DT::datatable(
          df,
          selection = list(mode = "single"),
          editable = FALSE, rownames = FALSE,
          options = list(
            dom = "t",
            paging = FALSE,
            ordering = FALSE,
            info = FALSE,
            searching = FALSE,
            autoWidth = TRUE
          )
        )
      })

    #track the row being selected
      observeEvent(input$review_table_rows_selected,{
        dd(review_tab()$diff_name[input$review_table_rows_selected])
      })

    #if mode changes, clear the selection
      observeEvent(mode(), {
        if(nrow(review_tab()) == 0){dd(NULL)
        }else{
          dd(review_tab()$diff_name[1])
          }
        })

    #keep track of the changelog info
     change_info <- reactive({
       req(dd(), sondeproj(), mode())
       log <- sondeproj()$changelog
       info <- log[log$diff_name == dd(),]

       #set current note
       note <-  switch(mode(),
                       "analyst" =info$analyst_note,
                       "reviewer" = info$review_note,
                       "everything" = "")
       note_update(note)

       info
     })

    #update text output
      observeEvent(dd(), {
        updateTextAreaInput(session, "note", value=note_update())
      })

    #update the button options based on mode
      output$buttons <- renderUI({
        if(mode() == "reviewer"){
          tags$div(
            class = "d-flex gap-2",
            actionButton(ns("approve"),tagList(icon("check"), " Approve"),class = "btn btn-success"),
            actionButton(ns("reject"),tagList(icon("rotate-left"), " Return"),class = "btn btn-danger")
          )}else if(mode() == "analyst"){
            tags$div(
              class = "d-flex gap-2",
              actionButton(ns("justify"),tagList(icon("comment"), " Justify"), class = "btn btn-info"),
              actionButton(ns("undo"),tagList(icon("trash-arrow-up"), " Undo"), class = "btn btn-danger"))
        }
      })

    #track button clicks and update the change status
      #helper fun to update project status
      update_review_status <- function(status){
        req(dd(), sondeproj(), mode())
        proj <- sondeproj()
        #update note/add note
        if(mode() == "analyst"){
          proj <- update_log(proj, dd(), status, analyst=username(), note = input$note)
        }else if(mode() == "reviewer"){
          proj <- update_log(proj, dd(), status, reviewer=username(), note = input$note)
        }

        sondeproj(proj) #update changelog

        #figure out next row
        next_row <- ifelse(nrow(review_tab()) < 1, NA, review_tab()$diff_name[1])
        if(is.na(next_row)){dd(NULL)}else{dd(next_row)} #go to next row
        note_update("") #clear after hitting button
        updateTextAreaInput(session, "note", value=note_update())
      }

      observeEvent(input$approve, {update_review_status("approved")})
      observeEvent(input$reject, {update_review_status("returned")})
      observeEvent(input$justify, {update_review_status("justified")})

      #keep track of if we are okay restoring changes
      observeEvent(input$undo,{
        #only undo changes if something is selected
        if(!is.null(dd())){
          ##confirmation here
          shinyalert::shinyalert(title = "Confirm a undo of a change",
                                 text = "If you continue you will lose any edits made to those data points after that change.",
                                 type = "warning",
                                 showCancelButton = TRUE,
                                 inputId = "conf")
        }else{
          shinyalert::shinyalert(title = "Select a change to undo",
                                 text = "Select the row in the table to undo.",
                                 type = "info")}
      })

      #undo a change when selected
      observeEvent(input$conf,{
        if(input$conf == TRUE){
          #get current data
          proj <- sondeproj()
          data <- proj$data
          diffs <- proj$diffs
          log <- change_info()

          #get diff to undo
          diff <- diffs[[dd()]]
          diff_name <- dd()

          #get reverted data
          new_data <- apply_diff(data, diff, invert=TRUE)

          #get rows affected
          index <- which(!sapply(diff, is.null))[1]
          rows <- seq_along(1:nrow(diff[[index]]))

          #apply as if it's a new change
          edit <- list(data = new_data,
                       rows = rows,
                       y_var = y_var(),
                       step = "data review",
                       note = paste0("undoing change ", diff_name, ": ", log$step),
                       flag = NA)

          proj <- apply_edit(proj, edit, username())

          #make returned row at undone
          proj <- update_log(proj, dd(), "undone", analyst=username(), note = input$note)

          #set as sondeproj
          sondeproj(proj)
        }
      })

    #output a pretty view of data change
      output$change_header <- renderText({
        req(change_info())
        paste0("Edit Type: ", change_info()$step)
      })

      output$change_details <- renderUI({
        req(sondeproj(), dd())

        #get info for describing data
        info <- change_info()
        dd <- sondeproj()$diffs[[dd()]]

        vars <- names(dd)[!sapply(dd, is.null)][1]

        # Get date range, excluding missing values
        rng <- range(dd[[vars]]$DateTime_rd, na.rm = TRUE)

        date_ranges <- if(all(is.finite(as.numeric(rng)))){
          format(rng, "%Y-%m-%d %H:%M")
        }else{"—"}

        tags$div(
          class = "change-info-container",
          # Overall information (top)

          tags$div(
            class = "change-info-overall",
            tags$div(
              class = "change-info-grid",
              info_field("Parameter", get_yvar(info$parameter)),
              info_field("Points Changed (#)", info$n_changed),
              info_field("Status", info$review_status),
              info_field("Change Date Range", paste(date_ranges, collapse = " to "))
            )
          ),

          # Analyst and Reviewer information (side by side)
          tags$div(
            class = "change-info-columns",

            info_section(
              "Analyst Information",
              info_field("Analyst", info$analyst,TRUE),
              info_field("Edit Time", format_datetime(info$datetime),TRUE),
              info_field("Analyst Note", info$analyst_note, TRUE)
            ),

            info_section(
              "Reviewer Information",
              info_field("Reviewer", info$reviewer,TRUE),
              info_field("Review Date", format_datetime(info$review_date),TRUE),
              info_field("Reviewer Note", info$review_note, TRUE)
            )
          ))

          })

      return(dd)
  }
  )}
