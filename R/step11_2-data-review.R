#' Allow interactive review of data changes
#'
#' Creates UI and backend to accept/reject changes, including adding notes.
#'
#' @param id the shiny ID of the module
#' @param sondeproj A `reactiveVal` holding the current dataset.
#' @param mode A `reactiveVal` with the review mode (analyst, reviewer, everything)
#' @param y_var A `reactiveVal` holding the Y-variable to plot on the y-axis.
#' @param dd A `reactiveVal` holding the name of the dd in the changelog.
#' @param username A `reactiveVal` holding the name of the analyst for the changelog
#'
#' @returns
#' @rdname data-review
#' @md
#' @export
#' @keywords internal
#'

data_review_side <- function(id) {
  ns <- NS(id)
  tagList(
    #simple table to select change
    DT::DTOutput(NS(id, "review_table"))

  )
}

#' @rdname data-review
#' @export
data_review_bottom <- function(id) {
  ns <- NS(id)
  tagList(
    #return the pretty changelog info
  )
}

#' @rdname data-review
#' @export
data_review_server <- function(id, sondeproj, mode, y_var, dd, username) {
  moduleServer(id, function(input, output, session){
    #create review table
      review_tab <- reactive({
        req(sondeproj(), mode(), y_var())
        log <- sondeproj()$changelog #get log

        #get rows to show
        statuses <- switch(mode(),
                           "analyst" = c("rejected"),
                           "reviewer" = c("pending", "justified"),
                           "everything" = c("pending", "approved", "returned", "justified"))

        log %>% filter(.data$review_status %in% statuses & .data$parameter == y_var())
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

        DT::datatable(
          df,
          selection = list(mode = "single"),
          editable = FALSE,
          rownames = FALSE,
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

    #output a pretty view of data change




  }
  )}
