# pulling out the weekly plotting as it's own module because it's a fair bit of code

#' Return weekly date ranges
#'
#' Used to adjust plotting to show weekly periods to better examine details.
#'
#' @param id the shiny ID of the module
#' @param min_date Minimum date in the `sondeproj`.
#' @param max_date Maximum date in the `sondeproj`.
#'
#' @returns a reactive of length two with the min and max dates.
#' @rdname weekly-range
#' @export
#' @keywords internal
#'
#'
weekly_range_sidebar_UI <- function(id){
  ns <- NS(id)
  tagList(
    tags$h5("Set Date Range"),
    dateRangeInput(ns("dates"),"Date Range"),
    input_switch(ns("week_view"),"View Data Weekly",value = FALSE)
  )}

#' @rdname weekly-range
#' @export
weekly_range_buttons_UI  <- function(id){
  ns <- NS(id)

  tagList(
      fluidRow(
      column(2,
        actionButton( ns("prev_week"),"Previous Week")),
      column(8),
      column(2,actionButton(ns("next_week"), "Next Week")))

  )}

#' @rdname weekly-range
#' @export
weekly_range_server <- function(id, min_date, max_date){
  moduleServer(id, function(input, output, session){

    observe({
      req(min_date(), max_date())

      updateDateRangeInput(
        session,"dates",
        start = min_date(),
        end   = max_date()
      )
    })

    #adjust date bounds
    observeEvent(input$week_view, {
      req(min_date(), max_date())

      if(input$week_view) {

        start <- min_date()
        end   <- start + 7

        updateDateRangeInput(
          session,"dates",
          start = start, end = end)

        updateActionButton(inputId = "next_week", disabled = FALSE)
        updateActionButton(inputId = "prev_week", disabled = FALSE)
      }else{
        updateDateRangeInput(
          session,"dates",
          start = min_date(),
          end   = max_date())

        updateActionButton(inputId = "next_week", disabled = TRUE)
        updateActionButton(inputId = "prev_week", disabled = TRUE)
      }
    })


    #update date range when buttons for next and previous clicked
    shift_week <- function(direction = c("prev", "next")) {
      direction <- match.arg(direction)
      req(input$dates)

      step <- if(direction == "prev") -7 else 7
      start <- input$dates[1] + step

      start <- max(start, min_date())
      start <- min(start, max_date() - 7)

      end <- start + 7

      updateDateRangeInput(session,"dates",
                           start = start,end = end)

    }

    observeEvent(input$next_week, shift_week("next"))
    observeEvent(input$prev_week, shift_week("prev"))

    reactive(input$dates)

  })

}
