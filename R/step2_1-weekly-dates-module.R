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

  #store weekly values here so we can refer to them even when replotting
    week_view <- reactiveVal(FALSE)
    week_start <- reactiveVal(NULL)

  #keep track of if user wants weekly view, update starting date
    observeEvent(input$week_view, {
      week_view(input$week_view)
      if(input$week_view){
        if(is.null(week_start()))
          week_start(min_date())
      }else{
        week_start(NULL)
      }
    })

  #update date range when weeks are clicked
    observeEvent(input$next_week, {
      req(week_start())
      start <- week_start() + 7
      start <- min(start, max_date() - 6)
      week_start(start)
    })

    observeEvent(input$prev_week, {
      req(week_start())
      start <- week_start() - 7
      start <- max(start, min_date())
      week_start(start)
    })

  #update UI
    observe({
      req(min_date(), max_date())
      if (week_view()) {
        start <- week_start()

        if(is.null(start)){start <- min_date()}
          start <- max(start, min_date())
          start <- min(start, max_date() - 6)

          updateDateRangeInput(session,"dates",
            start = start,end = start + 6)
      }else {
        updateDateRangeInput(session,"dates",
          start = min_date(),end = max_date())
      }
    })

    reactive(input$dates)

  })

}
