#' View data by a specified time length
#'
#' Used to adjust plotting to show periods to better examine details.
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
    bslib::layout_columns(
      col_widths = c(7, 1, 4),
      dateRangeInput(ns("dates"),"Date Range"),
      tags$div(
        style = "width: 1px; height: 85px; background-color: #6c7881; display: inline-block; margin: 0 15px; vertical-align: middle;"
      ),
      numericInput(ns("length"),"Period Length (days):",value = 7,min = 1)
      ),
      input_switch(ns("period_view"),"View data by period",value = FALSE)

  )}

#' @rdname weekly-range
#' @export
weekly_range_buttons_UI  <- function(id){
  ns <- NS(id)

  tagList(
      fluidRow(
      column(2,
        actionButton( ns("prev_period"),"Previous Period")),
      column(8),
      column(2,actionButton(ns("next_period"), "Next Period") ,
             htmlOutput(ns("n_period"))))

  )}

#' @rdname weekly-range
#' @export
weekly_range_server <- function(id, min_date, max_date){
  moduleServer(id, function(input, output, session){

  #store weekly values here so we can refer to them even when replotting
    period_view <- reactiveVal(FALSE)
    period_start <- reactiveVal(NULL)

  #keep track of if user wants weekly view, update starting date
    observeEvent(input$period_view, {
      period_view(input$period_view)
      if(input$period_view){
        if(is.null(period_start()))
          period_start(min_date())
      }else{
        period_start(NULL)
      }
    })

  #update date range when weeks are clicked
    observeEvent(input$next_period, {
      req(period_start())
      start <- period_start() + input$length
      start <- min(start, max_date() - (input$length -1))
      period_start(start)
    })

    observeEvent(input$prev_period, {
      req(period_start())
      start <- period_start() - input$length
      start <- max(start, min_date())
      period_start(start)
    })

  #update UI
    observe({
      req(min_date(), max_date())
      if (period_view()) {
        start <- period_start()

        if(is.null(start)){start <- min_date()}
          start <- max(start, min_date())
          start <- min(start, max_date() - (input$length-1))

          updateDateRangeInput(session,"dates",
            start = start,end = start + (input$length-1))


      }else {
        updateDateRangeInput(session,"dates",
          start = min_date(),end = max_date())
      }
    })

    output$n_period <- renderUI({
        total <- ceiling(as.numeric(max_date() - min_date())/ input$length)

        if(period_view()){
          req(period_start())
          n <- ceiling((as.numeric(period_start() - min_date())+input$length)/input$length)
          div(style = paste0("color: #6c7881; font-size: 14px;"),paste(n, "/", total))
        }

    })
    reactive(input$dates)

  })

}
