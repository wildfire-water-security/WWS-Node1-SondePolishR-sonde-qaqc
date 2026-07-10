#' View data by a specified time length
#'
#' Used to adjust plotting to show periods to better examine details.
#'
#' @param id the shiny ID of the module
#' @param sondeproj A `reactiveVal` holding the current dataset.
#' @param data_ver A `reactiveVal` holding a number used to track when new data is added to trigger resets.
#' @param dates The date range to view the data.
#' @param period_view Should data be viewed by period?
#' @param n_length The length of the period to view.
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
      numericInput(ns("p_length"),"Period Length (days):",value = 7,min = 1)
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
weekly_range_server <- function(id, sondeproj, period_view, dates, p_length, data_ver){
  moduleServer(id, function(input, output, session){

  abs_dates <- reactiveVal() #store absolute dates for project

  ##manage the date range --
      #get total range of data (only update if data version changes)
      observeEvent(data_ver(), {
        req(sondeproj())
        abs <- c(
          min(sondeproj()$data$Date, na.rm = TRUE),
          max(sondeproj()$data$Date, na.rm = TRUE))

        abs_dates(abs)
        dates(abs)   # initialize current selection
      })

    #update user input
      observeEvent(dates(), {
        req(dates())
        if (!identical(input$dates, dates())){
          updateDateRangeInput(session,"dates",
                               start = dates()[1],end = dates()[2],
                               min   = abs_dates()[1],max = abs_dates()[2])
        }
      })

    #update date value
      observeEvent(input$dates,{
        if(!identical(input$dates, dates())){
          dates(input$dates)
        }}, ignoreInit = TRUE)

  ##sync the period slider --
    observeEvent(period_view(),{
      if(!identical(period_view(), input$period_view)){
        update_switch(id="period_view", value=period_view(), session=session)
      }})
    observeEvent(input$period_view,{
      if(!identical(period_view(), input$period_view)){
        period_view(input$period_view)
      }})

  ##sync the period value --
    observeEvent(p_length(),{updateNumericInput(session, "p_length", value=p_length())})
    observeEvent(input$p_length,{p_length(input$p_length)})

  ## return plotting date range based on selections --
    plot_date <- reactive({
      if(period_view()){
        start <- dates()[1] + period((p_length()*(period_n()-1)), "days")
        end <- start + (p_length()-1)
        c(start, end)

      }else{
        dates()
      }})

  #determine total number of periods
    total <- reactive({
      total <- ceiling(as.numeric(dates()[2] - dates()[1])/ p_length())
    })

  #keep track of what period we're on
    period_n <- reactiveVal(1) #stores what period we're on

    #reset period length when period/range changes to prevent being way out of date
    observeEvent(list(p_length(), dates()),{
      period_n(1)
    })

    #update period counter
    observeEvent(input$next_period,{
      if(period_n() < total()){period_n(period_n() + 1)}})

    observeEvent(input$prev_period,{
      if(period_n() > 1){period_n(period_n() - 1)}})

    #decrease by 1

  #show what data period you're on
    output$n_period <- renderUI({
      if(period_view()){
        req(period_n(), total())
        #n <- ceiling((as.numeric(plot_date()[1] - dates()[1])+p_length())/p_length())
        div(style = paste0("color: #6c7881; font-size: 14px;"),paste(period_n(), "/", total()))
      }})

  #return ranges for plotting
    plot_date

  })

}
