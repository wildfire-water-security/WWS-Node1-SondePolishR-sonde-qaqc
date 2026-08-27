#' View data by a specified time length
#'
#' Used to adjust plotting to show periods to better examine details.
#'
#' @param id the shiny ID of the module
#' @param sondeproj A `reactiveVal` holding the current dataset.
#' @param data_ver A `reactiveVal` holding a number used to track when new data is added to trigger resets.
#' @param view_state A `reactiveVal` holding a list of items specifying the view state:
#'  - dates: The range of dates being viewed via the date selector
#'  - period_view: Logical if the period view is being used
#'  - period_length: Length of period view
#'  - period_n: The period number to view.
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
weekly_range_server <- function(id, sondeproj, data_ver, view_state){
  moduleServer(id, function(input, output, session){

  #keep track of what period we're on
    period_n <- reactiveVal(1) #stores what period we're on

  #update user input when view state changes
      observeEvent(view_state(), {
        req(view_state(), input$dates)

        view_dat <- view_state()
        #reset starting period if date range or period length changes
        if(!identical(input$dates, view_dat$dates)|
           !identical(view_dat$period_length, input$p_length)){
            period_n(1)
        }

        #update date ranges
        if(!identical(input$dates, view_dat$dates)){
          updateDateRangeInput(session,"dates",
                               start = view_dat$dates[1],end = view_dat$dates[2],
                               min   = view_dat$abs_dates[1],max = view_dat$abs_dates[2])
        }

        #update period view
        if(!identical(view_dat$period_view, input$period_view)){
          update_switch(id="period_view", value=view_dat$period_view, session=session)
        }

        #update period length
        if(!identical(view_dat$period_length, input$p_length)){
          updateNumericInput(session, "p_length", value=view_dat$period_length)
          }

        #update period being viewed
        if(!identical(view_dat$period_n, period_n())){
          period_n(view_dat$period_n)
        }
      },ignoreInit = TRUE)

    #update view state if user changes values in the module
      observeEvent(input$dates,{
        if(!identical(input$dates, view_state()$dates)){
          update_view_state(view_state, dates = input$dates)}
        },ignoreInit = TRUE)

      observeEvent(input$period_view,{
        if(!identical(input$period_view, view_state()$period_view)){
          update_view_state(view_state, period_view = input$period_view)}
      },ignoreInit = TRUE)

      observeEvent(input$p_length,{
        if(!identical(input$p_length, view_state()$period_length)){
          update_view_state(view_state, period_length = input$p_length)

          #reset the period being viewed
          period_n(1)
          update_view_state(view_state, period_n = period_n())
        }
      },ignoreInit = TRUE)

      observeEvent(period_n(),{
        if(!identical(period_n(), view_state()$period_n)){
          update_view_state(view_state, period_n = period_n())}
      },ignoreInit = TRUE)

  ## return plotting date range based on selections --
    plot_date <- reactive({
      view_dat <- view_state()

      if(view_dat$period_view){
        start <- view_dat$dates[1] + period((view_dat$period_length*(period_n()-1)), "days")
        end <- start + (view_dat$period_length-1)
        c(start, end)

      }else{
        view_dat$dates
      }})

  #determine total number of periods
    total <- reactive({
      view_dat <- view_state()

      total <- ceiling(as.numeric(view_dat$dates[2] - view_dat$dates[1])/ view_dat$period_length)
    })

    #update period counter
    observeEvent(input$next_period,{
      if(period_n() < total()){period_n(period_n() + 1)}})

    observeEvent(input$prev_period,{
      if(period_n() > 1){period_n(period_n() - 1)}})

    #decrease by 1

  #show what data period you're on
    output$n_period <- renderUI({
      view_dat <- view_state()

      if(view_dat$period_view){
        req(period_n(), total())
        #n <- ceiling((as.numeric(plot_date()[1] - dates()[1])+p_length())/p_length())
        div(style = paste0("color: #6c7881; font-size: 14px;"),paste(period_n(), "/", total()))
      }})

  #return ranges for plotting
    plot_date

  })

}
