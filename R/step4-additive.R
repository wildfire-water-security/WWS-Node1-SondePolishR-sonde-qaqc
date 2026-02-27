#' @export
#' @rdname additive
additive_UI <- function(id){
  ns <- NS(id) #line to make module work

  tagList(

    sidebarLayout(
      sidebarPanel(
        SondePolishR::update_parms_UI(ns("update_parms")), #get parameters to view
        accordion(
          accordion_panel(
            title= "Shift Value",
            #get value to shift points
            numericInput(
              NS(id, "shift_val"),
              "Enter the value to shift selected data",
              value = 0,
              step=0.1)
          ),
          accordion_panel(
            title="Flag Points",
            SondePolishR::confirm_changes_UI(ns("flag2"))
          )
        )

      ),
      mainPanel(
        plotlyOutput(NS(id, "shift_plot")),
      ))

  )}

#' Address any additive shifts
#'
#' Plots loaded dataset, user can select a group of points and apply a additive shift to the data to correct for shifts.
#'
#' @param id An ID string passed to shiny::NS(), used for namespacing UI inputs/outputs.
#' @param prj_path A `reactiveVal` holding the path to save the project to.
#' @param sdata A `reactiveVal` holding the current dataset.
#' @param log A `reactiveVal` holding the change log.
#' @md
#' @keywords internal
#' @export
#' @rdname additive
#' @returns Invisible NULL
#'
additive_server <- function(id, sdata, prj_path, log){
  moduleServer(id, function(input, output, session){

    #initialize
      hover_reactive <- reactiveVal()

    #update choices based on data table
      y_var <- SondePolishR::update_parms_server("update_parms", sdata, choices_fun = nice_yvar)

    #select values via hover
      observe({
        req(sdata(), y_var())
        hover_data <- tryCatch(
          event_data("plotly_selected", source = "shift_plot"),
          error = function(e)
            NULL)
        if (!is.null(hover_data))
          hover_reactive(hover_data)
      })

    #select data (gets the rows selected by user)
      rows <- reactive({
        if (is.null(hover_reactive()) || nrow(hover_reactive()) == 0) {
          return(NULL)}   # no selection

        x <- as.POSIXct(hover_reactive()$x, tz = tz(sdata()$DateTime))
        y <- hover_reactive()$y

        rows <- numeric()
        for (i in 1:length(x)) {
          row <- sdata()$Index[sdata()$DateTime == x[i] &
                                      sdata()[y_var()] == y[i]]
          rows <- c(rows, row)}
        rows
      })

    #update shift val
      observeEvent(hover_reactive(),{
        #req(length(hover_reactive()) > 0)
        updateNumericInput(
          session,
          "shift_val",
          value = guess_shift(sdata(), y_var(), rows())
        )
      })

        #update shift_val to 0
          observeEvent(y_var(),{
            updateNumericInput(session,"shift_val",value = 0)
            plot_lyout <- reactiveValues(xaxis = list(), yaxis = list())})

        #take selected points and move
          data_plot <- reactive({
            req(sdata(), y_var())

            if (is.null(rows())) {
              sdata()  # return unchanged data if no rows selected
            } else {
              shift_points(sdata(), y_var(), rows(), input$shift_val)}
          })

        #preserve zoom
          plot_lyout <- preserve_zoom(data_plot, y_var, "shift_plot")

        #make plot
          output$shift_plot <- renderPlotly({
            req(data_plot(), y_var())
            if(is.null(hover_reactive())){
              p <- ggplot(data_plot(), aes(x=.data$DateTime, y=.data[[y_var()]])) + geom_point()
            }else{
              base <- data_plot()[!(data_plot()$Index %in% rows()),]
              adj <- data_plot()[data_plot()$Index %in% rows(),]
              p <- ggplot() + geom_point(data=base, aes(x=.data$DateTime, y=.data[[y_var()]])) +
               geom_point(data=adj, aes(x=.data$DateTime, y=.data[[y_var()]]), color="red")
            }
            p <- p  %>% ggplotly(source = "shift_plot") %>% layout(dragmode = "select") %>%
              event_register("plotly_selected") %>% event_register("plotly_relayout")

            #adjust axis if needed
            if (length(plot_lyout$xaxis) > 0 || length(plot_lyout$yaxis) > 0) {
              p <- layout(p,xaxis = plot_lyout$xaxis,yaxis = plot_lyout$yaxis)}

            p
            })

    #confirm changes
    SondePolishR::confirm_changes_server(
          id = "flag2",
          sdata = sdata,
          index = rows,
          par = y_var,
          flag_name = "abs_shift",
          note = paste0("absolute shift of ", input$shift_val),
          prj_path = prj_path,
          log = log)
  })
}

