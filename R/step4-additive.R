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
            title= "Shift Values",
            #get value to shift points
            fluidRow(
              div(style="margin-bottom: 8px; font-size:14px",
                  "Adjust the slope and intercept to shift the selected data:"),
              numericInput(
              NS(id, "slope_val"),
              "Slope",
              value = 0,
              step=0.1),
              numericInput(
                NS(id, "int_val"),
                "Intercept",
                value = 0,
                step=0.1)

          )),
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
      newdata <- reactiveVal() #stores updated dataset when shift is made
      index <- reactiveVal() #stores index of selected points
      observeEvent(sdata(), {newdata(sdata())}, ignoreInit = TRUE) #make newdata look like sdata when sdata changes

    #update choices based on data table
      y_var <- SondePolishR::update_parms_server("update_parms", sdata, choices_fun = nice_yvar)

    #select values via hover (gets the index selected by user)
      observe({
        req(sdata(), y_var())
        hover_data <- event_data("plotly_selected", source = "shift_plot")

        if(!is.null(hover_data) && length(hover_data) > 0  && nrow(hover_data) > 0){
          x <- as.POSIXct(hover_data$x, tz = tz(sdata()$DateTime))
          y <- hover_data$y

          ind <- numeric()
          for (i in 1:length(x)) {
            row <- sdata()$Index[sdata()$DateTime == x[i] &
                                   sdata()[y_var()] == y[i]]
            ind <- c(ind, row)}

          index(ind)
        }
      })

    #update shift val
      observeEvent(index(),{
        updateNumericInput(
          session,
          "slope_val",
          value = guess_shift(sdata(), y_var(), index())$slope)

        updateNumericInput(
          session,
          "int_val",
          value = guess_shift(sdata(), y_var(), index())$int)
      })

        #update vals to 0
          observeEvent(y_var(),{
            updateNumericInput(session,"slope_val",value = 0)
            updateNumericInput(session,"int_val",value = 0)

            plot_lyout <- reactiveValues(xaxis = list(), yaxis = list())})

        #take selected points and move
          data_plot <- reactive({
            req(sdata(), y_var())

            if (is.null(index())) {
              sdata()  # return unchanged data if no rows selected
            } else {
              shift_points(sdata(), y_var(), index(), list(slope=input$slope_val, int=input$int_val))}
          })

        #preserve zoom
          plot_lyout <- preserve_zoom(data_plot, y_var, "shift_plot")

        #make plot
          output$shift_plot <- renderPlotly({
            req(data_plot(), y_var())
            if(is.null(index())){
              p <- ggplot(data_plot(), aes(x=.data$DateTime, y=.data[[y_var()]])) + geom_point()
            }else{
              base <- data_plot()[!(data_plot()$Index %in% index()),]
              adj <- data_plot()[data_plot()$Index %in% index(),]
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


    #make new dataset
       observeEvent(index(), {
            req(index(), y_var(), is.data.frame(newdata()))
            colnum <- which(colnames(newdata()) == y_var())
            updated <- newdata()
            updated[index(),colnum] <- (updated[index(),colnum] * input$slope_val) + input$int_val
            newdata(updated)
          })

    #confirm changes
    SondePolishR::confirm_changes_server(
          id = "flag2",
          newdata = newdata,
          sdata = sdata,
          index = index,
          par = y_var,
          flag_name = "abs_shift",
          note = paste0("shift with slope of ", input$slope_val, "and intercept of ",input$int_val),
          prj_path = prj_path,
          log = log)
  })
}

