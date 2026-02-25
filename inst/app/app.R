library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(plotly)
library(shinyFiles)
library(DT)

## TODO:
  #just marking bad points


#ensure plots inherit theme
  thematic::thematic_shiny()

#getting timezones and making nice
  tz <- SondePolishR:::nice_tz()

#front end
ui <-  page_fillable(
  #set theme
  theme = bs_theme(preset = "superhero",
                   primary = "#E3795E"),

  #set menu and navigation
    navset_card_pill(
      #step 1: load data
      nav_panel("1. Load Data",
                SondePolishR::load_data_UI("data1"),
                ),
      nav_panel("2. Visualize",
                SondePolishR::explore_data_UI("data2")
                ),
      nav_panel("3. Physical Limits",
                SondePolishR::limits_UI("data3")
                ),
      nav_panel("4. Shift Correction",
                sidebarLayout(
                  sidebarPanel(
                    SondePolishR::update_parms_UI("update_parms"), #get parameters to view
                    accordion(
                      accordion_panel(
                        title= "Shift Value",
                        #get value to shift points
                        numericInput(
                          "shift_val",
                          "Enter the value to shift selected data",
                          value = 0,
                          step=0.1)
                      ),
                      accordion_panel(
                        title="Flag Points",
                        SondePolishR::confirm_changes_UI("flag2")
                      )
                    )

                     ),
                  mainPanel(
                     plotlyOutput("shift_plot"),
                  ))

                ),
      nav_panel("5. Manual Removal", "Remove points manually"),
      nav_panel("6. Interpolation", "Interpolate Missing Data"),
      nav_panel("7. fDOM Corrections", "fDOM Corrections"),
      nav_panel("8. Download Data", "Download Processed Data")

  ))

#backend
#' Title
#'
#' @param input
#' @param output
#' @param session
#'
#' @returns
#' @export
#'
#' @examples
server <- function(input, output, session) {
  #define things that get passed around
    prj_path <- reactiveVal(NULL) #the project path to save data to
    data <- reactiveVal(NULL) #the current dataset
    log <- reactiveVal(NULL) #the data log

  #step 1: load data
   SondePolishR::load_data_server("data1", data, prj_path, log)

  #step 2: plot data
   SondePolishR::explore_data_server("data2", data, log)

  #step 3: physical limits
   SondePolishR::limits_server("data3", data, prj_path, log)

  #step 4: shift points up
     #make a copy of file for this step
  #      data_shift <- reactive({
  #        data()})
  #
  #    #make a version to plot
  #      data_plot <- reactive({data_shift()})
  #
  #    #get y_vars
  #      y_var <- update_parms_server("update_parms", data, choices_fun = nice_yvar)
  #
  #    #select values via hover
  #      hover_reactive <- reactiveVal()                 ## initialize
  #      observe({
  #        req(data_shift(), y_var())
  #
  #        hover_data <- tryCatch(
  #          event_data("plotly_selected", source = "shift_plot"),
  #          error = function(e) NULL
  #        )
  #
  #        if (!is.null(hover_data))
  #          hover_reactive(hover_data)                  ## set
  #      })
  #
  #
  #   #select data (gets the rows selected by user)
  #     rows <- reactive({
  #       if (is.null(hover_reactive()) || nrow(hover_reactive()) == 0) {
  #         return(NULL)   # no selection
  #       }
  #
  #       x <- as.POSIXct(hover_reactive()$x, tz=tz(data_shift()$DateTime))
  #       y <- hover_reactive()$y
  #
  #       rows <- numeric()
  #       for(i in 1:length(x)){
  #         row <- data_shift()$Index[data_shift()$DateTime == x[i] & data_shift()[y_var()] == y[i]]
  #         rows <- c(rows, row)
  #       }
  #
  #       rows
  #
  #     })
  #
  #     #update shift val
  #       observeEvent(hover_reactive(),{
  #         updateNumericInput(
  #           session,
  #           "shift_val",
  #           value = guess_shift(data_shift(), y_var(), rows())
  #         )
  #       })
  #
  #     #update shift_val to 0
  #       observeEvent(y_var(),{
  #         updateNumericInput(
  #           session,
  #           "shift_val",
  #           value = 0
  #         )
  #
  #         plot_lyout <- reactiveValues(xaxis = list(), yaxis = list())
  #       })
  #
  #     #take selected points and move
  #       data_plot <- reactive({
  #         req(data_shift(), y_var())
  #
  #         if (is.null(rows())) {
  #           data_shift()  # return unchanged data if no rows selected
  #         } else {
  #           shift_points(data_shift(), y_var(), rows(), input$shift_val)
  #         }
  #       })
  #
  #     #preserve zoom
  #       plot_lyout <- preserve_zoom(data_plot, y_var, "shift_plot")
  #
  #     #make plot
  #       output$shift_plot <- renderPlotly({
  #         req(data_plot(), y_var())
  #         if(is.null(hover_reactive())){
  #           p <- ggplot(data_plot(), aes(x=.data$DateTime, y=.data[[y_var()]])) + geom_point()
  #         }else{
  #           base <- data_plot()[!(data_plot()$Index %in% rows()),]
  #           adj <- data_plot()[data_plot()$Index %in% rows(),]
  #           p <- ggplot() + geom_point(data=base, aes(x=.data$DateTime, y=.data[[y_var()]])) +
  #            geom_point(data=adj, aes(x=.data$DateTime, y=.data[[y_var()]]), color="red")
  #         }
  #         p <- p  %>% ggplotly(source = "shift_plot") %>% event_register("plotly_selected") %>% event_register("plotly_relayout")
  #
  #         #adjust axis if needed
  #         if (length(plot_lyout$xaxis) > 0 || length(plot_lyout$yaxis) > 0) {
  #           p <- layout(p,
  #                       xaxis = plot_lyout$xaxis,
  #                       yaxis = plot_lyout$yaxis
  #           )}
  #         p
  #
  #       })
  #
  #      #output$text <- renderPrint({plot_lyout$yaxis$range})  ## get [remove later on]
  # #confirm changes
  #   confirm_changes_server(
  #         id = "flag2",
  #         data = data_plot,
  #         index = rows,
  #         par = y_var,
  #         flag_name = "abs_shift",
  #         prj_path = prj_path
  #       )
}

#create app
shinyApp(ui = ui, server = server)
