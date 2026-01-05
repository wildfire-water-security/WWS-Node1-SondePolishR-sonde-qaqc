##step 2 of the app: exploratory plotting with plotly

# UI Function
explore_data_UI <- function(id){
  ns <- NS(id) #line to make module work

  tagList(
    sidebarLayout(
      sidebarPanel(
        #select parameter to plot
        update_parms_UI(ns("update_parms")),

        tags$h3("To Explore Data:"),
        HTML("1. Click and drag to zoom in<br>
                   2. Double click to zoom out<br>
                   3. Hover to see values")
      ),

      mainPanel(
        #add plot
        plotlyOutput(NS(id,"exp_plot"))
      ))
  )
}

# Server Function
explore_data_server <- function(id, df){
  moduleServer(id, function(input, output, session){
    #get column names after file upload (dynamic)
    y_var <- update_parms_server("update_parms", df, choices_fun = nice_yvar)

    #create plot
    output$exp_plot <- renderPlotly({
      req(df(), y_var())
      ggplot2::ggplot(df(), ggplot2::aes(x=.data$DateTime, y=.data[[y_var()]])) +
        ggplot2::geom_point()
    })

  })
}
