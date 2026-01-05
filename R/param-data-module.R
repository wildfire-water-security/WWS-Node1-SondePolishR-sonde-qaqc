#general code used select parameter for plotting and update based on data

# UI
update_parms_UI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(
      ns("y_var"),
      "Select Parameter to Plot:",
      choices = NULL
    ),
  )
}

# Server
update_parms_server <- function(id, df, choices_fun = NULL) {
  moduleServer(id, function(input, output, session) {

    # update parameter choices dynamically
    observeEvent(df(), {
      req(df())
      choices <- if (!is.null(choices_fun)) {
        choices_fun(df())
      } else {
        names(df())
      }
      updateSelectInput(session, "y_var", choices = choices)
    })

    # return the reactive selected parameter
    return(reactive(input$y_var))
  })
}
