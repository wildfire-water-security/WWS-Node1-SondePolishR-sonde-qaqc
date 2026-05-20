#general code used select parameter for plotting and update based on data

# UI
#' @rdname update-parameters
#' @export
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
#' Shiny module to update plotting parameters
#'
#' Used to dynamically update the choices for selecting a y variable based on the names in the data or
#' a custom function.
#'
#' @param id An ID string passed to shiny::NS(), used for namespacing UI inputs/outputs.
#' @param sondeproj A `reactiveVal` holding the current dataset.
#' @param choices_fun Function used to determine the parameter choices, if `NULL` will use the column names of the data
#'
#' @returns the selected variable `y_var` as a reactive object
#' @md
#' @export
#' @keywords internal
#' @rdname update-parameters
update_parms_server <- function(id, sondeproj, choices_fun = NULL) {
  moduleServer(id, function(input, output, session) {

    data <- reactive({sondeproj()$data})

    # update parameter choices dynamically
    choices_r <- reactive({
      req(data())
      if (!is.null(choices_fun)) {
        choices_fun(data())
      } else {
        names(data())
      }
    })

    observeEvent(choices_r(), {
      choices <- choices_r()
      updateSelectInput(
        session,
        "y_var",
        choices = choices,
        selected = choices[[1]]
      )
    })

    #really just for tests, as in real life you can't select anything that's not in choices
    observeEvent(input$y_var, {
      req(input$y_var)
      if(!(input$y_var %in% choices_r())){stop("selected y variable not in choices")}
    })



    # return the reactive selected parameter
    return(reactive(input$y_var))
  })
}

