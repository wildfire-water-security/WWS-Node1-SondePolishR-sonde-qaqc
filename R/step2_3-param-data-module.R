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
#' @param data_ver A `reactiveVal` holding a number used to track when new data is added to trigger resets.
#' @param choices_fun Function used to determine the parameter choices, if `NULL` will use the column names of the data
#'
#' @returns the selected variable `y_var` as a reactive object
#' @md
#' @export
#' @keywords internal
#' @rdname update-parameters
update_parms_server <- function(id, sondeproj, data_ver, choices_fun = NULL) {
  moduleServer(id, function(input, output, session) {

  #only trigger when new data is added
  observeEvent(data_ver(), {
    req(data_ver() > 0)
    data <- sondeproj()$data

    # update parameter choices dynamically
      if(!is.null(choices_fun)) {
        choices_r <- choices_fun(data)
      }else {
        choices_r <- names(data)
      }

      updateSelectInput(
        session,
        "y_var",
        choices = choices_r,
        selected = choices_r[[1]]
      )

  })

    # return the reactive selected parameter
    return(reactive(input$y_var))
  })
}

