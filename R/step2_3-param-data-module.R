#general code used select parameter for plotting and update based on data

# UI
#' @rdname update-parameters
#' @export
update_parms_UI <- function(id, input_id = "y_var", text="Select Parameter to Plot:") {
  ns <- NS(id)
  tagList(
    selectInput(
      ns(input_id),
      text,
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
#' @param y_var Y-variable to plot on the y-axis.
#' @param text Text associated with the UI

#' @returns the selected variable `y_var` as a reactive object
#' @md
#' @export
#' @keywords internal
#' @rdname update-parameters
update_parms_server <- function(id, sondeproj, data_ver, y_var,input_id = "y_var",choices_fun = NULL) {
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

    if(input_id == "y2_var"){
      include_precip <- !is.null(sondeproj()$precip)
      if(include_precip){
        choices_r <- c("None" = "none", "Raw Data" = "raw", "Precipitation" = "precip", choices_r)
      }else{
        choices_r <- c("None" = "none","Raw Data" = "raw", choices_r)
      }
    }

      updateSelectInput(
        session,
        input_id,
        choices = choices_r,
        selected = choices_r[[1]]
      )

  })

  #update y_var when the input changes
    observeEvent(input[[input_id]], {
      y_var(input[[input_id]])
    })

  #update user UI
    observe({
      req(y_var())
      if(!identical(input[[input_id]], y_var())){
        updateSelectInput(
          session,
          input_id,
          selected = y_var())}
    })

    return(reactive(y_var()))

  })
}

