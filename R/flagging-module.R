#module to flag data and write a new version
#default note: "NOTE: To prevent a point from being flagged, select the appropriate row in the table"
#' Shiny module to added flags and save changes to  data
#'
#' The UI creates a button to allow user to save the changes made to the data. The server function will provide a message to the user to
#' let them know the changes (or lack of changes made), add flags to the dataset, and save the dataset as a new version.
#'
#' @param id the shiny ID of the action button
#' @param note an optional note to add to the action button to provide more directions
#' @param data the sonde data.frame that will be flagged (reactive)
#' @param index the index values for the rows to be flagged
#' @param prj_path the file path to save the sonde project to (reactive)
#' @param par the parameter to flag
#' @param flag_name a character with the name of the flag
#' @param index the index values of the rows to flag in data
#' @param log A `reactiveVal` holding the change log.

#' @rdname confirm-changes
#' @export
#' @keywords internal
#'
confirm_changes_UI <- function(id, note=NULL) {
  ns <- NS(id)

  tagList(
    div(style="margin-bottom: 8px; font-size:14px",
        "Points highlighted in the plot will be flagged"),

    if(!is.null(note)){
      div(style="margin-top: 8px; margin-bottom: 8px;font-size:10px",
          note)
    },
    actionButton(NS(id, "rm_points"), "Flag Points")

  )
}

#' @rdname confirm-changes
#' @export
confirm_changes_server <- function(id, data, index=NULL, par, flag_name, prj_path, log){

  # data: reactiveVal of the dataframe to update
  # data_plot: reactive that provides data with $outlier$Index
  # y_var: reactive of the column to flag
  # flag_name: name of flag column to add
  # prj_path: reactiveval project path for saving changes

  moduleServer(id, function(input, output, session) {

    updated_data <- data # start with the original data

    # When button is clicked, update data in place
    observeEvent(input$rm_points,{
      req(data(), par()) #ensure we have what we need

      #check if there's a project path, if no error
      if(is.null(index()) || length(index()) == 0){
        # only show alert if running in shiny
        if (interactive()) {
          shinyalert::shinyalert(
            title = "Nothing selected",
            text = "No points were selected to flag",
            type = "warning"
          )
        }}else if(length(prj_path()) == 0){
          # only show alert if running in shiny
          if (interactive()) {
            shinyalert::shinyalert(
              title = "No Project Path",
              text = "Specify the project path in 1. Load Data",
              type = "error"
            )
          }       #see if there are points selected, if not warn
        }else{
          #add flags to data and save
            updated <- flag_data(data(),
                                 par = par(),
                                 index = index(),
                                 flag_name = flag_name,
                                 prj_path = prj_path())


            #update data
            updated_data(updated)

            #update log
            log(get_log())

            }
            })

    return(updated_data)

    })
  }

