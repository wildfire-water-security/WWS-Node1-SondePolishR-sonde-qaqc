#' Keep track of visualized version and edits
#'
#' Keeps track of the version selected and returns the correct data to plot with any edits to view.
#'
#' @param id the shiny ID of the module
#' @param sondeproj A `reactiveVal` holding the current dataset.
#' @param y_var A `reactiveVal` holding the Y-variable to plot on the y-axis.
#' @param row A `reactiveVal` holding the number of the row selected in the changelog.
#'
#' @returns A list containing:
#'  - `current`: Reactive expression containing the version of the data corresponding to the selected changelog row.
#'  - `changed`: Reactive expression containing the previous values for #' the selected parameter when changes are being displayed.
#' @rdname data-versioning
#' @md
#' @export
#' @keywords internal
#'

data_version_UI <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      style = "display: flex; align-items: center; gap: 35px; margin-bottom: 20px;",
      actionButton(ns("undo_changes"), "Restore Selected Version"),
      span(
        "Select the row in the change log to ", strong("return"), " to.",
        style = "font-size: 0.85em;max-width: 200px;"
      )
    ),
    input_switch(
      ns("display_changes"),
      "Visualize Changed Data Points",
      value = FALSE
    )
  )
}

#' @rdname data-versioning
#' @export
data_version_server <- function(id, sondeproj, y_var, row) {
  moduleServer(id, function(input, output, session){
  #keep track of diffs to apply
    curr_diffs <- reactive({
      req(sondeproj())

      if(is.null(row())) {
        return(NULL)
      }

      diff_list <- sondeproj()$changelog$diff_name[(row()):nrow(sondeproj()$changelog)]
      diff_list <- diff_list[grepl("^dd", diff_list)]
      diffs <- sondeproj()$diffs[names(sondeproj()$diffs) %in% diff_list]

      diffs
    })

  #get version to plot or revert back to
    curr_ver <- reactive({
      req(sondeproj())
      undo_diffs <- curr_diffs()

      if(!is.null(undo_diffs) && !is.null(row()) && row() < nrow(sondeproj()$changelog)){
        undo_diffs <- undo_diffs[-1] #don't undo changed to get to selected version
        apply_diff(sondeproj()$data, undo_diffs,
                   id = c("DateTime_rd", "DupNum"),
                   invert = TRUE)
      }else{
        sondeproj()$data
      }

    })

  #keep track of changes to current version
    changed_points <- reactive({
      req(sondeproj())

      if(input$display_changes && !is.null(curr_diffs())){
        diff <- curr_diffs()[[1]] #get the changes to get to data

        #get previous values for thing being plotted
        yvar_diff <- diff[[y_var()]]

        #if it's data added plot that as green
        if(!is.null(yvar_diff)){
          yvar_diff <- yvar_diff %>% mutate(old = ifelse(.data$op_type %in% c("data_added", "data_merge"), .data$new, .data$old))
        }
        return(yvar_diff)

      }else{
        return(NULL)
      }
    })

  #keep track of if we are okay restoring changes
    observeEvent(input$undo_changes,{
      #only undo changes if something is selected
      if(!is.null(row()) && row() < nrow(sondeproj()$changelog)){
        ##confirmation here
        shinyalert::shinyalert(title = "Confirm restoring past data version",
                               text = "If you continue you will lose any edits made after the selected version.",
                               type = "warning",
                               showCancelButton = TRUE,
                               inputId = "conf")
      }else{
        shinyalert::shinyalert(title = "Select a Version to Restore",
                               text = "Select the row in the table to restore to\n(should not be the last row).",
                               type = "info")}
    })

  #reset version selected when project
    #restore a version
    observeEvent(input$conf,{
      if(input$conf == TRUE){
        proj <- sondeproj()

        #update dataset
        proj$data <- curr_ver()

        #remove extra diffs
        #get the diffs to apply
        #only get diffs if not current data
        if(row() < nrow(sondeproj()$changelog)){
          diff_list <- sondeproj()$changelog$diff_name[1:row()]
          diff_list <- diff_list[grepl("^dd", diff_list)]
          diffs <- sondeproj()$diffs[names(sondeproj()$diffs) %in% diff_list]}
        proj$diffs <- diffs

        #roll back changelog
        proj$changelog <- proj$changelog[1:row(),]

        #set as sondeproj
        sondeproj(proj)
      }
    })

  #return things to make plot
    list(
      current = curr_ver,
      changed = changed_points)
  }
  )}
