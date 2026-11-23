#' Keep track of visualized version and edits
#'
#' Keeps track of the version selected and returns the correct data to plot with any edits to view.
#'
#' @param id the shiny ID of the module
#' @param sondeproj A `reactiveVal` holding the current dataset.
#' @param y_var A `reactiveVal` holding the Y-variable to plot on the y-axis.
#' @param dd A `reactiveVal` holding the name of the dd in the changelog.
#' @param username A `reactiveVal` holding the name of the analyst for the changelog
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
      actionButton(ns("undo_changes"), "Undo Selected Change"),
      span(
        "Select the row in the change log to ", strong("undo"), ".",
        style = "font-size: 0.85em;max-width: 200px;"
      )
    )
  )
}

#' @rdname data-versioning
#' @export
data_version_server <- function(id, sondeproj, y_var, dd, username) {
  moduleServer(id, function(input, output, session){
  #keep track of diffs to apply
    curr_diffs <- reactive({
      req(sondeproj())

      if(is.null(dd())){return(NULL)}
      changelog <- sondeproj()$changelog
      diffs <- sondeproj()$diffs

      row <- which(dd() == changelog$diff_name)
      if(length(row) == 0){return(NULL)} #guard against now having a diff to get

      diff_list <- changelog$diff_name[(row):nrow(changelog)] #gets the row before for the pre-correction line
      diff_list <- diff_list[grepl("^dd", diff_list)]
      diffs <- diffs[names(diffs) %in% diff_list]

      diffs
    })

  #get version to plot or revert back to
    curr_ver <- reactive({
      req(sondeproj())
      undo_diffs <- curr_diffs()
      changelog <- sondeproj()$changelog
      row <- which(dd() == changelog$diff_name)


      if(!is.null(undo_diffs) && !is.null(row) && dd() != tail(sondeproj()$changelog$diff_name, n=1)){
        undo_diffs <- undo_diffs[-1] #don't undo changed to get to selected version
        apply_diff(sondeproj()$data, undo_diffs,
                   id = c("DateTime_rd", "DupNum"),
                   invert = TRUE)
      }else{
        sondeproj()$data
      }

    })

  #get previous version to show the line of the data before the change
    prev_ver <- reactive({
      req(sondeproj())
      undo_diffs <- curr_diffs()
      changelog <- sondeproj()$changelog
      row <- which(dd() == changelog$diff_name)

      if(!is.null(undo_diffs) && !is.null(row)){
        undo_diffs <- undo_diffs #do undo changed to get to previous version
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

      if(!is.null(curr_diffs())){
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

  #return things to make plot
    list(
      current = curr_ver,
      previous = prev_ver,
      changed = changed_points)
  }
  )}
