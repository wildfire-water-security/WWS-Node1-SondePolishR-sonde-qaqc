#module to flag data and write a new version
#default note: "NOTE: To prevent a point from being flagged, select the appropriate row in the table"
#' Shiny module to added flags and save changes to  data
#'
#' The UI creates a button to allow user to save the changes made to the data. The server function will provide a message to the user to
#' let them know the changes (or lack of changes made), add flags to the dataset, and save the dataset as a new version.
#'
#' @param id the shiny ID of the action button
#' @param sondeproj A `reactiveVal` holding the current `sondeproj`.
#' @param edit A `reactiveVal` holding a list of length six:
#' - data: new updated data as a `data.frame`
#' - rows: logical vector which specifies rows changed a TRUE
#' - y_var: parameter being edited
#' - step: name of the editing step for the changelog
#' - note: an optional note to add to the changelog
#' - flag: character flag to use for edits to the data
#'
#' @rdname apply-edit
#' @export
#' @keywords internal
#'
apply_edit_UI <- function(id, note=NULL) {
  ns <- NS(id)

  tagList(
    tags$h5("Flag Points"),

    div(style="margin-bottom: 8px; font-size:14px",
        "Points highlighted in the plot will be flagged"),

    if(!is.null(note)){
      div(style="margin-top: 8px; margin-bottom: 8px;font-size:10px",
          note)
    },
    div(style="margin-bottom: 8px",
        textInput(ns("flag_notes"), "Analyst Notes (optional):",
                  value = "",
                  placeholder = "Enter text...")),
    actionButton(ns("apply_flags"), "Flag Points")


  )
}

#' @rdname apply-edit
#' @export
apply_edit_server <- function(id, sondeproj, edit){
  moduleServer(id, function(input, output, session) {

  #when button is hit, apply flags, and edit data
    observeEvent(input$apply_flags, {
      req(sondeproj(), edit())

    edit2 <- edit()
    #update note with any user text
      if(input$flag_notes != ""){
        edit2$note <- paste(edit2$note, input$flag_notes, sep="; ")
      }

    #log edits
      proj <- apply_edit(sondeproj(), edit2)

    #update sondeproj
      sondeproj(proj)

    #clear user note
      updateTextInput(session,"flag_notes",value = "")

   # return(apply = reactive(input$apply_flags))

      })

  })
}
