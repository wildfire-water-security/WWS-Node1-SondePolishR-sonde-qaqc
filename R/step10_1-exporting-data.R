#' File save selector UI
#'
#' @param id module id
#' @param label button label
#' @param title dialog title
#' @param filetype file extension
#' @param data data to save to specified file on click
#' @param startname the default name for the file
#' @param data_ver A `reactiveVal` holding a number used to track when new data is added to trigger resets.
#' @param username A `reactiveVal` holding the name of the analyst for the changelog
#' @param type a `reactiveVal` holding a character specifying the type of data being saved options include
#' "sondeproj", "data", "dups", "gaps", "changelog", "precip", "calcheck", "fieldform", "change-report"
#' @rdname file-export
#' @export
#' @md
#' @keywords internal
save_path_UI <- function(id,
                         button_label = "Export") {
  ns <- NS(id)

  tagList(
      div(
        class = "d-flex align-items-start gap-2",
        uiOutput(ns("save")),
        div(style = "flex-grow:1; min-width:0;",
            uiOutput(ns("path_text")))),
        div(
          class = "text-left",
          actionButton(ns("export"),button_label, width = "220px"))
)

}

#' @rdname file-export
save_path_server <- function(id, data,
                             startname = "sonde_export",
                             label = "Choose Location",
                             title = "Select save path",
                             filetype = reactiveVal(".csv"),
                             username, type,
                             data_ver) {
  moduleServer(id, function(input, output, session) {

    ns = session$ns #needed to make updating UI work

  parsed_path <- reactiveVal() #initialize so box always shows
  save_okay <- reactiveVal(FALSE) #store if we can actual save file

  #define preset roots for file path
    roots <- reactive({
      c(
        "Working Directory" = getOption("SondePolishR.default_path"),
        Downloads = file.path(fs::path_home(), "Downloads"),
        Documents = file.path(fs::path_home(), "Documents"),
        "C Drive" = "C:/")
    })

    observe({shinyFiles::shinyFileSave(input,"save",
        roots = roots(),session = session)
      })

    observeEvent(input$save, {
      req(input$save)
      parsed_path(shinyFiles::parseSavePath(roots(),input$save))
    })

    #clear path if data is cleared
    observeEvent(data_ver(), {
      if(data_ver() == 0){
        parsed_path(NULL)
        save_okay(FALSE)}
    })

    #clear selected path when metadata type changes
    observeEvent(startname(),{
      path <- parsed_path()

      if(!is.null(path) && nrow(path) > 0){
        path$name <- paste0(startname(), ".csv")
        path$datapath <- file.path(dirname(path$datapath), path$name)

        parsed_path(path)
        save_okay(FALSE)
      }
    })

    #clear the okay any time the data changes
    observeEvent(data(),{save_okay(FALSE)})

    output$save <- renderUI({
      shinyFiles::shinySaveButton(ns("save"),label = label,
                                  title = title,filetype = filetype(),
                                  filename = startname())
    })

    output$path_text <- renderUI({
      fileinfo <- parsed_path()

      display <- ifelse(!is.null(fileinfo) && nrow(fileinfo) > 0, fileinfo$datapath, "No location selected")

      color <- ifelse(!is.null(fileinfo) && nrow(fileinfo) > 0, "#343a40", "gray")
      tags$span(
        display,
        style = paste(
          "background-color:#fff;",
          "border:1px solid #ddd;",
          "padding:6px 12px;",
          "border-radius:6px;",
          "display:inline-block;",
          "min-width:250px;",
          paste0("color:", color, ";")
        )
      )
    })

  #check if overwrite is allowed
  observeEvent(input$export, {
    if(is.null(parsed_path())){
      shinyalert::shinyalert(
        title = "No Path Specified",
        text = "Please specify a save location before exporting.",
        type = "warning")

      return()
    }

    #if no data available to save give warning
    if(is.null(data())){
      shinyalert::shinyalert(
        title = "No Data Available",
        text = "Selected data is not available.",
        type = "warning")
      }else{
        #if file exists, make sure it's okay to overwrite
        if(file.exists(parsed_path()$datapath)){
          shinyalert::shinyalert(title = "File Already Exists",
                                 text = "Do you want to overwrite existing data?",
                                 type = "warning",
                                 showCancelButton = TRUE,
                                 inputId = "conf",
                                 callbackR = function(value){
                                   if(isTRUE(value)){save_file(parsed_path()$datapath, data(), type(), username())}})
        }else{
          save_file(parsed_path()$datapath, data(), type(), username())
        }

        }


  })

    return(
      reactive({
        req(parsed_path())
        parsed_path()
      })
    )
  })
}


