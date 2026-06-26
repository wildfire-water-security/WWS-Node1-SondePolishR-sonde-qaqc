#' File save selector UI
#'
#' @param id module id
#' @param label button label
#' @param title dialog title
#' @param filetype file extension
#' @param data data to save to specified file on click
#'
#' @rdname file-export
#' @export
#' @md
#' @keywords internal
save_path_UI <- function(id,
                         label = "Choose Location",
                         title = "Select save path",
                         filetype = ".csv",
                         button_label = "Export") {
  ns <- NS(id)

  tagList(
      div(
        class = "d-flex align-items-start gap-2",
        shinyFiles::shinySaveButton(ns("save"),label = label,
                                    title = title,filetype = filetype),
        div(style = "flex-grow:1; min-width:0;",
            uiOutput(ns("path_text")))),
        div(
          class = "text-left",
          actionButton(ns("export"),button_label, width = "220px"))
)

}

#' @rdname file-export
save_path_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {

  parsed_path <- reactiveVal() #initialize so box always shows

  #define preset roots for file path
    roots <- c(
      "Working Directory" = getwd(),
      Downloads = file.path(fs::path_home(), "Downloads"),
      Documents = file.path(fs::path_home(), "Documents"),
      "C Drive" = "C:/")

    observe({shinyFiles::shinyFileSave(input,"save",
        roots = roots,session = session)
      })

    observe({
      req(input$save)
      parsed_path(shinyFiles::parseSavePath(roots,input$save))
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

    observeEvent(input$export, {
      if(is.null(data())){
        if (interactive()) {
          shinyalert::shinyalert(
            title = "No Data Available",
            text = "Selected data is not available.",
            type = "warning"
          )
        }
      }

      if(tools::file_ext(parsed_path()$datapath) == "csv" && !is.null(data())){
        write.csv(data(), parsed_path()$datapath, row.names = FALSE, quote = FALSE)

        #print a message so you know data downloaded
        if (interactive()) {
          shinyalert::shinyalert(
            title = "Data Downloaded",
            text = "Selected data has been downloaded.",
            type = "success"
          )
        }
      }
      if(tools::file_ext(parsed_path()$datapath) == "RDS"&& !is.null(data())){
        saveRDS(data(), parsed_path()$datapath)

        #print a message so you know data downloaded
        if (interactive()) {
          shinyalert::shinyalert(
            title = "Project Saved",
            text = "Current Sonde Project has been saved.",
            type = "success"
          )
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
