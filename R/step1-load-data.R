##step 1 of the app: load data into the app

# UI Function
#' @rdname load-data
#' @export
load_data_UI <- function(id){

  ns <- NS(id) #line to make module work

  tagList(
  #change button color
    tags$style(
      HTML("
           .btn-default {
           background-color: #E3795E !important;
           border-color: #E3795E !important;
           color: white;
          }"),
      HTML(".selectize-input {color: white;}")),
  page_fillable(
    accordion(
      accordion_panel("Load Data",
                      #load csv files
                      fluidRow(
                        column(8,fileInput(
                          inputId = NS(id, "csv_files"),
                          label = span(style = "font-size:16px; white-space: nowrap;",
                                       "Raw Sonde Data File(s) (.csv)"),
                          accept = c(".csv"),  # restrict to CSV
                          multiple = TRUE,
                          width = "60%")),
                        column(4,
                               #have user pick the timezone
                               selectInput(
                                 inputId=NS(id, "tz"),
                                 label = "Data Timezone:",
                                 choices = nice_tz(),
                                 selected = "Etc/GMT+8",
                                 selectize=TRUE))
                      ),
                      #load project
                      fileInput(
                        inputId = NS(id, "pj_file"),
                        label = span(style = "font-size:16px; white-space: nowrap;",
                                     "Sonde Project File (.RDS)"),
                        accept = c(".RDS"),
                        width = "40%")
                      ),
      accordion_panel("Load Metadata",
                      #load ff file
                      fileInput(
                        inputId = NS(id, "ff_file"),
                        label = span(style = "font-size:16px; white-space: nowrap;",
                                     "Field Form File"),
                        accept = c(".csv"),
                        width = "40%"),

                      #load cal check
                      fileInput(
                        inputId = NS(id, "cc_file"),
                        label = span(style = "font-size:16px; white-space: nowrap;",
                                     "Calibration Check File"),
                        accept = c(".csv"),
                        width = "40%")),

    accordion_panel("Save Project",
           fluidRow(
             column(12,
                    div(
                      class = "d-flex align-items-center gap-2",  # Bootstrap flex classes
                      shinyFiles::shinyDirButton(NS(id, "save_file"),
                                                 label = "Processed Data Save Location",
                                                 style="font-size:16px",
                                                 title= "Select location to save processed data", multiple=FALSE),
                      uiOutput(NS(id, "path_text_box")))
             )),
           checkboxInput(NS(id, "overwrite"), "Overwrite Existing Project?"),
           actionButton(NS(id, "save_prj"), "Save Sonde Project", width ="30%")
           ))

  ))


}

# Server Function
#' Read in the dataset or project and set save path
#'
#' Takes in a dataset as a `.csv` or a sonde project as an `.RDS` file via file selection.
#' If the data is a sonde project the save path will default it it's existing path, otherwise
#' the user will need to select a save path with the file name based on the name of the data file.
#'
#' @param id An ID string passed to shiny::NS(), used for namespacing UI inputs/outputs.
#' @param prj_path A `reactiveVal` holding the path to save the project to.
#' @param sdata A `reactiveVal` holding the current dataset.
#' @param log A `reactiveVal` holding the change log.
#' @md
#' @export
#' @keywords internal
#' @returns The loaded data as a reactive object.
#' @rdname load-data
load_data_server <- function(id, sdata, prj_path, log){
  moduleServer(id, function(input, output, session){

  #NEW PROJECT
    #load data
      type <- reactiveVal()
      base_path <- reactiveVal()

    # observeEvent(list(input$file, input$save_file){
    #   req(type())
    #   if(type() == "RDS" & is.null(input$save_file)){
    #     versions <- list.files(here::here(input))
    #   }
    #
    # })

      observeEvent(input$file, {
        req(input$tz)

        type(tools::file_ext(input$file$datapath))

        if (type() == "csv") {
          sdata(read_sonde(input$file$datapath, tz = input$tz))
          base_path(get_prjpath())
          prj_path(get_prjpath())
          log(get_log())
        }

        if (type() == "RDS") {
          read_project(input$file$datapath)

          base_path(get_prjpath())
          prj_path(
            if (input$overwrite)
              base_path()
            else
              version_path(base_path())
          )

          prj <- get_data()
          sdata(prj[[length(prj)]])
          log(get_log())
        }
      })



      #update timezone if it's a project
        observeEvent(sdata(), {
          req(sdata(), type())

          if(type() == "RDS"){
            tz <- lubridate::tz(sdata()$DateTime)
            nice_tz <- nice_tz()
            tz <- nice_tz[nice_tz == tz]
            updateSelectInput(
              session,
              "tz",
              selected = tz)}
            })

    #define preset roots for file path
      roots <- c(
            wd        = getwd(),
            Downloads = file.path(fs::path_home(), "Downloads"),
            Documents = file.path(fs::path_home(), "Documents"),
            "C Drive" = "C:/")

      #when user selects a file update path
      observeEvent(input$save_file, {
        req(input$file)

        dir <- shinyFiles::parseDirPath(roots, input$save_file)
        req(length(dir) > 0)

        name <- paste0(
          tools::file_path_sans_ext(input$file$name),
          ".RDS"
        )

        base_path(list(type="absolute", path = file.path(dir, name)))

        prj_path(
          if (input$overwrite)
            base_path()
          else
            version_path(base_path())
        )
      })

    #dealing with overwrite toggle
      observeEvent(input$overwrite, {
        req(base_path())

        prj_path(
          if (isTRUE(input$overwrite))
            base_path()
          else
            version_path(base_path())
        )
      })

    #sync path with prj enviornment
      observeEvent(prj_path(), {
        req(prj_path())
        set_prjpath(prj_path())
      })

      #get file name and save path to save as project file
      shinyFiles::shinyDirChoose(input,"save_file", roots = roots, session = session,
                                 defaultRoot = "Documents")

    #show file path in UI
      output$path_text_box <- renderUI({
        tags$span(
          prj_path()$path,
          style = "background-color: #fff;border: 1px solid #ddd;padding: 6px 12px;
            border-radius: 6px; display: inline-block;min-width: 120px;color: #343a40")
      })

      #export plot so we can check it
      exportTestValues(
        type = type()
      )

  })
}
