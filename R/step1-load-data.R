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
           color: white !important;
          }")),

    #select either new data or load data
        fluidRow(
          #load csv
          column(8,
                 fileInput(
                   inputId = NS(id, "file"),
                   label = span(style = "font-size:16px; white-space: nowrap;",
                                "Choose New or Existing Sonde Data File"),
                   accept = c(".csv", ".RDS"),  # restrict to CSV
                   width = "100%"
                 )),
          column(4,
                 #have user pick the timezone
                 selectInput(
                   inputId=NS(id, "tz"),
                   label = "Data Timezone:",
                   choices = nice_tz(),
                   selected = "Etc/GMT+8",
                   selectize=TRUE))
        ),

        fluidRow(
          column(12,
                 div(
                 class = "d-flex align-items-center gap-2",  # Bootstrap flex classes
                 shinyFiles::shinyDirButton(NS(id, "save_file"),
                                label = "Processed Data Save Location",
                                style="font-size:16px",
                                title= "Select location to save processed data", multiple=FALSE),
                 uiOutput(NS(id, "path_text_box"))

                      )
          ))

       )

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

    observeEvent(input$file,{
      req(input$file, input$tz)

      type(tools::file_ext(input$file$datapath))

      if(type() == "csv"){
        data(read_sonde(input$file$datapath, tz=input$tz))  # read into R
        prj_path(character()) #set prj_path var

        log(get_log())

      }

      if(type() == "RDS"){
        read_project(input$file$datapath)  # read into R
        prj_path(get_prjpath()) #set prj_path var

        #get data
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
        prj_path(
          file.path(
            shinyFiles::parseDirPath(roots, input$save_file),
            paste0(tools::file_path_sans_ext(input$file$name), ".RDS")
          )
        )
        #keep environment updated with save location
        set_prjpath(prj_path())
      })

      #get file name and save path to save as project file
      shinyFiles::shinyDirChoose(input,"save_file", roots = roots, session = session,
                                 defaultRoot = "Documents")

    #show file path in UI
      output$path_text_box <- renderUI({
        tags$span(
          prj_path(),
          style = "background-color: #fff;border: 1px solid #ddd;padding: 6px 12px;
            border-radius: 6px; display: inline-block;min-width: 120px;color: #343a40")
      })

      #export plot so we can check it
      exportTestValues(
        type = type()
      )

  })
}
