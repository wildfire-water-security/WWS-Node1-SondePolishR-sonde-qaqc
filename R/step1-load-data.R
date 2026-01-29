##step 1 of the app: load data into the app

# UI Function
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
          )),
      fluidRow(verbatimTextOutput(NS(id, "sheets_dir")))


       )

}

# Server Function
load_data_server <- function(id){
  moduleServer(id, function(input, output, session){

  #NEW PROJECT
    #load data
      type <- reactive({
        req(input$file)
        type <- tools::file_ext(input$file$datapath)
      })


      df <- reactive({
        req(input$file, input$tz)

        if(type() == "csv"){
          #req() # located here because if it's a project we don't need tz
          data <- read_sonde(input$file$datapath, tz=input$tz)  # read into R

          #clear the log and dataframe
          clear_log()
          clear_data()

          #write raw data
          write_data(data, "raw")

          return(data)
        }

        if(type() == "RDS"){
          read_project(input$file$datapath)  # read into R

          #get data
          prj <- get_data()

          return(prj[[length(prj)]])
        }

      })

      #update timezone if it's a project
        observeEvent(df(), {
          req(df(), type())

          if(type() == "RDS"){
            tz <- lubridate::tz(df()$DateTime)
            nice_tz <- nice_tz()
            tz <- nice_tz[nice_tz == tz]
            updateSelectInput(
              session,
              "tz",
              selected = tz)}
            })

    #define preset roots for file path
        baseroots <- c(
          wd        = getwd(),
          Downloads = file.path(fs::path_home(), "Downloads"),
          Documents = file.path(fs::path_home(), "Documents"),
          "C Drive" = "C:/")


    #specify prj_path so it exists
      prj_path_rv <- reactiveVal() # set up

      #when user loads file, update if RDS
      observeEvent(input$file, {
        if (type() == "RDS") prj_path_rv(.SondePolishR$prj_path)
      })

      #when user selects a file update
      observeEvent(input$save_file, {
        prj_path_rv(
          file.path(
            shinyFiles::parseDirPath(roots, input$save_file),
            paste0(tools::file_path_sans_ext(input$file$name), ".RDS")
          )
        )
      })


      #keep environment updated with save location
      observeEvent(prj_path_rv(), {set_prjpath(prj_path_rv())})


      #get file name and save path to save as project file
      Theroots <- reactive({
        base <- c(
          wd        = getwd(),
          Downloads = file.path(fs::path_home(), "Downloads"),
          Documents = file.path(fs::path_home(), "Documents"),
          "C Drive" = "C:/"
        )

        # If project path exists, prepend it
        path <- prj_path_rv()
        if (!is.null(path) && length(path) > 0) {
          c(project_root = dirname(path), base)
        } else {
          base
        }
      })

      shinyDirChoose(
        input,
        'sheets_dir',
        roots = Theroots,   # reactive allowed
        session = session
      )

      # Thesheets_dir <- reactive({
      #   shinyDirChoose(input, 'sheets_dir', roots = Theroots(), session = session)
      #   parseDirPath(roots = Theroots(), input$sheets_dir)
      # })

      Thesheets_dir <- reactive({
        req(input$sheets_dir)
        parseDirPath(roots = Theroots(), input$sheets_dir)
      })

      output$sheets_dir <- renderPrint({
        Thesheets_dir()
        #Theroots()
      })
      # shinyFiles::shinyDirChoose(input,"save_file", roots = roots, session = session,
      #                            defaultRoot = "Documents")

    #show file path in UI
      output$path_text_box <- renderUI({
        tags$span(
          prj_path_rv(),
          style = "background-color: #fff;border: 1px solid #ddd;padding: 6px 12px;
            border-radius: 6px; display: inline-block;min-width: 120px;color: #343a40")
      })

    # return the dataframe and file path as the module's "output"
    return(reactive({df()}))
  })
}
