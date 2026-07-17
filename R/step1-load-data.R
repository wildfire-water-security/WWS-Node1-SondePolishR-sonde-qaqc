##step 1 of the app: load data into the app

# UI Function
#' @rdname load-data
#' @export

load_data_UI <- function(id){
  ns <- NS(id)
  tagList(
    tags$style(HTML("
      .btn-default{
        background-color:#E3795E!important;
        border-color:#E3795E!important;
        color:white;}

      .section-label{
        font-weight:600;
        margin-bottom:0.5rem;}

      .selectize-input{color: white;}

      .load-data-module .bslib-card,
      .load-data-module .tab-content,
      .load-data-module .tab-pane,
      .load-data-module .card-body {
        overflow: visible !important;
      }

      ")),

    shinyjs::useShinyjs(),

    bslib::page_fluid(
      div( class = "load-data-module",

      bslib::layout_columns(
        col_widths = c(5,5,2),
        bslib::card(
          class = "upload-card",
          bslib::card_header("1. Sonde Data"),
            fileInput(ns("pj_file"),"Existing Sonde Project (.RDS)",accept = ".RDS", width = "100%"),
            fileInput(inputId = ns("csv_files"),label = "Raw Sonde Data (.csv)",
                            multiple = TRUE, width = "100%",accept = ".csv")),
        bslib::card(
          class = "upload-card",
          bslib::card_header("2. Metadata"),
            fileInput(ns("ff_file"),"Field Form (.csv)",accept = ".csv", width = "100%"),
            fileInput(ns("cc_file"),"Calibration Checks (.csv)",accept = ".csv", width = "100%")
        ),
      bslib::card(
        bslib::card_header("3. Specify Site Info"),
          textInput(ns("site"), label="Site Name/Code"),
          selectInput(inputId = ns("tz"),label = "Timezone",choices = nice_tz(),
                      selected = "Etc/GMT+8",selectize = TRUE),
          div(style="margin-top: -3px; margin-bottom: -3px;font-size:12px",
            "Note: Only needed for new sonde projects.")
        )),

      bslib::layout_columns(
        col_widths = c(4,8),
        bslib::card(
          bslib::card_header("4. Load Data"),
          div(
            class = "d-flex flex-column justify-content-center align-items-center gap-3 h-100",
            actionButton(ns("load_prj"),"Load Sonde Data",width = "60%"),
            actionButton(ns("reset"),"Clear Uploads",width = "60%")
            )),

        bslib::card(
          bslib::card_header("5. Add Precipitation"),
            radioButtons(ns("precip_source"),NULL,
              choices = c("Download from NASA POWER (Global)" = "merra-2",
                          "Download from NLDAS (North America)" = "nldas",
                "Upload Precipitation" = "upload"),
              selected = "merra-2"),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'merra-2' || input['%s'] == 'nldas'", ns("precip_source"),ns("precip_source")),
            div(
              class = "d-flex justify-content-center",
              div(
                style = "width: 60%;",
                bslib::layout_columns(
                  col_widths = c(6,6),
                  numericInput(ns("lat"),"Latitude",value = NA, width="100%"),
                  numericInput(ns("long"),"Longitude",value = NA, width="100%"),
                ),
            ))),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'nldas'", ns("precip_source")),
            div(
              class = "d-flex justify-content-center",
                textInput(ns("token"), "Earth Data Token", value=NA)),
                div(class = "d-flex justify-content-center",
                    style="margin-top: -10px; margin-bottom: -3px;font-size:12px",
                    "See ",
                    HTML("&nbsp;"),
                    tags$a(
                      href = "https://urs.earthdata.nasa.gov/documentation/for_users/user_token",
                      " here ", target = "_blank"),
                    HTML("&nbsp;"),
                    " for instructions on obtaining an Earthdata token.")
              ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'upload'", ns("precip_source")),
            div(
              style = "margin-bottom: -15px;",
              fileInput( ns("precip_file"),"Precipitation File (.csv)",
                accept = ".csv",width = "80%")),
            div(
              style = "margin-top: -15px; margin-bottom: -5px; font-size:12px",
              "Note: Data must have two columns: DateTime and Precip_mm_hr")),
          div(class = "d-flex flex-column align-items-center",
            actionButton(ns("load_precip"),"Load Precipitation",width = "45%"))
        )
        ))))}

# Server Function
#' Read in the dataset or project and set save path
#'
#' Takes in a dataset as a `.csv` or a sonde project as an `.RDS` file via file selection.
#' If the data is a sonde project the save path will default it it's existing path, otherwise
#' the user will need to select a save path with the file name based on the name of the data file.
#'
#' @param id An ID string passed to shiny::NS(), used for namespacing UI inputs/outputs.
#' @param sondeproj A `reactiveVal` holding the current dataset.
#' @param data_ver A `reactiveVal` holding a number used to track when new data is added to trigger resets.
#' @md
#' @export
#' @keywords internal
#' @returns The loaded data as a reactive object.
#' @rdname load-data
load_data_server <- function(id, sondeproj, data_ver){
  moduleServer(id, function(input, output, session){
    #store paths as reactive value so we can clean on reset
      csv_path <- reactiveVal()
      prj_path <- reactiveVal()
      ff_path <- reactiveVal()
      cc_path <- reactiveVal()
      precip_path <- reactiveVal()

    #keep track of paths
      observe({
        req(input$csv_files)
        csv_path(input$csv_files$datapath)})
      observe({
        req(input$pj_file)
        prj_path(input$pj_file$datapath)})
      observe({
        req(input$ff_file)
        ff_path(input$ff_file$datapath)})
      observe({
        req(input$cc_file)
        cc_path(input$cc_file$datapath)})
      observe({
        req(input$precip_file)
        precip_path(input$precip_file$datapath)})

    #reset when requested which should prevent files from being uploaded
      observeEvent(input$reset, {
        csv_path(NULL)
        prj_path(NULL)
        ff_path(NULL)
        cc_path(NULL)
        sondeproj(NULL)
        precip_path(NULL)
        data_ver(0)
        updateTextInput(session, "site", value="")
        updateTextInput(session, "lat", value="")
        updateTextInput(session, "long", value="")


        reset('csv_files')
        reset('pj_file')
        reset('ff_file')
        reset('cc_file')
        reset('precip_file')

      })

  #when button to load project is clicked, read in everything and merge together
    observeEvent(input$load_prj, {
    if(any(c(!is.null(input$pj_file), !is.null(input$csv_files)))){
      withProgress(message = "loading sonde files...", min=0,max=length(csv_path()), {
          obj <- load_project(csv_path(), csv_files=input$csv_files$name, prj_path=prj_path(),
                   ff_path=ff_path(), cc_path=cc_path(), tz=input$tz, site=input$site,
                   update_pb = function(amount){incProgress(amount)})
        })

      #save object as reactive
      sondeproj(obj)

      #update UI inputs with project values
      updateTextInput(session, "site", value=ifelse(is.null(obj$meta$site), "", obj$meta$site))
      updateSelectInput(session, "tz", selected=ifelse(is.null(obj$meta$tz), "", obj$meta$tz))
      updateTextInput(session, "lat", value=ifelse(is.null(obj$meta$coords[1]), "", obj$meta$coords[1]))
      updateTextInput(session, "long", value=ifelse(is.null(obj$meta$coords[2]), "", obj$meta$coords[2]))

      #print a message so you know data loaded
      if (interactive()) {
        shinyalert::shinyalert(
          title = "Data Loaded",
          text = "Selected data has been loaded and any new data has been merge into existing project.",
          type = "success"
        )
      }

      #track that new data was uploaded
      data_ver(data_ver() + 1)
    }else{
      if(interactive()){
        shinyalert::shinyalert(
          title = "No Data Specified",
          text = "Please specify the path to either a sonde project or a data .csv file.",
          type = "warning"
        )
      }
    }


    })

  #deal with precipitation
    observeEvent(input$load_precip, {
      proj <- sondeproj()

      #provide message if data isn't loaded first
        if(is.null(proj)){
          if(interactive()){
            shinyalert::shinyalert(
              title = "No Data Specified",
              text = "Please load sonde data first.",
              type = "warning")}
        }

      #otherwise get
      if(input$precip_source %in% c("merra-2", "nldas")){
        precip <- get_precip(proj$data, input$lat, input$long, input$precip_source, input$token)

        proj$meta$coords <- c(input$lat, input$long)
      }else{
        req(precip_path())
        precip <- read.csv(precip_path())
        colnames(precip) <- c("DateTime", "Precip_mm_hr")
        precip$DateTime <- lubridate::parse_date_time(precip$DateTime, tz=proj$meta$tz, orders = c("ymd HMS", "mdy HMS","ymd"))
      }

      proj$precip <- precip

      sondeproj(proj)

      if (interactive()) {
        shinyalert::shinyalert(
          title = "Precipitation Data Loaded",
          text = "Precipitation data has been added to existing project.",
          type = "success"
        )
      }
    })
  #export values so we can check them
    #save values we want to check as their own reactive
    exportTestValues(
      proj = {
        req(sondeproj())
        sondeproj()
      }
    )

  })
}


