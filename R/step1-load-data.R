##step 1 of the app: load data into the app

# UI Function
#' @rdname load-data
#' @export

load_data_UI <- function(id){
  ns <- NS(id)
  tagList(
    bslib::page_fluid(
      div(class = "load-data-module",

      bslib::layout_columns(
        col_widths = c(5,5,2),
        bslib::card(
          class = "upload-card",
          bslib::card_header("1. Sonde Data"),
          div(
            class = "d-flex justify-content-center align-items-center h-100",
            "Not available in demo mode"
          )),
        bslib::card(
          class = "upload-card",
          bslib::card_header("2. Metadata"),
          div(
            class = "d-flex justify-content-center align-items-center h-100",
            "Not available in demo mode"
          )
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
            actionButton(ns("reset"),"Clear Uploads",width = "60%"),
            textInput(ns("username"),"Analyst Name", width="75%")
            )),

        bslib::card(
          bslib::card_header("5. Add Precipitation"),
          div(
            class = "d-flex justify-content-center align-items-center h-100",
            "Not available in demo mode"
          )
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
#' @param view_state A `reactiveVal` holding a list of items specifying the view state:
#'  - abs_dates: The absolute range of dates within the dataset
#'  - dates: The range of dates being viewed via the date selector
#'  - period_view: Logical if the period view is being used
#'  - period_length: Length of period view
#'  - period_n: The period number to view.
#' @param username A `reactiveVal` holding the name of the analyst for the changelog
#'
#' @md
#' @export
#' @keywords internal
#' @returns The loaded data as a reactive object.
#' @rdname load-data
load_data_server <- function(id, sondeproj, data_ver, view_state, username){
  moduleServer(id, function(input, output, session){

  #when button to load project is clicked, read in everything and merge together
    observeEvent(input$load_prj,{

      withProgress(message = "loading sonde files...", min=0,max=1, {
        obj <-  tryCatch({
          SondePolishR::example_sondeproj
          },
         error = function(e){
             shinyalert::shinyalert(
              title = "Project Failed to Load",
              text = "Please check your file structure and/or submit a GitHub issue.",
              type = "error"
            )
            return(NULL)
          })
        })

    #if fails to load end reactive
      if(is.null(obj)){return(NULL)}

      if(all(is.na(obj$meta$coords)) | all(obj$meta$coords == "")){
        obj$meta$coords <- c(input$lat, input$long) #also write lat/long if provided
      }

      #set initial dates for everything
      update_view_state(view_state, dates = range(obj$data$Date, na.rm=TRUE))
      update_view_state(view_state, abs_dates = range(obj$data$Date, na.rm=TRUE))

      #save object as reactive
      sondeproj(obj)

      #update UI inputs with project values only if there's something there
      if(!is.null(obj$meta$site)){updateTextInput(session, "site", value=obj$meta$site)}
      if(!is.null(obj$meta$tz)){updateSelectInput(session, "tz", selected=obj$meta$tz)}
      if(!is.null(obj$meta$coords[1])){updateTextInput(session, "lat", value=obj$meta$coords[1])}
      if(!is.null(obj$meta$coords[2])){updateTextInput(session, "long", value=obj$meta$coords[2])}

      #print a message so you know data loaded
      if(interactive()){
        shinyalert::shinyalert(
          title = "Data Loaded",
          text = "Selected data has been loaded and any new data has been merge into the existing project.",
          type = "success"
        )
      }

      #track that new data was uploaded
      data_ver(data_ver() + 1)
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


