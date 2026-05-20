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
    bslib::page_fluid(
      bslib::layout_columns(
      col_widths = c(6, 6, 6, 6),
      bslib::card(bslib::card_header("1.1: Load Data"),
                      #load csv files
                      fluidRow(
                        column(8,fileInput(
                          inputId = NS(id, "csv_files"),
                          label = span(style = "font-size:16px; white-space: nowrap;",
                                       "Raw Sonde Data File(s) (.csv)"),
                          accept = c(".csv"),  # restrict to CSV
                          multiple = TRUE,
                          width = "80%")),
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
                        width = "80%")
                      ),
      bslib::card(bslib::card_header("1.2: Load Metadata"),
                      #load ff file
                      fileInput(
                        inputId = NS(id, "ff_file"),
                        label = span(style = "font-size:16px; white-space: nowrap;",
                                     "Field Form File (.csv)"),
                        accept = c(".csv"),
                        width = "80%"),

                      #load cal check
                      fileInput(
                        inputId = NS(id, "cc_file"),
                        label = span(style = "font-size:16px; white-space: nowrap;",
                                     "Calibration Check File (.csv)"),
                        accept = c(".csv"),
                        width = "80%")),

      bslib::card(bslib::card_header("1.3: Save Path"),
           fluidRow(
             column(12,
                    div(
                      class = "d-flex align-items-center gap-2",  # Bootstrap flex classes
                      shinyFiles::shinySaveButton(NS(id, "save_file"),
                                                 label = "Processed Data Save Location",
                                                 style="font-size:16px",
                                                 title= "Select location to save processed data", multiple=FALSE,
                                                 filetype = ".RDS"),
                      uiOutput(NS(id, "path_text_box"))))
             )),
      bslib::card(bslib::card_header("1.4: Load Data"),
         actionButton(NS(id, "load_prj"), "Load Sonde Project", width ="30%")
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
#' @param sondeproj A `reactiveVal` holding the current dataset.
#' @md
#' @export
#' @keywords internal
#' @returns The loaded data as a reactive object.
#' @rdname load-data
load_data_server <- function(id, sondeproj){
  moduleServer(id, function(input, output, session){
  #code for setting up the directory button
    #define preset roots for file path
    roots <- c(
      "Working Directory" = getwd(),
      Downloads = file.path(fs::path_home(), "Downloads"),
      Documents = file.path(fs::path_home(), "Documents"),
      "C Drive" = "C:/")

    observe({
      shinyFiles::shinyFileSave(input, "save_file", roots = roots, session = session)
    })

    output$path_text_box <- renderUI({
      fileinfo <- parseSavePath(roots, input$save_file)
      if(length(fileinfo) > 0){
        tags$span(fileinfo$datapath,
                  style = "background-color: #fff;border: 1px solid #ddd;padding: 6px 12px;
                  border-radius: 6px; display: inline-block;min-width: 120px;color: #343a40")
      }

    })

  #when button to load project is clicked, read in everything and merge together
    observeEvent(input$load_prj, {
    #parse save location


    #set csv merge as NULL if not loaded to prevent errors
      csv_merge <- NULL

    #read csv files if added
      if(!is.null(input$csv_files)){
        csv_merge <- lapply(input$csv_files$datapath,read_sonde, tz = input$tz) %>%
          dplyr::bind_rows()
      }

    #load existing project
      if(!is.null(input$pj_file)){
         obj <- readRDS(input$pj_file$datapath)
      }else{
        #create new project if one isn't loaded
        changelog <- write_log(NULL, "all", "initial load", n = 0, diff_name = "raw")
        empty_flags <- add_flags(csv_merge)

        #create sonde object
        obj <- list(data = csv_merge,
                    flags = list(
                            flag_rm = empty_flags,
                            flag_chg = empty_flags,
                            flag_add = empty_flags),
                     fieldform = NULL,
                     calcheck = NULL,
                     diffs = list(),
                     changelog = changelog)

        class(obj) <- "sondeproj"
      }


    #read in ff and cal file (these cover the entire period and we don't need to merge, just update)
      if(!is.null(input$ff_file)){
        fieldform <- read_ff(input$ff_file$datapath)
        obj$fieldform <- fieldform
      }

      if(!is.null(input$cc_file)){
        calcheck <- read_cal(input$cc_file$datapath)
        obj$calcheck <- calcheck
      }

   #if project and csv loaded, merge together (everything: data, flags, diffs, replace ff and cal)
    merge_flag <- !is.null(input$pj_file) && !is.null(input$csv_files)

    if(merge_flag){
      #document data addition (can't currently do diff because lines are different)
      obj <- write_log(obj, "all", "adding new data", n = nrow(csv_merge), diff_name = "data_upload", return="sondeproj")

      #merge data and flags
      obj$data <- obj$data %>% dplyr::bind_rows(csv_merge) %>% distinct(across(-.data$Index)) %>%
        arrange(.data$DateTime) %>% mutate(Index = 1:n(), .before="Date")

      #create flag tables for new data
      empty_flags <- add_flags(obj$data)

      #keep existing flags
      ext_flags <- obj$flags
      new_flags <- empty_flags %>% filter(!(.data$DateTime %in% ext_flags$flag_rm$DateTime))

      obj$flags <- lapply(obj$flags, function(x){
        x %>% dplyr::bind_rows(new_flags) %>%
          arrange(.data$DateTime) %>% mutate(Index = 1:n(), .before="DateTime")
      })

      #check that flags match data
      stopifnot(all(sapply(obj$flags, nrow) == nrow(obj$data)))
    }


  #save object as reactive
      sondeproj(obj)

  #print a message so you know data loaded
    shinyalert::shinyalert(
        title = "Data Loaded",
        text = "Selected data has been loaded and any new data has been merge into existing project.",
        type = "success"
      )

  #export values so we can check them

    exportTestValues(
         changelog = obj$changelog
        )
    })

  })
}


