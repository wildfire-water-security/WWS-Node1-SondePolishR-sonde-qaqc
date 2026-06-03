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
      shinyjs::useShinyjs(),
      bslib::layout_columns(
      col_widths = c(6, 6, 6, 6),
      bslib::card(bslib::card_header("1.1: Load Data"),

                      #load project
                      fileInput(
                        inputId = NS(id, "pj_file"),
                        label = span(style = "font-size:16px; white-space: nowrap;",
                                     "Sonde Project File (.RDS)"),
                        accept = c(".RDS"),
                        width = "80%"),
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
                  )
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
         actionButton(NS(id, "load_prj"), "Load Sonde Project", width ="30%"),
         actionButton(NS(id, "reset"), "Clear File Uploads", width ="30%"),

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
#' @param data_ver A `reactiveVal` holding a number used to track when new data is added to trigger resets.
#' @md
#' @export
#' @keywords internal
#' @returns The loaded data as a reactive object.
#' @rdname load-data
load_data_server <- function(id, sondeproj, data_ver){
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

    #store paths as reactive value so we can clean on reset
      csv_path <- reactiveVal()
      prj_path <- reactiveVal()
      ff_path <- reactiveVal()
      cc_path <- reactiveVal()

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

    #reset when requested which should prevent files from being uploaded
      observeEvent(input$reset, {
        csv_path(NULL)
        prj_path(NULL)
        ff_path(NULL)
        cc_path(NULL)
        sondeproj(NULL)

        reset('csv_files')
        reset('pj_file')
        reset('ff_file')
        reset('cc_file')

      })

  #when button to load project is clicked, read in everything and merge together
    observeEvent(input$load_prj, {
    #set csv merge as NULL if not loaded to prevent errors
      csv_merge <- NULL

      #initialize progress bar
    #read csv files if added
      if(!is.null(csv_path())){
        data_merge <- withProgress(min=0,max=length(csv_path()), message = "loading sonde files...",
                      lapply(csv_path(), function(x){
                        num <- which(x == csv_path())
                        dat <- read_sonde(x, tz = input$tz, return="list")
                        dat$data$FileName <- basename(input$csv_files$name[num]) #in shiny the default filename is nothing
                        setProgress(num)
                        dat
                      }))

        serials <- lapply(data_merge, "[[", 1) %>% bind_rows()
        csv_merge <- lapply(data_merge, "[[", 2)%>% dplyr::bind_rows() %>%
          dplyr::mutate(Index = 1:n())
      }

    #load existing project
      if(!is.null(prj_path())){
         obj <- readRDS(prj_path())
      }else{
        #create new project if one isn't loaded
        changelog <- write_log(NULL, "all", "initial load", n = nrow(csv_merge), diff_name = "raw")
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
                     changelog = changelog,
                    duplicates = NULL,
                    data_gaps = NULL)

        class(obj) <- "sondeproj"
      }

    #read in ff and cal file (these cover the entire period and we don't need to merge, just update)
      if(!is.null(ff_path())){
        fieldform <- read_ff(ff_path())
        obj$fieldform <- fieldform
      }

      if(!is.null(cc_path())){
        calcheck <- read_cal(cc_path())

        #link with serial records to know when probes were changed (only if new data is added)
        if(!is.null(csv_path())){
          switch_df <- serials %>% pivot_longer(-"Date", names_to = "Parameter", values_to = "serial") %>%
            dplyr::group_by(.data$Parameter) %>%
            dplyr::mutate(Probe_Switch = .data$serial != dplyr::lag(.data$serial, default = dplyr::first(.data$serial))) %>%
            select(-"serial")

          calcheck <- calcheck %>% left_join(switch_df, by = join_by("Date", "Parameter"))

        }

        if(!is.null(ff_path())){
          #use ff data to determine when cal data likely was
          mean_visit <- get_oow(fieldform) %>% rowwise() %>%
            mutate(Est_Time = mean(c(.data$start, .data$end)),
                   Date = as.Date(.data$Est_Time))

          calcheck <- calcheck %>%
            dplyr::left_join(mean_visit %>% select("Date", "Est_Time"),
                             dplyr::join_by("Date"))
        }



        obj$calcheck <- calcheck
      }

   #if project and csv loaded, merge together (everything: data, flags, diffs, replace ff and cal)
    merge_flag <- !is.null(prj_path()) && !is.null(csv_path())

    if(merge_flag){
      #store previous nrow so can see how many we actually added
      prev_lines <- nrow(obj$data)

      #merge data and flags
        #we want to keep the modified data if datetimes are the same
        all_data <- obj$data %>% mutate(source = "sondeproj") %>% bind_rows(csv_merge %>% mutate(source = "csv"))

        data_merge <- all_data %>%
          dplyr::arrange(dplyr::desc(source == "sondeproj")) %>%
          dplyr::group_by(.data$Date, .data$DateTime, .data$DateTime_rd) %>%
          dplyr::slice(1) %>%
          dplyr::ungroup() %>% dplyr::select(-"source") %>%
          dplyr::mutate(Index = 1:n())



      if(nrow(data_merge) > prev_lines){
        #document data addition
        obj <- write_log(obj, "all", "adding new data", n = (nrow(data_merge) - prev_lines), diff_name = diff_version(obj), return="sondeproj")

        #store diff
        diff <- list(get_diff(obj$data, data_merge))
        names(diff) <- diff_version(obj)
        obj$diffs <- append(obj$diffs, diff)

      }
        obj$data <- data_merge

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
    if (interactive()) {
        shinyalert::shinyalert(
          title = "Data Loaded",
          text = "Selected data has been loaded and any new data has been merge into existing project.",
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


