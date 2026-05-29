#' @export
#' @rdname additive
additive_UI <- function(id){
  ns <- NS(id) #line to make module work

  tagList(

    sidebarLayout(
      sidebarPanel(
        update_parms_UI(ns("update_parms")),

      accordion(
        id = ns("edit_type"),
        multiple =FALSE,
        accordion_panel("Additive Shift",
                        value="additive",
                        fluidRow(
                          div(style="margin-bottom: 8px; font-size:14px",
                              "Adjust the slope and intercept to shift the selected data:"),
                          numericInput(ns("slope"),"Slope",value = 0,step=0.001),
                          numericInput(ns("int"),"Intercept",value = 0,step=0.01))),
        accordion_panel("Drift Correction",
                        value="drift",
                        selectInput(
                          inputId=ns("file"),
                          label = "File to Drift Correct:",
                          choices = "",
                          selectize=TRUE),
                        fluidRow(
                          div(style="margin-bottom: 8px; font-size:14px",
                              "Adjust the corrected and uncorrected values to account for drift:"),
                          numericInput(ns("uncorrect"),"Uncorrected",value = 0,step=0.01),
                          numericInput(ns("correct"),"Corrected",value = 0,step=0.01)))

      ),
        #select physical limits


        HTML("<hr>"),

        apply_edit_UI(ns("apply_limits"), note=""),
        HTML("<hr>"),

        #date options
        weekly_range_sidebar_UI(ns("date_nav")),

        HTML("<hr>"),

        #plotting options
        plot_options_UI(ns("plot_opts"))

        ),

    mainPanel(
      plotlyOutput(ns("shift_plot"), height="60%"),
      #add buttons to navigate date
      weekly_range_buttons_UI(ns("date_nav")),
    ))

  )}

#' Address any additive shifts
#'
#' Plots loaded dataset, user can select a group of points and apply a additive shift to the data to correct for shifts.
#'
#' @param id An ID string passed to shiny::NS(), used for namespacing UI inputs/outputs.
#' @param sondeproj A `reactiveVal` holding the current dataset.
#' @param data_ver A `reactiveVal` holding a number used to track when new data is added to trigger resets.
#' @param y_var Y-variable to plot on the y-axis.
#' @md
#' @keywords internal
#' @export
#' @rdname additive
#' @returns Invisible NULL
#'
additive_server <- function(id, sondeproj, data_ver, y_var){
  moduleServer(id, function(input, output, session){

  index <- reactiveVal() #stores index of selected points

  #get column names after file upload (dynamic)
    update_parms_server("update_parms", sondeproj, data_ver, y_var, choices_fun = nice_yvar)

  #get what to plot via user options
    plot_opts <- plot_options_server("plot_opts")

  #keep track of dates
    dates <- weekly_range_server(
      "date_nav",
      min_date = reactive({req(sondeproj())
        min(sondeproj()$data$Date, na.rm = TRUE)}),
      max_date = reactive({req(sondeproj())
        max(sondeproj()$data$Date, na.rm = TRUE)}))


 ## code for drift corrections
  #update file options for drift corrections
    observe({
      req(sondeproj())

      updateSelectInput(session,"file",
        choices = unique(sondeproj()$data$FileName))
    })

  #update the drift correction values
    observeEvent(
      list(input$file, y_var()),{
        req(sondeproj(),input$file, y_var())
        vals <- guess_drift(sondeproj()$data, sondeproj()$calcheck, y_var(), input$file)

        updateNumericInput(session,"uncorrect",
          value = round(vals$uncorrect, 3))

        updateNumericInput(session,"correct",
          value = round(vals$correct, 3))
      })

 ## code for additive shift
  #filter data to plot
    plot_data <- reactive({
      req(sondeproj(), dates())
      dat <- sondeproj()$data %>% dplyr::filter(.data$Date >= dates()[1], .data$Date <= dates()[2])

      #if selected points, update where they're plotted
      if(!is.null(index())){
        rows <- which(dat$Index %in% index())
        dat <- shift_points(dat, y_var(), rows, shift_val = list(slope=input$slope, int=input$int))
      }

      dat
    })

    #observe selection from plot and get indices of selected
    observeEvent(
      req(sondeproj(), event_data("plotly_selected", source = "shift_plot"), input$edit_type == "additive"),{
        req(sondeproj(), y_var())

        sel <- event_data("plotly_selected", source = "shift_plot")

        if(!is.null(sel) && length(sel) && nrow(sel) > 0) {
          full_index <- plot_data()$Index[sel$pointNumber + 1]
          index(full_index)
        }else {index(NULL)}

        #update guesses
        guess <- guess_shift(sondeproj()$data, y_var(), index())
        updateNumericInput(session,"slope", value = guess$slope)
        updateNumericInput(session,"int", value = guess$int)


      })

    #create edit object
    edit <- reactive({
      req(!is.null(input$edit_type))
      newdata <- sondeproj()$data

    #if shift corr
     if(input$edit_type == "additive"){
       #get updated data
       newdata <- shift_points(newdata, y_var(), index(), shift_val = list(slope=input$slope, int=input$int))
       rows <- newdata$Index %in% index() #convert from row numbers to T/F
       note <- paste0("shift with slope ", input$slope," and intercept ", input$int)
       step <- "additive shifts"
       flag <- "CHG01"
     }

    #if drift corr
    if(input$edit_type == "drift"){
        #get updated data
        rows <- newdata$FileName == input$file #T/F
        newdata[[y_var()]] <- apply_drift_shift(newdata[[y_var()]], rows, input$correct, input$uncorrect)
        note <- paste0("drift correction based on an uncorrected value of ", input$uncorrect," and corrected value of ", input$correct,
                       "for file ", input$file)
        step <- "drift correction"
        flag <- "CHG02"
      }

      #make edit list
      list(
        data = newdata,
        rows = rows,
        y_var = y_var(),
        step = step,
        note = note,
        flag = flag,
        changetype = "flag_chg"
      )

    })

 ## exporting code
  #create plotly plot
    plot_obj <- reactive({
      req(y_var(), plot_data())

     #use function to plot sonde data
      p <- plot_sonde(plot_data(), y_var(), plot_opts(),sondeproj()$fieldform, sondeproj()$calcheck)

     #if points are selected color those
      if(!is.null(index()) & !is.null(input$edit_type) && input$edit_type == "additive"){
        flag_data <- plot_data()[plot_data()$Index %in% index(),]
        p <- p + ggplot2::geom_point(data=flag_data, aes(x = .data$DateTime_rd,y = .data[[y_var()]]), color = "darkred")

      }

      if(!is.null(input$edit_type) && input$edit_type == "drift"){
        p <- p + geom_line(data = edit()$data[edit()$rows,], aes(x=.data$DateTime_rd, y = .data[[y_var()]]), color="darkred")
      }

      #return plot
      p
    })

    #save to export
    output$shift_plot <- plotly::renderPlotly({
        req(plot_obj())

      # convert to plotly
      p <- plot_obj() %>%
                plotly::ggplotly(source = "shift_plot") %>%
                plotly::event_register("plotly_selected")

      #set to dragmode select as default for input
      if(!is.null(input$edit_type) && input$edit_type == "additive"){
        p <- p %>% plotly::layout(dragmode = "select")
      }

      p
    })

  #flagging module
    apply_edit_server("apply_limits", sondeproj, edit)

  #export plot so we can check it
    exportTestValues(
      plot_obj = plot_obj(),
      changelog = sondeproj()$changelog)

   })

}

