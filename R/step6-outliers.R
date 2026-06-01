
#' @export
#' @rdname outliers
outlier_UI <- function(id){
  ns <- NS(id) #line to make module work
  tagList(
    sidebarLayout(
      sidebarPanel(
        update_parms_UI(ns("update_parms")),

      #select physical limits
        tags$h5("Identify Outliers"),
                input_switch(ns("rm_flags"), "Hide Flagged Data"),
        HTML("<hr>"),

        apply_edit_UI(ns("apply_limits"), note=""),
        HTML("<hr>"),

        #date options
        weekly_range_sidebar_UI(ns("date_nav")),

        HTML("<hr>"),

      #plotting options
        plot_options_UI(ns("plot_opts")),



      ),
      mainPanel(
        plotlyOutput(ns("outlier_plot"), height="60%"),
        #add buttons to navigate date
        weekly_range_buttons_UI(ns("date_nav")),
      ))


  )}


#' Flag data identified as outliers either manually or view methods
#'
#' Looks for "weird" data where there are large spikes within a short period that are likely unrealistic and caused by
#' instrument malfunction or a bubble near the sensor.
#'
#' @keywords internal
#'
#' @param id An ID string passed to shiny::NS(), used for namespacing UI inputs/outputs.
#' @param sondeproj A `reactiveVal` holding the current dataset.
#' @param data_ver A `reactiveVal` holding a number used to track when new data is added to trigger resets.
#' @param y_var Y-variable to plot on the y-axis.
#'
#' @export
#' @rdname outliers
outlier_server <- function(id, sondeproj, data_ver, y_var){
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

  #track selected data
    observeEvent(
      req(sondeproj(), event_data("plotly_selected", source = "outlier_plot")),{
        req(sondeproj(), y_var())

        sel <- event_data("plotly_selected", source = "outlier_plot")

        if(!is.null(sel) && length(sel) && nrow(sel) > 0) {
          full_index <- plot_data()$Index[sel$pointNumber + 1]
          index(full_index)
        }else {index(NULL)}

      })
  #filter data to plot
    plot_data <- reactive({
      req(sondeproj(), dates())

      sondeproj()$data %>% dplyr::filter(.data$Date >= dates()[1], .data$Date <= dates()[2])

    })

  #create plotly plot
    plot_obj <- reactive({
      req(y_var(), plot_data())

      #if we want to filter out flagged points, filter before plotting
      if(input$rm_flags){
        filter_data <- plot_data() %>% filter(!(.data$Index %in% index()))
      }else{
        filter_data <- plot_data()
        flag_data <- plot_data() %>% filter(.data$Index %in% index())
      }

      #use function to plot sonde data
      p <- plot_sonde(filter_data, y_var(), plot_opts(),sondeproj()$fieldform, sondeproj()$calcheck)

      #color points outside limits as red
      if(!input$rm_flags){
        p <- p + ggplot2::geom_point(data=flag_data, aes(x = .data$DateTime_rd,y = .data[[y_var()]]), color = "darkred")
      }

      #return plot
      p
    })

    #save to export
    output$outlier_plot <- plotly::renderPlotly({
      req(plot_obj())

      # convert to plotly
      p <- plot_obj() %>%
        plotly::ggplotly(source = "outlier_plot") %>%
        plotly::event_register("plotly_selected") %>%
        plotly::layout(dragmode = "select")

      p
    })


  #create edit object
    edit <- reactive({
      newdata <- sondeproj()$data

      #get filtered data
      setna <- newdata$Index %in% index()
      newdata[[y_var()]][setna] <- NA

      #make edit list
      list(
        data = newdata,
        rows = setna,
        y_var = y_var(),
        step = "outlier removal",
        note = paste0("Data removed based on manual outlier detection."),
        flag = "RM03",
        changetype = "flag_rm"
      )

    })

  #flagging module
    apply_edit_server("apply_limits", sondeproj, edit)

  #export plot so we can check it
    exportTestValues(
      plot_obj = plot_obj(),
      changelog = sondeproj()$changelog)

  })}
