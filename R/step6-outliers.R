
#' @export
#' @rdname outliers
outlier_UI <- function(id){
  ns <- NS(id) #line to make module work
  tagList(
    sidebarLayout(
      sidebarPanel(
        update_parms_UI(ns("update_parms")),
        update_parms_UI(ns("update_parms"), input_id = "y2_var", text = "Select Second Parameter to Plot:"),

        HTML("<hr>"),
      #select physical limits
        tags$h5("Identify Outliers"),
            bslib::layout_columns(
              col_widths = c(7, 5),
              selectInput(ns("filter_type"),
                        "Select Outlier Detection Method:",
                        choices = c("None" = "none","Questionable Points" = "questionable",
                                    "Hampel Filter" = "hampel", "Relative Change" = "rel_change"),
                        selected = "none"),
              radioButtons(ns("selection_mode"),"Manual Selection Method",
                                     choices = c("Add" = "add","Remove" = "remove"))),
            bslib::layout_columns(
                  col_widths = c(3,3,1,5),
                    numericInput(ns("k"),"Window Size",value =5,step=2),
                    numericInput(ns("t"),"Threshold",value = 2, step=0.1),
                    tags$div(
                    style = "width: 1px; height: 85px; background-color: #6c7881; display: inline-block; margin: 0 30px; vertical-align: middle;"),
                    div(class = "d-flex justify-content-center align-items-center",
                        style = "height: 85px;",
                        actionButton(ns("clear_sel"), "Clear Selection")))
                ,
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
#' @param id An ID string passed to shiny::NS(), used for namespacing UI inputs/outputs.
#' @param sondeproj A `reactiveVal` holding the current dataset.
#' @param data_ver A `reactiveVal` holding a number used to track when new data is added to trigger resets.
#' @param y_var Y-variable to plot on the y-axis.
#' @param dates The date range to view the data.
#' @param period_view Should data be viewed by period?
#' @param p_length The length of the period to view.
#' @export
#' @rdname outliers
outlier_server <- function(id, sondeproj, data_ver, y_var,period_view, dates, p_length){
  moduleServer(id, function(input, output, session){

  #keep track of second y_variable
    y2_var <- reactiveVal()

  #stores index of selected points
    manual_add <- reactiveVal(integer())
    manual_rm <- reactiveVal(integer())
    plot_exist <- reactiveVal() #keeps warning about missing plot
    traces <- reactiveVal() #tracks which traces hold our points to track

  #clearing manual indices if y_var or data updates
    observeEvent(list(y_var(), data_ver(), sondeproj(), input$clear_sel),{
      manual_add(NULL)
      manual_rm(NULL)
      })

  #get column names after file upload (dynamic)
    update_parms_server("update_parms", sondeproj, data_ver, y_var, choices_fun = nice_yvar)
    update_parms_server("update_parms", sondeproj, data_ver, y2_var, input_id= "y2_var", choices_fun = nice_yvar)

  #get what to plot via user options
    plot_opts <- plot_options_server("plot_opts")

  #keep track of dates
    plot_dates <- weekly_range_server("date_nav", sondeproj, period_view, dates, p_length, data_ver)

  #implement outlier detection
    auto_index <- reactive({
     req(sondeproj(), y_var())
      data <- sondeproj()$data
      x <- data[[y_var()]] #needed by everything

      # interpolate to temp fill gaps so filter will work
      x_fill <- zoo::na.approx(x, na.rm = FALSE)
      x_fill <- zoo::na.locf(x_fill, na.rm = FALSE)        # forward fill
      x_fill <- zoo::na.locf(x_fill, fromLast = TRUE)      # backward fill

      if(input$filter_type == "hampel"){
        hampel_out <- pracma::hampel(x_fill, input$k, input$t)

        outlier <- rep(FALSE, length(x))
        outlier[hampel_out$ind] <- TRUE
      }

      if(input$filter_type == "rel_change"){
        rel_change_lead <- abs(x_fill - lead(x_fill)) / zoo::rollmedian(x_fill, input$k, fill= NA, align = "right") * 100
        rel_change_lag <- abs(x_fill - lag(x_fill)) / zoo::rollmedian(x_fill, input$k, fill= NA, align = "left") * 100

        outlier <- rel_change_lead >= input$t & rel_change_lag >= input$t
        outlier[is.na(outlier)] <- FALSE #deal with ending/starting NA
      }

      if(input$filter_type == "questionable"){
        outlier <- get_qual_flags(sondeproj()$flags$flag_qual, y_var())
      }

      #return flagged indices
      if(input$filter_type == "none" || sum(outlier) == 0){
        NULL
      }else{
        data$Index[outlier]

      }
      })

  #track selected data
    observeEvent(
      req(plot_exist(), event_data("plotly_selected", source = "outlier_plot")),{
        req(sondeproj(), y_var())

        data <- sondeproj()$data

        sel <- event_data("plotly_selected", source = "outlier_plot")

        if(is.data.frame(sel)){
          sel <- sel %>% filter(.data$curveNumber %in% traces()) %>%
            mutate(x = parse_date_time(x, tz=sondeproj()$meta$tz, orders = "Ymd HMS", truncated =3))
          #get points based on x and y
          full_index <- data %>%
            mutate(value = .data[[y_var()]],
                   DateTime_rd = .data$DateTime_rd) %>%
            inner_join(sel, by = c("DateTime_rd" = "x", "value" = "y")) %>%
            pull(.data$Index)

          if(input$selection_mode == "add"){
            manual_add(union(manual_add(), full_index))
            #also remove if index is in rm
            manual_rm(setdiff(manual_rm(), full_index))
          }else {
            manual_rm(union(manual_rm(), full_index))
            #also remove if index is in add
            manual_add(setdiff(manual_add(), full_index))

          }
        }


      })

  #keep track of the selected points
    selected_index <- reactive({
      auto <- auto_index()
      auto <- setdiff(auto, manual_rm())
      union(auto, manual_add())

    })

  #filter data to plot
    plot_data <- reactive({
      req(sondeproj(), plot_dates())

      sondeproj()$data %>% dplyr::filter(.data$Date >= plot_dates()[1], .data$Date <= plot_dates()[2])

    })

  #create plotly plot
    plot_obj <- reactive({
      req(y_var(),y2_var(), plot_data())
      if(y2_var() == "none"){y2 <- NULL}else{y2 <- y2_var()}

      #if we want to filter out flagged points, filter before plotting
      if(input$rm_flags){
        filter_data <- plot_data() %>% filter(!(.data$Index %in% selected_index()))
      }else{
        filter_data <- plot_data()
        flag_data <- plot_data() %>% filter(.data$Index %in% selected_index() & !is.na(.data[[y_var()]]))
      }

      #use function to plot sonde data
      p <- plot_sonde(data = filter_data, y_var=y_var(), y2_var= y2, proj = sondeproj(), opts=plot_opts(),
                      source="outlier_plot")

      #color points outside limits as red
      if(!input$rm_flags){
        y <- y_var()
        p <- p %>% add_trace(data= flag_data, x=~DateTime_rd, y=as.formula(paste0("~`", y, "`")), type="scatter", mode="markers",
                                 name = "Flagged", marker = list(color = "darkred"), yaxis="y", inherit = FALSE)
      }

      #set which traces hold points
      built_p <- plotly_build(p)
      names <- sapply(built_p$x$data, function(x){x$name})
      traces((which(names != "Flagged")-1))

      #return plot
      p
    })

    #save to export
    output$outlier_plot <- plotly::renderPlotly({
      validate(
        need(nrow(plot_data()) > 0,
             "No data available for the selected date range."))

      # convert to plotly
      p <- plot_obj() %>%
        plotly::event_register("plotly_selected") %>%
        plotly::layout(dragmode = "select")
      p <- toWebGL(p)

      plot_exist(TRUE)

      p

    })

    #redraw when back on module to prevent weird drawing issues
    observeEvent(input$modules, {
      req(input$modules == "step-6")

      plotlyProxy("outlier_plot", session) %>%
        plotlyProxyInvoke("resize")
    })

  #create edit object
    edit <- reactive({
      newdata <- sondeproj()$data

      #get filtered data
      setna <- newdata$Index %in% selected_index()
      newdata[[y_var()]][setna] <- NA

      nicemethod <- switch(input$filter_type,
                           "hampel" = "Hampel Filter",
                           "rel_change" = "Relative Change")
      #make edit list
      list(
        data = newdata,
        rows = setna,
        y_var = y_var(),
        step = "outlier removal",
        note = paste0("Data removed based on ", nicemethod,
                      " method with a window size of ", input$k, " and threshold of ", input$t,
                      " paired with manual outlier detection."),
        flag = "RM03",
        changetype = "flag_rm"
      )

    })

  #flagging module
     apply_edit_server("apply_limits", sondeproj, edit)

  #export plot so we can check it
    exportTestValues(
      plot_obj = plot_obj(),
      changelog = sondeproj()$changelog,
      manual_add = manual_add(),
      manual_rm = manual_rm(),
      selected = selected_index())

  })}
