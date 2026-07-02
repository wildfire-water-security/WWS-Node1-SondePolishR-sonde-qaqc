
#' @export
#' @rdname quality-flags
quality_UI <- function(id){
  ns <- NS(id) #line to make module work
  tagList(
    sidebarLayout(
      sidebarPanel(
        update_parms_UI(ns("update_parms")),
        update_parms_UI(ns("update_parms"), input_id = "y2_var", text = "Select Second Parameter to Plot:"),

        HTML("<hr>"),
      #select physical limits
        tags$h5("Apply Quality Flags"),
            bslib::layout_columns(
              col_widths = c(7, 5),
              selectInput(ns("quality_flag"),
                        "Select Quality Flag:",
                        choices = c("Questionable" = "questionable")),
              radioButtons(ns("selection_mode"),"Manual Selection Method",
                                     choices = c("Add" = "add","Remove" = "remove"))),

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
        plotlyOutput(ns("quality_plot"), height="60%"),
        #add buttons to navigate date
        weekly_range_buttons_UI(ns("date_nav")),
      ))


  )}


#' Flag data as questionable
#'
#' Allows the user to mark data that looks for "weird" and can be viewed as a plotting option or
#' used to auto-select as an outlier.
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
quality_server <- function(id, sondeproj, data_ver, y_var){
  moduleServer(id, function(input, output, session){

  #keep track of second y_variable
    y2_var <- reactiveVal()

  #stores index of selected points
    manual_add <- reactiveVal(integer())
    manual_rm <- reactiveVal(integer())
    plot_exist <- reactiveVal() #keeps warning about missing plot
    traces <- reactiveVal() #tracks which traces hold our points to track

  #clearing manual indices if y_var or data updates
    observeEvent(list(y_var(), data_ver()),{
      manual_add(NULL)
      manual_rm(NULL)
      })

  #get column names after file upload (dynamic)
    update_parms_server("update_parms", sondeproj, data_ver, y_var, choices_fun = nice_yvar)
    update_parms_server("update_parms", sondeproj, data_ver, y2_var, input_id= "y2_var", choices_fun = nice_yvar)

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
      req(plot_exist(), event_data("plotly_selected", source = "outlier_plot")),{
        req(sondeproj(), y_var())

        data <- sondeproj()$data

        sel <- event_data("plotly_selected", source = "outlier_plot")

        if(is.data.frame(sel)){
          sel <- sel %>% filter(.data$curveNumber %in% traces()) %>%
            mutate(x = parse_date_time(x, tz=tz(data$DateTime_rd), orders = "Ymd HMS", truncated =3))
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

  #filter data to plot
    plot_data <- reactive({
      req(sondeproj(), dates())

      sondeproj()$data %>% dplyr::filter(.data$Date >= dates()[1], .data$Date <= dates()[2])

    })

  #create plotly plot
    plot_obj <- reactive({
      req(y_var(),y2_var(), plot_data())
      if(y2_var() == "none"){y2 <- NULL}else{y2 <- y2_var()}

      #switch this up to change color based on flag?? show different kinds of flags??

      #if we want to filter out flagged points, filter before plotting
        filter_data <- plot_data()
        flag_data <- plot_data() %>% filter(.data$Index %in% manual_add() & !is.na(.data[[y_var()]]))

      #use function to plot sonde data
      p <- plot_sonde(data = filter_data, y_var=y_var(), y2_var= y2, opts=plot_opts(),fieldform=sondeproj()$fieldform,
                      calcheck =sondeproj()$calcheck, precip=sondeproj()$precip, source="outlier_plot")

      #color points outside limits as red
        y <- y_var()
        p <- p %>% add_trace(data= flag_data, x=~DateTime_rd, y=as.formula(paste0("~`", y, "`")), type="scatter", mode="markers",
                                 name = "Questionable", marker = list(color = "orange"), yaxis="y", inherit = FALSE)

      #set which traces hold points
      built_p <- plotly_build(p)
      names <- sapply(built_p$x$data, function(x){x$name})
      traces((which(names != "Questionable")-1))

      #return plot
      p
    })

    #save to export
    output$quality_plot <- plotly::renderPlotly({
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
    # observeEvent(input$modules, {
    #   req(input$modules == "step-5")
    #
    #   plotlyProxy("quality_plot", session) %>%
    #     plotlyProxyInvoke("resize")
    # })

  #create edit object
    edit <- reactive({
      newdata <- sondeproj()$data

      #get filtered data
      setna <- newdata$Index %in% selected_index()
      newdata[[y_var()]][setna] <- NA

      flag_info <- switch(input$quality_flag,
                           "questionable" = list(
                             nicename = "questionable",
                             flag = "QUAL01"))

      #make edit list
      list(
        data = newdata,
        rows = setna,
        y_var = y_var(),
        step = "questionable data",
        note = paste0("Data flagged as ", flag_info$nicename),
        flag = flag_info$flag,
        changetype = "flag_qual"
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
