#' @export
#' @rdname correction
correction_UI <- function(id){
  ns <- NS(id) #line to make module work

  tagList(

    sidebarLayout(
      sidebarPanel(
        accordion(
          open = c("Select Parameters", "Corrections"),
          accordion_panel(
            "Select Parameters",
            update_parms_UI(ns("update_parms")),
            update_parms_UI(ns("update_parms"), input_id = "y2_var", text = "Select Second Parameter to Plot:")
          ),
          accordion_panel(
            "Corrections",
            div(style = "margin-top: -15px;",
                radioButtons(ns("edit_type"),"",
                             choices = c("Additive" = "additive","Drift" = "drift", "Smoothing" = "smooth"))),
            uiOutput(ns('edit_options'))
          ),
          accordion_panel(
            "Save Edits",
            apply_edit_UI(ns("apply_limits"), edit_type = "change", note="Highlighted data will be adjusted"),
          ),
          accordion_panel(
            "Date Ranges",
            weekly_range_sidebar_UI(ns("date_nav")),
          ),
          accordion_panel(
            "Plotting Options",
            plot_options_UI(ns("plot_opts"))
          ))
        ),

    mainPanel(
      main_plot_UI(ns("shift_plot")),

      #add buttons to navigate date
      weekly_range_buttons_UI(ns("date_nav")),
    ))

  )}

#' Address any data shifts or corrections
#'
#' Plots loaded dataset, user can select a group of points and apply a additive shift to the data to correct for shifts, apply
#' a drift correction to a data file, or apply a smoothing function to selected data.
#'
#' @param id An ID string passed to shiny::NS(), used for namespacing UI inputs/outputs.
#' @param sondeproj A `reactiveVal` holding the current dataset.
#' @param data_ver A `reactiveVal` holding a number used to track when new data is added to trigger resets.
#' @param y_var Y-variable to plot on the y-axis.
#' @param view_state A `reactiveVal` holding a list of items specifying the view state:
#'  - abs_dates: The absolute range of dates within the dataset
#'  - dates: The range of dates being viewed via the date selector
#'  - period_view: Logical if the period view is being used
#'  - period_length: Length of period view
#'  - period_n: The period number to view.
#' @md
#' @keywords internal
#' @export
#' @rdname correction
#' @returns Invisible NULL
#'
correction_server <- function(id, sondeproj, data_ver, y_var,view_state){
  moduleServer(id, function(input, output, session){

  index <- reactiveVal() #stores index of selected points
  ns = session$ns #needed to make updating UI work
  plot_exist <- reactiveVal() #keeps warning about missing plot
  traces <- reactiveVal() #tracks which traces hold our points to track
  y2_var <- reactiveVal()   #keep track of second y_variable
  plot_exist <- reactiveVal() #keeps warning about missing plot
  curredit <- reactiveVal() #holds current edit
  currplot <- reactiveVal() #holds current plot
  currmethod <- reactiveVal() #holds current edit method

  #update UI options based on edit method
  output$edit_options <- renderUI({
    req(input$edit_type)
    switch(input$edit_type,
           "additive" = additive_UI(ns("additive_submod")),
           "drift" = drift_UI(ns("drift_submod"), sondeproj),
           "smooth" = smooth_UI(ns("smooth_submod")))
  })

  #get column names after file upload (dynamic)
    update_parms_server("update_parms", sondeproj, data_ver, y_var, choices_fun = nice_yvar)
    update_parms_server("update_parms", sondeproj, data_ver, y2_var, input_id= "y2_var", choices_fun = nice_yvar)

  #get what to plot via user options
    plot_opts <- plot_options_server("plot_opts")

  #keep track of dates
    plot_dates <- weekly_range_server("date_nav", sondeproj, data_ver,view_state)

  #code for tracking selected points
    #clearing manual indices if y_var or data updates
    observeEvent(list(y_var(), data_ver(), sondeproj()),{
      index(NULL)
    })

    observeEvent(
      req(plot_exist(), event_data("plotly_selected", source = "shift_plot", priority = "event"), input$edit_type != "drift"),{
        req(sondeproj(), y_var())
        data <- sondeproj()$data
        sel <- event_data("plotly_selected", source = "shift_plot")

        if(length(sel) && nrow(sel) > 0){
          sel <- sel %>%  filter(.data$curveNumber %in% traces()) %>%
            mutate(x = parse_date_time(.data$x, tz=sondeproj()$meta$tz, orders = "Ymd HMS", truncated =3))

          full_index <- data %>%
            mutate(value = .data[[y_var()]],
                   DateTime_rd = .data$DateTime_rd) %>%
            inner_join(sel, by = c("DateTime_rd" = "x")) %>%
            pull(.data$Index)

        if(input$edit_type == "smooth"){
          full_index <- seq(from=min(full_index), to=max(full_index), by=1)
        }

          index(full_index)
        }else{index(NULL)}

      })

  #filter data to plot
    plot_data <- reactive({
      req(sondeproj(), plot_dates())
      sondeproj()$data %>% dplyr::filter(.data$Date >= plot_dates()[1], .data$Date <= plot_dates()[2])
    })


  #create plotly plot, add in sub-mod additions
    plot_obj <- reactive({
      req(y_var(),y2_var(), plot_data())

      if(y2_var() == "none"){y2 <- NULL}else{y2 <- y2_var()}
      y <- y_var()

     #use function to plot sonde data
      p <- plot_sonde(data = plot_data(), y_var=y_var(), y2_var= y2, proj = sondeproj(), opts=plot_opts(), source="shift_plot")

      #set which traces hold points
      built_p <- plotly_build(p)
      names <- sapply(built_p$x$data, function(x){x$name})
      traces(which(names %in% c(get_yvar(y_var()), plot_data()$FileName))-1)


      #return plot
      p
    })

  #correction sub servers
    additive_server("additive_submod",sondeproj,y_var,plot_obj,plot_data,currplot, curredit, currmethod, index)
    drift_server("drift_submod",sondeproj,y_var,plot_obj,plot_data, currplot, curredit, currmethod)
    smooth_server("smooth_submod",sondeproj,y_var,plot_obj,plot_data, currplot, curredit, currmethod, index)

    observeEvent(input$edit_type,{
      req(input$edit_type)
      currmethod(input$edit_type)
    })

  #create plot
    sel_mode <- reactive({ifelse(input$edit_type != "drift", TRUE, FALSE)})
    # main_plot_server("shift_plot", data_ver,sondeproj, drift_out$plot, plot_data, y_var, sel_mode(), plot_exist)
    main_plot_server("shift_plot", data_ver,sondeproj, currplot, plot_data, y_var, sel_mode(), plot_exist)
    # observeEvent(input$modules, {
    #   req(input$modules == "step-8")
    #
    #   plotlyProxy("shift_plot", session) %>%
    #     plotlyProxyInvoke("resize")
    # })

  #flagging module
    apply_edit_server("apply_limits", sondeproj, curredit)

  #export plot so we can check it
    exportTestValues(
      edit_type = currmethod(),
      plot_obj = currplot(),
      changelog = sondeproj()$changelog,
      edit = curredit())

   })

}

