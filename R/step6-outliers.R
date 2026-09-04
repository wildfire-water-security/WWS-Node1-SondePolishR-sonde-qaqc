
#' @export
#' @rdname outliers
outlier_UI <- function(id){
  ns <- NS(id) #line to make module work
  tagList(
    sidebarLayout(
      sidebarPanel(
        accordion(
          open = c("Select Parameters", "Identify Outliers"),
          accordion_panel(
            "Select Parameters",
            update_parms_UI(ns("update_parms")),
            update_parms_UI(ns("update_parms"), input_id = "y2_var", text = "Select Second Parameter to Plot:")
          ),
          accordion_panel(
            "Identify Outliers",
            bslib::layout_columns(
              col_widths = c(7, 5),
              selectInput(ns("filter_type"),
                          "Select Starting Method:",
                          choices = c("None" = "none",
                                      "Hampel Filter" = "hampel", "Relative Change" = "rel_change", "High Variability" = "high_var"),
                          selected = "none"),
              radioButtons(ns("selection_mode"),"Selection Mode",
                           choices = c("Add Bad" = "bad", "Add Questionable" = "questionable", "Remove Selection" = "remove"))),
            bslib::layout_columns(
              col_widths = c(3,3,1,5),
              numericInput(ns("k"),"Window Size",value =7,step=2),
              numericInput(ns("t"),"Threshold",value = 7, step=0.5),
              tags$div(
                style = "width: 1px; height: 85px; background-color: #6c7881; display: inline-block; margin: 0 30px; vertical-align: middle;"),
              div(class = "d-flex justify-content-center align-items-center",
                  style = "height: 85px;",
                  actionButton(ns("clear_sel"), "Clear Selection")))
          ),
          accordion_panel(
            "Save Edits",
            div(style="margin-bottom: 8px; font-size:16px; font-weight: bold;",
                "Remove Bad Points"),
            tags$div(style = "margin-bottom: 20px;",
                     apply_edit_UI(ns("remove_outliers"), edit_type = "remove", note="")),
            div(style="margin-bottom: 8px; font-size:16px; font-weight: bold;",
                "Flag Questionable Points"),
            apply_edit_UI(ns("flag_question"), note=""),

          ),
          accordion_panel(
            "Date Ranges",
            weekly_range_sidebar_UI(ns("date_nav")),
          ),
          accordion_panel(
            "Plotting Options",
            plot_options_UI(ns("plot_opts"),start_val = c(TRUE,TRUE,FALSE,FALSE,FALSE,TRUE))
          ))
        ),
      mainPanel(
        main_plot_UI(ns("outlier_plot")),

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
#' @param view_state A `reactiveVal` holding a list of items specifying the view state:
#'  - abs_dates: The absolute range of dates within the dataset
#'  - dates: The range of dates being viewed via the date selector
#'  - period_view: Logical if the period view is being used
#'  - period_length: Length of period view
#'  - period_n: The period number to view.
#' @param username A `reactiveVal` holding the name of the analyst for the changelog
#' @export
#' @rdname outliers
outlier_server <- function(id, sondeproj, data_ver, y_var,view_state, username){
  moduleServer(id, function(input, output, session){

  #keep track of second y_variable
    y2_var <- reactiveVal()

  #stores index of selected points
    manual_chg <- reactiveVal(list("questionable" = integer(),
                                   "bad" = integer(),
                                   "remove" = integer()))
    plot_exist <- reactiveVal() #keeps warning about missing plot
    traces <- reactiveVal() #tracks which traces hold our points to track

  #clearing manual indices if y_var or data updates
    observeEvent(list(y_var(), data_ver(), input$clear_sel),{
      manual_chg(list("questionable" = integer(),
                      "bad" = integer(),
                      "remove" = integer()))
    })

  #keep track of auto selection
    auto_index <- reactive({
      req(sondeproj(), y_var(),input$filter_type)
      if(input$filter_type == "none"){
        integer()
      }else{
        req(input$k, input$t)
        data <- sondeproj()$data
        identify_outliers(data, y_var(), input$filter_type, input$k, input$t)
      }

    })

  #merge together
    selected <- reactive({
      req(manual_chg())

      bad <- union(auto_index(), manual_chg()$bad)
      questionable <- manual_chg()$questionable
      bad <- setdiff(bad,questionable) #remove anything marked as questionable, even if auto selected
      bad <- setdiff(bad, manual_chg()$remove) #remove anything manually removed
      list("bad"=bad, "questionable"=questionable)
    })

  #clear manual removal when method changes
    observeEvent(input$filter_type,{
      edit_add <- manual_chg()
      edit_add$remove <- integer()
      manual_chg(edit_add)
    })

  #clear only the points we're saving
  observeEvent(bad_flagged(),{
    edit_add <- manual_chg()
    edit_add$bad <- setdiff(edit_add$bad, bad_flagged())
    manual_chg(edit_add)
  })

  observeEvent(question_flagged(),{
    edit_add <- manual_chg()
    edit_add$questionable <- setdiff(edit_add$questionable, question_flagged())
    manual_chg(edit_add)
  })

  #get column names after file upload (dynamic)
    update_parms_server("update_parms", sondeproj, data_ver, y_var, choices_fun = nice_yvar)
    update_parms_server("update_parms", sondeproj, data_ver, y2_var, input_id= "y2_var", choices_fun = nice_yvar)

  #get what to plot via user options
    plot_opts <- plot_options_server("plot_opts")

  #keep track of dates
    plot_dates <- weekly_range_server("date_nav", sondeproj, data_ver, view_state)

  #track selected data
    observeEvent(
      req(plot_exist(), event_data("plotly_selected", source = "outlier_plot", priority = "event")),{
        req(sondeproj(), y_var())

        data <- sondeproj()$data
        curr_add <- manual_chg()

        sel <- event_data("plotly_selected", source = "outlier_plot")
        if(is.data.frame(sel)){
          sel <- sel %>% filter(.data$curveNumber %in% traces()) %>%
            mutate(x = parse_date_time(.data$x, tz= sondeproj()$meta$tz, orders = "Ymd HMS", truncated =3))
          #get points based on x and y
          full_index <- data %>%
            mutate(value = .data[[y_var()]],
                   DateTime_rd = .data$DateTime_rd) %>%
            inner_join(sel, by = c("DateTime_rd" = "x", "value" = "y")) %>%
            pull(.data$Index)

          if(input$selection_mode == "bad"){
            curr_add$bad <- union(full_index, curr_add$bad)
            curr_add$questionable <- setdiff(curr_add$questionable, full_index)
            curr_add$remove <- setdiff(curr_add$remove, full_index)
          }

          if(input$selection_mode == "questionable"){
            curr_add$questionable <- union(full_index, curr_add$questionable)
            curr_add$bad <- setdiff(curr_add$bad, full_index)
            curr_add$remove <- setdiff(curr_add$remove, full_index)
          }

          if(input$selection_mode == "remove"){
            curr_add$bad <- setdiff(curr_add$bad, full_index)
            curr_add$questionable <- setdiff(curr_add$questionable, full_index)
            curr_add$remove <- union(full_index, curr_add$remove)
          }

          manual_chg(curr_add)
        }
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

      filter_data <- plot_data() %>% filter(!is.na(.data[[y_var()]]))

      #use function to plot sonde data
      p <- plot_sonde(data = filter_data, y_var=y_var(), y2_var= y2, proj = sondeproj(), opts=plot_opts(),
                      source="outlier_plot")

      #color points
      y <- y_var()
      indices <- selected()

      for(m in c("questionable", "bad")){
        plot_opts <- switch(m,
                            "questionable" = list(
                              nicename = "Questionable (unsaved)",
                              color = "#fac769"),
                            "bad" = list(
                              nicename = "Bad (unsaved)",
                              color = "#b83d3d"))

        flag_data <- plot_data() %>% filter(.data$Index %in% indices[[m]] & !is.na(.data[[y_var()]]))

        p <- p %>% add_trace(data= flag_data, x=~DateTime_rd, y=as.formula(paste0("~`", y, "`")), type="scatter", mode="markers",
                             name = plot_opts$nicename, marker = list(color = plot_opts$color), yaxis="y2", inherit = FALSE)

      }

      #set which traces hold points
      built_p <- plotly_build(p)
      names <- sapply(built_p$x$data, function(x){x$name})
      traces(which(names %in% c(get_yvar(y_var()), filter_data$FileName))-1)


      #return plot
      p
    })

    #save to export
    main_plot_server("outlier_plot",data_ver, sondeproj, plot_obj, plot_data, y_var, sel_mode=TRUE,plot_exist)

  # create edit object for removing data
  edit_rm <- reactive({
    newdata <- sondeproj()$data

    #only flag data within date range
    index <- newdata %>% filter(.data$Index %in% selected()$bad) %>%
      dplyr::filter(.data$Date >= plot_dates()[1], .data$Date <= plot_dates()[2]) %>% pull(.data$Index)
    newdata[[y_var()]][newdata$Index %in% index] <- NA

    note <- switch(input$filter_type,
                        "hampel" = paste0("Data removed based on Hampel Filter",
                                          " method with a window size of ", input$k, " and threshold of ", input$t,
                                          " paired with manual selection."),
                        "rel_change" = paste0("Data removed based on Relative Percent Change",
                                              " method with a window size of ", input$k, " and threshold of ", input$t,
                                              " paired with manual selection."),
                        "high_var" =  paste0("Data removed based on regions of high variability",
                                             " with a window size of ", input$k, " and threshold of ", input$t,
                                             " paired with manual selection."),
                        "none" = "Data removed based on manual selection.")

    #make edit list
    list(
      data = newdata,
      rows = index,
      y_var = y_var(),
      step = "outlier removal",
      note = note,
      flag = "RM03"
    )

  })

  edit_chg <- reactive({
    newdata <- sondeproj()$data

    #only flag data within date range
    range_index <- plot_data()$Index[plot_data()$Index %in% selected()$questionable]
    index <- range_index

    #make edit list
    list(
      data = newdata,
      rows = index,
      y_var = y_var(),
      step = "outlier removal",
      note = paste0("Data flagged as questionable via manual selection."),
      flag = "QUAL02"
    )

  })

  #flagging modules
    bad_flagged <- apply_edit_server("remove_outliers", sondeproj, edit_rm, username)
    question_flagged <- apply_edit_server("flag_question", sondeproj, edit_chg, username)

  #export plot so we can check it
    exportTestValues(
      plot_obj = plot_obj(),
      changelog = sondeproj()$changelog,
      selected = selected())

  })}
