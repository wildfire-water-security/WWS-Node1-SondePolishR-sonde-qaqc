
#' @export
#' @rdname quality-flags
quality_UI <- function(id){
  ns <- NS(id) #line to make module work
  tagList(
    useShinyjs(),

    sidebarLayout(
      sidebarPanel(
        accordion(
          open = c("Select Parameters", "Apply Quality Flags"),
          accordion_panel(
            "Select Parameters",
            update_parms_UI(ns("update_parms")),
            update_parms_UI(ns("update_parms"), input_id = "y2_var", text = "Select Second Parameter to Plot:")
          ),
          accordion_panel(
            "Apply Quality Flags",
              bslib::layout_columns(
                col_widths = c(7, 5),
                selectInput(ns("quality_flag"),
                          "Select Quality Flag:",
                          choices = c("Questionable" = "questionable", "Bad" = "bad")),
                radioButtons(ns("selection_mode"),"Selection Method",
                                       choices = c("Add" = "add","Remove" = "remove")))
          ),
          accordion_panel(
            "Save Edits",
            apply_edit_UI(ns("apply_limits"), note="Highlighted points will be flagged"),
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
        main_plot_UI(ns("quality_plot")),

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
#' @export
#' @rdname quality-flags
quality_server <- function(id, sondeproj, data_ver, y_var,view_state){
  moduleServer(id, function(input, output, session){

  #keep track of second y_variable
    y2_var <- reactiveVal()

    #stores index of selected points
    manual_add <- reactiveVal(list("questionable" = integer(),
                                   "bad" = integer()))

    plot_exist <- reactiveVal() #keeps warning about missing plot
    traces <- reactiveVal() #tracks which traces hold our points to track

    #clearing manual indices if y_var or data updates
    observeEvent(list(y_var(), data_ver()),{
      manual_add(list("questionable" = integer(),
                      "bad" = integer()))
    })

    #clear only the one we're saving
    observeEvent(sondeproj(),{
      clear_add <- manual_add()
      clear_add[[input$quality_flag]] <- integer()

      manual_add(clear_add)
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
      req(plot_exist(), event_data("plotly_selected", source = "quality_plot")),{
        req(sondeproj(), y_var())

        data <- sondeproj()$data
        curr_add <- manual_add()

        sel <- event_data("plotly_selected", source = "quality_plot")
        if(is.data.frame(sel)){
          sel <- sel %>% filter(.data$curveNumber %in% traces()) %>%
            mutate(x = parse_date_time(.data$x, tz= sondeproj()$meta$tz, orders = "Ymd HMS", truncated =3))
          #get points based on x and y
          full_index <- data %>%
            mutate(value = .data[[y_var()]],
                   DateTime_rd = .data$DateTime_rd) %>%
            inner_join(sel, by = c("DateTime_rd" = "x", "value" = "y")) %>%
            pull(.data$Index)

          if(input$selection_mode == "add"){
            curr_add <- lapply(c("questionable", "bad"), function(x){
              if(x == input$quality_flag){
                union(curr_add[[x]], full_index)
              }else{
                setdiff(curr_add[[x]], full_index)
              }
            })

            names(curr_add) <- c("questionable", "bad")

          }else{
            curr_add[[input$quality_flag]] <- (setdiff(curr_add[[input$quality_flag]], full_index))
          }
          manual_add(curr_add)
        }
      })

  #clear selection when switching modes
    observeEvent(input$quality_flag, {
      plotlyProxy("quality_plot", session) %>%
        plotlyProxyInvoke("relayout", list(selections = list()))

     #also set method back to add
      updateRadioButtons(session, "selection_mode", selected = "add")
    },ignoreInit = TRUE)

  #filter data to plot
    plot_data <- reactive({
      req(sondeproj(), plot_dates())

      sondeproj()$data %>% dplyr::filter(.data$Date >= plot_dates()[1], .data$Date <= plot_dates()[2])

    })

  #create plotly plot
    plot_obj <- reactive({
      req(y_var(),y2_var(), plot_data())
      if(y2_var() == "none"){y2 <- NULL}else{y2 <- y2_var()}

      #switch this up to change color based on flag?? show different kinds of flags??

      #if we want to filter out flagged points, filter before plotting
        filter_data <- plot_data()
        indices <- manual_add()

        flag_data <- plot_data() %>% filter(!is.na(.data[[y_var()]]))

      #use function to plot sonde data
        p <- plot_sonde(data = filter_data, y_var=y_var(), y2_var= y2, proj = sondeproj(), opts=plot_opts(), source="quality_plot")

      #color points outside limits as red
        y <- y_var()

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
    main_plot_server("quality_plot", data_ver, sondeproj, plot_obj, plot_data, y_var, sel_mode=TRUE,plot_exist)

    #redraw when back on module to prevent weird drawing issues
    observeEvent(input$modules, {
      req(input$modules == "step-4")

      plotlyProxy("quality_plot", session) %>%
        plotlyProxyInvoke("resize")
    })

  #create edit object
    edit <- reactive({
      newdata <- sondeproj()$data

      #get flags
      setna <- newdata$Index %in% manual_add()[[input$quality_flag]]

      flag_info <- switch(input$quality_flag,
                           "questionable" = list(
                             nicename = "questionable",
                             flag = "QUAL02"),
                          "bad" = list(
                            nicename = "bad",
                            flag = "QUAL01")
                          )

      #make edit list
      list(
        data = newdata,
        rows = setna,
        y_var = y_var(),
        step = "quality flags",
        note = paste0("Data flagged as ", flag_info$nicename),
        flag = flag_info$flag)

    })

  #flagging module
    apply_edit_server("apply_limits", sondeproj, edit)


  #export plot so we can check it
    exportTestValues(
      plot_obj = plot_obj(),
      changelog = sondeproj()$changelog,
      manual_add = manual_add())

  })}
