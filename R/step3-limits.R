#' @export
#' @rdname limits
limits_UI <- function(id){
  ns <- NS(id) #line to make module work
  tagList(
    sidebarLayout(
      sidebarPanel(
        update_parms_UI(ns("update_parms")),

      #select physical limits
        tags$h4("Set Physical Limits"),
            numericInput(ns("max"),
              HTML("<b>Maximum</b> Physical Limit"), value = NULL),
            numericInput(ns("min"),
                   HTML("<b>Minimum</b> Physical Limit"),value = NULL),

            input_switch(ns("rm_flags"), "Hide Flagged Data"),
        HTML("<hr>"),
        #date options
        weekly_range_sidebar_UI(ns("date_nav")),

        HTML("<hr>"),
        tags$h4("Flag Points"),
        confirm_changes_UI(ns("flag1"), note="NOTE: To prevent a point from being flagged, select the appropriate row in the table"),


      ),
      mainPanel(
        plotlyOutput(ns("limit_plot")),
        #add buttons to navigate date
        weekly_range_buttons_UI(ns("date_nav")),
        #DTOutput(NS(id, "flagged_table")),
        #selectableDT_UI(NS(id, "flagged_table"))
      ))


  )}


#' Flagging data the is outside specified limits
#'
#' There are certain thresholds for some of the sonde parameters that aren't physical possible (i.e, water temperature above 100 deg C).
#' This module visualizes those limits and flags data outside specified limits.
#'
#' @keywords internal
#'
#' @param id An ID string passed to shiny::NS(), used for namespacing UI inputs/outputs.
#' @param sondeproj A `reactiveVal` holding the current dataset.
#'
#' @export
#' @rdname limits
limits_server <- function(id, sondeproj){
  moduleServer(id, function(input, output, session){

  #get column names after file upload (dynamic)
    y_var <- update_parms_server("update_parms", sondeproj, choices_fun = nice_yvar)

  #update limits in UI
    observeEvent(list(y_var(), sondeproj()), {
      req(sondeproj(), y_var())

      updateNumericInput(session,"min",
        value = min(sondeproj()$data[[y_var()]], na.rm=TRUE))

      updateNumericInput(session,"max",
        value = max(sondeproj()$data[[y_var()]], na.rm=TRUE))
    })

  #keep track of dates
    dates <- weekly_range_server(
      "date_nav",
      min_date = reactive({req(sondeproj())
        min(sondeproj()$data$Date, na.rm = TRUE)}),
      max_date = reactive({req(sondeproj())
        max(sondeproj()$data$Date, na.rm = TRUE)}))


  #   #initialize
  #   usgs_limit <- reactiveVal()
  #   data_plot <- reactiveVal()
  #   newdata <- reactiveVal()
  #   #data_lim <- reactiveVal()
  #
  #   observeEvent(sdata(), {newdata(sdata())}, ignoreInit = TRUE)
  #
  #
 #
  # #get limits for plotting
  #   observeEvent(list(input$usgs_limit, input$ecoregion, y_var()), {
  #     req(y_var(), input$ecoregion, input$usgs_limit == TRUE)
  #
  #     lim <- SondePolishR::phys_limits %>% filter(.data$ecoregion == input$ecoregion & .data$metric == y_var())
  #
  #     if(nrow(lim) > 0){
  #       usgs_limit(lim)
  #       updateNumericInput(session, "min", value=lim$min)
  #       updateNumericInput(session, "max", value=lim$max)
  #     }else{
  #       shinyalert::shinyalert(
  #         title = "No USGS Limits Found",
  #         text = "No USGS limits available for this parameter and ecoregion, please use manual limits",
  #         type = "error")
  #         shinyWidgets::updateSwitchInput(session, "usgs_limit", value=FALSE)
  #       }
  #
  #   })
  #
  # #get data for plotting
  #   data_table <- reactive({
  #     req(y_var(), sdata(), input$min, input$max)
  #     SondePolishR::physical_limit(sdata(), input$min, input$max, par=y_var())})
  #
  #   observe({
  #     req(y_var(),sdata(),input$min, input$max)
  #     if(any(!is.na(selected()))){
  #       sep_data <- SondePolishR::physical_limit(sdata(),input$min, input$max, par=y_var(), keep=selected())
  #     }else{
  #       sep_data <- SondePolishR::physical_limit(sdata(),input$min, input$max, par=y_var())
  #     }
  #       data_plot(sep_data)
  #
  #   })
  #
  #   observeEvent(input$rm_flags,{
  #     req(data_plot())
  #     init_dat <- data_plot()
  #     rm_dat <- remove_flagged(init_dat, "limits")
  #     data_plot(rm_dat)
  #   })
  #
  # #make table
  #   dat <- reactive({
  #     req(data_table()$outlier, y_var())
  #      data_table()$outlier %>% dplyr::select(dplyr::any_of(c("Date_MM_DD_YYYY", "Time_HH_mm_ss", y_var()))) %>%
  #        dplyr::mutate(dplyr::across(dplyr::any_of("Date_MM_DD_YYYY"), ~ as.Date(.)))
  #     })
  #
  #   rows_selected <- selectableDT_server("flagged_table", dat)
  #
  # #get selected rows to exclude from flagging
  #   selected <- reactive({
  #     req(data_table())
  #     if(is.null(rows_selected())){
  #       return(NA)
  #     }else{
  #       data_table()$outlier$Index[rows_selected()]
  #     }
  #   })
  #
  # #make plot
  #   plot_obj <- reactive({
  #     req(data_plot(), input$min, input$max, y_var())
  #
  #     p <- ggplot(data_plot()$within, aes(x=.data$DateTime, y=.data[[y_var()]])) + geom_point(na.rm=TRUE) +
  #       ggplot2::geom_hline(yintercept = input$min, color="darkred") +
  #       ggplot2::geom_hline(yintercept = input$max, color="darkred")
  #
  #     if(nrow(data_plot()$outlier) > 0 & !input$rm_flags){
  #       p <- p + geom_point(data=data_plot()$outlier, mapping=aes(x=.data$DateTime, y=.data[[y_var()]]), color="darkred")
  #     }
  #
  #     return(p)
  #   })
  #
  #   #return plot
  #   output$limit_plot <- renderPlotly({plot_obj()})
  #
  #   #export plot and table so we can check it
  #   exportTestValues(
  #     plot_obj = plot_obj(),
  #     outlier_tab = dat()
  #   )
  #
  # #confirm changes
  #   index <- reactive({data_plot()$outlier$Index}) #get index of points to remove
  #
  # #make new dataset
  #   observeEvent(data_plot()$outlier$Index, {
  #     req(index(), y_var(), is.data.frame(newdata()))
  #     colnum <- which(colnames(newdata()) == y_var())
  #     updated <- newdata()
  #     updated[index(),colnum] <- NA
  #     newdata(updated)
  #   })
  #
  #  SondePolishR::confirm_changes_server(
  #     id = "flag1",
  #     newdata = newdata,
  #     sdata = sdata,
  #     index = index,
  #     par = y_var,
  #     flag_name = "Absolute Limits",
  #     note = reactive(paste0("limits (",input$min, "-", input$max ,")")),
  #     prj_path = prj_path,
  #     log = log
  #   )


  })}
