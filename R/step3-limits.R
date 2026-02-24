#' @export
#' @rdname limits
limits_UI <- function(id){
  ns <- NS(id) #line to make module work

  tagList(

    sidebarLayout(
      sidebarPanel(
        SondePolishR::update_parms_UI(ns("update_parms")),

        #manually specify
        accordion(
          accordion_panel(
            #select physical limits
            title = "Manually Set Limits",
            numericInput(
              NS(id, "min"),
              HTML("<b>Minimum</b> Physical Limit"),
              value = NULL
            ),

            numericInput(
              NS(id, "max"),
              HTML("<b>Maximum</b> Physical Limit"),
              value = NULL
            )),
          accordion_panel(
            #select physical limits
            title = "USGS Derived Limits",
            input_switch(NS(id, "usgs_limit"), "Use USGS Derived Limits?", FALSE),

            tags$h6("Select either Ecoregion or input site coordinates:"),

            selectInput(NS(id, "ecoregion"),
                        "Select Site Ecoregion",
                        choices = SondePolishR::ecoregions$NA_L3NAME),
            fluidRow(
              column(6,
                     textInput(NS(id,"lat"), "Latitude:")
              ),
              column(6,
                     textInput(NS(id,"long"), "Longitude:")
              ))
          ),
          accordion_panel(
            title="Flag Points",
            confirm_changes_UI(ns("flag1"), note="NOTE: To prevent a point from being flagged, select the appropriate row in the table")

          ))
      ),
      mainPanel(
        plotlyOutput(NS(id, "limit_plot")),
        #DTOutput(NS(id, "flagged_table")),
        selectableDT_UI(NS(id, "flagged_table"))
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
#' @param data A reactive holding the loaded dataset.
#'
#' @export
#' @rdname limits
limits_server <- function(id, data){
  moduleServer(id, function(input, output, session){

    # initialize copy of file for this step
    data_lim <- reactiveVal()
    observeEvent(data(), {data_lim(data())})

  #update choices based on data table
    y_var <- SondePolishR::update_parms_server("update_parms", data, choices_fun = nice_yvar)

  #update ecoregion choices based on y_var
      observeEvent(y_var(), {
        req(data(), y_var())
        eco_options <- SondePolishR::phys_limits %>% filter(.data$metric == y_var())
        #if no ecoregions available
        if(nrow(eco_options) == 0){choices <- "No Limits Available"}else{
          choices <- eco_options$ecoregion
        }
        updateSelectInput(
          session,
          "ecoregion",
          choices = choices,
          selected = choices[1]
        )
      })

  #update limits values from y_var
      SondePolishR::update_limits(data_lim, y_var, session)

  #get ecoregion if points are supplied
    observeEvent(c(input$lat, input$long), {
      req(input$lat, input$long)
      updateSelectInput(
        session,
        "ecoregion",
        selected = SondePolishR::get_ecoregion(c(input$lat, input$long))
      )})

  #get limits for plotting
    usgs_limit <- reactiveVal() #initialize
    observeEvent(list(input$usgs_limit, input$ecoregion, y_var()), {
      req(y_var(), input$ecoregion, input$usgs_limit == TRUE)

      lim <- SondePolishR::phys_limits %>% filter(.data$ecoregion == input$ecoregion & .data$metric == y_var())

      if(nrow(lim) > 0){
        usgs_limit(lim)
        updateNumericInput(session, "min", value=lim$min)
        updateNumericInput(session, "max", value=lim$max)
      }else{
        shinyalert::shinyalert(
          title = "No USGS Limits Found",
          text = "No USGS limits available for this parameter and ecoregion, please use manual limits",
          type = "error")
          shinyWidgets::updateSwitchInput(session, "usgs_limit", value=FALSE)
        }

    })

  #get data for plotting
    data_table <- reactive({
      req(y_var(), data_lim(), input$min, input$max)
      SondePolishR::physical_limit(data_lim(), input$min, input$max, par=y_var())})

    data_plot <- reactive({
      req(y_var(),data_lim(),input$min, input$max)
      if(any(!is.na(selected()))){
        sep_data <- SondePolishR::physical_limit(data_lim(),input$min, input$max, par=y_var(), keep=selected())
      }else{
        sep_data <- SondePolishR::physical_limit(data_lim(),input$min, input$max, par=y_var())
      }
    })

  #make table
    dat <- reactive({
      req(data_table()$outlier, y_var())
       data_table()$outlier %>% dplyr::select(dplyr::any_of(c("Date_MM_DD_YYYY", "Time_HH_mm_ss", y_var()))) %>%
         dplyr::mutate(dplyr::across(dplyr::any_of("Date_MM_DD_YYYY"), ~ as.Date(.)))
      })

    rows_selected <- selectableDT_server("flagged_table", dat)

  #get selected rows to exclude from flagging
    selected <- reactive({
      req(data_table())
      if(is.null(rows_selected())){
        return(NA)
      }else{
        data_table()$outlier$Index[rows_selected()]
      }
    })

  #make plot
    plot_obj <- reactive({
      req(data_plot(), input$min, input$max, y_var())

      p <- ggplot(data_plot()$within, aes(x=.data$DateTime, y=.data[[y_var()]])) + geom_point(na.rm=TRUE) +
        ggplot2::geom_hline(yintercept = input$min, color="darkred") +
        ggplot2::geom_hline(yintercept = input$max, color="darkred")

      if(nrow(data_plot()$outlier) > 0){
        p <- p + geom_point(data=data_plot()$outlier, mapping=aes(x=.data$DateTime, y=.data[[y_var()]]), color="darkred")
      }

      return(p)
    })

    #return plot
    output$limit_plot <- renderPlotly({plot_obj()})

    #export plot and table so we can check it
    exportTestValues(
      plot_obj = plot_obj(),
      outlier_tab = dat()
    )

  #confirm changes
    index <- reactive({data_plot()$outlier$Index }) #get index of points to remove

    SondePolishR::confirm_changes_server(
      id = "flag1",
      data = data_lim,
      index = index,
      par = y_var,
      flag_name = "limits",
      prj_path = get_prjpath()
    )

  })}
