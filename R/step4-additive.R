#' @export
#' @rdname additive
additive_UI <- function(id){
  ns <- NS(id) #line to make module work

  tagList(

    sidebarLayout(
      sidebarPanel(
        SondePolishR::update_parms_UI(ns("update_parms")), #get parameters to view
        accordion(
          accordion_panel(
            title= "Shift Values",
            #get value to shift points
            fluidRow(
              div(style="margin-bottom: 8px; font-size:14px",
                  "Adjust the slope and intercept to shift the selected data:"),
              numericInput(
              NS(id, "slope_val"),
              "Slope",
              value = 0,
              step=0.001),
              numericInput(
                NS(id, "int_val"),
                "Intercept",
                value = 0,
                step=0.01)

          )),
          accordion_panel(
            title="Flag Points",
            SondePolishR::confirm_changes_UI(ns("flag2"))
          )
        )

      ),
      mainPanel(
        plotlyOutput(NS(id, "shift_plot")),
      ))

  )}

#' Address any additive shifts
#'
#' Plots loaded dataset, user can select a group of points and apply a additive shift to the data to correct for shifts.
#'
#' @param id An ID string passed to shiny::NS(), used for namespacing UI inputs/outputs.
#' @param prj_path A `reactiveVal` holding the path to save the project to.
#' @param sdata A `reactiveVal` holding the current dataset.
#' @param log A `reactiveVal` holding the change log.
#' @md
#' @keywords internal
#' @export
#' @rdname additive
#' @returns Invisible NULL
#'
additive_server <- function(id, sdata, prj_path, log){
  moduleServer(id, function(input, output, session){

    #initialize
      index <- reactiveVal() #stores index of selected points

    #update choices based on data table
      y_var <- SondePolishR::update_parms_server("update_parms", sdata, choices_fun = nice_yvar)

    #reset when dataset or yvar changes
      observeEvent(list(sdata(), y_var()), {
        index(NULL)

        updateNumericInput(session,"slope_val", value = 0)
        updateNumericInput(session,"int_val", value = 0)
      })

    #observe selection from plot and get indices of selected
      observeEvent(
        event_data("plotly_selected", source = "shift_plot"),{
          req(sdata(), y_var())

          sel <- event_data("plotly_selected", source = "shift_plot")

          if(!is.null(sel) && length(sel) && nrow(sel) > 0) {
            index(sel$pointNumber + 1)
          }else {index(NULL)}
        })

    #guess slope/intercept when points selected
      observeEvent(index(), {
          if(is.null(index())){
            new_slope <- 0
            new_int <- 0
          }else {
            shift <- guess_shift(sdata(), y_var(), index())

            new_slope <- shift$slope
            new_int <- shift$int
          }

          updateNumericInput(session,"slope_val", value = new_slope)
          updateNumericInput(session,"int_val", value = new_int)
        })


    #preview data with shift
      data_plot <- reactive({
            req(sdata(), y_var())

            dat <- sdata()
            dat$sel <- FALSE

            if(!is.null(index())){
              colnum <- which(names(dat) == y_var())

              add <- (input$slope_val * (seq_along(index())-1)) + input$int_val

              dat[index(), colnum] <- dat[index(), colnum] + add
              dat$sel <- dat$Index %in% index()
            }

            dat
          })

    # preserve zoom
      plot_lyout <- preserve_zoom(data_plot, y_var, "shift_plot")

   #plot
    output$shift_plot <- renderPlotly({
        req(data_plot(), y_var())

        dat <- data_plot()

        #if no points selected just plot normally
        if(is.null(index())){
          p <- ggplot2::ggplot(dat, ggplot2::aes(x = .data$DateTime, y = .data[[y_var()]])) + ggplot2::geom_point()
        }else {

          p <- ggplot2::ggplot(dat, ggplot2::aes(.data$DateTime, .data[[y_var()]], color = sel)) +
            ggplot2::geom_point() +
            ggplot2::scale_color_manual(values = c("white","red"), guide = "none")
        }

        p <- p %>%
          plotly::ggplotly(source = "shift_plot") %>%
          plotly::layout(dragmode = "select") %>%
          plotly::event_register("plotly_selected") %>%
          plotly::event_register("plotly_relayout")

        if(length(plot_lyout$xaxis) > 0 || length(plot_lyout$yaxis) > 0){

          p <- layout(
            p,
            xaxis = plot_lyout$xaxis,
            yaxis = plot_lyout$yaxis
          )
        }

        p

      })

    #confirm changes
    SondePolishR::confirm_changes_server(
      id = "flag2",
      newdata = data_plot,
      sdata = sdata,
      index = index,
      par = y_var,
      flag_name = "Additive Shift",
      note = reactive(paste0("shift with slope ", input$slope_val," and intercept ", input$int_val)),
      prj_path = prj_path,
      log = log)

   })
}

