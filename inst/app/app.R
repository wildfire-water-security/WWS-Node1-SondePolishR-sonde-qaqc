library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(plotly)
library(shinyFiles)
library(DT)

## TODO:
  #just marking bad points


#ensure plots inherit theme
  thematic::thematic_shiny()

#getting timezones and making nice
  tz <- SondePolishR:::nice_tz()

#front end
#' @export
#' @rdname SondePolishR-app
ui <-  page_fillable(
  #set theme
  theme = bs_theme(preset = "superhero",
                   primary = "#E3795E"),

  #set menu and navigation
    navset_card_pill(
      #step 1: load data
      nav_panel("1. Load Data",
                SondePolishR::load_data_UI("data1"),
                ),
      nav_panel("2. Visualize",
                SondePolishR::explore_data_UI("data2")
                ),
      nav_panel("3. Physical Limits",
                SondePolishR::limits_UI("data3")
                ),
      nav_panel("4. Shift Correction",
                SondePolishR::additive_UI("data4")

                ),
      nav_panel("5. Manual Removal", "Remove points manually"),
      nav_panel("6. Interpolation", "Interpolate Missing Data"),
      nav_panel("7. fDOM Corrections", "fDOM Corrections"),
      nav_panel("8. Download Data", "Download Processed Data")

  ))

#backend
#' Main SondePolishR app
#'
#' @param input
#' @param output
#' @param session
#'
#' @returns
#' @export
#' @rdname SondePolishR-app
#' @keywords internal
#' @examples
server <- function(input, output, session) {
  #define things that get passed around
    prj_path <- reactiveVal(NULL) #the project path to save data to
    sdata <- reactiveVal(NULL) #the current dataset
    log <- reactiveVal(NULL) #the data log

  #step 1: load data
   SondePolishR::load_data_server("data1", sdata, prj_path, log)

  #step 2: plot data
   SondePolishR::explore_data_server("data2", sdata, log)

  #step 3: physical limits
   SondePolishR::limits_server("data3", sdata, prj_path, log)

  #step 4: additive shift
   SondePolishR::additive_server("data4", sdata, prj_path, log)
  #export values for tests
   exportTestValues(
     prj_path = prj_path(),
     data = {
       req(sdata())
       head(sdata())
     },
     log = list(
       value = log(),
       stamp = Sys.time()
     )
   )

}

#create app
shinyApp(ui = ui, server = server)
