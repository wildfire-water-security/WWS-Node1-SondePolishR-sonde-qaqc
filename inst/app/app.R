library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(plotly)
library(shinyFiles)
library(DT)

## TODO:
  #just marking bad points

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
      id = "modules",
      #step 1: load data
      nav_panel("1. Load Data",
                value = "step-1",
                SondePolishR::load_data_UI("data1"),
                ),
      nav_panel("2. Visualize",
                value = "step-2",
                SondePolishR::explore_data_UI("data2")
                ),
      nav_panel("3. Data Checks",
                value = "step-3",
                SondePolishR::check_data_UI("data3")
                ),
      nav_panel("4. Physical Limits",
                value = "step-4",
                SondePolishR::limits_UI("data4")
                ),
      nav_panel("5. Outlier Removal",
                value = "step-5",
                SondePolishR::outlier_UI("data5")
      ),
      nav_panel("6. Interpolation",
                value = "step-6",
                SondePolishR::interp_UI("data6")
      ),
      nav_panel("7. Shift Corrections",
                value = "step-7",
                SondePolishR::additive_UI("data7")
                ),
      nav_panel("8. fDOM Corrections",
                value = "step-8",
                SondePolishR::fdom_UI("data8")
      ),
      nav_panel("9. Download Data",
                value = "step-9",
                SondePolishR::export_UI("data9"))

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
  #allow upload of larger files
  options(shiny.maxRequestSize=50*1024^2)

  #define things that get passed around
    sondeproj <- reactiveVal(NULL) #the sonde project
    data_ver <- reactiveVal(0) #keeping track of when new data is uploaded
    y_var <- reactiveVal(NULL) #the y-variable being looked at
  #step 1: load data
   SondePolishR::load_data_server("data1", sondeproj, data_ver)

  #step 2: plot data
   SondePolishR::explore_data_server("data2", sondeproj, data_ver, y_var)

  #step 2: plot data
   SondePolishR::check_data_server("data3", sondeproj, data_ver, y_var)

  #step 4: physical limits
   SondePolishR::limits_server("data4", sondeproj, data_ver, y_var)

  #step 5: outlier corrections
   SondePolishR::outlier_server("data5", sondeproj, data_ver, y_var)

  #step 6: data interpolation
   SondePolishR::interp_server("data6", sondeproj, data_ver, y_var)

  #step 7: additive shift
   SondePolishR::additive_server("data7", sondeproj, data_ver, y_var)

  #step 8: fdom corrections
   SondePolishR::fdom_server("data8", sondeproj, data_ver, y_var)

  #step 9: export data
   SondePolishR::export_server("data9", sondeproj, data_ver, y_var)


}

#create app
shinyApp(ui = ui, server = server)
