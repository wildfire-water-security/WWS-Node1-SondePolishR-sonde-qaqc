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
      nav_panel("3. Data Checks",
                SondePolishR::check_data_UI("data3")
                ),
      nav_panel("4. Physical Limits",
                SondePolishR::limits_UI("data4")
                ),
      nav_panel("5. Shift Corrections",
                SondePolishR::additive_UI("data5")
                ),
      nav_panel("6. Outlier Removal",
                SondePolishR::outlier_UI("data6")
      ),
      nav_panel("7. Interpolation", "Interpolate Missing Data and apply smoothing"),
      nav_panel("8. fDOM Corrections", "fDOM Corrections"),
      nav_panel("9. Download Data", "Download Processed Data")

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

  #step 5: additive shift
   SondePolishR::additive_server("data5", sondeproj, data_ver, y_var)

  #step 6: outlier corrections
   SondePolishR::outlier_server("data6", sondeproj, data_ver, y_var)

  #export values for tests
   # exportTestValues(
   #   prj_path = prj_path(),
   #   data = {
   #     req(sdata())
   #     head(sdata())
   #   },
   #   log = list(
   #     value = log(),
   #     stamp = Sys.time()
   #   )
   # )

}

#create app
shinyApp(ui = ui, server = server)
