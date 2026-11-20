#helpers for formatting
  #format missing values
    display_value <- function(x) {
      if (is.null(x) || length(x) == 0 || is.na(x[1]) || x[1] == "") {
        return("")}
      x
    }

  #format dates
  format_datetime <- function(x) {
    if (is.null(x) || length(x) == 0 || is.na(x[1])) {
      return("")
    }
    format(x, "%Y-%m-%d %H:%M")
  }

  #create an information section
    info_section <- function(title, ...) {
      tags$div(
        class = "change-info-section",
        tags$h6(title, class = "change-info-title"),
        tags$div(
          class = "change-info-grid",
          ...
        )
    )}

  #individual information fields
    info_field <- function(label, value, full_width = FALSE) {
      if(full_width){
        tags$div(
          style = "grid-column: 1 / -1;",
          tags$strong(paste0(label, ": ")),
          display_value(value)
        )
      }else{
        tags$div(
          tags$strong(paste0(label, ": ")),
          display_value(value)
        )
      }
    }
