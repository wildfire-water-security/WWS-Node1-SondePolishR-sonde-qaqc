#' Handle file saving depending on the type
#'
#' @param export_path the path to save the data to including the filename.
#' @param data either a sonde project used to generate the data to save or the data to save
#' @param type a character specifying the type of data being saved options include
#' "sondeproj", "data", "dups", "gaps", "changelog", "precip", "calcheck", "fieldform", "change-report"
#' @param username the username of the user only used for report writing
#'
#' @returns invisible NULL. Save the requested data to the specified path.
#' @noRd
save_file <- function(export_path, data, type, username) {
  if(type == "data") {
    tryCatch({
      write.csv(data, export_path, row.names = FALSE, quote = TRUE)
      shinyalert::shinyalert(
        title = "Data Downloaded",
        text = "Selected data has been downloaded.",
        type = "success")
    }, warning = function(w) {
      shinyalert::shinyalert(
        title = "Download Failed",
        text = "Please ensure the file is not open.",
        type = "error")
      FALSE
    }, error = function(e) {
      shinyalert::shinyalert(
        title = "Download Failed",
        text = "Please ensure the file is not open.",
        type = "error")
      FALSE
    })
  }else if(type == "sondeproj") {
    tryCatch({
      saveRDS(data, export_path)
      shinyalert::shinyalert(
        title = "Data Downloaded",
        text = "Selected data has been downloaded.",
        type = "success")

    }, warning = function(w) {
      shinyalert::shinyalert(
        title = "Download Failed",
        text = "Please ensure the file is not open.",
        type = "error")
      FALSE
    }, error = function(e) {
      shinyalert::shinyalert(
        title = "Download Failed",
        text = "Please ensure the file is not open.",
        type = "error")
      FALSE
    })
  }else{
    tryCatch({
      if(type == "change-report"){
        show_modal_spinner(text = "Generating report...", spin="fading-circle")
        on.exit(remove_modal_spinner(), add = TRUE)
      }

      #get metadata and save
      export_metadata(type, data, username, export_path)

    }, warning = function(w) {
      shinyalert::shinyalert(
        title = "Download Failed",
        text = "Please ensure the file is not open.",
        type = "error")
      FALSE
    }, error = function(e) {
      shinyalert::shinyalert(
        title = "Download Failed",
        text = "Please ensure the file is not open.",
        type = "error")
      FALSE
    })
  }

}

#' Generate and export metadata
#'
#' @param type the type of metadata to export options include: "dups", "gaps", "changelog", "precip", "calcheck", "fieldform", "change-report"
#' @param proj the sonde project to export
#' @param username the username of the user
#' @param export_path the path to save the data to including the filename.
#'
#' @returns invisible NULL. Save the requested data to the specified path.
#' @noRd
#'
export_metadata <- function(type, proj, username, export_path){
  stopifnot(inherits(proj, "sondeproj"), type %in% c("dups", "gaps", "changelog", "precip", "calcheck", "fieldform", "change-report"),
            dir.exists(dirname(export_path)))
  #if change report generate that and save to path
  if(type == "change-report"){
    generate_report(proj, username, export_path)

    shinyalert::shinyalert(
      title = "Metadata Downloaded",
      text = "Selected data has been downloaded.",
      type = "success")

  }else{
    #otherwise
    meta <- switch(type,
                   "dups" = proj$duplicates,
                   "gaps" = proj$data_gaps,
                   "changelog" = proj$changelog %>% mutate(datetime = format(.data$datetime, "%Y-%m-%d %H:%M:%S")),
                   "precip" = proj$precip %>% mutate(DateTime = format(.data$DateTime, "%Y-%m-%d %H:%M:%S")),
                   "calcheck" = proj$calcheck %>% mutate(Est_Time = format(.data$Est_Time, "%Y-%m-%d %H:%M:%S")),
                   "fieldform" = proj$fieldform)

  #guard against missing metadata options
    if(is.null(meta)){
      shinyalert::shinyalert(
        title = "No Data Available",
        text = "Selected data is not available.",
        type = "warning")
      }else{
        write.csv(meta, export_path, row.names = FALSE, quote = TRUE)

        shinyalert::shinyalert(
          title = "Metadata Downloaded",
          text = "Selected data has been downloaded.",
          type = "success")
        }
  }

  return(invisible(NULL))

}
