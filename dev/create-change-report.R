## testing code out to create a report to summarize data changes

#read in project with changes
proj <- example_sondeproj
log <- proj$changelog


#' Convert lines of text to markdown
#'
#' Appends lines of text with `"#'` to create markdown text using `knit::spin()`.
#'
#' @param lines a vector of characters to convert ot markdown
#'
#' @returns a character the same length as `lines`
#' @noRd
#'
#' @examples
#' lines <- c("Hello", "this", "is", "a", "test")
#' make_markdown(lines)
make_markdown <- function(lines){
  paste("#'", lines)
}

#create summary for each line in changelog
create_report_record <- function(row,changelog){
  info <- changelog[row,]

  header <- c(paste("**Edit Type:**", info$step, "\\"),
              paste("**Parameter:**", info$parameter, "\\"),
              paste("**Analyst:**", info$user, "\\"),
              paste("**Edit Time:**", info$datetime, "\\"),
              paste("**Points Changed (#):**", info$n_changed, "\\"),
              paste("**Note:**", info$note, "\\"),
              "\\",
              "\\")

  make_markdown(header)

}

#create report header
create_report_header <- function(user, site){
  header <- c(paste("---"),
              paste("title:", site, "SondePolishR Change Report"),
              paste("author: Generated on", Sys.Date(), "by", user),
              paste("date: SondePolishR version", packageVersion("SondePolishR")),
              "---", "")

  make_markdown(header)

}

#create plot (ideally before and after zoomed in but let's start with just a plot)
create_plot_code <- function(dd){
  data <- proj$data
  ggplot(data, aes(x=DateTime_rd, y=fDOM_QSU)) + geom_line() + geom_point()
}

username <- "K. Wampler"
export_path <- file.path(getwd(), "dev/test-report.pdf")

generate_report <- function(proj, username, export_path){
  proj <- example_sondeproj
  log <- proj$changelog[-1,] #remove first row that's just loading data

  header <- create_report_header(username, proj$meta$site)
  text <- sapply(1:nrow(log), create_report_record, log)

  #create temp r file
  path <- tempfile("sondepolishR-report", fileext=".R")
  writeLines(c(header, text), path)

  #knit together
  rmarkdown::render(path, output_format = "pdf_document", output_file=export_path)

}

generate_report(proj, username, export_path)
