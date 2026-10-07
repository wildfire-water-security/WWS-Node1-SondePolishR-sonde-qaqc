#' Generate a change report from a sonde project
#'
#' Use the version control information to create a `.pdf` report
#' @param proj the `sondeproj` you want to generate the report for
#' @param username the username of the person generating the report
#' @param export_path the path including the file name to save the report to, should end with a `.pdf`
#'
#' @returns a `.pdf` document with a page for each line in the change log of the `sondeproj`.
#' @export
#' @md
#'
#' @examples
#' temp_report <- withr::local_tempfile(fileext = ".pdf")
#' generate_report(example_sondeproj, "Smith", temp_report)
generate_report <- function(proj, username, export_path){
  log <- proj$changelog[-1,] #remove first row that's just loading data
  old <- get_raw_data(proj) #get data before change

  #create temp r file
  dir <- withr::local_tempdir()
  path <- file.path(dir, "sondepolishR-report.R")

  header <- create_report_header(username, proj$meta$site)

  text <- c() #spot to put the text generated
  for(x in 1:nrow(log)){
    info <- log[x,] #get info for report
    dd <- proj$diffs[[log$diff_name[x]]]
    new <- apply_diff(old, dd,
                      id = c("DateTime_rd", "DupNum"))
    text <- c(text, create_report_record(dir, old, new, info, dd))
    old <- new #replace old data with the revised data
  }

  #write lines to render
  writeLines(unlist(c(header, text)), path)

  #knit together
  rmarkdown::render(path, output_format = "pdf_document", output_file=export_path,quiet=TRUE)

}

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

#' Create markdown record for a data change
#'
#' Uses information from the changelog and project to create the markdown text/code required
#' to write the report page about the change.
#'
#' @param dir the directory to write the plot to
#' @param old the dataset before the change
#' @param new the dataset after the change
#' @param info the info related to the change from the changelog
#' @param dd the data `diff` object
#'
#' @returns a string of text that when knitted results in the report page.
#' @noRd

create_report_record <- function(dir, old, new, info, dd){
  #replace unicode character
  info$note <- gsub("\U03C1", "rho", info$note)

  header <- c(paste("**Edit Type:**", info$step, "\\"),
              paste("**Parameter:**", info$parameter, "\\"),
              paste("**Analyst:**", info$user, "\\"),
              paste("**Edit Time:**", info$datetime, "\\"),
              paste("**Points Changed (#):**", info$n_changed, "\\"),
              paste("**Note:**", info$note, "\\"),
              "\\",
              "\\")

  header <- make_markdown(header)

  plot_path <- file.path(dir, paste0("change-", info$diff_name, ".png"))

  #create plot image
  create_plot(old, new, dd=dd,plot_path)

  #add text to read it into the markdown
  plot_code <- make_markdown(c(paste0("![](", plot_path, ")"),"\\", "\\newpage"))

  return(c(header, plot_code))

}

#create report header
#' Create change report header
#'
#' Writes the report header in a knit-able form.
#'
#' @param user the username of the person generating the report
#' @param site the site the report is being generated for
#'
#' @returns a string of text that when knitted results in the yaml header for the markdown.
#' @noRd
#'
create_report_header <- function(user, site){
  header <- c(paste("---"),
              paste("title:", site, "SondePolishR Change Report"),
              paste("author: Generated on", Sys.Date(), "by", user),
              paste("date: SondePolishR version", packageVersion("SondePolishR")),
              "output:",
              "  pdf_document:",
              "    latex_engine: xelatex",
              "---", "", "\\newpage")

  make_markdown(header)

}


#' Get groups and date ranges for plotting
#'
#' Given the dates of the changed data, will determine groups to plot that
#' best showcase the changes made.
#'
#' @param vars the variable(s) being plotted
#' @param int the interval time between the measurements in minutes
#' @param dd the data `diff`
#'
#' @returns a `data.frame` with the group numbers, and the start and end datetime of the group.
#' @noRd

get_plot_groups <- function(vars, int, dd){
  #determine x axis range
  dates <- dd[[vars[1]]]$DateTime_rd
  xrng <- range(dates)
  gap <- 8*60
  dates <- sort(dates)

  grp <- cumsum(c(TRUE, diff(dates) > minutes(gap)))

  if(length(unique(grp)) < 4){
    ranges <- data.frame(
      start = as.POSIXct(tapply(dates, grp, min)),
      end   = as.POSIXct(tapply(dates, grp, max)))
  }else{
    ranges <- data.frame(start = min(dates),end = max(dates))
  }

  #extend a little
  ranges <- ranges %>% mutate(length = as.numeric(difftime(.data$end, .data$start, units = "mins"))/int) %>%
    mutate(start = as.POSIXct(ifelse(length > 50, .data$start - period(int * 20, units="minutes"), .data$start - period(int * 5, units="minutes"))),
           end = as.POSIXct(ifelse(length > 50, .data$end + period(int * 20, units="minutes"), .data$end + period(int * 5, units="minutes"))))

  return(ranges)
}

#' Generate plots for change report
#'
#' Creates and saves a plot showing the data that was changed for inclusion in the report.
#' @param old the dataset before the change
#' @param new the dataset after the change
#' @param dd the data `diff` object
#' @param plot_path the file path to save the plot to
#'
#' @md
#' @returns the plot that is returned depends on the change type:
#' - **data addition/merge:** returns a plot showing all of the parameters within the data across the entire time range
#' - **multiple parameters changed:** such as removing OOW periods, returns a plot showing all of the parameters with the before and after
#' denoted by different colors and zoomed into the regions that were changed.
#' - **one parameter changed:** returns a plot showing the regions that were changed with the before and after denoted by different colors.
#' - **points marked as questionable:** returns a plot showing the regions of the parameter marked as questionable with the marked points
#' shown in orange.
#'
#' The created plot is saved to the file specified in `plot_path` as a png.
#'
#' @noRd
create_plot <- function(old, new, dd, plot_path){

  #determine which parameters to plot
  vars <- names(dd)[!sapply(dd, is.null)]
  #if(all("SpCond_uS_cm_flag" == vars)){browser()}

  quest <- all(grepl("flag$", vars)) #determine if it's just questionable flags

  #otherwise determine the variables to plot
  vars_noflag <- vars[vars %in% get_parms(new)]

  #if no vars changed (date times added, return basic plot of all vars)
  if(length(vars_noflag) == 0 & !quest){
    vars <- get_parms(new)
    plot_dat <- new %>% tidyr::pivot_longer(any_of(vars), names_to = "variable", values_to = "value")
    y_var_nice <- get_yvar(vars)
    names(y_var_nice) <- vars

    p <- ggplot(plot_dat, aes(x = .data$DateTime_rd, y = .data$value)) + geom_line(na.rm = TRUE) +
      labs(x="DateTime", y="Parameter Value") +
      ggplot2::facet_wrap(~variable, labeller = labeller(variable = y_var_nice), scales="free_y", ncol=2) +
      scale_x_datetime(date_labels = "%Y-%m-%d\n%H:%M")
    ggsave(plot_path, p, width = 8, height = ceiling(length(vars)/2)*2.5)
    return(invisible())
  }

  #otherwise we need to filter data to changes
  #determine x axis range
  int <- get_interval(new)
  rng <- get_plot_groups(vars, int, dd)

  #if questionable show those points
  if(quest){
    plot_dat <- lapply(1:nrow(rng), function(y){
      #combine data for plotting
      new_plot <- new %>% filter(.data$DateTime_rd >= rng[y,1], .data$DateTime_rd <= rng[y,2]) %>% mutate(group = y)
      return(new_plot)
    }) %>% bind_rows()
    quest_points <- dd[[vars]] %>% left_join(plot_dat, by=c("DateTime_rd", "DupNum"))

    plot_vars <- gsub("_flag$", "", vars) #get variable marked as questionable
    y_var_nice <- get_yvar(plot_vars)
    p <- ggplot(plot_dat, aes(x = .data$DateTime_rd, y = .data[[plot_vars]])) + geom_line(na.rm = TRUE, alpha=0.7) +
      labs(x="DateTime", y=y_var_nice) +
      facet_wrap(~group, scales="free", ncol=2) +
      scale_x_datetime(date_labels = "%Y-%m-%d\n%H:%M")

    if(mean(rng$length) < 30){
      p <- p + geom_point(na.rm = TRUE, alpha=0.7)
    }

    #add questionable points
    p <- p + geom_point(na.rm = TRUE, data=quest_points, color="orange")

    ggsave(plot_path, p, width = 8, height = ceiling(nrow(rng)/2)*4)
    return(invisible())
  }

  #for each var create plot groups
  plot_dat <- lapply(1:nrow(rng), function(y){
    #combine data for plotting
    old_plot <- old %>% filter(.data$DateTime_rd >= rng[y,1], .data$DateTime_rd <= rng[y,2]) %>% mutate(type = "before")
    new_plot <- new %>% filter(.data$DateTime_rd >= rng[y,1], .data$DateTime_rd <= rng[y,2]) %>% mutate(type = "after")
    plot_dat <- bind_rows(old_plot, new_plot) %>% mutate(type = factor(.data$type, levels=c("before", "after"), ordered=TRUE),
                                                         group = y)
    return(plot_dat)
  }) %>% bind_rows()
  if(length(vars_noflag) == 1){
    y_var_nice <- get_yvar(vars_noflag)
    p <- ggplot(plot_dat, aes(x = .data$DateTime_rd, y = .data[[vars_noflag]], color = .data$type)) + geom_line(na.rm = TRUE, alpha=0.7) +
      labs(x="DateTime", y=y_var_nice, color = "Edit") +
      facet_wrap(~group, scales="free", ncol=2) +
      scale_x_datetime(date_labels = "%Y-%m-%d\n%H:%M") + scale_color_manual(values=c("#ef8a62", "#67a9cf"))

    if(mean(rng$length) < 30){
      p <- p + geom_point(na.rm = TRUE, alpha=0.7)
    }

    ggsave(plot_path, p, width = 8, height = ceiling(nrow(rng)/2)*4)

  }else{
    y_var_nice <- get_yvar(vars_noflag)
    names(y_var_nice) <- vars_noflag

    plot_dat <- plot_dat %>%
      select(-ends_with("_flag")) %>% tidyr::pivot_longer(any_of(vars_noflag), names_to = "variable", values_to = "value")

    p <- ggplot(plot_dat, aes(x = .data$DateTime_rd, y = .data$value,color = .data$type)) + geom_line(na.rm = TRUE, alpha=0.7) +
      labs(x="DateTime", y="Parameter Value", color="Edit") +
      facet_grid(variable~group, labeller = labeller(variable = y_var_nice), scales="free") +
      scale_x_datetime(date_labels = "%Y-%m-%d\n%H:%M") + scale_color_manual(values=c("#ef8a62", "#67a9cf"))

    if(mean(rng$length) < 30){
      p <- p + geom_point(na.rm = TRUE, alpha=0.7)
    }

    ggsave(plot_path, p, width = 8, height = ceiling(length(vars_noflag))*1.5)
  }
  return(invisible())

}
