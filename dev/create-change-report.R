## testing code out to create a report to summarize data changes

#still to figure out/do
  #-figure out how to deal with marking questionable -> look for step and show points?
  #-check fDOM correction plot/correction
  #rho value in correction still blank
#read in project with changes
proj <- example_sondeproj
log <- proj$changelog


#get groups and ranges for plotting
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
  ranges <- ranges %>% mutate(length = as.numeric(difftime(end, start, units = "mins"))/int) %>%
    mutate(start = as.POSIXct(ifelse(length > 50, start - period(int * 20, units="minutes"), start - period(int * 5, units="minutes"))),
                              end = as.POSIXct(ifelse(length > 50, end + period(int * 20, units="minutes"), end + period(int * 5, units="minutes"))))

  return(ranges)
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

#create summary for each line in changelog
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
create_report_header <- function(user, site){
  header <- c(paste("---"),
              paste("title:", site, "SondePolishR Change Report"),
              paste("author: Generated on", Sys.Date(), "by", user),
              paste("date: SondePolishR version", packageVersion("SondePolishR")),
              "output:",
              "  pdf_document:",
              "    latex_engine: xelatex",
              "---", "")

  make_markdown(header)

}

#create plot (ideally before and after zoomed in but let's start with just a plot)
  #dd <- proj$diffs[[3]]
create_plot <- function(old, new, dd="dd1", plot_path){

  #determine which parameters to plot
    vars <- names(dd)[!sapply(dd, is.null)]
    vars <- vars[vars %in% get_parms(new)]

  #if no vars changed (date times added, return basic plot of all vars)
  if(length(vars) == 0){
    vars <- get_parms(new)
    plot_dat <- new %>% tidyr::pivot_longer(any_of(vars), names_to = "variable", values_to = "value")
    y_var_nice <- get_yvar(vars)
    names(y_var_nice) <- vars

    p <- ggplot(plot_dat, aes(x = DateTime_rd, y = value)) + geom_line(na.rm = TRUE) +
      labs(x="DateTime", y="Parameter Value") +
      facet_wrap(~variable, labeller = labeller(variable = y_var_nice), scales="free_y", ncol=2) +
      scale_x_datetime(date_labels = "%Y-%m-%d\n%H:%M")
    ggsave(plot_path, p, width = 8, height = ceiling(length(vars)/2)*2.5)
    return(invisible())
  }

  #otherwise we need to filter data to changes
    #determine x axis range
      int <- get_interval(new)
      rng <- get_plot_groups(vars, int, dd)

    #for each var create plot groups
    plot_dat <- lapply(1:nrow(rng), function(y){
        #combine data for plotting
        old_plot <- old %>% filter(DateTime_rd >= rng[y,1], DateTime_rd <= rng[y,2]) %>% mutate(type = "before")
        new_plot <- new %>% filter(DateTime_rd >= rng[y,1], DateTime_rd <= rng[y,2]) %>% mutate(type = "after")
        plot_dat <- bind_rows(old_plot, new_plot) %>% mutate(type = factor(type, levels=c("before", "after"), ordered=TRUE),
                                                             group = y)
        return(plot_dat)
      }) %>% bind_rows()
    if(length(vars) == 1){
      y_var_nice <- get_yvar(vars)
      p <- ggplot(plot_dat, aes(x = DateTime_rd, y = .data[[vars]],color = type)) + geom_line(na.rm = TRUE, alpha=0.7) +
        labs(x="DateTime", y=y_var_nice, color = "Edit") +
        facet_wrap(~group, scales="free", ncol=2) +
        scale_x_datetime(date_labels = "%Y-%m-%d\n%H:%M") + scale_color_manual(values=c("#ef8a62", "#67a9cf"))

      if(mean(rng$length) < 30){
        p <- p + geom_point(na.rm = TRUE, alpha=0.7)
      }

      ggsave(plot_path, p, width = 8, height = ceiling(nrow(rng)/2)*4)

    }else{
      y_var_nice <- get_yvar(vars)
      names(y_var_nice) <- vars

      plot_dat <- plot_dat %>%
        select(-ends_with("_flag")) %>% tidyr::pivot_longer(any_of(vars), names_to = "variable", values_to = "value")

      p <- ggplot(plot_dat, aes(x = DateTime_rd, y = value,color = type)) + geom_line(na.rm = TRUE, alpha=0.7) +
             labs(x="DateTime", y="Parameter Value", color="Edit") +
             facet_grid(variable~group, labeller = labeller(variable = y_var_nice), scales="free") +
             scale_x_datetime(date_labels = "%Y-%m-%d\n%H:%M") + scale_color_manual(values=c("#ef8a62", "#67a9cf"))

      if(mean(rng$length) < 30){
        p <- p + geom_point(na.rm = TRUE, alpha=0.7)
      }

      ggsave(plot_path, p, width = 8, height = ceiling(length(vars))*1.5)
     }
    return(invisible())

}

username <- "K. Wampler"
export_path <- file.path(getwd(), "dev/test-report.pdf")
proj <- readRDS("../WWS-Node1-SONDE-postfire-sonde-network/code/exploratory/WWS-2026/FAL_sondeproj-clean.rds")

generate_report <- function(proj, username, export_path){
  log <- proj$changelog[-1,] #remove first row that's just loading data
  old <- get_raw_data(proj) #get data before change

  #create temp r file
  path <- tempfile("sondepolishR-report", fileext=".R")
  dir <- tempdir()

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
  rmarkdown::render(path, output_format = "pdf_document", output_file=export_path)

}

generate_report(proj, username, export_path)
