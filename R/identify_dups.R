
#' Identify duplicated observations with sonde data
#'
#' Uses the interval-rounded datetime (`DateTime_rd`) observations to identify periods of duplicated
#' observations. Based on if they match and the length of the section it identifies a likely cause of
#' the duplication.
#'
#' @param data a `data.frame` with Sonde data.
#'
#' @returns a `data.frame` with the following columns with a row for each duplicated period:
#' - start: starting datetime of the duplicated data
#' - end: ending datetime of the duplicated data
#' - duptype: either "same file" or "multiple files" indicating the type of duplication
#' - ndif: number of points with the duplicated section that are not exactly the same
#' - length: number of points in the duplicated section
#' - perc_dif: percentage of the data that's different
#' - likely_issue: based on information about the duplicates, a guess about the cause of the duplicate
#' - user_note: user input information about the duplicated section
#'
#' @export
#' @md
#' @examples
#' identify_dups(example_data)
identify_dups <- function(data){
  stopifnot(inherits(data, "data.frame"))
  #gets start and end of duplicated sections
  dup_rng <- data %>% dplyr::select(-c("FileName", "DateTime")) %>% group_by(.data$DateTime_rd) %>%
    summarise(count = n()) %>% filter(.data$count > 1) %>%
    reframe(start = summarise_date_ranges(.data$DateTime_rd, ignore=0)$start,
            end = summarise_date_ranges(.data$DateTime_rd, ignore=0)$end)

  if(nrow(na.omit(dup_rng)) == 0){
    return(NULL)
  }
  #get more information about the duplicates to determine the likely issue
  problems <- data.frame()
  for(r in 1:nrow(dup_rng)){
    plot_dat <- data %>% filter(.data$DateTime_rd >= dup_rng$start[r] &
                                  .data$DateTime_rd <= dup_rng$end[r]) %>%
      pivot_longer(-c("Index":"Site_Name"), names_to = "parameter", values_to = "measure")

    #check for different values (if more than one file)
    if(length(unique(plot_dat$FileName)) > 1){
      dat <- plot_dat %>% select(-"Index") %>% pivot_wider(names_from="FileName", values_from="measure")
      colstart <- which(colnames(dat) == "parameter") +1
      colend <- ncol(dat)
      colnames(dat)[colstart:colend] <- paste0("file", 1:length(colstart:colend))

      if("file3" %in% colnames(dat)){
        dif1 <- dat$file1 - dat$file2
        dif2 <- dat$file1 - dat$file3
        dif3 <- dat$file2 - dat$file3

        avg_dif <- rowSums(cbind(dif1,dif2,dif3), na.rm=TRUE)

        dat$dif <- avg_dif
      }else{
        dat$dif <- dat$file1 - dat$file2
      }
      problem <- dat %>% group_by(.data$DateTime_rd) %>% summarise(dif = sum(.data$dif, na.rm = TRUE), .groups="keep") %>%
        mutate(exact = ifelse(.data$dif != 0, FALSE, TRUE),
               duptype = "multiple files",
               duprow = r)

    }

    #if single file due to malfunction
    if(length(unique(plot_dat$FileName)) == 1){
      problem <- plot_dat %>%
        group_by(.data$DateTime_rd, .data$parameter) %>%
        summarise(dif = max(.data$measure)-min(.data$measure), .groups="keep") %>%
        group_by(.data$DateTime_rd) %>% summarise(dif = sum(.data$dif, na.rm = TRUE), .groups="keep") %>%
        mutate(exact = ifelse(.data$dif != 0, FALSE, TRUE),
               duptype= "same file",
               duprow = r)
    }

    problems <- problems %>% bind_rows(problem)
  }

  #get ranges again to see which ranges are exact and which are different
  dup_check_dif <- problems %>% dplyr::select("DateTime_rd", "duptype","exact") %>% distinct() %>% group_by(.data$duptype) %>%
    reframe(start = summarise_date_ranges(.data$DateTime_rd, .data$exact, ignore=0)$start,
            end = summarise_date_ranges(.data$DateTime_rd,.data$exact, ignore=0)$end,
            ndif = summarise_date_ranges(.data$DateTime_rd,.data$exact, ignore=0)$n_dif)

  #guess at cause (but have to verify)
  dup_check <- dup_check_dif %>% mutate(length = time_diff(.data$start, .data$end, "15 minutes")+1,
                                        perc_dif = round(.data$ndif / .data$length *100, 2),
                                        likely_issue = case_when(
                                          duptype == "multiple files" & .data$perc_dif < 2 ~ "data downloaded multiple times",
                                          duptype == "same file" & .data$perc_dif == 100 & .data$length < 2 ~ "sonde malfunctioned creating erroneous reading",
                                          duptype == "same file" & .data$ndif <= 1 ~ "sonde malfunctioned duplicating data",
                                          duptype == "multiple files" & .data$length > 50 & .data$perc_dif > 10 ~ "!!data likely has a mislabeled site name",
                                          duptype == "multiple files" & .data$length <= 50 & .data$perc_dif >50 ~ "multiple readings during sonde switching",
                                          TRUE ~ "!!unknown")) %>%
    arrange(.data$start) %>% select("start", "end","duptype":"likely_issue") %>% mutate(user_note = NA)

  return(dup_check)
}
