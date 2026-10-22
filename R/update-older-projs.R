#' Split OOW removals into a step for each parameter
#'
#' As of version 0.0.9013, removing out of water periods is moved to remove similar to other outliers
#' this means it no longer applies to all analytes at once. This function will convert older projects
#' to this style consistent with the current project format. It will run on project load.
#'
#' @param proj a `sondeproj`
#'
#' @returns a `sondeproj` with any out of water periods split up by variable. Additionally,
#' data diffs will be renamed to account for the newly inserted diffs
#' @noRd

split_oow <- function(proj){
  stopifnot(inherits(proj, "sondeproj"), proj$meta$pkg_version < as.package_version("0.0.9013"))

  changelog <- proj$changelog
  diffs <- proj$diffs
  data <- proj$data

  #if no OOW periods removed just return project as normal
  if(all(!changelog$step == "removing oow")){
    return(proj)
  }

  #otherwise split up the OOW periods
  oow_steps <- any(changelog$step == "removing oow" & changelog$parameter == "all")

  while(oow_steps){
    x <- min(which(changelog$step == "removing oow" & changelog$parameter == "all"))
    diff <- proj$diffs[[changelog$diff_name[x]]]
    parms <- get_parms(data)

  #get new split diffs per parameter
    split_diff <- lapply(parms, function(y){
      new_diff <- diff
      new_diff[!grepl(y, names(new_diff))] <- list(NULL)
      new_diff
    })

  #rename diff names
    new_names <- c(changelog$diff_name[x],diff_version(changelog$diff_name[x], n=length(parms)-1))
    names(split_diff) <- new_names
    new_log <- changelog[rep(x,length(parms)),] %>% mutate(parameter = parms, diff_name = new_names)

  #update data after the change
    prelog <- changelog %>% slice(seq_len(x - 1))
    postlog <- changelog %>% slice((x + 1):n()) %>% filter(.data$diff_name != changelog$diff_name[x])

    prediffs <- diffs[prelog$diff_name[-1]] #first should be just loading data
    postdiffs <- diffs[postlog$diff_name]

    if(nrow(postlog) > 1){
      post_names <- diff_version(new_names[length(new_names)], n=length(postdiffs))
      names(postdiffs) <- post_names
      postlog <- postlog %>% mutate(diff_name = post_names)
    }

  #insert diffs
    diffs <- c(prediffs, split_diff, postdiffs)

  #update changelog
   changelog <- bind_rows(prelog, new_log, postlog)

  #check if there's more to update
   oow_steps <- any(changelog$step == "removing oow" & changelog$parameter == "all")

  }

#update project
  proj$diffs <- diffs
  proj$changelog <- changelog

 return(proj)
}
