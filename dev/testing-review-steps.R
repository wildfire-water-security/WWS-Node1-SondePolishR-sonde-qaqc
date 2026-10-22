#TODO figure out: OOW periods, flags!! -> this requires tracking what other steps affect it and removing those flags

#NOTES
  #don't just use inverse as diff, make new so we can go back and forth
  #we could tell when an undo would effect/remove part of a different correction (make note??)

#next steps -> build into app a basic review with an undo step button to see how workflow would work

## testing out how the review workflow should work/undoing a data removal
library(ggplot2)
library(dplyr)
library(gridExtra)

proj <- example_sondeproj
log <- proj$changelog
diffs <- proj$diffs
data <- proj$data

#testing undoing a single step -------
  #undo a data removal
    new_data <- apply_diff(data, diffs[["dd4"]], invert=TRUE)

    ggplot(data=data, aes(x=DateTime_rd, y=Temp_C)) + geom_point(color="black") +
      geom_point(data=new_data, color="red", alpha=0.5) + scale_x_datetime(limits=c(min(data$DateTime_rd), as.POSIXct("2024-08-15")))

    #-> puts the data back to what it looked like before the removal, removes the interpolation

  #undo a data interpolation
    new_data <- apply_diff(data, diffs[["dd5"]], invert=TRUE)

    ggplot(data=data, aes(x=DateTime_rd, y=Temp_C)) + geom_point(color="black") +
      geom_point(data=new_data, color="red", alpha=0.5) + scale_x_datetime(limits=c(min(data$DateTime_rd), as.POSIXct("2024-08-15")))
    #-> removes the interpolation

  #undo a shift correction
    new_data <- apply_diff(data, diffs[["dd3"]], invert=TRUE)

    ggplot(data=data, aes(x=DateTime_rd, y=ODO_mg_L)) + geom_point(color="black") +
      geom_point(data=new_data, color="red", alpha=0.5) + scale_x_datetime(limits=c(min(data$DateTime_rd), as.POSIXct("2024-08-15")))
    #-> removes the shift

  #so far just undoing the single step works quite well, in the case of the data removal, it should keep the other points interpolated, just affecting the removal

# testing undoing a step that's got a more complicated change history --------
  proj <- readRDS("dev/FAL_sondeproj-clean.RDS")
  log <- proj$changelog
  diffs <- proj$diffs
  data <- proj$data

  #try undoing each step by itself to see what happens
  test_undo <- function(dd){
    new_data <- apply_diff(data, diffs[[dd]], invert=TRUE)

    diff <- diffs[[dd]]
    index <- which(!sapply(diff, is.null))[1]
    date_range <- range(diff[[index]]$DateTime_rd)
    y <- names(diff)[index]

    if(y %in% get_parms(data)){
      plot_dat <- data %>% mutate(type = "before undo") %>% bind_rows(new_data %>% mutate(type = "after undo")) %>%
        filter(DateTime_rd >= date_range[1] & DateTime_rd <= date_range[2])
      p1 <- ggplot(data=plot_dat, aes(x=DateTime_rd, y=.data[[y]], color=type)) +
        geom_point(na.rm = TRUE, alpha=0.5) +
        scale_x_datetime(limits=date_range) +
        labs(title=dd)

      p2 <- ggplot(data=plot_dat, aes(x=DateTime_rd, y=.data[[y]], color=type)) +
        geom_line(na.rm = TRUE, alpha=0.5) +
        scale_x_datetime(limits=date_range) +
        labs(title=dd)

      return(list(p1,p2))
    }


  }

  test_undos <- lapply(log$diff_name[-c(1:3)], test_undo)

  grid.arrange(grobs=test_undos[[21]])

  #step dd5/dd10 overlap in time
  #step dd16/ overlap in time, undoing undos shift from dd17

