#' Track differences between sonde data
#'
#' Compares the values between the two `data.frames` and creates a small `diff` object that stores the
#' datetimes of the change and the new and old values. Used to move between versions. Note that this function
#' does not currently support addition and removal of columns, but row additions/subtractions are supported.
#'
#' @param olddata a `data.frame` with the original data.
#' @param newdata a `data.frame` with the revised data.
#' @param id name of the column name used to match observations between `olddata` and `newdata`. Must be convertible to a number.
#' @param ignore a character vector including any column names to not track.
#'
#' @returns A `diff` object with a named list item for each column being tracked.
#' Each list item will either be `NULL` if there were no changes to that column or have the following structure:
#' - op_type: a character describing the type of change made
#' - id: the id values for the change made
#' - new_data: the values of the changed values in `newdata`
#' - old_data: the values of the changed values in `olddata`
#' @export
#' @md
#' @examples
#' data1 <- example_data[1:10,]
#' data2 <- data1
#' data2$fDOM_QSU[1:4] <- NA
#' get_diff(data1, data2)

get_diff <- function(olddata, newdata, id=c("DateTime_rd", "DupNum"), ignore=NA){
  #don't support adding/removing columns right now
  x <- colnames(olddata)
  y <- colnames(newdata)

  if(length(union(setdiff(x, y), setdiff(y, x))) > 0){
    stop("Column names differ between old and new data, diff can't be determined.")
  }

  #join together so we can match datetimes and make any added data NA in old data
  data_merge <- olddata %>% mutate(source = "old") %>% bind_rows(newdata %>% mutate(source = "new"))

  #get columns we want to check for differences (include the id column)
  cols <- colnames(olddata)
  cols <- cols[!(cols %in% na.omit(c(id, ignore)))]

  #across tracked columns get diff
  diff <- lapply(cols, .col_diff, data_merge, id=id)

  #put names of columns for nicer storing
  names(diff) <- cols

  #if all columns are added with same number of rows, rename operation to data_merge
  if(.is_data_merge(diff)){
    #skip flags
    flags <- grep("_flag$", names(diff))

    diff[-flags] <- lapply(diff[-flags], function(x){x$op_type <- "data_merge"
    return(x)})}

  #make class "diff"
  class(diff) <- "diff"

  return(diff)
}

#' Identify column-level differences
#'
#' Used as a helper for `get_diff` to identify column level differences and format the list if difference tracking.
#'
#' @param param the column name to compare.
#' @param data_merge a data.frame of the old and new data row bound together, appended with a new column `source`
#' to define if it was `old` from `olddata` or `new` from `newdata`.
#' @param id name of the column name used to match observations between `olddata` and `newdata`.

#' @returns
#' Either `NULL` if there were no changes to the specified column or a `data.frame` with the following columns:
#' - id: the id values for the change made (may be more than one column)
#' - new_data: the values of the changed values in `newdata`
#' - old_data: the values of the changed values in `olddata`
#' - op_type: a character describing the type of change made

#' @noRd
.col_diff <- function(param, id=c("DateTime_rd", "DupNum"), data_merge){
  merge <- data_merge %>%
    dplyr::select(dplyr::all_of(c(id, "source", param))) %>%
    tidyr::pivot_wider(names_from = "source", values_from=dplyr::all_of(param))

 #if flag column, set old from NULL to NA
  if(grepl("_flag$",param)){
    merge$old[sapply(merge$old, is.null)] <- list(c(NA))
    merge$new[sapply(merge$new, is.null)] <- list(c(NA))
  }

  changed <- !mapply(identical, merge$old, merge$new)

  if(sum(changed) == 0){return(NULL)}
  old <- merge$old[changed]
  new <- merge$new[changed]

  op_type <- dplyr::case_when(
    is.na(old) ~ "data_added",
    is.na(new) ~ "data_removed",
    !is.na(old) & !is.na(new) ~ "data_changed")

  col_diff <- merge[changed,] %>%  ungroup() %>% mutate(op_type = op_type)

  return(col_diff)
}

#' Apply a diff object to a dataset
#'
#' Once changes between `data.frame`s have been saved as a `diff` object, they can be used to move between
#' the changes made by applying the `diff` to data.
#'
#' @param data the data to apply the `diff` to. Must contain all the columns in `diff` or a list of flags.
#' @param diff a list of `diff` or a single `diff` objects generated using `get_diff`.
#' @param invert logical. If `TRUE` changes will be reversed.
#' @param id name of the column name used to match observations between `olddata` and `newdata`.
#' @param skip_merge logical. If `TRUE` will skip any `diff` with are data merges.
#'
#' @returns a `data.frame` with the same columns as `data` with the changes from `diff` applied.
#' Note that this could increase the number of rows if diff is a data merge.
#' @export
#' @md
#'
#' @examples
#' data1 <- example_data[1:10,]
#' data2 <- data1
#' data2$fDOM_QSU[1:4] <- NA
#' diff <- get_diff(data1, data2)
#'
#' #get new data from original
#' newdata2 <- apply_diff(data1, diff)
#' all.equal(data2, newdata2)
#'
#' #get orginal data from new data
#' newdata1 <- apply_diff(data2, diff, invert=TRUE)
#' all.equal(data1, newdata1)
#'
apply_diff <- function(data, diff, id=c("DateTime_rd", "DupNum"), invert = FALSE, skip_merge=TRUE){
#apply multiple diffs if provided
  if(inherits(diff, "list")){
    if(invert){diff <- rev(diff)} #need to flip the order we apply in if we're inverting
    #loop through list
    for(x in diff){
      #skip data merge if requested
      if(!.is_data_merge(x) | (.is_data_merge(x) & !skip_merge)){
        data <- apply_diff(data, x, id, invert)
      }}

    return(data)
  }

  #if data merge, do all together
  if(.is_data_merge(diff) & !skip_merge){
    if(!invert){
      add_data <- lapply(1:length(diff), function(x){
        new <- data.frame(val=diff[[x]]$new)
        if(nrow(new) == 0){new <- data.frame(list(c(NA)))}
        colnames(new) <- names(diff)[x]
        new }) %>% bind_cols()

      #add id cols
      ids <- diff[[1]] %>% select(any_of(id))

      add_data <- ids %>% bind_cols(add_data)

      #merge with existing data
      data <- data %>% bind_rows(add_data)

    }else{
      rm <- diff[[1]] %>% select(any_of(id))
      data <- data %>% anti_join(rm,by = id)
    }

    return(data)
  }else if(!.is_data_merge(diff)){
    for(x in names(diff)){
      data <- .col_apply(x, data, diff, id, invert)
    }
  }

  return(data)
}

#' Helper function to apply column-level changes
#'
#' Used as a helper for `apply_diff` to make column level changes and return the data.
#'
#' @param param the column name to apply changes to.
#' @param data the data to apply the `diff` to. Must contain all the columns in `diff`.
#' @param diff a `diff` object generated using `get_diff`.
#' @param id name of the column name used to match observations between `olddata` and `newdata`.
#' @param invert logical. If `TRUE` changes will be reversed.
#'
#' @returns
#' a `data.frame` with the same columns as `data` with the changes from `diff` applied.
#' @noRd
#'
.col_apply <- function(param, data, diff,id, invert){
  #get col diff and make sure there's changes
    col_diff <- diff[[param]]
    if(is.null(col_diff)){return(data)}

  #identify rows to modify
    rows <- data %>% ungroup() %>%
      dplyr::mutate(.row = dplyr::row_number()) %>%
      dplyr::inner_join(col_diff, by = id) %>%
      dplyr::pull(.row)

  #apply change
  newdata <- data
  if(invert){
    newdata[[param]][rows] <- col_diff$old
  }else{
    newdata[[param]][rows] <- col_diff$new

  }

  return(newdata)
}

#' Determine if a diff is a data merge
#'
#' Checks if all tracked rows had data added which would indicate a data.frame got added,
#' ignores flag columns
#'
#' @param diff a `diff` object generated using `get_diff`.
#'
#' @returns TRUE or FALSE
#' @noRd
.is_data_merge <- function(diff){
  #ignore flags for determining merge
  flags <- grep("_flag$", names(diff))
  diff <- diff[-flags]

  #see if change to each col
  has_diff <- all(sapply(diff, is.data.frame))

  #determine if merged
  op <- sapply(diff, function(x){unique(x$op_type)})

  merge <- has_diff && (all(op == "data_added") | all(op == "data_merge"))
  return(merge)
}

#' Get raw data from sonde project
#'
#' Includes any raw data merges and returns the raw data without any changes. Useful for viewing data before changes were made.
#' Will include all raw data, even if additional datasets were merged partway through cleaning.
#'
#' @param proj A `sondeproj` object.
#' @returns a `data.frame` with raw data.
#' @export
#' @md
#' @examples
#' raw <- get_raw_data(example_sondeproj)
get_raw_data <- function(proj){
  curr_data <- proj$data
  diff <- proj$diffs

  if(length(diff) >0){
    raw_data <- apply_diff(curr_data, diff, invert=TRUE, skip_merge = TRUE, id=c("DateTime_rd", "DupNum"))
    return(raw_data)
  }else{return(curr_data)}

}
