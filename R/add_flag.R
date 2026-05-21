#' Flag dataset and save a copy
#'
#' Adds flags to the dataset, saves the new version of the dataset, and saves the project to the
#' project path specified by the user as a `.RDS` object. Also provides messages about the
#' changes being made.
#'
#' @param data a data.frame with sonde data
#' @param par the parameter to check
#' @param flag_name a character with the name of the flag
#' @param index the index values for the rows to be flagged
#' @param makeNA if `TRUE` the flagged data will be converted to NA values
#' @md
#' @export
#' @returns returns the flagged copy of the `data.frame`

# @param note a note from the analyst about the change made
# @param prj_path the file path to save the sonde project to, if `NULL` will try to use `prj_path` stored in package environment.

# flag_data <- function(data, par, flag_name, index, note="", prj_path=NULL, makeNA = FALSE){
#   stopifnot(inherits(data, "data.frame"), is.character(par), is.character(flag_name),
#             is.null(resolve_path(prj_path)) || dir.exists(dirname(resolve_path(prj_path))), all(is.numeric(index)), is.logical(makeNA))
#
#   #add flags to data
#   data <- add_flags(data, par, flag_name, index, makeNA)
#
#   #save version
#   if(!new_version(data)){
#     if (!is.null(shiny::getDefaultReactiveDomain())) {
#       shinyalert::shinyalert(
#         title = "No New Changes",
#         text = "Changes have already been saved",
#         type = "info")}
#
#   }else{
#     version <- digest::digest(data)
#     write_log(par, step=flag_name, n=length(index), note=note, version)
#     write_data(data, version)
#
#     #write to save path
#     if(is.null(prj_path)){prj_path <- get_prjpath()}
#     save_project(get_data(), get_log(), prj_path)
#
#     if (!is.null(shiny::getDefaultReactiveDomain())) {
#       shinyalert::shinyalert(
#         title = "Processing Complete",
#         text = "Flags were added and a copy of the project was written successfully!",
#         type = "success")}
#
#   }
#
#   return(data)
# }

#' Add flags to sonde data
#'
#' Works either to provide a skeleton for the flag data (if no parameter or flag names are provided) or
#' adds a flag to the specified data. Used to keep track of which datapoints were flagged in which QA/QC steps.
#' If only some flags are added, it will try to add the missing flags.
#'
#' @param data a data.frame with sonde data
#' @param par the parameter to check
#' @param flag_name a character with the name of the flag
#' @param index the index values for the rows to be flagged
#' @param makeNA if `TRUE` the flagged data will be converted to NA values
#' @md
#' @returns a data.frame
#' - if `par` and `flag_name` are `NULL` it will return a `data.frame` with the same number of rows as `data` but
#'    with a blank column for each parameter in the `data.frame` with the form <*_flag> and only the index, datetime and datetime_rd columns.
#' - if `par` and `flag_name` are specified it will add a named vector to the <par_flag> column specifying `TRUE` for flagged and `FALSE` for not flagged
#' @export
#'
#' @examples
#' #add flag columns
#' data <- add_flags(example_data)
#' colnames(data)
#'
#' #add a flag
#' data <- add_flags(example_data, "fDOM_QSU", "test_flag", c(1,2,3))
#' head(data)
add_flags <- function(data, par=NULL, flag_name=NULL, index=NULL, makeNA=FALSE){
  stopifnot(inherits(data, "data.frame"), is.character(flag_name)|is.null(flag_name), is.character(par)|is.null(par),
            is.logical(makeNA))

  #add flag columns if they don't exist
   #guess pars
     pars <- paste(c("Cond", "fDOM", "ODO", "Sal", "TDS", "Turbidity","TSS","pH","Temp", "Depth"), collapse="|")
     par_names <- grep(pars, names(data), value = TRUE)
     par_names <- par_names[!grepl("_flag$", par_names)] #remove existing flag columns

     missing <- par_names[!(paste0(par_names, "_flag") %in% colnames(data))]

    #add spot for flags for each parameter
      for(x in missing){
        data <- data %>% dplyr::mutate(!!paste0(x, "_flag") := NA, .after=tidyselect::all_of(x))
      }

  #add flag if inputting specific flags #TODO will need to update with new workflow for storing flags
  # if(!is.null(flag_name)){
  #
  #   stopifnot(is.character(par), length(index) > 0)
  #
  #   flag_col <- paste0(par, "_flag", collapse="")
  #
  #   #add flags
  #   flag_list <- list()
  #     for(i in data$Index){
  #       #get existing flags
  #       ext_flag <- data[[flag_col]][[i]]
  #
  #       #check to see if it already exists
  #       # ##checking logic, keep for testing for now
  #       #  #case 1: ext_flag is NA -> return TRUE
  #       #     ext_flag <- NA
  #       #     new <- !(!all(is.na(ext_flag)) && flag_name %in% names(ext_flag))
  #       #
  #       #   #case 2: ext_flag is "text_flag", flag_name = "test" -> return TRUE
  #       #     ext_flag <-  c("test_flag" = TRUE)
  #       #     flag_name <- "test"
  #       #     new <- !(!all(is.na(ext_flag)) && flag_name %in% names(ext_flag))
  #       #
  #       #   #case 3: ext_flag is "text", flag_name = "text" -> return FALSE
  #       #     ext_flag <- c("test" = TRUE)
  #       #     flag_name <- "test"
  #       #     new <- !(!all(is.na(ext_flag)) && flag_name %in% names(ext_flag))
  #
  #       new <- !(!all(is.na(ext_flag)) && flag_name %in% names(ext_flag))
  #
  #       if(new){
  #         # create a named logical
  #         flag <- stats::setNames(FALSE, flag_name)
  #
  #         # set TRUE if i is in the selected index
  #         if (i %in% index) {
  #           flag[flag_name] <- TRUE
  #         }
  #
  #         #append if already flags
  #         if(any(!is.na(ext_flag))){
  #           flag <- c(ext_flag, flag)}
  #       }else{
  #         #change value
  #         # set TRUE if i is in the selected index
  #         flag <- ext_flag
  #
  #         if (i %in% index) {
  #           flag[flag_name] <- TRUE
  #         }
  #       }
  #
  #
  #      #add to list
  #       flag_list[[i]] <- flag
  #     }
  #
  #   data[[flag_col]] <- flag_list
  #
  #   #make NA
  #   if(makeNA){
  #     colnum <- which(colnames(data) == par)
  #     data[index,colnum] <- NA
  #   }
  # }

  #return empty flag dataframe
  if(is.null(flag_name) & is.null(par)){
    data <- data %>% dplyr::select(-any_of(c(par_names, "Battery_V", "Date", "Time_HH_mm_ss","Site_Name", "FileName"))) %>%
      mutate(across(-c("Index", "DateTime", "DateTime_rd"), ~ as.character(.x)))

  }

  return(data)
  }

#' Set flagged values to missing
#'
#' @param data the `data.frame` to remove flagged values from
#' @param flag_names the flag names to set to `NA` if the flag is `TRUE`
#'
#' @returns a `data.frame` with the flagged values removed
#' @export
#'
# remove_flagged <- function(data, flag_names){
#   flags <- grep("_flag$", colnames(data), value = TRUE)
#
#   #for one column
#   for(f in flags){
#     flag_vals <- data[[f]]
#
#     #determine values with flag
#     rm <- sapply(flag_vals, function(x){
#       flag <- x[names(x) %in% flag_names]
#       return(any(flag))
#     })
#
#     #make values NA
#     if(sum(rm) > 0){
#       data[rm,gsub("_flag$", "", f)] <- NA
#     }
#   }
#   return(data)
# }
#
