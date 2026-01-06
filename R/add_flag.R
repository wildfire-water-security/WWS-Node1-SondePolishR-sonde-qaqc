#' Flag dataset and save a copy
#'
#' Adds flags to the dataset, saves the new version of the dataset, and saves the project to the
#' project path specified by the user as a `.qs` object. Also provides messages about the
#' changes being made.
#'
#' @param df a data.frame with sonde data
#' @param par the parameter to check
#' @param flag_name a character with the name of the flag
#' @param index the index values for the rows to be flagged
#' @param prj_path the file path to save the sonde project to
#' @md
#' @returns returns the flagged copy of the `data.frame`
#' @export
#'
#' @examples
#' prj_path <- file.path(withr::local_tempdir(), "test_prj.qs")
#'
#' #do intial versioning things (automatic in app)
#'   clear_data()
#'   clear_log()
#'   write_data(raw_sonde, "raw")
#'
#' #flag data
#'   df <- flag_data(raw_sonde, "fDOM_QSU", "test_flag", 1:4, prj_path)

flag_data <- function(df, par, flag_name, index, prj_path){
  stopifnot(inherits(df, "data.frame"), is.character(par), is.character(flag_name),
            is.character(prj_path), all(is.numeric(index)))

  #add flags to df
  df <- add_flags(df, par, flag_name, index)

  #save version
  if(!new_version(df)){
    if (!is.null(shiny::getDefaultReactiveDomain())) {
      shinyalert::shinyalert(
        title = "No New Changes",
        text = "Changes have already been saved",
        type = "info")}

  }else{
    version <- digest::digest(df)
    write_log(par, step=flag_name, n=length(index), version)
    write_data(df, version)

    #write to save path
    save_project(.SondePolishR$data_ver, .SondePolishR$log, prj_path)

    if (!is.null(shiny::getDefaultReactiveDomain())) {
      shinyalert::shinyalert(
        title = "Processing Complete",
        text = "Flags were added and a copy of the project was written successfully!",
        type = "success")}

  }

  return(df)
}

#' Add flags to sonde data
#'
#' Works either to provide a skeleton for the flag data (if no parameter or flag names are provided) or
#' adds a flag to the specified data. Used to keep track of which datapoints were flagged in which QA/QC steps.
#' If only some flags are added, it will try to add the missing flags.
#'
#' @param df a data.frame with sonde data
#' @param par the parameter to check
#' @param flag_name a character with the name of the flag
#' @param index the index values for the rows to be flagged
#'
#' @md
#' @returns a data.frame
#' - if `par` and `flag_name` are `NULL` it will return a new column for each parameter in the data.frame with the form <*_flag>
#' - if `par` and `flag_name` are specified it will add a named vector to the <par_flag> column specifying `TRUE` for flagged and `FALSE` for not flagged
#' @export
#'
#' @examples
#' #add flag columns
#' df <- add_flags(raw_sonde)
#' colnames(df)
#'
#' #add a flag
#' df <- add_flags(raw_sonde, "fDOM_QSU", "test_flag", c(1,2,3))
#' head(df$fDOM_QSU_flag)
add_flags <- function(df, par=NULL, flag_name=NULL, index=NULL){
  stopifnot(inherits(df, "data.frame"), is.character(flag_name)|is.null(flag_name), is.character(par)|is.null(par))

  #add flag columns if they don't exist
   #guess pars
     pars <- paste(c("Cond", "fDOM", "ODO", "Sal", "TDS", "Turbidity","TSS","pH","Temp", "Depth"), collapse="|")
     par_names <- grep(pars, names(df), value = TRUE)
     par_names <- par_names[!grepl("_flag$", par_names)] #remove existing flag columns

     missing <- par_names[!(paste0(par_names, "_flag") %in% colnames(df))]

    #add spot for flags for each parameter
      for(x in missing){
        df <- df %>% dplyr::mutate(!!paste0(x, "_flag") := NA, .after=tidyselect::all_of(x))
      }

  #add flag if inputting specific flags
  if(!is.null(flag_name)){

    stopifnot(is.character(par), length(index) > 0)

    flag_col <- paste0(par, "_flag", collapse="")

    #add flags
    flag_list <- list()
      for(i in df$Index){
        #get existing flags
        ext_flag <- df[[flag_col]][[i]]

        #check to see if it already exists
        # ##checking logic, keep for testing for now
        #  #case 1: ext_flag is NA -> return TRUE
        #     ext_flag <- NA
        #     new <- !(!all(is.na(ext_flag)) && flag_name %in% names(ext_flag))
        #
        #   #case 2: ext_flag is "text_flag", flag_name = "test" -> return TRUE
        #     ext_flag <-  c("test_flag" = TRUE)
        #     flag_name <- "test"
        #     new <- !(!all(is.na(ext_flag)) && flag_name %in% names(ext_flag))
        #
        #   #case 3: ext_flag is "text", flag_name = "text" -> return FALSE
        #     ext_flag <- c("test" = TRUE)
        #     flag_name <- "test"
        #     new <- !(!all(is.na(ext_flag)) && flag_name %in% names(ext_flag))

        new <- !(!all(is.na(ext_flag)) && flag_name %in% names(ext_flag))

        if(new){
          # create a named logical
          flag <- stats::setNames(FALSE, flag_name)

          # set TRUE if i is in the selected index
          if (i %in% index) {
            flag[flag_name] <- TRUE
          }

          #append if already flags
          if(any(!is.na(ext_flag))){
            flag <- c(ext_flag, flag)}
        }else{
          #change value
          # set TRUE if i is in the selected index
          flag <- ext_flag

          if (i %in% index) {
            flag[flag_name] <- TRUE
          }
        }


       #add to list
        flag_list[[i]] <- flag
      }

    df[[flag_col]] <- flag_list
  }

  return(df)
  }

