
#' Get skeleton flagging dataframe
#'
#' Works to provide a skeleton for the flag data (if no parameter or flag names are provided).
#'
#' @param data a data.frame with sonde data
#' @param proj a `sondeproj` object to add the flags to. If it has existing flags they will be merged
#'    with the new flags from `data` with existing flags.
#' @md
#' @returns a data.frame
#' - if `par` and `flag_name` are `NULL` it will return a `data.frame` with the same number of rows as `data` but
#'    with a blank column for each parameter in the `data.frame` only the index, datetimme, datetime_rd, and DupNum columns.
#' @export
#'
#' @examples
#' #add flag columns
#' updated_proj <- add_flags(example_sondeproj, example_data)
#' colnames(updated_proj$flags$flag_rm)
add_flags <- function(proj, data){
  stopifnot(inherits(data, "data.frame"), inherits(proj, "sondeproj"))

  #get blank flags for data
    #add flag columns if they don't exist
     #guess pars
       par_names <- get_parms(data)
       par_names <- par_names[!grepl("_flag$", par_names)] #remove existing flag columns

    #get empty flag dataframe
      flags <- data %>% dplyr::select(-any_of(c("Battery_V", "Date", "Time_HH_mm_ss","Site_Name", "FileName"))) %>%
        mutate(across(-c("Index", "DupNum", "DateTime", "DateTime_rd"), ~ NA)) %>%
        mutate(across(-c("Index", "DupNum", "DateTime", "DateTime_rd"), ~ as.character(.x)))

 #merge with existing flags if needed, otherwise just add to project
   if(!is.null(proj$flags)){
     ext_flags <- proj$flags
     new_flags <- flags %>% anti_join(ext_flags$flag_rm, by=c("DateTime_rd", "DupNum"))

     proj$flags <- lapply(proj$flags, function(x){
       x %>% dplyr::bind_rows(new_flags) %>%
         arrange(.data$DateTime, .data$DupNum) %>% mutate(Index = 1:n(), .before="DateTime")})
   }else{
     proj$flags <- list(
       flag_qual=flags,
       flag_rm = flags,
       flag_chg = flags,
       flag_add = flags)
   }

  return(proj)
  }

