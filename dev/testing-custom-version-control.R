## sadly daff doesn't work so we're going to have to custom a tracking system
  #looked into other options but they don't quite do what I want them to do...

#structure for diff object that's stored --------
  #option 1: removing data
    diff <- list(
      op_type = "remove_data",
      param = "fDOM_QSU",
      datetime = as.numeric(),  #store as number
      new_data = c(NA,NA,NA),
      original_data = c(12,15,20))


  #option 2: adding data
    diff <- list(
      op_type = "add_data",
      param = "fDOM_QSU",
      datetime = as.numeric(),  #store as number
      new_data = c(12,15,20),
      original_data = c(NA,NA,NA))


  #option 3: altering data
    diff <- list(
      op_type = "change_data",
      param = "fDOM_QSU",
      datetime = as.numeric(),  #store as number
      new_data = c(12,15,20),
      original_data = c(9,5,10))

# figuring out how to get diff --------
  #removing data
    data1 <- example_data[1:10,]
    data2 <- data1
    data2$fDOM_QSU[1:4] <- NA

    changed <- vctrs::vec_compare(data1$fDOM_QSU,data2$fDOM_QSU, na_equal=TRUE) != 0

    diff <- list(
      op_type = "remove_data",
      param = "fDOM_QSU",
      datetime = as.numeric(data1$DateTime[changed]),  #store as number
      new_data = data2$fDOM_QSU[changed],
      original_data = data1$fDOM_QSU[changed])

    data1 <- example_data[1:10,]
    data1$fDOM_QSU[1:4] <- NA
    data2 <- example_data[1:15,]
  olddata <- data1
  newdata <- data2

#functions to track the changes made -------
  get_diff <- function(olddata, newdata,
                       ignore=c("DateTime")){
    stopifnot(all(colnames(olddata) == colnames(newdata))) #don't support addding/removing columns right now

    #join together so we can match datetimes and make any added data NA in old data
    data_merge <- olddata %>% mutate(source = "old") %>% bind_rows(newdata %>% mutate(source = "new"))

    #get columns we want to check for differences
      cols <- colnames(olddata)
      cols <- cols[!(cols %in% ignore)]

    #across tracked columns get diff
    diff <- lapply(cols, .col_diff, data_merge)

    #if all columns are added with same number of rows, rename operation to data_merge
      if(.is_data_merge(diff)){
        diff <- lapply(diff, function(x){x$op_type <- "data_merge"
        return(x)})}

  #put names of columns for nicer storing
    names(diff) <- cols

    return(diff)
  }

#helper function to track changes by column
.col_diff <- function(param, data_merge){
  merge <- data_merge %>%
    dplyr::select(dplyr::all_of(c("DateTime", "source", param))) %>%
    tidyr::pivot_wider(names_from = "source", values_from=param)

  changed <- vctrs::vec_compare(merge$old,merge$new, na_equal=TRUE) != 0

  if(sum(changed) == 0){return(NULL)}
  old <- merge$old[changed]
  new <- merge$new[changed]

  op_type <- dplyr::case_when(
    all(is.na(old)) ~ "data_added",
    all(is.na(new)) ~ "data_removed",
    all(!is.na(old)) & all(!is.na(new)) ~ "data_changed")

  col_diff <- list(
    op_type = op_type,
    datetime = as.numeric(merge$DateTime[changed]),
    new_data = new,
    old_data = old
  )

  return(col_diff)
}

#test 1: removing data
  data1 <- example_data[1:10,]
  data2 <- data1
  data2$fDOM_QSU[1:4] <- NA

  rm_ex <- get_diff(data1, data2)

#test 2: adding data
  add_ex <- get_diff(data2, data1)

#test 3: changing data
  data1 <- example_data[1:10,]
  data2 <- data1
  data2$Temp_C[5:7] <- c(5,4,20)

  cng_ex <- get_diff(data1, data2)

#test 4: adding data
  data1 <- example_data[1:10,]
  data2 <- example_data[1:15,]
  add_lines_ex <- get_diff(data1, data2)

#functions for applying the diff to either get back to previous data, or go forward ------
  #testing workflow
  data <- example_data[1:10,]
  diff <- rm_ex

  data$fDOM_QSU
  col_diff <- diff$fDOM_QSU #check if NULL or not

  #identify rows to modify
  rows <- match(data$DateTime,as.POSIXct(col_diff$datetime))
  rows <- na.omit(rows)

  #apply change
  newdata <- data
  newdata$fDOM_QSU[rows] <- col_diff$new_data

apply_diff <- function(data, diff, invert = FALSE){
  #if data merge, do all together
    if(.is_data_merge(diff)){
      if(!invert){
        add_data <- lapply(1:length(diff), function(x){
          new <- data.frame(val=diff[[x]]$new_data)
          colnames(new) <- names(diff)[x]
          new }) %>% bind_cols()

        add_data$DateTime <- as.POSIXct(diff[[1]]$datetime, tz= tz(data$DateTime))

        data <- data %>% bind_rows(add_data)
      }else{
        data <- data %>% filter(!(.data$DateTime %in% as.POSIXct(diff[[1]]$datetime, tz= tz(data$DateTime))))
      }

        return(data)
      }

  #otherwise apply .col_apply across data
    for(x in names(diff)){
      data <- .col_apply(x, data, diff, invert)
    }

    return(data)
  }

.col_apply <- function(param, data, diff, invert){
  #get col diff and make sure there's changes
   col_diff <- diff[[param]]
   if(is.null(col_diff)){return(data)}

   #identify rows to modify
   rows <- match(as.POSIXct(col_diff$datetime), data$DateTime)
   rows <- na.omit(rows)

   #apply change
   newdata <- data
   if(invert){
     newdata[[param]][rows] <- col_diff$old_data
   }else{
     newdata[[param]][rows] <- col_diff$new_data

   }

   return(newdata)
 }

.is_data_merge <- function(diff){
  has_diff <- sapply(diff, class) == "list"

  return(all(has_diff) && all(sapply(diff, "[[", 1) == "data_added") | all(sapply(diff, "[[", 1) == "data_merge"))
}

#testing scenarios from before
 #test 1: removing data
   data1 <- example_data[1:10,]
   data2 <- data1
   data2$fDOM_QSU[1:4] <- NA

   rm_ex <- get_diff(data1, data2)
   newdata1 <- apply_diff(data2, rm_ex, invert=TRUE)
   all.equal(data1, newdata1)

   newdata2 <- apply_diff(data1, rm_ex, invert=FALSE)
   all.equal(data2, newdata2)

 #test 2: adding data
  add_ex <- get_diff(data2, data1)
  newdata1 <- apply_diff(data2, add_ex, invert=FALSE)
  all.equal(data1, newdata1)

  newdata2 <- apply_diff(data1, add_ex, invert=TRUE)
  all.equal(data2, newdata2)

 #test 3: changing data
   data1 <- example_data[1:10,]
   data2 <- data1
   data2$Temp_C[5:7] <- c(5,4,20)

   cng_ex <- get_diff(data1, data2)

   newdata1 <- apply_diff(data2, cng_ex, invert=TRUE)
   all.equal(data1, newdata1)

   newdata2 <- apply_diff(data1, cng_ex, invert=FALSE)
   all.equal(data2, newdata2)

 #test 4: adding data
   data1 <- example_data[1:10,]
   data2 <- example_data[1:15,]
   add_lines_ex <- get_diff(data1, data2)

   newdata1 <- apply_diff(data2, add_lines_ex, invert=TRUE)
   all.equal(data1, newdata1)

   newdata2 <- apply_diff(data1, add_lines_ex, invert=FALSE)
   all.equal(data2, newdata2)

#functions to apply multiple differences at once to go from raw to fully processed or fully processed to raw -------
   data1 <- example_data[1:10,]
   data2 <- data1
   data2$fDOM_QSU[1:4] <- NA
   rm_ex <- get_diff(data1, data2)
   data3 <- data2
   data3$Temp_C[5:7] <- c(5,4,20)
   cng_ex <- get_diff(data2, data3)

   data4 <- data3 %>% bind_rows(example_data[11:15,])
   add_lines_ex <- get_diff(data3, data4)

   data5 <- data4
   data5$fDOM_QSU[1:2] <- c(5,10)
   add_data <- get_diff(data4, data5)

   diff_list <- list(rm_ex, cng_ex, add_lines_ex, add_data)

#function to apply multiple diffs
  apply_mult_diff <- function(data, diff_list, invert = FALSE, skip_merge=TRUE){
    if(invert){diff_list <- rev(diff_list)} #need to flip the order we apply in if we're inverting
    #loop through list
      for(x in diff_list){
        #skip data merge if requested
        if(!.is_data_merge(x) | (.is_data_merge(x) & !skip_merge)){
          data <- apply_diff(data, x, invert)
        }}

    return(data)

  }

#test 1: go from og data to data 5
  newdata5 <- apply_mult_diff(data1, diff_list, skip_merge=FALSE)
  all.equal(data5, newdata5)

#test 2: go from og data to data 5 (skip merge)
  newdata5 <- apply_mult_diff(data1, diff_list, skip_merge=TRUE)

#test 3: go from data 5 to data with reversing merge
  newdata1 <- apply_mult_diff(data5, diff_list, invert=TRUE, skip_merge = FALSE)
  all.equal(data1, newdata1)

#test 4: go from data 5 to data without reversing merge
  newdata1 <- apply_mult_diff(data5, diff_list, invert=TRUE, skip_merge = TRUE)

## Testing how to do version control with duplicates -------
 #start with a small dataset for testing
  proj <- example_sondeproj
  messy <- example_data[1:20,]
  same_dif <- messy[2:4,]
  same_dif[,9:15] <- same_dif[,9:15] * 1.1
  messy <- rbind(messy, same_dif) #same file
  messy <- rbind(messy, messy[8:10,]) # different file

  messy$FileName[8:10] <- "dupfile2.csv"

  messy <- messy %>% arrange(FileName, DateTime_rd) %>% mutate(Index= 1:n())

  messy <- messy %>% group_by(.data$DateTime_rd) %>% mutate(DupNum = row_number(), .after="Index")

  #testing removing data from a duplicate file
    messy2 <- messy
    messy2[messy2$FileName == "dupfile2.csv",9:15] <- NA

  #testing removing data from same file
    messy2 <- messy
    messy2[messy2$DupNum == 2,9:15] <- NA

  #testing summarizing two files into one
    pars <- paste(c("Cond", "fDOM", "ODO", "Sal", "TDS", "Turbidity","TSS","pH","Temp", "Depth"), collapse="|")
    par_names <- grep(pars, names(messy), value = TRUE)

    messy2 <- messy
    messy2 <- messy2 %>%
      group_by(Date, DateTime_rd) %>%
      mutate(across(any_of(c(par_names, "Battery_V")), ~ if_else(DupNum == 1,mean(.x, na.rm = TRUE),NA_real_)),
            across("FileName", ~ if_else(DupNum == 1,paste(unique(.), collapse = ";"),NA_character_))) %>%
      ungroup()

#getting dif including index
  dif <- get_diff(messy, messy2, id=c("DateTime_rd", "DupNum"))
  new_messy <- apply_diff(messy, dif, id=c("DateTime_rd", "DupNum"))
  identical(messy2, new_messy)

  #apply flags
    flag_diff <- dif[names(dif) %in% colnames(proj$flags$flag_rm)]
    flag_diff <- flag_diff[!sapply(flag_diff, is.null)]

    for(y_var in names(flag_diff)){
      col_diff <- flag_diff[[y_var]]

      proj$flags$flag_chg <- update_dup_flags(proj$flags$flag_chg,
                                              col_diff, y_var,"data_changed","DUP01")

      proj$flags$flag_rm  <- update_dup_flags(proj$flags$flag_chg,
                                              col_diff, y_var,"data_removed","DUP02")

    }

  test1 <- proj$flags$flag_rm
  test2 <- proj$flags$flag_chg
  proj$flags[[edit$changetype]][[edit$y_var]][edit$rows] <- edit$flag

  #update log entry
  proj <- write_log(proj, edit$y_var, edit$step, n=sum(edit$rows, na.rm=TRUE),
                    note = edit$note, diff_name = names(dif), return = "sondeproj")


  #add in new df and diff
  proj$data <- newdata
  proj$diffs <- c(proj$diffs, dif)

## test with our messy project

