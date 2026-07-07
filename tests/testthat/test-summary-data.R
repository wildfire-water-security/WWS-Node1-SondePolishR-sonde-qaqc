library(dplyr)
library(lubridate)

test_that("combining flags works", {
  data <- SondePolishR:::combine_flags(example_sondeproj)

  #check flags got added
  expect_equal(sum(grepl("_flag", colnames(data))), 6)

  #check flags are combined
  expect_true(all(data$Temp_C_flag[52:60] == "RM02;AD02"))

  #ensure no rows are lost and nothing is rearranged
  expect_equal(nrow(example_sondeproj$data), nrow(data))
  expect_equal(example_sondeproj$data$DateTime_rd, data$DateTime_rd)
})

test_that("data summarizing works", {
  #add some more flags for testing combining flags
  proj <- example_sondeproj
  proj$flags$flag_rm$Temp_C[91:92] <- "TEST01"

  data <- SondePolishR:::combine_flags(proj) #this is input to summarize data

  #test 1 hour summary
    freq <- lubridate::period(1, "hour")
    sum_data <- summarize_data(data, freq, "mean")

      #check number of rows
      exp_rows <- nrow(data %>% dplyr::mutate(DateTime_rd = lubridate::floor_date(.data$DateTime_rd, freq)) %>% dplyr::select(DateTime_rd) %>% unique())
      expect_equal(nrow(sum_data), exp_rows)

      #check flags
      expect_equal(sum_data$fDOM_QSU_flag[1],"RM01") #testing summary
      expect_equal(sum_data$Temp_C_flag[23],"RM02;TEST01") #testing merge

      #check values
      test <- data %>% mutate(DateTime_rd = floor_date(.data$DateTime_rd, freq)) %>% group_by(DateTime_rd)
      test_mean <- test %>% summarise(fDOM_QSU = mean(fDOM_QSU, na.rm=TRUE))
      expect_equal(sum_data$fDOM_QSU, test_mean$fDOM_QSU)

      #try other sum methods
      test_median <- test %>% summarise(fDOM_QSU = median(fDOM_QSU, na.rm=TRUE))
      expect_equal(sum_data$fDOM_QSU, test_mean$fDOM_QSU)

      test_max <- test %>% summarise(fDOM_QSU = max(fDOM_QSU))
      expect_equal(sum_data$fDOM_QSU, test_mean$fDOM_QSU)

      test_min <- test %>% summarise(fDOM_QSU = min(fDOM_QSU))
      expect_equal(sum_data$fDOM_QSU, test_mean$fDOM_QSU)

      expect_false(all(test_mean$fDOM_QSU == test_min$fDOM_QSU))

  #test 1 day summary
    freq <- lubridate::period(1, "day")
    sum_data <- summarize_data(data, freq, "mean")

    #check number of rows
      exp_rows <- nrow(data %>% mutate(DateTime_rd = floor_date(.data$DateTime_rd, freq)) %>% select(DateTime_rd) %>% unique())
      expect_equal(nrow(sum_data), exp_rows)
      expect_equal(nrow(sum_data), 152)
      expect_equal(min(as.Date(data$DateTime_rd)), min(as.Date(sum_data$DateTime_rd)),ignore_attr = TRUE) #make sure we're rounding down

    #check flags
      test <- data %>% mutate(DateTime_rd = floor_date(.data$DateTime_rd, freq)) %>% left_join(sum_data, by = join_by(DateTime_rd))
      expect_equal(test$fDOM_QSU_flag.x[!is.na(test$fDOM_QSU_flag.x)], test$fDOM_QSU_flag.y[!is.na(test$fDOM_QSU_flag.x)]) #fdom if one in full, should be in merged
      expect_true(!all(test$Temp_C_flag.x[!is.na(test$Temp_C_flag.x)] == test$Temp_C_flag.y[!is.na(test$Temp_C_flag.x)])) #temp ones get merged

    #check values
      test <- data %>% mutate(DateTime_rd = floor_date(.data$DateTime_rd, freq)) %>% group_by(DateTime_rd)
      test_mean <- test %>% summarise(fDOM_QSU = mean(fDOM_QSU, na.rm=TRUE))
      expect_equal(sum_data$fDOM_QSU, test_mean$fDOM_QSU)

  #test 7 day summary
      freq <- lubridate::period(7, "day")
      sum_data <- summarize_data(data, freq, "mean")

      #check number of rows
      exp_rows <- nrow(data %>% mutate(DateTime_rd = floor_date(.data$DateTime_rd, freq)) %>% select(DateTime_rd) %>% unique())
      expect_equal(nrow(sum_data), exp_rows)
      expect_equal(nrow(sum_data), 26)

      #check flags
      test <- data %>% mutate(DateTime_rd = floor_date(.data$DateTime_rd, freq)) %>% left_join(sum_data, by = join_by(DateTime_rd))
      expect_equal(test$fDOM_QSU_flag.x[!is.na(test$fDOM_QSU_flag.x)], test$fDOM_QSU_flag.y[!is.na(test$fDOM_QSU_flag.x)]) #fdom if one in full, should be in merged
      expect_true(!all(test$Temp_C_flag.x[!is.na(test$Temp_C_flag.x)] == test$Temp_C_flag.y[!is.na(test$Temp_C_flag.x)])) #temp ones get merged

      #check values
      test <- data %>% mutate(DateTime_rd = floor_date(.data$DateTime_rd, freq)) %>% group_by(DateTime_rd)
      test_mean <- test %>% summarise(fDOM_QSU = mean(fDOM_QSU, na.rm=TRUE))
      expect_equal(sum_data$fDOM_QSU, test_mean$fDOM_QSU)

  #test 1-month summary
      freq <- lubridate::period(1, "month")
      sum_data <- summarize_data(data, freq, "mean")

      #check number of rows
      exp_rows <- nrow(data %>% mutate(DateTime_rd = floor_date(.data$DateTime_rd, freq)) %>% select(DateTime_rd) %>% unique())
      expect_equal(nrow(sum_data), exp_rows)
      expect_equal(nrow(sum_data), 6)

      #check flags
      test <- data %>% mutate(DateTime_rd = floor_date(.data$DateTime_rd, freq)) %>% left_join(sum_data, by = join_by(DateTime_rd))
      expect_equal(test$fDOM_QSU_flag.x[!is.na(test$fDOM_QSU_flag.x)], test$fDOM_QSU_flag.y[!is.na(test$fDOM_QSU_flag.x)]) #fdom if one in full, should be in merged
      expect_true(!all(test$Temp_C_flag.x[!is.na(test$Temp_C_flag.x)] == test$Temp_C_flag.y[!is.na(test$Temp_C_flag.x)])) #temp ones get merged

      #check values
      test <- data %>% mutate(DateTime_rd = floor_date(.data$DateTime_rd, freq)) %>% group_by(DateTime_rd)
      test_mean <- test %>% summarise(fDOM_QSU = mean(fDOM_QSU, na.rm=TRUE))
      expect_equal(sum_data$fDOM_QSU, test_mean$fDOM_QSU)

  #test annual summary
      freq <- lubridate::period(1, "year")
      sum_data <- summarize_data(data, freq, "mean")

      #check number of rows
      exp_rows <- nrow(data %>% mutate(DateTime_rd = floor_date(.data$DateTime_rd, freq)) %>% select(DateTime_rd) %>% unique())
      expect_equal(nrow(sum_data), exp_rows)
      expect_equal(nrow(sum_data), 1)

      #check flags
      test <- data %>% mutate(DateTime_rd = floor_date(.data$DateTime_rd, freq)) %>% left_join(sum_data, by = join_by(DateTime_rd))
      expect_equal(test$fDOM_QSU_flag.x[!is.na(test$fDOM_QSU_flag.x)], test$fDOM_QSU_flag.y[!is.na(test$fDOM_QSU_flag.x)]) #fdom if one in full, should be in merged
      expect_true(!all(test$Temp_C_flag.x[!is.na(test$Temp_C_flag.x)] == test$Temp_C_flag.y[!is.na(test$Temp_C_flag.x)])) #temp ones get merged

      #check values
      test <- data %>% mutate(DateTime_rd = floor_date(.data$DateTime_rd, freq)) %>% group_by(DateTime_rd)
      test_mean <- test %>% summarise(fDOM_QSU = mean(fDOM_QSU, na.rm=TRUE))
      expect_equal(sum_data$fDOM_QSU, test_mean$fDOM_QSU)

})
