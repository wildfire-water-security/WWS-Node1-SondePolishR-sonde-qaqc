## trying to improve interpolation methods

data_prep <- prep_interp(example_sondeproj)

library(missForest)
## get a segment of non missing data
 #find cont segments
  which(is.na(data_prep$fill$Temp_C))

  #get set with no NA's
  data_test <- data_prep$fill[8075:14219,]

  ggplot(data_test, aes(x=DateTime_rd, y=Temp_C)) + geom_line()

#add some gaps
  prodNA(data_test$Temp_C)
