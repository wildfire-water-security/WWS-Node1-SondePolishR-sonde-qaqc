test_that("version control works as expected", {
  data <- example_data

  #make some changes and save
    data1 <- data
    data1$fDOM_QSU[1:4] <- NA
    dd1 <- commit_diff(data, data1)

    expect_s3_class(dd1,"data_diff")

  #check that we can get from data to data 1 using the dd1
    data_redo <- apply_diff(data, dd1)
    expect_equal(data1, data_redo)

  #check if we make multiple sets
    #make more changes
    data2 <- data1
    data2$ODO_mg_L[5:7] <- data2$ODO_mg_L[5:7] * 0.8
    dd2 <- commit_diff(data1, data2)

    #more changes
    data3 <- data2
    data3$Temp_C[1:100] <- NA
    dd3 <- commit_diff(data2, data3)

  #get to whatever level we want from raw
    diffs <- list(dd1,dd2,dd3)

    newdata1 <- apply_diff(data, diffs[[1]])
    expect_equal(data1, newdata1)

    newdata2 <- apply_diff(data, diffs[1:2])
    expect_equal(data2, newdata2)

    newdata3 <- apply_diff(data, diffs)
    expect_equal(data3, newdata3)

  #make sure it errors if the dates aren't the same
    expect_warning(expect_error(commit_diff(data, data[-1,]),
                                "Dates are different between the two datasets"))

  #check that it works if we apply the changes to a dataset with rows added
    split1 <- data[1:500,]
    split2 <- data[500:nrow(data),]

    #make change
    data1 <- split1
    data1$fDOM_QSU[1:4] <- NA
    dd1 <- commit_diff(split1, data1)

    #add extra data
    data <- rbind(split1, split2)

    #apply changes
    data_chg <- apply_diff(data, dd1)

    #check that changes remained
    expect_equal(data_chg$fDOM_QSU[1:500], data1$fDOM_QSU[1:500])
    expect_true(nrow(data_chg) > nrow(split1))
})
