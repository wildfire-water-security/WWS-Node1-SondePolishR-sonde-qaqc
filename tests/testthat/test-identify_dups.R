test_that("duplicates are identifed", {
  #test with example with no dups, should return NULL
  expect_equal(identify_dups(example_data), NULL)

  #add some dups
    messy <- readRDS(file.path(test_path(), "testdata/example-sondeproj-messy.RDS"))
    tab <- identify_dups(messy$data)

    #check vals
      expect_true(inherits(tab, "data.frame"))
      expect_equal(nrow(tab), 2)
      expect_equal(tab$length, c(14,14))
      expect_equal(tab$likely_issue, c("sonde malfunctioned duplicating data", "multiple readings during sonde switching"))

  #see if ndif works
    messy$data$fDOM_QSU[messy$data$FileName == "dupfile2.csv"] <- messy$data$fDOM_QSU[messy$data$FileName == "dupfile2.csv"] * 1.1
    tab <- identify_dups(messy$data)
    expect_true(tab$ndif[2] > 0)
    expect_true(tab$perc_dif[2] > 0)

})

test_that("duplicates are dealt with", {
  messy <- readRDS(file.path(test_path(), "testdata/example-sondeproj-messy.RDS"))
  messy$duplicates <- identify_dups(messy$data)

  #test different keep options
    #average values
      flagged <- apply_dup_edits(messy, messy$duplicates[1,], "use_mean")

      expect_equal(flagged$changelog$n_changed[-1], 14)
      expect_equal(flagged$changelog$note[-1], "averaged across duplicate values")

      #check flags are added in the right spots
      expect_equal(unlist(flagged$data$fDOM_QSU_flag[1:14]), rep("DUP01", 14))
      dup_rows <- which(flagged$data$DupNum == 2 & flagged$data$FileName == "example-csv-data1.csv")
      expect_equal(unlist(flagged$data$fDOM_QSU_flag[dup_rows]), rep("DUP02", 14))


      flagged <- apply_dup_edits(messy, messy$duplicates[2,], "use_mean")
      expect_equal(flagged$changelog$n_changed[-1], 14)
      expect_equal(unlist(flagged$data$fDOM_QSU_flag[251:264]), rep("DUP01", 14))
      dup_rows <- which(flagged$data$DupNum == 2 & flagged$data$FileName == "dupfile2.csv")
      expect_equal(unlist(flagged$data$fDOM_QSU_flag[dup_rows]), rep("DUP02", 14))

    #keep a single set
      flagged <- apply_dup_edits(messy, messy$duplicates[2,], "dupfile2.csv")

      expect_equal(flagged$changelog$n_changed[-1], 14) #only 14 since we just removed 1 set
      expect_equal(flagged$changelog$note[-1], "kept duplicates from duplicate set dupfile2.csv")
      expect_equal(flagged$changelog$n_changed[-1], 14)
      expect_equal(unlist(flagged$data$fDOM_QSU_flag[251:264]), rep("DUP02", 14))
      dup_rows <- which(flagged$data$DupNum == 2 & flagged$data$FileName == "dupfile2.csv")
      expect_equal(unlist(flagged$data$fDOM_QSU_flag[dup_rows]), rep(NA, 14)) #shouldn't be flagged since not changed

      #check naming with a single file
      flagged <- apply_dup_edits(messy, messy$duplicates[1,], "1")
      expect_equal(flagged$changelog$n_changed[-1], 14) #only 14 since we just removed 1 set
      expect_equal(flagged$changelog$note[-1], "kept duplicates from duplicate set 1")

    #remove both sets
      flagged <- apply_dup_edits(messy, messy$duplicates[2,], "remove_both")

      expect_equal(flagged$changelog$n_changed[-1], 28)
      expect_equal(flagged$changelog$note[-1], "removed all duplicated values")
      dup_rows <- which(flagged$data$DupNum == 2 & flagged$data$FileName == "dupfile2.csv")
      expect_equal(unlist(flagged$data$fDOM_QSU_flag[c(251:264, dup_rows)]), rep("DUP02", 28))


})
