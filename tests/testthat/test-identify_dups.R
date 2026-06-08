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

      expect_equal(flagged$changelog$n_changed[-1], 14) #only 14 since mean didn't change values
      expect_equal(flagged$changelog$note[-1], "averaged across duplicate values")
      flags_rm <- flagged$flags$flag_rm
      flags_chg <- flagged$flags$flag_chg #shouldn't be any differences here because values are the same exactly
      expect_equal(flags_chg, messy$flags$flag_chg)
      expect_true(all(sapply(flags_rm[-(1:4)], function(x){sum(x == "DUP02", na.rm=TRUE)}) == 14))

      flagged <- apply_dup_edits(messy, messy$duplicates[2,], "use_mean")

      expect_equal(flagged$changelog$n_changed[-1], 28)
      flags_rm <- flagged$flags$flag_rm
      flags_chg <- flagged$flags$flag_chg
      expect_true(all(sapply(flags_chg[-(1:4)], function(x){sum(x == "DUP01", na.rm=TRUE)}) == 14))
      expect_true(all(sapply(flags_rm[-(1:4)], function(x){sum(x == "DUP02", na.rm=TRUE)}) == 14))

    #keep a single set
      flagged <- apply_dup_edits(messy, messy$duplicates[2,], "dupfile2.csv")

      expect_equal(flagged$changelog$n_changed[-1], 14) #only 14 since we just removed 1 set
      expect_equal(flagged$changelog$note[-1], "kept duplicates from duplicate set dupfile2.csv")
      flags_rm <- flagged$flags$flag_rm
      flags_chg <- flagged$flags$flag_chg #shouldn't be any differences here because we didn't change any values
      expect_equal(flags_chg, messy$flags$flag_chg)
      expect_true(all(sapply(flags_rm[-(1:4)], function(x){sum(x == "DUP02", na.rm=TRUE)}) == 14))

      #check naming with a single file
      flagged <- apply_dup_edits(messy, messy$duplicates[1,], "1")
      expect_equal(flagged$changelog$n_changed[-1], 14) #only 14 since we just removed 1 set
      expect_equal(flagged$changelog$note[-1], "kept duplicates from duplicate set 1")

    #remove both sets
      flagged <- apply_dup_edits(messy, messy$duplicates[2,], "remove_both")

      expect_equal(flagged$changelog$n_changed[-1], 28)
      expect_equal(flagged$changelog$note[-1], "removed all duplicated values")
      flags_rm <- flagged$flags$flag_rm
      flags_chg <- flagged$flags$flag_chg
      expect_equal(flags_chg, messy$flags$flag_chg) #didn't change, just removed
      expect_true(all(sapply(flags_rm[-(1:4)], function(x){sum(x == "DUP02", na.rm=TRUE)}) == 28)) #28 now because removed both sets


})
