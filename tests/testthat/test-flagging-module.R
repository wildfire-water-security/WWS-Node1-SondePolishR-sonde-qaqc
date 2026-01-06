#define args above so they're cleaning testing function

test_dir <- file.path(tempdir(), "shiny-tests")
testServer(confirm_changes_server,
           args(df = raw_sonde,
                index = 1:3,
                par = test_dir,
                flag_name = "fDOM_QSU",
                prj_path = "test"), {

          session$setInputs(rm_points = 1)
          expect_true(length(list.files(test_dir)) == 1)


})
