test_that("plotting sonde data works", {
  #initial basic plot
     p <- plot_sonde(example_data, "Temp_C")
     vdiffr::expect_doppelganger("basic plotting", p)

#check that options work
  # plot only line
    p <- plot_sonde(example_data, "fDOM_QSU", list(points=FALSE,line=TRUE,files=FALSE,
                                                   oow=FALSE,calcheck=FALSE,precip=FALSE))
    vdiffr::expect_doppelganger("only lines", p)

  # color by filename
    p <- plot_sonde(example_data, "fDOM_QSU", list(points=TRUE,line=TRUE,files=TRUE,
                                                   oow=FALSE,calcheck=FALSE,precip=FALSE))
    vdiffr::expect_doppelganger("color by filename", p)

  # add OOW periods
    p <- plot_sonde(example_data, "fDOM_QSU", list(points=TRUE,line=TRUE,files=FALSE,
                                                   oow=TRUE,calcheck=FALSE,precip=FALSE),
                    fieldform = example_fieldform, calcheck = example_calcheck,
                    precip = example_precip)
    vdiffr::expect_doppelganger("oow periods", p)

  # add cal checks
    p <- plot_sonde(example_data, "fDOM_QSU", list(points=TRUE,line=TRUE,files=FALSE,
                                                   oow=FALSE,calcheck=TRUE,precip=FALSE),
                    fieldform = example_fieldform, calcheck = example_calcheck,
                    precip = example_precip)
    vdiffr::expect_doppelganger("cal check", p)

  # show precipitation
    p <- plot_sonde(example_data, "fDOM_QSU", list(points=TRUE,line=TRUE,files=FALSE,
                                                   oow=FALSE,calcheck=FALSE,precip=TRUE),
                    fieldform = example_fieldform, calcheck = example_calcheck,
                    precip = example_precip)
    vdiffr::expect_doppelganger("adding precip", p)

})
