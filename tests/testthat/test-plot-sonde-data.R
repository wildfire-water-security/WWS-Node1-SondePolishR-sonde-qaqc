test_that("plotting sonde data works", {
  #initial basic plot
    p <- plot_sonde(data=example_data, y_var="Temp_C")

    #inspect elements
      p <- plotly::plotly_build(p)
      expect_equal(length(p$x$data), 1)
      expect_equal(p$x$data[[1]]$name, "Temperature (\u00B0C)")
      expect_equal(p$x$data[[1]]$mode, "lines+markers")
      expect_equal(p$x$data[[1]]$x, example_data$DateTime_rd,ignore_attr = TRUE)
      expect_equal(p$x$data[[1]]$y, example_data$Temp_C,ignore_attr = TRUE)


#check that options work
  # plot only line
    p <- plot_sonde(data=example_data,  y_var="fDOM_QSU", opts=list(points=FALSE,line=TRUE,files=FALSE,
                                                   oow=FALSE,calcheck=FALSE,qualflag=FALSE))
    #inspect elements
    p <- plotly::plotly_build(p)
    expect_equal(length(p$x$data), 1)
    expect_equal(p$x$data[[1]]$name, "fDOM (QSU)")
    expect_equal(p$x$data[[1]]$mode, "lines")
    expect_equal(p$x$data[[1]]$x, example_data$DateTime_rd,ignore_attr = TRUE)
    expect_equal(p$x$data[[1]]$y, example_data$fDOM_QSU,ignore_attr = TRUE)

  # color by filename
    p <- plot_sonde(data=example_data,  y_var="fDOM_QSU", opts=list(points=TRUE,line=TRUE,files=TRUE,
                                                   oow=FALSE,calcheck=FALSE,qualflag=FALSE))
    #inspect elements
    p <- plotly::plotly_build(p)
    expect_equal(length(p$x$data), 3)
    expect_equal(p$x$data[[1]]$x, example_data$DateTime_rd[example_data$FileName == "example-csv-data1.csv"],ignore_attr = TRUE)
    expect_equal(p$x$data[[1]]$y, example_data$fDOM_QSU[example_data$FileName == "example-csv-data1.csv"],ignore_attr = TRUE)

  # add OOW periods
    p <- plot_sonde(data=example_data,  y_var="fDOM_QSU", proj = example_sondeproj, opts=list(points=TRUE,line=TRUE,files=FALSE,
                                                   oow=TRUE,calcheck=FALSE,qualflag=FALSE))
    p <- plotly::plotly_build(p)
    expect_equal(length(p$x$layout$shapes), 3)

  # add cal checks
    p <- plot_sonde(data=example_data,  y_var="fDOM_QSU", proj = example_sondeproj, opts=list(points=TRUE,line=TRUE,files=FALSE,
                                                   oow=FALSE,calcheck=TRUE,qualflag=FALSE))
    p <- plotly::plotly_build(p)
    expect_equal(length(p$x$data), 3)
    expect_equal(sapply(p$x$data, function(x){x$name}), c("fDOM (QSU)", "Check_Value", "Resident_Value"))
    expect_equal(length(p$x$data[[2]]$x), 2)

  # show second axis (precip)
    p <- plot_sonde(data=example_data,  y_var="fDOM_QSU", y2_var = "precip", proj = example_sondeproj, opts=list(points=TRUE,line=TRUE,files=FALSE,
                                                   oow=FALSE,calcheck=FALSE,qualflag=FALSE))
    p <- plotly::plotly_build(p)
    expect_equal(length(p$x$data), 2)
    expect_equal(sapply(p$x$data, function(x){x$name}), c("Precipitation (mm hr\U207B\U00B9)","fDOM (QSU)"))
    expect_equal(p$x$data[[1]]$y, example_precip$Precip_mm_hr,ignore_attr = TRUE)

  # show second axis (non-precip)
    p <- plot_sonde(data=example_data,  y_var="fDOM_QSU", y2_var = "Turbidity_FNU", proj = example_sondeproj,
                    opts=list(points=TRUE,line=TRUE,files=FALSE,oow=FALSE,calcheck=FALSE,qualflag=FALSE))
    p <- plotly::plotly_build(p)
    expect_equal(length(p$x$data), 2)
    expect_equal(sapply(p$x$data, function(x){x$name}), c("Turbidity (FNU)","fDOM (QSU)"))
    expect_equal(p$x$data[[1]]$y, example_data$Turbidity_FNU,ignore_attr = TRUE)

})
