#interactive plotting options
  #highcharter, seems like it would work (maybe)
  #plotly, works
  #dygraphs
  #anycharts
  #ggiraph  #seems like the best alternative

library(plotly)
library(ggiraph)

#testing to see if we like ggiraph or plotly better (plotly appears to be quite a bit faster and I'm already mostly familiar with...)
  #load big data to plot to test speed
  large_data <- readRDS("../WWS-Node1-SONDE-postfire-sonde-network/data/03_merged-data/prelim-cleaned-sonde-data.RDS")
  data <- large_data %>% filter(site_code == "JOS")

  #ggiraph
  p <- ggplot(large_data, aes(x=DateTime_15, y=fDOM_QSU, color=site_code)) + geom_point_interactive(aes(tooltip = fDOM_QSU, data_id = fDOM_QSU),
                                                                                                    size = 3, hover_nearest = TRUE)()
  start <- Sys.time()
  girafe(ggobj = p)
  print(paste("time to create:", Sys.time() - start))

  #plotly
  p <- plot_ly(data = large_data, x = ~DateTime_15,y = ~fDOM_QSU,color = ~site_code,type = "scatter", mode="markers")  %>%
    layout(xaxis = list(title = "Date"), yaxis = list(title = "fDOM (QSU)",zeroline = FALSE)) %>% toWebGL()
  start <- Sys.time()
  p
  print(paste("time to create:", Sys.time() - start))

  #with ggplotly
  p <- ggplot(large_data, aes(x=DateTime_15, y=fDOM_QSU, color=site_code)) + geom_point()
  start <- Sys.time()
  ggplotly(p)%>% toWebGL()
  print(paste("time to create:", Sys.time() - start))

## use to better learn how to do trace and tooltips in plotly and hopefully prevent the warnings

y_var <- "fDOM_QSU"
data <- example_data

p <- plot_sonde(example_data, "fDOM_QSU")

pp <- ggplotly(p)

  #look at traces
  plotly_json(pp)


#try to make with straight plotly
  p <- plot_ly(example_data, x = ~DateTime_rd, y=~fDOM_QSU) %>%
        add_lines(name="line", showlegend=FALSE) %>% add_markers(name="points", showlegend=FALSE)

  p <- ggplot(data, aes(x = .data$DateTime_rd,y = .data[[y_var]], color=.data$FileName)) +
                geom_line() + geom_point()

  pp <- ggplotly(p)

  p <- plot_ly(data = data, x = ~DateTime_rd,y = as.formula(paste0("~`", y_var, "`")),color = ~FileName,type = "scatter",
               mode = "lines+markers")  %>%
      layout(xaxis = list(title = "Date"), yaxis = list(title = "fDOM (QSU)",zeroline = FALSE))


#we have to redo tests....
