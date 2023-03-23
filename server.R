shinyServer(function(input, output, session) {
  
  annual_percent_data <- reactive({
    year_percent %>% 
      dplyr::filter(period == input$year,
                    value_type == input$value,
                    parameter == input$parameter)
  })
  
  output$annual_percent_map <- renderLeaflet({
    
    pal <- colorNumeric(palette = "viridis",
                        domain = annual_percent_data()$value)
    
    leaflet(annual_percent_data()) %>%
      addTiles() %>%
      addPolygons(color = ~pal(value),
                  stroke = FALSE,
                  highlight = highlightOptions(fillOpacity = 0.4),
                  label = paste0(annual_percent_data()$COUNTRY, " | ", annual_percent_data()$parameter, ": ", annual_percent_data()$value)) %>% 
      addLegend(pal = pal,
                position = "bottomleft",
                values = annual_percent_data()$value)
    
  })
})