

function(input, output, session) {
  
  # Structure (excluding 'state' column)
  output$structure <- renderPrint({
    # Structure of the data
    my_data %>%
      select(-State) %>% 
      str()
  })
  
  # Summary (excluding 'State' column)
  output$summary <- renderPrint({
    my_data %>%
      select(-State) %>%  # Remove 'State' column before summarizing
      summary()
  })
  
  # DataTable (excluding 'State' column)
  output$dataT <- renderDataTable({
    my_data %>%
      select(-State)  # Remove 'State' column from the DataTable
  })
  
  # Stacked Histogram and Box Plot
  output$histplot <- renderPlotly({
    p1 = my_data %>%
      plot_ly() %>% 
      add_histogram(~get(input$var1)) %>% 
      layout(xaxis = list(title = input$var1))
    
    # Box Plot
    p2 = my_data %>% 
      plot_ly() %>% 
      add_boxplot(~get(input$var1)) %>% 
      layout(yaxis = list(showticklabels = F))
    
    # Stacking plots
    subplot(p2, p1, nrows = 2) %>% 
      hide_legend() %>% 
      layout(
        title = 'Distribution chart - Histogram and Box plot',
        yaxis = list(title = "Frequency")
      )
    
  })
  
  # scatter plot
  output$scatter <- renderPlotly({
    
    # creating scatter plot for relationships
    p = my_data %>%  
      ggplot(aes(x=get(input$var3), y= get(input$var4))) +
      geom_point() +
      geom_smooth(method=get(input$fit)) +
      labs(title = paste("relation b/w", input$var3, 'and', input$var4),
           x = input$var3,
           y = input$var4) +
      theme(plot.title = element_textbox_simple(size =10, halign=0.5))
    
    ggplotly(p)
  })
  
  
  
  ## Correlation plot
  output$cor <- renderPlotly({
    my_df <- my_data %>% 
      select(-State)
    
    # Compute a correlation matrix
    corr <- round(cor(my_df), 1)
    
    # Compute a matrix of correlation p-values
    p.mat <- cor_pmat(my_df)
    
    corr.plot <- ggcorrplot(
      corr, 
      hc.order = TRUE, 
      lab= TRUE,
      outline.col = "white",
      p.mat = p.mat
    )
    
    ggplotly(corr.plot)
    
  })
  
  # Bar charts - state wide trends
  output$bar <- renderPlotly({
    my_data %>%
      plot_ly() %>%
      add_bars(x = ~State, y = ~get(input$var2)) %>%
      layout(
        title = paste('Statewide Arrests for', input$var2),
        xaxis = list(title = 'State'),
        yaxis = list(title = paste(input$var2, 'Arrests per 100,000 residents'))
      )
  })
  
  
  # renedring the box header
  
  output$head1 <- renderText(
    paste('5 states with high rate of', input$var2, 'Arrests')
  )
  
  # renedring the box header
  
  output$head2 <- renderText(
    paste('5 states with low rate of', input$var2, 'Arrests')
    
  )
  
  
  # rendering table with 5 states for highest arrests for specific crimes
  output$top5 <- renderTable({
    # top 5 states with high crime rates
    my_data %>% 
      select(State, input$var2) %>% 
      arrange(desc(get(input$var2))) %>% 
      head(5)
    
  })
  
  # rendering table with 5 states for lowest arrests for specific crimes
  output$low5 <- renderTable({
    #top 5 states with low crime rates
    my_data %>% 
      select(State, input$var2) %>% 
      arrange(get(input$var2)) %>% 
      head(5)
  })
  
  
  
  
  # Add a reactive expression for the pie chart based on the selected state
  pieData <- reactive({
    # Filter the data for the selected state
    state_data <- my_data %>% filter(State == input$stateSelect)
    
    # Create a data frame for the pie chart with predefined crime types
    pie_df <- data.frame(
      Category = c("Murder", "Assault", "Rape"),
      Value = c(
        state_data$Murder[1],  # Assuming only one row per state
        state_data$Assault[1], 
        state_data$Rape[1]
      )
    )
    
    # Return the data frame
    pie_df
  })
  
  # Render the pie chart
  output$pieChart <- renderPlotly({
    pie_df <- pieData()
    
    # Generate the pie chart
    pie_plot <- pie_df %>%
      plot_ly(labels = ~Category, values = ~Value, type = 'pie', 
              textinfo = 'percent', 
              insidetextorientation = 'radial') %>%
      layout(
        title = paste('Proportions of Crimes in', input$stateSelect),
        showlegend = TRUE,
        # Improve layout and design
        margin = list(l = 0, r = 0, t = 50, b = 0),
        paper_bgcolor = 'white',
        plot_bgcolor = 'white'
      )
    
    pie_plot
  })
  
  
  
  
  
  
  
  
  # choropleth map
  output$map_plot <- renderPlot({
    new_join %>% 
      ggplot(aes(x = long, y = lat, fill = get(input$crimetype), group = group)) +
      geom_polygon(color = 'black', size = 0.4) +
      scale_fill_gradient(low = '#73A5c6', high = '#001B3A', name = paste(input$crimetype, 'Arrest Rate')) +
      theme_void() +
      labs(title = paste('Choropleth map of', input$crimetype, 'Arrests per 100,000 residents by state in 1973')) +
      theme(
        plot.title = element_textbox_simple(face = 'bold', size = 18, halign = 0.5),
        legend.position = c(0.2, 0.1),
        legend.direction = 'horizontal'
      ) +
      geom_text(aes(x = x, y = y, label = abb), size = 4, color = 'white')
  })
  
  
  
  
  
  
  
  
}
