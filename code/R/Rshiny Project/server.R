library("shiny")
library("dplyr")
library("tidyr")
library("plyr")
library("tidyverse")
library("ggplot2")
library("plotly")
library("skimr")
library("psych")







data = read.csv("2015_Street_Tree_Census_-_Tree_Data.csv")

function(input, output) {



output$summary <- renderPrint({
  summary(data1)
})



output$Plot <- renderPlot({
  
  # Render a barplot
  barplot(height = r[,input$region],names = row.names(r), 
          main=input$region,
          ylab="Number of Tree",
          xlab="condition",col = "#75AADB", border = "white")
})


output$plot2 <- renderPlot({
  
  # Render a barplot
  barplot(r2[,input$region],names = row.names(r), 
          main=input$region,
          ylab="Number of Tree",
          xlab="status",col = "#75AADB", border = "white")
})



output$plot3 <- renderPlot({
                             
  barplot(height = d1$`tree diameter mean`[seq_len(input$n)],names = d1$`tree species`[seq_len(input$n)],breaks = 40,las=2,col ="#ffa600",cex.names=0.8)
  
})

output$plot4 <- renderPlot({
  
  barplot(height = d2$`tree diameter mean`[seq_len(input$n)],names = d2$`tree species`[seq_len(input$n)],breaks = 40,las=2,col ="#ffa600",cex.names=0.8)
  
})




output$outTable <- renderTable({ 
  
  head(d, n = input$obs)
  
})






}

  

