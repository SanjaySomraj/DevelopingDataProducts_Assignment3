#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(plyr)
library(ggplot2)
library(tidyr)
# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

     touristData <- read.csv("./data/tours.csv", stringsAsFactors = FALSE)
     statsData <- read.csv("./data/tourist.csv", stringsAsFactors = FALSE)
     helpData <- read.csv("./data/help.csv", stringsAsFactors = FALSE)
     aboutData <- read.csv("./data/about.csv", stringsAsFactors = FALSE)
     
     plotLable <- reactive({
          pLable <- paste("Tourists visiting India from year:",input$sliderYear[1]," to ",input$sliderYear[2],", Top:",input$sliderRank)
          pLable
     })
     
     tableData <- reactive({
          startYear <- input$sliderYear[1]
          endYear <- input$sliderYear[2]
          rank <- input$sliderRank

          africa<- input$checkbox1
          america <- input$checkbox2
          asia <- input$checkbox3
          australia <- input$checkbox4
          europe <- input$checkbox5
          all <- input$checkbox6
          
          zoneFilter <- c()
          
          if(all) {
               zoneFilter <- c(unique(touristData$Zone))
          } else {
               if (africa) {
                    zoneFilter <- c(zoneFilter, "Africa")
               }
               if (america) {
                    zoneFilter <- c(zoneFilter, "North America", "Central & South America")
               }
               if (asia) {
                    zoneFilter <- c(zoneFilter, "West Asia", "South Asia","East Asia","South East Asia")
               }
               if (australia) {
                    zoneFilter <- c(zoneFilter, "Australasia")
               }
               if (europe) {
                    zoneFilter <- c(zoneFilter, "Western Europe", "Eastern Europe")
               }
          } 
          
          mydata1 <- touristData[which (touristData$Year >= input$sliderYear[1] & touristData$Year <= input$sliderYear[2]), ]
          mydata1 <- na.omit(subset(mydata1, Zone %in% zoneFilter))

          groupColumns = c("Country")
          dataColumns = c("Tourists")
          mydata2 <- ddply(mydata1, groupColumns, function(x) colSums(x[dataColumns]))
          mydata3 <- mydata2[order(mydata2$Tourists, decreasing = TRUE),]
          finalData <- mydata3[1:rank,]
          finalData
     })
     

     output$displayData <- renderTable({
        head(tableData(), n = 10)},  
          striped = TRUE, bordered = TRUE, digits = 0, na = '')
     
     #* This observer will update checkboxes 1 - 5 to TRUE whenever checkbox 6 is TRUE
     observeEvent(
          eventExpr = input$checkbox6,
          handlerExpr = 
          {
               if (input$checkbox6)
                    lapply(paste0("checkbox", 1:5),
                           function(x)
                           {
                                updateCheckboxInput(session, x, value = input$checkbox6)
                           }
                    )
          }
     )
     
     #* This observer will set checkbox 6 to FALSE whenever any of checkbox 1-5 is FALSE
     lapply(paste0("checkbox", 1:5),
            function(x) 
            {
                 observeEvent(
                      eventExpr = input[[x]], 
                      handlerExpr = 
                      {
                           if (!input[[x]]) updateCheckboxInput(session, "checkbox6", value = FALSE)
                      }
                 )
            }
     )
     
     output$myGraph <- renderPlot({
          tData <- tableData()
          ggplot(data = tData, aes(x=tData$Country,y=tData$Tourists/1000))+
               geom_bar(aes(fill=tData$Country), stat = "identity")+
               ggtitle(plotLable()) +
               ylab("Number of Tourists (1000s)") + xlab('Country') +
               scale_fill_manual(values = as.factor(tData$Country)) +
               labs(fill = "Country")+
               geom_text(aes(label = tData$Tourists), hjust = 0.5, vjust = -0.5, size = 4)
     })
     
     output$help <- renderTable({
          head(helpData, n = 7)},  
          striped = TRUE, bordered = TRUE, digits = 0, na = ''
     )

     summaryData <- reactive({
          startYear <- input$sliderYear[1]
          endYear <- input$sliderYear[2]
          africa<- input$checkbox1
          america <- input$checkbox2
          asia <- input$checkbox3
          australia <- input$checkbox4
          europe <- input$checkbox5
          all <- input$checkbox6
          
          zoneFilter <- c()

          if(all) {
               zoneFilter <- c(unique(touristData$Zone))
          } else {
               if (africa) {
                    zoneFilter <- c(zoneFilter, "Africa")
               }
               if (america) {
                    zoneFilter <- c(zoneFilter, "North America", "Central & South America")
               }
               if (asia) {
                    zoneFilter <- c(zoneFilter, "West Asia", "South Asia","East Asia","South East Asia")
               }
               if (australia) {
                    zoneFilter <- c(zoneFilter, "Australasia")
               }
               if (europe) {
                    zoneFilter <- c(zoneFilter, "Western Europe", "Eastern Europe")
               }
          } 
          
          i<-startYear
          keepCols <- c()
          newCols <- c()
          for (i in startYear:endYear){
               col <- paste("X",i, sep = '')
               rcol <- paste("Yr",i)
               keepCols <- c(keepCols,col)
               newCols <- c(newCols,rcol)
          }
          newStats <- na.omit(subset(statsData, Zone %in% zoneFilter))     
          newStats <- newStats[,(names(newStats) %in% keepCols)]
          colnames(newStats) <- newCols
          print(paste("Data Summary for Period -",startYear,"to",endYear))
          newStats
     })
     
     output$summary <- renderPrint({
          summary(summaryData())
     })

     output$about <- renderTable({
          head(aboutData, n = 2)},  
          striped = TRUE, bordered = TRUE, digits = 0, na = ''
     )
})