#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(plyr)
# Define UI for application that draws a histogram
shinyUI(fluidPage(
     # Application title
     titlePanel("Tourist Data in INDIA - 2004 : 2015"),

     # Sidebar with a slider input for number of bins 
     sidebarLayout(
          sidebarPanel(
               sliderInput("sliderRank", "Top rankings",min=2, max=10, value=2),
               sliderInput("sliderYear", "Start and Ending year", min=2004, max=2015, value = c(2010,2015), sep = ""),
               br(),
               h4("Tourist origination zone"),
               checkboxInput("checkbox6", label = "Select All", value = TRUE),
               checkboxInput("checkbox1", label = "Africa", value = TRUE),
               checkboxInput("checkbox2", label = "North & South America", value= TRUE),
               checkboxInput("checkbox3", label = "Asia", value = TRUE),
               checkboxInput("checkbox4", label = "Australia", value = TRUE),
               checkboxInput("checkbox5", label = "Europe", value = TRUE)
          ),

          # Show tabs defined
          mainPanel(
               tags$style(HTML(".tabbable > .nav > li[class=active] > a {background-color: #012154;color: #FFF;}")),
               
               tabsetPanel(type="tabs",
                           tabPanel("Tourist Data", br(), tableOutput("displayData")),
                           tabPanel("Data Visualization", br(), plotOutput("myGraph")),
                           tabPanel("Data Summary", br(), verbatimTextOutput("summary")),
                           tabPanel("Help", br(), tableOutput("help")),
                           tabPanel("About", br(), tableOutput("about"))
               )
          )
     )
))
