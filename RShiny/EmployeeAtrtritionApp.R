#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(caret)
library(ggplot2)
library(tidyverse)
library(rsconnect)
rsconnect::setAccountInfo(name='derekrogers',
                          token='F14E9C1E285FAB88E1874B5A874AA023',
                          secret='c5wPZX/UiPWyZ1aTGgMCwmtmAutWeB6fD4Kb364c')

rsconnect::deployApp("C:/SMU MS Data Science/DoingDataScience6306/EmployeeAttrition/RShiny")

# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("YearsAtCompany and MonthlyIncome Analysis"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      #Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),
      
      selectInput("select", label = h3("YearsAtCompany or MonthlyIncome"), 
                  choices = list("YearsAtCompany" = "YearsAtCompany", "MonthlyIncome" = "MonthlyIncome"), 
                  selected = 1),
      
      radioButtons("radio", label = h3("Histogram or Boxplot"),
                   choices = list("Histogram" = 1, "Boxplot" = 2), 
                   selected = 1),
      
      hr(),
      fluidRow(column(3, verbatimTextOutput("value")))
      
    ),
    
    # Main panel for displaying outputs ----
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
    )
    
  )
)


# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  
  
  output$distPlot <- renderPlot({
    
    employeeData <- read.csv(input$file1$datapath, header = TRUE)
    employeeData = employeeData %>% filter(!is.na(MonthlyIncome))
    
    if(input$radio == 1){
      if(input$select == "YearsAtCompany")
      {
        x    <- employeeData$YearsAtCompany
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        
        hist(x, breaks = bins, col = "#75AADB", border = "white",
             xlab = "YearsAtCompany",
             main = "Histogram of YearsAtCompany")
      }
      if(input$select == "MonthlyIncome")
      {
        x    <- employeeData$MonthlyIncome
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        
        hist(x, breaks = bins, col = "#75AADB", border = "white",
             xlab = "MonthlyIncome",
             main = "Histogram of MonthlyIncome")
      }
    }
    if(input$radio == 2){
      if(input$select == "YearsAtCompany")
      {
        x    <- employeeData$YearsAtCompany
        
        boxplot(x, col = "#75AADB",
             xlab = "YearsAtCompany",
             main = "Histogram of YearsAtCompany")
      }
      if(input$select == "MonthlyIncome")
      {
        x    <- employeeData$MonthlyIncome
        
        boxplot(x, col = "#75AADB",
             xlab = "MonthlyIncome",
             main = "Histogram of MonthlyIncome")
      }
    }
  })
  
}

shinyApp(ui, server)
