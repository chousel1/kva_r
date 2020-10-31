library(shiny)
library(tidyverse)
library(plotly)
ui <- fluidPage(   
  # Sidebar layout with a input and output definitions    
  sidebarLayout(    
    sidebarPanel(    
      helpText("This program recreates the Housel-Kanevsky Knowledge Value Added Methodology with Learning Time as the output unit"),
      br(),
      helpText("Eventually other ways of measuring output such as lines of code will be added. This app is still under construction"),
      hr(),
      tags$strong("All Arguments Must Have Matching Lengths or Output Will Error"),
      hr(),
      submitButton(),
      br(),
     textInput(inputId="no_emp", value = '1, 15, 1', label="Number of Employees by Process (separate by comma)"), 
     textInput(inputId="learn_time", value = '11, 47, 16', label="Learning Time by Process (separate by comma)"), 
     textInput(inputId="pctauto", value = '0.9, 0.14, 0.60', label="Percent Automated by Process - ENTER AS DECIMAL VALUE (separate by comma)"), 
     textInput(inputId="times_perf_year", value = '690, 6500, 230', label="Times Performed Per Year by Process (separate by comma)"), 
     textInput(inputId="avg_time_complete", value = '0.5, 0.33, 1', label="Average Time to Complete a Task by Process (separate by comma)"), 
     textInput(inputId="wage", value = '9.5, 12.00, 20.00', label="Employees' Wages by Process (separate by comma)"), 
     textInput(inputId="revenue", value = '4916.25, 579150, 6900', label="Revenues by Process (separate by comma)"),
     textInput(inputId="proc_name", value = 'Order Processing, Packaging & Shipping, Order Confirmation', label="Process Names (separate by comma)"),
     br(),
     selectInput(inputId = "logg", label = "Log ROK in Graph?:",
                 choices = c("Yes", "No"), multiple = F, selected = "No"
     ),
     ),
    mainPanel(
      plotlyOutput('rokplot'),
      br(),
      br(),
      column(12, verbatimTextOutput('rev')),
      column(12, verbatimTextOutput('rok')),
      column(12, verbatimTextOutput('totl')),
      column(12, verbatimTextOutput('tlt'))
    )
  )
)