library(shiny)
library(ggplot2)
library(rWordCloud)
library(DT)

# dataset <- diamonds
# all_data <- read.csv("all_data.csv")

shinyUI(fluidPage(
  
  headerPanel(""),
  
  sidebarPanel(
    conditionalPanel(
      condition = "input.tabs1=='Full Table'",
      uiOutput("choose_gmver"),
      br()
    ),
   
    uiOutput("choose_month"),
    
    br(),
    
    uiOutput("choose_day"),
    
    uiOutput("choose_year"),
    
    uiOutput("choose_period")
    
  ),
  
#     
#     conditionalPanel(
#       condition = "input.tabs1=='Comparison Table'",
#       uiOutput("choose_strat_comp"),
#       br()
#     ),
    
    

    
  mainPanel(
    tabsetPanel(
      id = "tabs1",
      tabPanel("Summary Table", dataTableOutput("basictable")),
      tabPanel("Word Cloud", d3CloudOutput("plot", width = "100%", height = 500))

      )
  )
)
)