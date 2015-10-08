library(shiny)
library(ggplot2)
library(xtable)
library(DT)
library(googleVis)
library(rWordCloud)
library(shinyapps)
# library(dplyr)

messages <- read.csv("messages.csv")

shinyServer(function(input, output) {
  
  output$choose_month <- renderUI({
    
   month_list<-unique(as.character(messages$month))   
    
   checkboxGroupInput("month", "Choose month",
                       choices = month_list,
                       selected = month_list)      
  })
  
  output$choose_day <- renderUI({
    
    day_list<-unique(as.character(messages$day))   
    
    checkboxGroupInput("day", "Choose day",
                       choices = day_list,
                       selected = day_list)      
  })
  
  output$choose_year <- renderUI({
    
    year_list<-unique(as.character(messages$year))   
    
    checkboxGroupInput("year", "Choose year",
                       choices = year_list,
                       selected = year_list)      
  })
  
  output$choose_period <- renderUI({
    
    tod_list<-unique(as.character(messages$time_of_day))   
    
    checkboxGroupInput("tod", "Choose time of day",
                       choices = tod_list,
                       selected = tod_list)      
  })
  
  output$basictable<-renderDataTable({
    
    datatable(
      filter(messages,
             messages$month %in% input$month,
             messages$day %in% input$day,
             messages$year %in% input$year,
             messages$time_of_day %in% input$tod
             ) %>%
      group_by(sender) %>%
        summarise(count=n()),
      options=list(pagelength=5))
  })
  

  })
  

  
#   
#     # drop-down box for selecting query category
#     output$choose_cat <- renderUI({
#       
#       cat_list<-unique(as.character((dataset$category)))
#       
#       checkboxGroupInput("category", "Select query categories",
#                          choices = cat_list,
#                          selected = cat_list)
#       
#       # create list of categories + "All"
#     #  cat_opts<-c("All",unique(as.character((dataset$category))))
#       
#      # selectInput('category', 'Query Category', cat_opts)
#     })
#   
#     output$choose_gmver <- renderUI({
#       # If prior input is missing, return to avoid errors
#       if(is.null(input$category))
#         return()
#       
#       # Filter down dataset to display correct GM version choices (note that this is currently
#       # superfluous, but could avoid future errors)
#       # dat <- dataset[dataset$category == input$category,]
#       
#       gmver <- unique(as.character(dataset$gmver))
#       
#       checkboxGroupInput("gmver", "Choose GeoMesa version",
#                          choices = gmver,
#                          selected = gmver)
#       
#     })
#     

#     
#     output$choose_sample <- renderUI({
#       if(is.null(input$index))
#         return()
#       
# 
#       checkboxGroupInput("sample", "Choose measurement",
#                          choices = c("all_mean", "nomax_mean", "max_only"),
#                          selected = "nomax_mean")
#     })
#     
#     # Allow selection of strategy for main table
#     output$choose_strat <- renderUI({
#       if(is.null(input$index))
#         return()
# 
#       dat <- filter(dataset, dataset$index %in% input$index)
#       strat_opts<-unique(as.character((dat$strategy)))
#       
#       checkboxGroupInput("strategy", "Select strategy",
#                          choices = strat_opts,
#                          selected = strat_opts)
#     })
#     
#     # Allow selection of strategy for comparison
#     output$choose_strat_comp <- renderUI({
#       if(is.null(input$index))
#         return()
#       
#       dat <- filter(compare_df, compare_df$index %in% input$index)
#       strat_opts<-unique(as.character((dat$gm_rc4_strat)))
#       
#       checkboxGroupInput("strategy_comp", "Select RC4 strategy",
#                          choices = strat_opts,
#                          selected = strat_opts)
#     })
#     
#     # Choose columns for main table
#     output$choose_col_main <- renderUI ({
#       
#       columns<-colnames(dataset)
#       
#       checkboxGroupInput("col_main", "Choose columns to view",
#                          choices = columns,
#                          selected = columns)
#     })
#     
#     
#     # Choose columns for comparison table
#     output$choose_col_comp <- renderUI ({
#       
#       columns<-colnames(compare_df)
#       
#       checkboxGroupInput("col_comp", "Choose columns to view",
#                          choices = columns,
#                          selected = columns)
#     })
#     
#     
# #     # Choose to view barchart by strategy or query category
# #     output$choose_bar <- renderUI ({
# #       selectInput("choose_bar", "View by:", c("rc4 Strategy", "query category"))
# #     })
#     
#     
#     
#     # Creating data table to display full results
#     output$data_table<-renderDataTable({
#       
#       colNums <- match(input$col_main, names(dataset))
#       
#       datatable(
#         filter(dataset, dataset$sample %in% input$sample &
#                          dataset$index %in% input$index &
#                          dataset$gmver %in% input$gmver &
#                          dataset$category %in% input$category &
#                  dataset$strategy %in% input$strategy) %>%
#                select(colNums))
#                                                            })
#     
#     # Creating data table to display ratio of RC4 to RC5 query times
#     output$compare_table<-renderDataTable({
#       
#       colNums <- match(input$col_comp, names(compare_df))
#       
#         datatable(
#           filter(compare_df, compare_df$sample %in% input$sample &
#                    compare_df$index %in% input$index &
#                    compare_df$category %in% input$category &
#                    compare_df$gm_rc4_strat %in% input$strategy_comp) %>%
#             select(colNums))
#       })
#     
#     output$barchart<- renderGvis({
#       
#       dat <- filter(compare_df, index %in% input$index, sample %in% input$sample) %>%
#         group_by(gm_rc4_strat) %>%
#         summarise(mean_ratio=round(mean(rc4_rc5),3), count= n())
#       
#       BC<-gvisColumnChart(dat, xvar="gm_rc4_strat", yvar="mean_ratio",
#                      options = list(width = "automatic", height = "automatic", legend="top"))
#                       
#       TB <-gvisTable(dat, options = list(width = "automatic"))
#       gvisMerge(TB,BC, horizontal=FALSE)
#       })
    




