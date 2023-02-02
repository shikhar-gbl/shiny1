#Dashboard for sipmonlite quality for 2022
#Author: Shikhar Rana
#Date: 2022 November

#Input: Quality Master
#Output: Quality Dashboard

#Last Edited By: Shikhar Rana




library(ggplot2)
library(ggthemes)
library(shiny)
library(dplyr)
library(tidyverse)
library (plotly)

db <- read_csv("quality.csv")
db <- db %>%
  mutate(production_date = lubridate::dmy(production_date))

db <- db %>% 
  rename("Mean_Daily_Production" = "mean_prod_sl",
         "Missing_Obs" = "missing_obs", "Percentage_Constant_4"="percent_constant_four",
         "Percentage_Constant_8"="percent_constant_eight", "Standard_Deviation"="std",
         "Percentage_Duplicates"="dup", "Percentage_All_8_hrs"="percent_all_eight",
         "Percentage_First_4_hrs"="percent_first_four", "Percentage_Last_4_hrs"="percent_last_four")




#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Quality"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          dateRangeInput("dates", h3("Date range"), start  = "2022-11-01",
                         end    = "2022-12-17", startview = "year", 
                         format = "dd/mm/yy"),
        
          selectInput("select", h3("Select Unit"), 
                      choices = c(unique(db$unit_code)),
                      selected = "UNIT-28"),
        
          
        ),

        # Show a plot of the generated distribution
        mainPanel(
           tabsetPanel(
             type = "tabs",
             tabPanel(
               "Missing Observations",
               plotlyOutput("missing"),
               
               hr(),
               
               
               downloadButton(
                 outputId = "missing_d",
                 label = "Download Plot"
               ),
               
               hr(),
               p("Percentage of missing observations: Percentage of empty piecesproduced datapoints compared to all the piecesproduced datapoints each day."),
               p("Percentage_All_8_Hours: Percentage of empcodes with all 8 hours of production compared to all the empcodes each day.")
             ),
             
             
             tabPanel(
               "Constant Prod Data",
               plotlyOutput("constant"),
               
               hr(),
               
               downloadButton(
                 outputId = "constant_d",
                 label = "Download Plot"
               ),
               
               hr(),
               p("Percentage_Constant_4: Percentage of empcodes with any 4 consecutive hours having identical pieces_produced value compared to all the empcodes each day."),
               p("Percentage_Constant_8: Percentage of empcodes with all 8 consecutive hours having identical pieces_produced value compared to all the empcodes each day.")
               
             ),
             
             tabPanel(
               "First 4 hrs vs Last 4 hrs",
               plotlyOutput("first_vs_last"),
               
               hr(),
               
               downloadButton(
                 outputId = "first_vs_last_d",
                 label = "Download Plot"
               ),
               
               hr(),
               p("Percentage_First_4_hrs: Percentage of empcodes with only first 4 hours pieces_produced values compared to all the empcodes each day."),
               p("Percentage_Last_4_Hours: Percentage of empcodes with only last 4 hours pieces_produced values compared to all the empcodes each day.")
               
             ),
             
             tabPanel(
               "Production (Mean & SD)",
               plotlyOutput("prod"),
               
               hr(),
               
               downloadButton(
                 outputId = "prod_d",
                 label = "Download Plot"
               ),
               
               hr(),
               p("Mean_Daily_Production: Mean of total number of pieces produced against all empcodes daily."),
               p("Standard_Deviation: Standard Deviation of total number of pieces produced against all empcodes daily.")
               
               
             ),
             
             tabPanel(
               "Duplicate Employee IDs",
               plotlyOutput("duplicate"),
               
               hr(),
               
               downloadButton(
                 outputId = "duplicate_d",
                 label = "Download Plot"
               ),
               
               hr(),
               p("Percentage of Duplicate Employee IDs: Percentage of empcodes which occur more than 3 times compared to all the unique empcodes each day.")
               
               
             ),
             
             tabPanel(
               "Data",
               DT::dataTableOutput("raw_data"),
               
               hr(),
               downloadButton(
                 outputId = "download_quality_data",
                 label = "Download Data"
               )
             )
             
             
             
           )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  #output missing plot
  output$missing <- renderPlotly({
    db %>% 
      filter(production_date >= input$dates[1]) %>%
      filter(production_date <= input$dates[2]) %>%
      filter(unit_code == input$select) %>%
      gather(key = Database, value = Percent,
             Missing_Obs, Percentage_All_8_hrs) %>%
      ggplot(aes(x=production_date, y = Percent, fill = Database, colour = Database)) + 
      geom_line() + scale_fill_brewer(palette="Accent") +theme_classic() + 
      labs(x = "Date", y = "Percent") +
      ggtitle("Percentage of Missing Observations vs Empcodes with all 8 hrs Productivity Data")
  })
  
  #output constant plot
  output$constant <- renderPlotly({
    db %>% 
      filter(production_date >= input$dates[1]) %>%
      filter(production_date <= input$dates[2]) %>%
      filter(unit_code == input$select) %>%
      gather(key = Database, value = Percent,
             Percentage_Constant_4, Percentage_Constant_8) %>%
      ggplot(aes(x=production_date, y = Percent, fill = Database, colour = Database)) + 
      geom_line() + scale_fill_brewer(palette="Accent") +theme_classic() + 
      labs(x = "Date", y = "Percent") +
      ggtitle("Percentage of Empcodes with Constant 4 vs 8 hrs Productivity Data")
  })
  
  #output first4 vs last4
  output$first_vs_last <- renderPlotly({
    db %>% 
      filter(production_date >= input$dates[1]) %>%
      filter(production_date <= input$dates[2]) %>%
      filter(unit_code == input$select) %>%
      gather(key = Database, value = Percent,
             Percentage_First_4_hrs, Percentage_Last_4_hrs) %>%
      ggplot(aes(x=production_date, y = Percent, fill = Database, colour = Database)) + 
      geom_line() + scale_fill_brewer(palette="Accent") +theme_classic() + 
      labs(x = "Date", y = "Percent") +
      ggtitle("Percentage of Empcodes with Only First 4 vs Last 4 hrs Productivity Data")
  })
  
  #output prod mean & sd
  output$prod <- renderPlotly({
    db %>% 
      filter(production_date >= input$dates[1]) %>%
      filter(production_date <= input$dates[2]) %>%
      filter(unit_code == input$select) %>%
      gather(key = Database, value = Quantity,
             Mean_Daily_Production, Standard_Deviation) %>%
      ggplot(aes(x=production_date, y = Quantity, fill = Database, colour = Database)) + 
      geom_line() + scale_fill_brewer(palette="Accent") +theme_classic() + 
      labs(x = "Date", y = "Percent") +
      ggtitle("Mean Daily Production vs Standard Deviation of Productivity Data")
  })
  
  #output duplicate empcodes
  output$duplicate <- renderPlotly({
    db %>% 
      filter(production_date >= input$dates[1]) %>%
      filter(production_date <= input$dates[2]) %>%
      filter(unit_code == input$select) %>%
      ggplot(aes(x=production_date, y=Percentage_Duplicates, group = 1)) +
      geom_line() + scale_fill_brewer(palette="Accent") +
      theme_classic() + labs(x = "Date", y = "Percent") +
      ggtitle("Percentage of Duplicate Employee IDs")
  })
  
  #download missing plot
  output$missing_d <- downloadHandler(
    filename = function() {
      stringr::str_glue("Missing_{input$select}_{input$dates[1]}_{input$dates[2]}.png")
    },
    
    content = function(file) {
      ggsave(file, 
             db %>% 
               filter(production_date >= input$dates[1]) %>%
               filter(production_date <= input$dates[2]) %>%
               filter(unit_code == input$select) %>%
               gather(key = Database, value = Percent,
                      Missing_Obs, Percentage_All_8_hrs) %>%
               ggplot(aes(x=production_date, y = Percent, fill = Database, colour = Database)) + 
               geom_line() + scale_fill_brewer(palette="Accent") +theme_classic() + 
               labs(x = "Date", y = "Percent") +
               ggtitle("Percentage of Missing Observations vs Empcodes with all 8 hrs Productivity Data"),
             width = 8, height = 5, dpi = 300)
    }
    
  )
  
  #download constant plot
  output$constant_d <- downloadHandler(
    filename = function() {
      stringr::str_glue("Constant_4_vs_8_hrs_{input$select}_{input$dates[1]}_{input$dates[2]}.png")
    },
    
    content = function(file) {
      ggsave(file, 
             db %>% 
               filter(production_date >= input$dates[1]) %>%
               filter(production_date <= input$dates[2]) %>%
               filter(unit_code == input$select) %>%
               gather(key = Database, value = Percent,
                      Percentage_Constant_4, Percentage_Constant_8) %>%
               ggplot(aes(x=production_date, y = Percent, fill = Database, colour = Database)) + 
               geom_line() + scale_fill_brewer(palette="Accent") +theme_classic() + 
               labs(x = "Date", y = "Percent") +
               ggtitle("Percentage of Empcodes with Constant 4 vs 8 hrs Productivity Data"),
             width = 8, height = 5, dpi = 300)
    }
    
  )
  
  #download first4 vs last4
  output$first_vs_last_d <- downloadHandler(
    filename = function() {
      stringr::str_glue("First4_vs_Last4_{input$select}_{input$dates[1]}_{input$dates[2]}.png")
    },
    
    content = function(file) {
      ggsave(file, 
             db %>% 
               filter(production_date >= input$dates[1]) %>%
               filter(production_date <= input$dates[2]) %>%
               filter(unit_code == input$select) %>%
               gather(key = Database, value = Percent,
                      Percentage_First_4_hrs, Percentage_Last_4_hrs) %>%
               ggplot(aes(x=production_date, y = Percent, fill = Database, colour = Database)) + 
               geom_line() + scale_fill_brewer(palette="Accent") +theme_classic() + 
               labs(x = "Date", y = "Percent") +
               ggtitle("Percentage of Empcodes with Only First 4 vs Last 4 hrs Productivity Data"),
             width = 8, height = 5, dpi = 300)
    }
    
  )
  
  #download prod mean and sd
  output$prod_d <- downloadHandler(
    filename = function() {
      stringr::str_glue("Productivity_Mean_SD_{input$select}_{input$dates[1]}_{input$dates[2]}.png")
    },
    
    content = function(file) {
      ggsave(file, 
             db %>% 
               filter(production_date >= input$dates[1]) %>%
               filter(production_date <= input$dates[2]) %>%
               filter(unit_code == input$select) %>%
               gather(key = Database, value = Quantity,
                      Mean_Daily_Production, Standard_Deviation) %>%
               ggplot(aes(x=production_date, y = Quantity, fill = Database, colour = Database)) + 
               geom_line() + scale_fill_brewer(palette="Accent") +theme_classic() + 
               labs(x = "Date", y = "Percent") +
               ggtitle("Mean Daily Production vs Standard Deviation of Productivity Data"),
             width = 8, height = 5, dpi = 300)
    }
    
  )
  
  #download duplicate plot
  output$duplicate_d <- downloadHandler(
    filename = function() {
      stringr::str_glue("Duplicate_Emp_ID_{input$select}_{input$dates[1]}_{input$dates[2]}.png")
    },
    
    content = function(file) {
      ggsave(file, 
             db %>% 
               filter(production_date >= input$dates[1]) %>%
               filter(production_date <= input$dates[2]) %>%
               filter(unit_code == input$select) %>%
               ggplot(aes(x=production_date, y=Percentage_Duplicates, group = 1)) +
               geom_line() + scale_fill_brewer(palette="Accent") +
               theme_classic() + labs(x = "Date", y = "Percent") +
               ggtitle("Percentage of Duplicate Employee IDs"),
             width = 8, height = 5, dpi = 300)
    }
    
  )
  
  # render data table to ui
  output$raw_data <- DT::renderDT(
    db %>%
      filter(unit_code == input$select)%>% 
      filter(production_date >= input$dates[1]) %>%
      filter(production_date <= input$dates[2])
  )
  
  output$download_quality_data <- downloadHandler(
    filename = function() {
      stringr::str_glue("Coverage_Quality_{input$select}_{input$dates[1]}_{input$dates[2]}.csv")
    },
    
    content = function(file) {
      write.csv(db %>%
        filter(unit_code == input$select)%>% 
        filter(production_date >= input$dates[1]) %>%
        filter(production_date <= input$dates[2]), file, row.names = FALSE)
    }
    
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
