#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(readr)
library(tidyverse)
library(janitor)
library(readr)
first_clean <- read_rds("raw_data/first_clean")
source("premade_text.R")
source("Changingdata.R")


# Define UI for application that draws a histogram
ui <- navbarPage(theme = shinytheme("yeti"),
                 "Type of State Revenue and State Capacity",
                 
                 tabPanel("Resource Dependence",
                          
                          fluidPage(
                              fluidRow(column(7, 
                                              plotOutput("Resource_rent_vs_tax_revenue_GDP")),
                                       column(5,
                                              h3("Tax Revenue vs Resource Revenue"),
                                              p(tab1_row1_col)))
                              
                          )
                 ),
                 
                 tabPanel("Comparing Countries",
                          fluidPage(
                            fluidRow(column(7, 
                                            plotOutput("world_map")),
                                     column(5,
                                            h3("World Map")
                                     )
                                     
                            )
                          ),
                          
                          sidebarLayout(
                            sidebarPanel(
                              h4("Construct the Model:"),
                              
                              # Select X variable(s) for model.
                              
                              selectInput(
                                "varOI_x",
                                "X variable:",
                                choices = c(
                                  "Number of conflict events" = "event_count",
                                  "Total basin GDP" = "gdp_total",
                                  "Average GDP" = "gdp_avg",
                                  "Total basin population" = "pop_total",
                                  "Average population" = "pop_avg",
                                  "Average % GDP made up by trade" = "trade_percent_gdp_avg",
                                  "Average rate of water consumption" = "water_withdraw_avg",
                                  "Total basin agricultural land" = "ag_land_total",
                                  "Average agricultural land" = "ag_land_avg",
                                  "Average % of population affected by drought" = "droughts_avg",
                                  "Average democratization index score" = "eiu_avg",
                                  "Log(number of conflict events)" = "event_count_log",
                                  "Log(total basin GDP)" = "gdp_total_log",
                                  "Log(average GDP)" = "gdp_avg_log",
                                  "Log(total basin population)" = "pop_total_log",
                                  "Log(average population)" = "pop_avg_log",
                                  "Log(average % gdp made up by trade)" = "trade_percent_gdp_avg_log",
                                  "Log(average rate of water consumption)" = "water_withdraw_avg_log",
                                  "Log(total basin agricultural land)" = "ag_land_total_log",
                                  "Log(average agricultural land)" = "ag_land_avg_log",
                                  "Log(average % of population affected by drought)" = "droughts_avg_log",
                                  "Log(average democratization index score)" = "eiu_avg_log"
                                )),
                    
                              
                              # Select Y variable(s) for model.
                              
                              selectInput(
                                "varOI_y",
                                "Y variable:",
                                choices = c(
                                  "Number of conflict events" = "event_count",
                                  "Total basin GDP" = "gdp_total",
                                  "Average GDP" = "gdp_avg",
                                  "Total basin population" = "pop_total",
                                  "Average population" = "pop_avg",
                                  "Average % GDP made up by trade" = "trade_percent_gdp_avg",
                                  "Average rate of water consumption" = "water_withdraw_avg",
                                  "Total basin agricultural land" = "ag_land_total",
                                  "Average agricultural land" = "ag_land_avg",
                                  "Average % of population affected by drought" = "droughts_avg",
                                  "Average democratization index score" = "eiu_avg",
                                  "Log(number of conflict events)" = "event_count_log",
                                  "Log(total basin GDP)" = "gdp_total_log",
                                  "Log(average GDP)" = "gdp_avg_log",
                                  "Log(total basin population)" = "pop_total_log",
                                  "Log(average population)" = "pop_avg_log",
                                  "Log(average % gdp made up by trade)" = "trade_percent_gdp_avg_log",
                                  "Log(average rate of water consumption)" = "water_withdraw_avg_log",
                                  "Log(total basin agricultural land)" = "ag_land_total_log",
                                  "Log(average agricultural land)" = "ag_land_avg_log",
                                  "Log(average % of population affected by drought)" = "droughts_avg_log",
                                  "Log(average democratization index score)" = "eiu_avg_log"
                                  ))),
                            
                            mainPanel(
                            
                              plotOutput("Country_Indicator")
                            )
                            
                          )),
                 
                 
                 
                 tabPanel("About",
                          h2("Project Ideas"),
                          p(about_p1),
                          h2("Plans For Future Milestones"),
                          p(about_p2),
                          h2("About Me"),
                          p(about_p3),
                          h2("Acknowledgements"),
                          p("Thank you to the Professor and Teaching Staff at Gov50 for
               teaching me the art of data science"),
                          p("This project's GitHub repository lives",
                            a("here", href = "https://github.com/joshwillcox/Shiny_App_Milestone_4")))
                 
                 

)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$Resource_rent_vs_tax_revenue_GDP <- renderPlot({
      
        first_graph
      

    })
    
    output$world_map <- renderPlot({
      
      new
      
    })
    
    output$CountryIndicator <- renderPlot({
      
      Country_Indicator
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

