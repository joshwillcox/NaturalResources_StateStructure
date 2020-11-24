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
                          
                          h3("How Are Natural Resource Dependence and State Structure Related?"),
                          p(intro),
                          h3("Things to Keep in Mind"),
                          p(to_think_about),
                          plotOutput("Resource_Dependence")
                          
                          
                 ),
                 
                 tabPanel("Comparing Countries",
                          h3("Looking at State Efficiency"),
                          
                          sidebarLayout(
                            sidebarPanel(
                              h4("Measurements of the State:"),
                              
                              # Select X variable(s) for model.
                              
                              selectInput(
                                "varOI_x",
                                "Country Measurements",
                                choices = c(
                                  "Military expenditure as % of general government expenditure" = "Military expenditure (% of general government expenditure)",
                                  "Mineral rents as % of GDP" = "Mineral rents (% of GDP)",
                                  "Oil rents as % of GDP" = "Oil rents (% of GDP)",
                                  "Tax revenue as % of GDP" = "Tax revenue (% of GDP)",
                                  "Total natural resources rents as % of GDP" = "Total natural resources rents (% of GDP)",
                                  "Armed forces personnel as % of total labor force" = "Armed forces personnel (% of total labor force)",
                                  "Bribery incidence" = "Bribery incidence (% of firms experiencing at least one bribe payment request)",
                                  "Central government debt, total (% of GDP)" = "Central government debt, total (% of GDP)",
                                  "Ease of doing business index" = "Ease of doing business index (1=most business-friendly regulations)",
                                  "GINI index" = "GINI index (World Bank estimate)",
                                  "Government expenditure on education, total as % of GDP" = "Government expenditure on education, total (% of GDP)",
                                  "Health expenditure, public as % of GDP" = "Health expenditure, public (% of GDP)",
                                  "Internet users (per 100 people)" = "Internet users (per 100 people)",
                                  "Strength of legal rights index (0=weak to 12=strong)",
                                  "Time required to get electricity (days)"
                                  
                                  
                                  
                                  
                                ))),
                            
                            mainPanel(
                            
                              plotOutput("Country_Indicator")
                            )
                            
                          ),
                          h3("Different Types of Regimes Worldwide"),
                          fluidPage(fluidRow(column(6,
                                                    gt_output("regime_count")),
                                             column(6,
                                                    plotOutput("regime_map"))
                                             ))),
                 tabPanel("Model",
                          
                          
                          fluidPage(
                            fluidRow(column(7, 
                                            plotOutput("Resource_rent_vs_tax_revenue_GDP")),
                                     column(5,
                                            h3("Tax Revenue vs Resource Revenue"),
                                            p(tab1_row1_col))),
                            fluidRow(column(6,
                                            gt_output("model1_table")),
                                     column(6,
                                            h3("Effects on Natural Resource Dependence"))),
                            fluidRow(column(6,
                                            h3("Effects on Tax Revenue")),
                                     column(6,
                                            gt_output("model2_table")))
                             
                            
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
  
    output$Resource_Dependence <- renderPlot({
      
      
      x <- fixed_names %>%
        filter(indicator_name == "Total natural resources rents (% of GDP)" ) %>%
        select(country_name, indicator_name, x2010:x2016) %>%
        pivot_longer(cols = x2010:x2016, names_to = "year", values_to = "rating") %>%
        group_by(country_name) %>%
        summarise(average = mean(rating, na.rm = TRUE))


      countrynames_worldmap %>%
        left_join(x, by = "country_name") %>%
        ggplot(aes(fill = average)) +
        geom_sf() +
        labs(title = "World Map Showing Natural Resource Rents as a % of GDP") +
        scale_fill_viridis_c(option = "plasma",
                             direction = -1)

      
    })
    
    output$Resource_rent_vs_tax_revenue_GDP <- renderPlot({
      
        first_graph
      

    })
    
    
    output$Country_Indicator <- renderPlot({
      
      x <- fixed_names %>%
        filter(indicator_name == input$varOI_x) %>%
        select(country_name, indicator_name, x2010:x2016) %>%
        pivot_longer(cols = x2010:x2016, names_to = "year", values_to = "rating") %>%
        group_by(country_name) %>%
        summarise(average = mean(rating, na.rm = TRUE))
      
      
      countrynames_worldmap %>%
        left_join(x, by = "country_name") %>%
        ggplot(aes(fill = average)) +
        geom_sf()
      
    
    })
    
    output$regime_map <- renderPlot({
      
      regime_map
    })
    
    output$regime_count <- render_gt({
      
      regime_count
    })
    
    output$model1_table <- render_gt({
      
      model1_table
    })
    
    output$model2_table <- render_gt({
      
    model2_table
      
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)

