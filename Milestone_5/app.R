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
                                "Country Measurements",
                                choices = c(
                                  "Business Regulatory Environment" = "CPIA business regulatory environment rating (1=low to 6=high)",
                                  "Efficency of Revenue Mobilisation" = "CPIA efficiency of revenue mobilization rating (1=low to 6=high)",
                                  "Property Rights and Rule Based Governance" = "CPIA property rights and rule-based governance rating (1=low to 6=high)",
                                  "Quality of Public Administration" = "CPIA quality of public administration rating (1=low to 6=high)",
                                  "Transparency and Accountability in the Public Sector" = "CPIA transparency, accountability, and corruption in the public sector rating (1=low to 6=high)",
                                  "Military expenditure as % of general government expenditure" = "Military expenditure (% of general government expenditure)",
                                  "Mineral rents as % of GDP" = "Mineral rents (% of GDP)",
                                  "Oil rents as % of GDP" = "Oil rents (% of GDP)",
                                  "Ores and metals exports (% of merchandise exports)" = "Ores and metals exports (% of merchandise exports)",
                                  "Tax revenue as % of GDP" = "Tax revenue (% of GDP)",
                                  "Total natural resources rents as % of GDP" = "Total natural resources rents (% of GDP)"
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
    
    output$Country_Indicator <- renderPlot({
      
      x <- fixed_names %>%
        filter(indicator_name == input$varOI_x) %>%
        select(country_name, indicator_name, x2010:x2018) %>%
        pivot_longer(cols = x2010:x2018, names_to = "year", values_to = "rating") %>%
        group_by(country_name) %>%
        summarise(average = mean(rating, na.rm = TRUE))
      
      
      countrynames_worldmap %>%
        left_join(x, by = "country_name") %>%
        ggplot(aes(fill = average)) +
        geom_sf()
      
    
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

