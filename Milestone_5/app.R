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
}

# Run the application 
shinyApp(ui = ui, server = server)

