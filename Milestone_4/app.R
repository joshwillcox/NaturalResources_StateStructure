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
read_rds("first_clean")

# Define UI for application that draws a histogram
ui <- navbarPage(theme = shinytheme("yeti"),
                 "Type of State Revenue and State Capacity",
                 
                 tabPanel("Resource Dependence",
                          
                          fluidPage(
                              fluidRow(column(7, 
                                              plotOutput("Resource_rent_vs_tax_revenue_GDP")),
                                       column(5,
                                              h3("Tax Revenue vs Resource Revenue"),
                                              p("This graph shows us that there is a negative 
                          correlation between the percentage of GDP that is 
                          from Tax Revenue and that which is from Natural 
                          Resources. While we cannot draw causal conclusions from
                          the data, it does show that countries which rely on
                          natural resources, are less likely to rely on tax
                          revenue")))
                          )
                 ),
                 
                 tabPanel("About",
                          h2("Project Ideas"),
                          p("For my project, I want to look at the effects that natural 
              resources/foreign aid can have on the strucutre of the state and
              its capacity. In one of my classes recently, we talked about how
              access to resources or foreign aid incentivises countries not to 
              develop a strong state. They prefer to not tax citizens and depend
              on other sources because then they do not have to provide benefits
              to the population. In the small graph I have made for this
              milestone, I have tried to see if there is any connection between 
              tax revenue as a  percentage of GDP and natural resource rents as 
              a percentage of GDP."),
                          h2("Plans For Future Milestones"),
                          p("One of the data frames that I need to bring in to this project
              is one which allows me to make maps that correspond to the 
              countries as I'd like to be able to create a map that shows the 
              countries which rely on natural resources the most. I also need
              to work alot on the best way to keep all my data and what the 
              'gather' files should look like because I am quite confused about
              that at the moment. I haven't uploaded any of my raw_data files 
              because they were too big for a github push, and so I don't know
              the right format for the other things. Should graphs be kept in 
              seperate files? Hopefully I can ask some of these questions in 
              recitation!"),
                          h2("About Me"),
                          p("My name is Josh Willcox and I am currently a second-year 
              undergraduate intending to do a joint concentration in Near 
              Eastern Languages and Civilisations with History."),
                          h2("Acknowledgements"),
                          p("Thank you to the Professor and Teaching Staff at Gov50 for
               teaching me the art of data science"),
                          p("This project's GitHub repository lives",
                            a("here", href = "https://github.com/joshwillcox/Shiny_App_Final_Project")))
                 
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$Resource_rent_vs_tax_revenue_GDP <- renderPlot({
        ggplot(first_clean %>%
                   select(country_name, country_code, indicator_name, x2015:x2018) %>%
                   drop_na() %>%
                   filter(indicator_name %in% 
                              c("Total natural resources rents (% of GDP)",
                                "Tax revenue (% of GDP)")) %>%
                   mutate(average = (x2015 + x2016 + x2017 +x2018)/4) %>%
                   filter(average > 0) %>%
                   pivot_wider(names_from = indicator_name,
                               values_from = average,
                               id_cols = c("country_name", "country_code")) %>%
                   drop_na() %>%
                   clean_names(),
               aes(x = total_natural_resources_rents_percent_of_gdp,
                   y = tax_revenue_percent_of_gdp)) +
            geom_point(color = "red") +
            geom_smooth(method = lm,
                        se = FALSE,
                        color = "dodgerblue") +
            labs(x = "Natural Resources Rent as % of GDP",
                 y = "Tax Revenue as % of GDP",
                 title = "Relationship Between Tax Revenue and Natural Resources Rent as % of GDP",
                 subtitle  = "Does Depending on Natural Resources Mean Taxing Less?",
                 caption = "Source: World Bank Development Inidicators")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

