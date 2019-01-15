library(shiny)
library(tidyverse)
library(here)

clean_data <- read_csv(here("data", "clean_data.csv"))
list_states <- clean_data %>% 
                    distinct(state_name) %>% 
                    arrange(state_name)
list_cities <- clean_data %>%
                    distinct(city_name) %>%
                    arrange(city_name)
                

# Define UI for application that draws a histogram
ui <- fluidPage(
    # h1("Crime Visualizer"),
    titlePanel("Marshall Violent Crime Visualizer", 
               windowTitle = "Crime App"),
    sidebarLayout(
        sidebarPanel(
            selectInput("state", h4("State"), 
                        choices = list_states),
            selectInput("city", h4("City"), 
                        choices = list_cities),
            sliderInput("dateRange", h4("DateRange"),
                        min = 1975, max = 2015, value = c(1975, 2015))

        ),
        mainPanel(plotOutput("robbery"),
                  tableOutput("reactive_data")
        ),
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #For debugging purposes
    observe(print(input$state))
    observe(print(input$city))
    observe(print(input$dateRange))
    
    # updateSelectInput(session, city, choices = cities_choice)

    reactive_data <- reactive(clean_data %>% 
                                  filter(year >= input$dateRange[1],
                                         year <= input$dateRange[2],
                                         state_name == input$state,
                                         city_name == input$city) %>% 
                                  group_by(state_name, city_name, year) %>% 
                                  summarize(total_homs = sum(homs_sum),
                                            total_rape = sum(rape_sum),
                                            total_rob = sum(rob_sum),
                                            total_agg_ass = sum(agg_ass_sum),
                                            total = sum(violent_crime),
                                            sum_pop = sum(total_pop),
                                            wa_homs_per_100k = 100000 * total_homs / sum_pop,
                                            wa_rape_per_100k = 100000 * total_rape / sum_pop,
                                            wa_rob_per_100k = 100000 * total_rob / sum_pop,
                                            wa_agg_ass_per_100k = 100000 * total_agg_ass / sum_pop
                                            )
                              )
    
    output$robbery <- renderPlot(
        reactive_data() %>%
            ggplot(aes(y = wa_rob_per_100k, x = year)) +
            geom_col()
    )
    # 
    output$reactive_data <- renderTable(
        reactive_data()
    )
    output$dateRangeText  <- renderText({
        paste("input$dateRange is", 
              paste(as.character(input$dateRange), collapse = " to ")
        )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)