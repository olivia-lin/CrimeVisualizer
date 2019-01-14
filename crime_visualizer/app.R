library(shiny)
library(tidyverse)
library(here)

clean_data <- read_csv(here("data", "ucr_crime_1975_2015.csv"))

# Define UI for application that draws a histogram
ui <- fluidPage(
    # h1("Crime Visualizer"),
    titlePanel("Marshall Violent Crime Visualizer", 
               windowTitle = "Crime App"),
    sidebarLayout(
        sidebarPanel(
            selectInput("state", h4("State"), 
                        choices = list("Choice 1" = 1, "Choice 2" = 2,
                                       "Choice 3" = 3), selected = 1),
            selectInput("city", h4("City"), 
                        choices = list("Choice 1" = 1, "Choice 2" = 2,
                                       "Choice 3" = 3), selected = 1),
            dateRangeInput('dateRange',
                            label = h4('Date range'),
                            start = Sys.Date() - 2, end = Sys.Date() + 2)

        ),
        mainPanel(#plotOutput("price_hist"),
                  #tableOutput("price_table")
        ),
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #For debugging purposes
    observe(print(input$state))
    observe(print(input$city))
    observe(print(input$dateRange))
    # 
    reactive_data <- reactive(clean_data %>% 
                                  filter(year >= input$dateRange[1],
                                         year <= input$dateRange[2],
                                         state == input$state,
                                         city == input$city) %>% 
                                  group_by(state, city, year) %>% 
                                  summarize(total_homs = sum(homs_sum),
                                            total_rape = sum(rape_sum),
                                            total_rob = sum(rob_sum),
                                            total_agg_ass = sum(agg_ass_sum),
                                            total = sum)
                              )
    # 
    output$robbery <- renderPlot(
        reactive_data() %>%
            ggplot(aes(Price)) +
            geom_histogram()
    )
    # 
    # output$price_table <- renderTable(
    #     bcl_filtered()
    # )
    output$dateRangeText  <- renderText({
        paste("input$dateRange is", 
              paste(as.character(input$dateRange), collapse = " to ")
        )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)