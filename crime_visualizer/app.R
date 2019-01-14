library(shiny)
library(tidyverse)

# data <- read_csv("..")

# Define UI for application that draws a histogram
ui <- fluidPage(
    # h1("Crime Visualizer"),
    titlePanel("Marshall Violent Crime Visualizer", 
               windowTitle = "Crime App"),
    sidebarLayout(
        sidebarPanel(sliderInput("Date range", "Select your desired date range.",
                                 min = 0, max = 100, value = c(15, 30), pre="$")),
        mainPanel(plotOutput("price_hist"),
                  tableOutput("price_table"))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # observe(print(input$priceInput))
    # 
    # bcl_filtered <- reactive(bcl %>% 
    #                              filter(Price > input$priceInput[1],
    #                                     Price < input$priceInput[2])
    # )
    # 
    # output$price_hist <- renderPlot(
    #     bcl_filtered() %>% 
    #         ggplot(aes(Price)) +
    #         geom_histogram()
    # )
    # 
    # output$price_table <- renderTable(
    #     bcl_filtered()
    # )
    # 
}

# Run the application 
shinyApp(ui = ui, server = server)