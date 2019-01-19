library(shiny)
library(tidyverse)
library(here)

clean_data <- read_csv(here("data", "clean_data.csv"))
list_states <- clean_data %>% 
                distinct(state_name) %>% 
                arrange(state_name) %>% 
                add_row(state_name = "All", .before=1)

# Define UI for application that draws a histogram
ui <- fluidPage(
    # h1("Crime Visualizer"),
    titlePanel("Marshall Violent Crime Visualizer", 
               windowTitle = "Crime App"),
    sidebarLayout(
        sidebarPanel(width = 3,
            selectInput("state", h4("State"), 
                        choices = list_states),
            uiOutput("citySelection"),
            sliderInput("dateRange", h4("DateRange"),
                        min = 1975, max = 2015, value = c(1975, 2015))

        ),
        mainPanel(
            tabsetPanel(
                tabPanel("Per type of crime",
                         fluidRow(
                             column(6, plotOutput("homs")),
                             column(6, plotOutput("rape"))
                         ),
                         fluidRow(
                             column(6, plotOutput("rob")),
                             column(6, plotOutput("agg_ass"))
                         )
                         
                ),
                tabPanel("Total", plotOutput("total"))
            )
            

        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    list_cities <- reactive({
        if (input$state == "All") {
            c("All")
        } else {
            clean_data %>%
                filter(state_name == input$state) %>%
                distinct(city_name) %>%
                arrange(city_name) %>% 
                add_row(city_name="All", .before=1)
        }

    })

    output$citySelection <- renderUI({
        selectInput("city", h4("City"), choices=list_cities())
        })
        

    reactive_data <- reactive({
        if (input$state == "All"){
            clean_data %>% 
                filter(year >= input$dateRange[1],
                       year <= input$dateRange[2]) %>% 
                group_by(year) %>% 
                summarize(total_homs = sum(homs_sum),
                          total_rape = sum(rape_sum),
                          total_rob = sum(rob_sum),
                          total_agg_ass = sum(agg_ass_sum),
                          total = sum(violent_crime),
                          sum_pop = sum(total_pop),
                          wa_homs_per_100k = 100000 * total_homs / sum_pop,
                          wa_rape_per_100k = 100000 * total_rape / sum_pop,
                          wa_rob_per_100k = 100000 * total_rob / sum_pop,
                          wa_agg_ass_per_100k = 100000 * total_agg_ass / sum_pop, 
                          wa_total_per_100k = 100000 * total / sum_pop)
            
        } else if (input$city == "All"){
            clean_data %>% 
                filter(year >= input$dateRange[1],
                       year <= input$dateRange[2],
                       state_name == input$state) %>% 
                group_by(state_name, year) %>% 
                summarize(total_homs = sum(homs_sum),
                          total_rape = sum(rape_sum),
                          total_rob = sum(rob_sum),
                          total_agg_ass = sum(agg_ass_sum),
                          total = sum(violent_crime),
                          sum_pop = sum(total_pop),
                          wa_homs_per_100k = 100000 * total_homs / sum_pop,
                          wa_rape_per_100k = 100000 * total_rape / sum_pop,
                          wa_rob_per_100k = 100000 * total_rob / sum_pop,
                          wa_agg_ass_per_100k = 100000 * total_agg_ass / sum_pop, 
                          wa_total_per_100k = 100000 * total / sum_pop)
            
        } else {
            clean_data %>% 
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
                          wa_agg_ass_per_100k = 100000 * total_agg_ass / sum_pop, 
                          wa_total_per_100k = 100000 * total / sum_pop)
        }

    })
    crime_types <- tribble(
        ~name, ~title, ~variable,
        "homs",   "Homicides", "wa_homs_per_100k",
        "rape",   "Rapes", "wa_rape_per_100k",
        "rob",   "Robberies", "wa_rob_per_100k",
        "agg_ass",   "Aggravated Assaults", "wa_agg_ass_per_100k"
    )

    for (i in 1:nrow(crime_types)) {
        local({
        c <- crime_types$name[[i]]
        title <- crime_types$title[[i]]
        var <- crime_types$variable[[i]]

        output[[c]] <- renderPlot(
            reactive_data() %>%
                ggplot(aes_string(y = var, x = "year")) +
                geom_line(color="#386cb0") +
                geom_point(shape="square", size=2, color="#253494") +
                ggtitle(title) +
                ylab("Per 100k citizens") +
                theme_minimal() +
                theme(plot.title = element_text(size = 15, face = "bold")) +
                theme(plot.title = element_text(hjust = 0.5)))
        })
    }
    
    output$total <- renderPlot(
        reactive_data() %>%
            ggplot(aes(y = wa_total_per_100k, x = year)) +
            geom_col(fill="#386cb0", color="#386cb0") +
            ggtitle("Total Violent Crimes") +
            ylab("Per 100k citizens") +
            theme_minimal() +
            theme(plot.title = element_text(size = 15, face = "bold")) +
            theme(plot.title = element_text(hjust = 0.5))
    )
    
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