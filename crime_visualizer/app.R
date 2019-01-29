library(shiny)
library(plotly)
library(tidyverse)
library(here)

# Load data
clean_data <- read_csv(here("data", "clean_data_tidy.csv"))

# Initial list of states for first drop-down list
list_states <- clean_data %>% 
  distinct(state_name) %>% 
  arrange(state_name) %>% 
  add_row(state_name = "All States", .before=1)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Violent Crime Visualizer", 
             windowTitle = "Violent Crime Visualizer"),
  sidebarLayout(
    sidebarPanel(width = 3,
                 selectInput("state", h4("State"), 
                             choices = list_states),
                 uiOutput("citySelection"),
                 sliderInput("dateRange", h4("DateRange"),
                             min = 1975, max = 2015, value = c(1975, 2015), sep = "")
                 
    ),
    mainPanel(
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"),
      tabsetPanel(
        tabPanel("Per type of crime",
                 fluidRow(
                   tags$h3("How are crime rates evolving over time?")
                 ),
                 fluidRow(
                   column(6, plotOutput("homicides")),
                   column(6, plotOutput("rape")),
                   br(),br(),
                   br(),br(),
                   br(),br()
                 ),
                 fluidRow(
                   column(6, plotOutput("robbery")),
                   column(6, plotOutput("aggravated assault"))
                 )
                 
        ),
        tabPanel("Total", plotOutput("total"))
      )
    ) # Close Main Panel
  ) # Close SideBar Layout
) # Close Fluid Page 

server <- function(input, output) {
  # Dynamic list of cities for second drop-down list
  list_cities <- reactive({
    if (input$state == "All States") {
      c("All Cities")
    } else {
      clean_data %>%
        filter(state_name == input$state) %>%
        distinct(city_name) %>%
        arrange(city_name) %>% 
        add_row(city_name="All Cities", .before=1)
    }
  })
  
  # The city drop-down list is dynamic - must be rendered in the server and exported to the front
  output$citySelection <- renderUI({
    selectInput("city", h4("City"), choices=list_cities())
  })
  
  
  reactive_data <- reactive({
    if (input$state == "All States"){
      cols = c("year", "crime")
    } else if (input$city == "All Cities"){
      cols = c("state_name", "year", "crime")
    } else {
      cols = c("state_name", "city_name", "year", "crime")
    }
    
    clean_data %>%
      filter(year >= input$dateRange[1],
             year <= input$dateRange[2]) %>%
      group_by_at(.vars=vars(cols)) %>% 
      summarize(sum_cases = sum(cases),
                sum_pop = sum(total_pop),
                wa_per_100k = 100000 * sum_cases / sum_pop,
                avg_state = mean(avg_case)) %>% 
      mutate(crime = fct_reorder(crime, wa_per_100k))
  })

  city_data <- reactive({
    reactive_data() %>% 
    when(
      input$state != "All States" & input$city == "All Cities" ~ filter(., state_name == input$state),
      input$state != "All States" & input$city != "All Cities" ~ filter(., state_name == input$state,
                                                                        city_name == input$city),
      ~ .)
  })
  
  crime_types <- tribble(
    ~name, ~title,
    "homicides",   "Homicides",
    "rape",   "Rapes",
    "robbery",   "Robberies",
    "aggravated assault",   "Aggravated Assaults",
  )
  
  for (i in 1:nrow(crime_types)) {
    # For each iteration, generate plot of a specific crime type
    local({
      c <- crime_types$name[[i]] # get crime type
      title <- crime_types$title[[i]] #get plot title
      
      output[[c]] <- renderPlot({
        g <- city_data() %>%
              filter(crime == c) %>% 
              ggplot(aes(y = wa_per_100k, x = year)) +
              geom_line(aes(y = wa_per_100k, x = year, color="Average")) +
              ggtitle(title) +
              ylab("No. of crimes per 100k citizens") +
              scale_color_manual(name = "", values = c("Average" = "black")) +
              theme_minimal() +
              theme(plot.title = element_text(size = 15, face = "bold")) +
              theme(plot.title = element_text(hjust = 0.5)) + 
              theme(axis.title = element_text(size = 15),
                    axis.title.x=element_blank()) +
              theme(axis.text=element_text(size=12)) +
              theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
              theme(legend.text=element_text(size=10)) +
              theme(legend.position="bottom")
        
        # generating the line of comparison with State average. Only if the user picked a specific city.
        if (input$state != "All States" & input$city != "All Cities"){
          state_data <- reactive_data() %>%
                          filter(crime == c,
                                 state_name == input$state) %>% 
                          group_by(state_name, year, crime) %>% 
                          summarize(sum_state_crimes = sum(sum_cases),
                                    sum_state_population = sum(sum_pop),
                                    wa_per_100k_state = 100000 * sum_state_crimes / sum_state_population
                                    )
          
          g <- g + 
            geom_line(data = state_data, aes(y = wa_per_100k_state, x = year, color="State Average")) +
            scale_color_manual(name = "", values = c("State Average" = "red", "Average" = "black")) +
            theme(legend.text=element_text(size=10)) +
            theme(legend.position="bottom")
        } 
        
        g # This line is necessary to return the actual plot that will be displayed in the front end
        
        })
    })
  }
  
  #Aggregated plot of all crime types together
  output$total <- renderPlot(
    reactive_data() %>%
      filter(crime != 'total violent crime') %>% 
      ggplot(aes(y = wa_per_100k, x = year)) +
      geom_col(aes(fill = crime)) +
      ggtitle("Total Violent Crimes") +
      ylab("No. of crimes per 100k citizens") +
      theme_minimal() +
      theme(plot.title = element_text(size = 15, face = "bold")) +
      theme(plot.title = element_text(hjust = 0.5)) + 
      theme(axis.title = element_text(size = 15),
            axis.title.x=element_blank()) +
      theme(axis.text=element_text(size=12)) +
      theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
      theme(legend.text=element_text(size=10))
  )
  
  output$dateRangeText  <- renderText({
    paste("input$dateRange is", 
          paste(as.character(input$dateRange), collapse = " to ")
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)