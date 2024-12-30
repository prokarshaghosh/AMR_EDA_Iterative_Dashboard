library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(bslib)

data = read.csv(file.choose())
View(data)

# UI definition
ui <- fluidPage(
  theme = bs_theme(version = 4, bootswatch = "lux"),
  titlePanel("Country and Continent Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Filters"),
      selectInput("continent", "Select Continent:", choices = unique(data$Continent), multiple = FALSE),
      uiOutput("countryUI"),
      br(),
      tags$div(style = "text-align: center;", h5("Developed by PROKARSHA KUMAR GHOSH", style = "color: #2c3e50; font-weight: bold;")),
      hr()
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Population & GDP", 
                 h4("Population and GDP Comparison", style = "text-align: center; margin-top: 20px; color: #2c3e50;"),
                 plotOutput("pop_gdp_bar", height = "400px")),
        tabPanel("GDP per Capita & Health", 
                 h4("GDP per Capita vs Health Expenditure", style = "text-align: center; margin-top: 20px; color: #2c3e50;"),
                 plotOutput("gdp_capita_hexp_bar", height = "400px")),
        tabPanel("AMR Comparison", 
                 h4("AMR: Female vs Male", style = "text-align: center; margin-top: 20px; color: #2c3e50;"),
                 plotOutput("amr_bar", height = "400px")),
        tabPanel("Details", 
                 h4("Development Level and Average CDR", style = "text-align: center; margin-top: 20px; color: #2c3e50;"),
                 verbatimTextOutput("dev_info", placeholder = TRUE))
      )
    )
  )
)

# Server definition
server <- function(input, output, session) {
  
  # Dynamically generate country dropdown based on selected continent
  output$countryUI <- renderUI({
    req(input$continent)
    countries <- data[data$Continent == input$continent, "Countries"]
    selectInput("country", "Select Country:", choices = unique(countries), multiple = FALSE)
  })
  
  # Filter data based on inputs
  filtered_data <- reactive({
    req(input$country)
    data[data$Countries == input$country, ]
  })
  
  # Bar diagram: Average Population to Average GDP
  output$pop_gdp_bar <- renderPlot({
    req(filtered_data())
    plot_data <- filtered_data()
    ggplot(data.frame(Metric = c("Average_Pop", "Average_GDP"),
                      Value = c(plot_data$Average_Pop, plot_data$Average_GDP)),
           aes(x = Metric, y = Value, fill = Metric)) +
      geom_bar(stat = "identity", width = 0.6, show.legend = FALSE) +
      labs(title = "Population vs GDP", x = "Metric", y = "Value (in thousands)") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "#34495e")) +
      scale_fill_manual(values = c("#3498db", "#2ecc71"))
  })
  
  # Bar diagram: Average GDP per capita to Average Health Expenditure
  output$gdp_capita_hexp_bar <- renderPlot({
    req(filtered_data())
    plot_data <- filtered_data()
    ggplot(data.frame(Metric = c("Average_GDP_per_capita", "Average_HEXP"),
                      Value = c(plot_data$Average_GDP_per_capita, plot_data$Average_HEXP)),
           aes(x = Metric, y = Value, fill = Metric)) +
      geom_bar(stat = "identity", width = 0.6, show.legend = FALSE) +
      labs(title = "GDP per Capita vs Health Expenditure", x = "Metric", y = "Value (in $)") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "#34495e")) +
      scale_fill_manual(values = c("#9b59b6", "#e74c3c"))
  })
  
  # Bar diagram: AMR Female to AMR Male
  output$amr_bar <- renderPlot({
    req(filtered_data())
    plot_data <- filtered_data()
    ggplot(data.frame(Metric = c("AMR_female", "AMR_male"),
                      Value = c(plot_data$AMR_female, plot_data$AMR_male)),
           aes(x = Metric, y = Value, fill = Metric)) +
      geom_bar(stat = "identity", width = 0.6, show.legend = FALSE) +
      labs(title = "AMR Female vs AMR Male", x = "Metric", y = "Rate (per 1000 adult females/males)") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "#34495e")) +
      scale_fill_manual(values = c("#1abc9c", "#f39c12"))
  })
  
  # Display development level and Average CDR
  output$dev_info <- renderText({
    req(filtered_data())
    plot_data <- filtered_data()
    paste("Development Level:", plot_data$Development_level, "\nAverage CDR:", plot_data$Average_CDR)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
