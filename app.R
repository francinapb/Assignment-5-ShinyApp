#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
# Load Required Libraries
# Load Required Libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(DT)
library(readr)

# Load the Data
DIG_data <- read_csv("DIG.csv")

# Handle Missing Values (Optional, based on data needs)
DIG_data <- DIG_data %>% mutate(across(everything(), ~ replace_na(.x, 0)))

# UI Definition
ui <- dashboardPage(
  dashboardHeader(title = "Digitalis Investigation Group Trial"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Summary", tabName = "summary", icon = icon("table")),
      menuItem("Visualization", tabName = "visualization", icon = icon("chart-bar")),
      menuItem("Interactive Analysis", tabName = "interactive", icon = icon("sliders-h"))
    )
  ),
  dashboardBody(
    tabItems(
      
      # Summary Tab
      tabItem(tabName = "summary",
              fluidRow(
                box(title = "Summary Statistics", width = 12, solidHeader = TRUE,
                    DTOutput("summaryTable"))
              )),
      
      # Visualization Tab
      tabItem(tabName = "visualization",
              fluidRow(
                box(title = "Age Distribution", width = 6, solidHeader = TRUE,
                    plotOutput("basicPlot")),
                box(title = "BMI vs Age", width = 6, solidHeader = TRUE,
                    plotOutput("advancedPlot"))
              )),
      
      # Interactive Tab
      tabItem(tabName = "interactive",
              fluidRow(
                box(title = "Filter Data", width = 4, solidHeader = TRUE,
                    selectInput("variable", "Select Variable:", choices = c("AGE", "BMI", "SEX")),
                    sliderInput("range", "Select Range:", 
                                min = min(DIG_data$AGE, na.rm = TRUE), 
                                max = max(DIG_data$AGE, na.rm = TRUE), 
                                value = c(50, 80))),
                box(title = "Filtered Data Plot", width = 8, solidHeader = TRUE,
                    plotOutput("filteredPlot"))
              ))
    )
  )
)

# Server Definition
server <- function(input, output) {
  
  # Summary Table
  output$summaryTable <- renderDT({
    DIG_data %>%
      summarise(
        Count = n(),
        Avg_Age = mean(AGE, na.rm = TRUE),
        Avg_BMI = mean(BMI, na.rm = TRUE),
        Deaths = sum(DEATH, na.rm = TRUE)
      )
  })
  
  # Basic Plot - Age Distribution
  output$basicPlot <- renderPlot({
    ggplot(DIG_data, aes(x = AGE)) +
      geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
      labs(title = "Age Distribution", x = "Age", y = "Count") +
      theme_minimal()
  })
  
  # Advanced Plot - BMI vs Age
  output$advancedPlot <- renderPlot({
    ggplot(DIG_data, aes(x = AGE, y = BMI, color = factor(SEX))) +
      geom_point(alpha = 0.7) +
      labs(title = "BMI vs Age by Gender", x = "Age", y = "BMI", color = "Sex") +
      theme_minimal()
  })
  
  # Filtered Data Plot
  filtered_data <- reactive({
    DIG_data %>%
      filter(AGE >= input$range[1], AGE <= input$range[2])
  })
  
  output$filteredPlot <- renderPlot({
    ggplot(filtered_data(), aes_string(x = "AGE", y = input$variable)) +
      geom_point(color = "dodgerblue") +
      labs(title = paste("Filtered Plot:", input$variable, "vs Age"), x = "Age", y = input$variable) +
      theme_minimal()
  })
}

# Run the Application
shinyApp(ui = ui, server = server)

