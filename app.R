library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(DT)
library(readr)

####################################### Load the Data#######################

DIG_data <- read_csv("C:/Users/yasha/Downloads/Assignment5/DIG.csv")

#################################### UI Design ##############################

ui <- dashboardPage(
  dashboardHeader(title = tagList(icon("heartbeat"), "DIG Trial Dashboard")),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("tachometer-alt")),
      menuItem("Summary", tabName = "summary", icon = icon("table")),
      menuItem("Visualizations", tabName = "visualization", icon = icon("chart-bar")),
      menuItem("Interactive Analysis", tabName = "interactive", icon = icon("sliders-h"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "overview",
              fluidRow(
                box(
                  title = "DIG Trial Overview",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  infoBox("Total Patients", nrow(DIG_data), icon = icon("users"), color = "green", width = 4),
                  infoBox("Average Age", round(mean(DIG_data$AGE, na.rm = TRUE), 1), icon = icon("calendar"), color = "blue", width = 4),
                  infoBox("Avg BMI", round(mean(DIG_data$BMI, na.rm = TRUE), 1), icon = icon("weight"), color = "red", width = 4)
                ),
                box(
                  title = "Study Description",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  HTML("<p>This dataset provides comprehensive data on the effects of digitalis treatment on heart failure patients. Analysis covers demographics, treatment details, and clinical outcomes.</p>")
                )
              )),
      tabItem(tabName = "summary",
              fluidRow(
                box(
                  title = "Summary Statistics",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  DTOutput("summaryTable")
                )
              )),
      tabItem(tabName = "visualization",
              fluidRow(
                box(
                  title = "Age Distribution",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  plotOutput("basicPlot")
                ),
                box(
                  title = "BMI vs Age by Gender",
                  status = "success",
                  solidHeader = TRUE,
                  width = 6,
                  plotOutput("advancedPlot")
                )
              )),
      tabItem(tabName = "interactive",
              fluidRow(
                box(
                  title = "Filter Data",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 4,
                  selectInput("variable", "Select Variable:", choices = c("AGE", "BMI", "SEX")),
                  sliderInput("range", "Select Range:", 
                              min = min(DIG_data$AGE, na.rm = TRUE), 
                              max = max(DIG_data$AGE, na.rm = TRUE), 
                              value = c(50, 80))
                ),
                box(
                  title = "Filtered Data Plot",
                  status = "danger",
                  solidHeader = TRUE,
                  width = 8,
                  plotOutput("filteredPlot")
                )
              ))
    )
  )
)

################################### Server Code ##############################################
server <- function(input, output) {
  
  # Overview InfoBox content
  output$totalPatients <- renderInfoBox({
    infoBox("Total Patients", nrow(DIG_data), icon = icon("users"), color = "green", width = 4)
  })
  
  output$avgAge <- renderInfoBox({
    infoBox("Average Age", round(mean(DIG_data$AGE, na.rm = TRUE), 1), icon = icon("calendar"), color = "blue", width = 4)
  })
  
  output$avgBMI <- renderInfoBox({
    infoBox("Avg BMI", round(mean(DIG_data$BMI, na.rm = TRUE), 1), icon = icon("weight"), color = "red", width = 4)
  })
  
  # Summary Table
  
  output$summaryTable <- renderDT({
    summary_stats <- DIG_data %>%
      summarise(
        Count = n(),
        Avg_Age = mean(AGE, na.rm = TRUE),
        Avg_BMI = mean(BMI, na.rm = TRUE),
        Deaths = sum(DEATH, na.rm = TRUE)
      )
    
    # Define dynamic breaks for Avg_Age and Avg_BMI
    
    age_breaks <- c(60, 70, 80)  # Example for age
    bmi_breaks <- c(18, 25, 30)  # Example for BMI
    
    # Apply dynamic styling with sorted breaks and ensure that the number of colors is one less than the number of breaks
    
    datatable(summary_stats, 
              options = list(
                pageLength = 5, 
                dom = 't', 
                search = list(regex = TRUE, caseInsensitive = TRUE)  # Corrected the search option
              )) %>%
      formatStyle(
        'Avg_Age', 
        backgroundColor = styleInterval(age_breaks, c('#1F78B466', '#1F78B480', '#1F78B499', '#1F78B4B3'))  # 4 colors for 3 breaks
      ) %>%
      formatStyle(
        'Avg_BMI', 
        backgroundColor = styleInterval(bmi_breaks, c('#1F78B466', '#1F78B480','#1F78B499', '#1F78B4B3'))  # 4 colors for 3 breaks
      )
  })
  
  # Basic Plot - Age Distribution
  
  output$basicPlot <- renderPlot({
    ggplot(DIG_data, aes(x = AGE)) +
      geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
      labs(title = "Age Distribution", x = "Age", y = "Count") +
      theme_minimal()
  })
  
  # Advanced Plot - BMI vs Age by Gender
  
  output$advancedPlot <- renderPlot({
    ggplot(DIG_data, aes(x = AGE, y = BMI, color = factor(SEX))) +
      geom_point(alpha = 0.7) +
      labs(title = "BMI vs Age by Gender", x = "Age", y = "BMI", color = "Sex") +
      theme_minimal()
  })
  
  # Reactive Filtered Data based on Slider and Variable Selection
  
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

##################################### Running the app#########################################################
shinyApp(ui = ui, server = server)
