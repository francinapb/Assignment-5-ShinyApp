#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(ggplot2)
library(ggmosaic)
library(readr)

# Load the dataset
dig.df <- read_csv("C:/Users/yasha/Downloads/Assignment5/DIG.csv")

# Data Preparation
dig.df <- dig.df %>%
  select(ID, TRTMT, AGE, SEX, BMI, KLEVEL, CREAT, DIABP, SYSBP, HYPERTEN, CVD, WHF, DIG, HOSP, HOSPDAYS, DEATH, DEATHDAY) %>%
  mutate(ID = as.numeric(ID),
         TRTMT = factor(TRTMT, levels = c(0, 1), labels = c("Placebo", "Treatment")),
         AGE = as.numeric(AGE),
         SEX = factor(SEX, levels = c(1, 2), labels = c("Male", "Female")),
         BMI = as.numeric(BMI),
         KLEVEL = as.numeric(KLEVEL),
         CREAT = as.numeric(CREAT),
         DIABP = as.numeric(DIABP),
         SYSBP = as.numeric(SYSBP),
         HYPERTEN = factor(HYPERTEN, levels = c(0, 1), labels = c("No", "Yes")),
         CVD = factor(CVD, levels = c(1, 0), labels = c("Yes", "No")),
         WHF = factor(WHF, levels = c(1, 0), labels = c("Yes", "No")),
         DIG = factor(DIG, levels = c(0, 1), labels = c("No", "Yes")),
         HOSP = factor(HOSP, levels = c(1, 0), labels = c("Yes", "No")),
         DEATH = factor(DEATH, levels = c(0, 1), labels = c("Alive", "Death"))
  )

# Simulated Mortality Rate Data
set.seed(123)
risk_mortality_summary2 <- data.frame(
  Month = rep(1:12, each = 4),
  Mortality_Rate = runif(48, 0.05, 0.3),
  CVD = rep(c("Yes", "No"), 24),
  TRTMT = rep(c("Placebo", "Treatment"), each = 24)
)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Clinical Trial Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "Introduction", icon = icon("book")),
      menuItem("Graphs", tabName = "Graphs", icon = icon("chart-line"),
               menuSubItem("Hospitalization Plot", tabName = "hospital_plot"),
               menuSubItem("CVD and Mortality", tabName = "cvd_plot"),
               menuSubItem("WHF and Hospitalization", tabName = "whf_plot"),
               menuSubItem("Mortality Over Time", tabName = "mortality_plot"),
               menuSubItem("Blood Pressure Relationship", tabName = "bp_plot")
      )
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
      .content-wrapper {
        background-image: url('background.jpeg');
        background-size: cover;
        background-position: center;
        color: #FFFFFF;
      }
      .small-box { background-color: rgba(31, 41, 55, 0.8); color: white; }
      .box { background-color: rgba(31, 41, 55, 0.8); border: 1px solid #374151; }
      .introduction-box {
          width: 80%;
          margin: 50px auto;
          padding: 30px;
          text-align: justify;
          line-height: 1.8;
          font-size: 16px;
      }
      .box-title {
        color: #98F5FF;  /* Title color updated to desired shade */
        text-align: center;  /* Center the title */
        width: 100%;  /* Ensure the title spans full width */
      }
    "))
    ),
  )
)
#Server
server <- function(input, output) {
  
  # Reactive filtered dataset
  filtered_data <- reactive({
    dig.df %>%
      filter(TRTMT == input$treatment_filter,
             SEX == input$sex_filter,
             CVD == input$cvd_filter)
  })
  
  # Value Boxes
  output$death_count <- renderValueBox({
    valueBox(
      sum(dig.df$DEATH == "Death", na.rm = TRUE),
      "Total Deaths",
      icon = icon("heartbeat"),
      color = "red"
    )
  })
  
  output$hospital_count <- renderValueBox({
    valueBox(
      sum(dig.df$HOSP == "Yes", na.rm = TRUE),
      "Total Hospitalizations",
      icon = icon("hospital"),
      color = "blue"
    )
  })
  
  output$treatment_count <- renderValueBox({
    valueBox(
      table(dig.df$TRTMT)["Treatment"],
      "Patients on Treatment",
      icon = icon("prescription-bottle"),
      color = "green"
    )
  })
}

shinyApp(ui, server)

