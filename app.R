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
dig.df <- read_csv("DIG.csv")

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
    
    tabItems(
      # Introduction Tab
      tabItem(tabName = "Introduction",
              fluidRow(
                box(
                  title = "Welcome to the DIG Trial Dashboard",
                  width = 12,
                  class = "introduction-box",
                  tags$p(HTML("
              <strong>The DIG (Digitalis Investigation Group) Trial</strong> was a randomized, double-blind, multicenter trial with more than 300 centers in the United States and Canada participating. The purpose of the trial was to examine the safety and efficacy of Digoxin in treating patients with congestive heart failure in sinus rhythm. Digitalis was introduced clinically more than 200 years ago and has since become a commonly prescribed medication for the treatment of heart failure; however, there was considerable uncertainty surrounding its safety and efficacy. <br><br>
            
              Small trials indicated that Digoxin alleviated some of the symptoms of heart failure, prolonged exercise tolerance, and generally improved the quality of patients' lives. Unfortunately, these trials were generally small and although they did focus on the effect of treatment on patients’ relief from heart failure symptoms and quality of life, they failed to address the effect of treatment on cardiovascular outcomes. Questions about the safety of Digoxin were also a concern. Digoxin toxicity is uncommon in small trials with careful surveillance, however, the long-term effects of therapeutic levels of Digoxin were less clear. <br><br>
            
              The DIG dataset consists of baseline and outcome data from the main DIG trial. In the main trial, heart failure patients meeting the eligibility criterion and whose ejection fraction was 45% or less were randomized to receive either a placebo or digoxin. Outcomes assessed in the trial included: cardiovascular mortality, hospitalization or death from worsening heart failure, hospitalization due to other cardiovascular causes and hospitalization due to non-cardiovascular causes. <br><br>
            
              The <strong>DIG</strong> dataset was obtained for the purpose of this assignment and is enclosed with this assignment. The codebook associated with the variables is also enclosed with your assignment. <br><br>
            
              In order to create an anonymous dataset that protects patient confidentiality, most variables have been permuted over the set of patients within the treatment group. Therefore, it would be inappropriate to use this dataset for other research or publication purposes.
            "))
                )
              )
      ),
      
      # Hospitalization Plot Tab
      tabItem(tabName = "hospital_plot",
              fluidRow(
                box(title = "Hospitalizations by Treatment", width = 12,
                    plotlyOutput("hospital_plot"))
              )
      ),
      
      # CVD and Mortality Plot Tab
      tabItem(tabName = "cvd_plot",
              fluidRow(
                box(title = "Cardiovascular Disease and Mortality", width = 12,
                    plotlyOutput("cvd_mortality_plot"))
              )
      ),
      
      # WHF and Hospitalization Plot Tab
      tabItem(tabName = "whf_plot",
              fluidRow(
                box(title = "Worsening Heart Failure and Hospitalizations", width = 12,
                    plotlyOutput("whf_hosp_plot"))
              )
      ),
      # Mortality Over Time Plot Tab
      
      tabItem(tabName = "mortality_plot",
              fluidRow(
                box(title = "Mortality Over Time by CVD", width = 12,
                    plotlyOutput("mortality_rate_plot"))
              )
      )
    )
  )
)

# Server
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
  
  
  # Hospitalization Bar Plot
  output$hospital_plot <- renderPlotly({
    hospitalizations <- ggplot(dig.df, aes(x = HOSP, fill = TRTMT)) +
      geom_bar(position = "dodge") +
      labs(title = "Hospitalizations by Treatment", x = "Hospitalized?", fill = "Treatment") +
      scale_fill_manual(values = c("#FFAEB9", "#B03060"))
    
    ggplotly(hospitalizations)
  })
  
  # CVD and Mortality Mosaic Plot
  output$cvd_mortality_plot <- renderPlotly({
    CVD_mortality <- ggplot(dig.df) +
      geom_mosaic(aes(x = product(CVD), fill = DEATH)) +
      labs(title = "Cardiovascular Disease and Mortality by Treatment",
           x = "Suffers from CVD",
           fill = "Mortality") +
      facet_wrap(~TRTMT) +
      scale_fill_manual(values = c("#FFF68F", "#4682B4"))
    
    ggplotly(CVD_mortality)
  })
  
  # WHF and Hospitalization Plot
  output$whf_hosp_plot <- renderPlotly({
    WHF_HOSP <- ggplot(dig.df, aes(x = HOSP, fill = WHF)) +
      geom_bar() +
      facet_wrap(~TRTMT) +
      scale_fill_manual(values = c("Yes" = "#90EE90", "No" = "#607B6B")) +
      labs(title = "Hospitalizations for Worsening Heart Failure by Treatment",
           x = "Hospitalized?", fill = "WHF")
    
    ggplotly(WHF_HOSP)
  })
  # Line Plot for Mortality Rate Over Time
  output$mortality_rate_plot <- renderPlotly({
    plot2 <- ggplot(risk_mortality_summary2, aes(x = Month, y = Mortality_Rate, color = CVD, linetype = CVD)) +
      geom_line(size = 0.7) +
      geom_point(size = 2) +
      facet_wrap(~TRTMT, nrow = 2) +
      labs(x = "Time in Months", y = "Mortality Rate",
           title = "Effect of Cardiovascular Disease on Mortality Over Time by Treatment",
           color = "CVD", linetype = "CVD")
    ggplotly(plot2)
  })
}

shinyApp(ui, server)
