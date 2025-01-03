#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
library(rsconnect)
library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(ggplot2)
library(ggmosaic)
library(readr)
library(table1)

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
          background-color: #E0F7FA;  
          color: #374151;  
          border-radius: 20px;  
      }
      .box-title {
        color: #66FF99;  
        text-align: center;
        width: 100%;
      }
      .summary-table-container {
        background-color: #FFFFFF;  
        padding: 20px;
        border-radius: 10px;
        box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1); 
      }
      .summary-table-container table {
        width: 100%;
        border-collapse: collapse;
      }
      .summary-table-container th, .summary-table-container td {
        border-bottom: 1px solid #ccc;
        padding: 10px;
      }
      .summary-table-container th {
        background-color: #f2f2f2;
        font-weight: bold;
      }
    "))
    ),
    
    tags$head(
      tags$style(HTML("
      .skin-blue .main-sidebar {
        background-color: #F0F4F8;
      }
      
      .skin-blue .sidebar-menu>li>a {
        color: #374151;
      }
      
      .skin-blue .sidebar-menu>li.active>a {
        background-color: #D1E9FF;
        color: #003366;
        font-weight: bold;
      }
      
      .skin-blue .sidebar-menu>li>.treeview-menu {
        background-color: #4A708B;
      }
      
      .skin-blue .sidebar-menu>li>.treeview-menu>li>a {
        color: #9FB6CD;
      }
      
      .skin-blue .sidebar-menu>li>.treeview-menu>li.active>a {
        background-color: #BBDEFB;
        color: #003366;
        font-weight: bold;
      }
      
      .skin-blue .sidebar-menu>li>.treeview-menu>li:hover>a {
        background-color: #D6EAF8;
        color: #003366;
      }
      
      .skin-blue .sidebar-menu>li:hover>a {
        background-color: #E3F2FD;
        color: #003366;
      }
      
      .skin-blue .main-header .navbar {
        background-color: #836FFF;
      }
      
      .skin-blue .main-header .logo {
        background-color: #27408B;
        color: #FFFFFF;
        font-weight: bold;
      }
      
      .box {
        background-color: #FFFFFF;
        border: 1px solid #D1D5DB;
        color: #374151;
      }
      
      .box-title {
        color: #1E88E5;  
        text-align: center;
      }
      
      .content-wrapper {
        background-color: #F9FAFB;
      }
      
      .footer-text {
        text-align: center;
        font-size: 14px;
        color: #374151;
        margin-top: 10px;
        background-color: #E3F2FD;
        padding: 10px;
        border-radius: 5px;
      }
    "))
    ),
    
    tabItems(
      # Introduction Tab
      tabItem(tabName = "Introduction",
              fluidRow(
                box(
                  title = "Welcome to the DIG Trial Dashboard",
                  width = 16,
                  class = "introduction-box",
                  tags$p(HTML("
              <strong>The DIG (Digitalis Investigation Group) Trial</strong> was a randomized, double-blind, multicenter trial with more than 300 centers in the United States and Canada participating. The purpose of the trial was to examine the safety and efficacy of Digoxin in treating patients with congestive heart failure in sinus rhythm. Digitalis was introduced clinically more than 200 years ago and has since become a commonly prescribed medication for the treatment of heart failure; however, there was considerable uncertainty surrounding its safety and efficacy. <br><br>
            
              Small trials indicated that Digoxin alleviated some of the symptoms of heart failure, prolonged exercise tolerance, and generally improved the quality of patients' lives. Unfortunately, these trials were generally small and although they did focus on the effect of treatment on patients’ relief from heart failure symptoms and quality of life, they failed to address the effect of treatment on cardiovascular outcomes. Questions about the safety of Digoxin were also a concern. Digoxin toxicity is uncommon in small trials with careful surveillance, however, the long-term effects of therapeutic levels of Digoxin were less clear. <br><br>
            
              The DIG dataset consists of baseline and outcome data from the main DIG trial. In the main trial, heart failure patients meeting the eligibility criterion and whose ejection fraction was 45% or less were randomized to receive either a placebo or digoxin. Outcomes assessed in the trial included: cardiovascular mortality, hospitalization or death from worsening heart failure, hospitalization due to other cardiovascular causes and hospitalization due to non-cardiovascular causes. <br><br>
            
              The <strong>DIG</strong> dataset was obtained for the purpose of this assignment and is enclosed with this assignment. The codebook associated with the variables is also enclosed with your assignment. <br><br>
            
              In order to create an anonymous dataset that protects patient confidentiality, most variables have been permuted over the set of patients within the treatment group. Therefore, it would be inappropriate to use this dataset for other research or publication purposes.
            ")),
                  tags$p(HTML("
              The DIG dataset consists of baseline and outcome data from the main DIG trial. In the main trial, heart failure patients meeting the eligibility criterion and whose ejection fraction was 45% or less were randomized to receive either a placebo or digoxin. <br><br>
            ")),
                  div(class = "summary-table-container",
                      htmlOutput("summary_table"))
                )
              )
      ),
      
      # Hospitalization Plot Tab
      tabItem(tabName = "hospital_plot",
              fluidRow(
                box(title = "Hospitalizations by Treatment", width = 16,
                    plotlyOutput("hospital_plot"),
                    div(class = "footer-text",
                        "Interact with the plot. You can select the different variables of the legend.")
                  )
              )
      ),
      
      # CVD and Mortality Plot Tab
      tabItem(tabName = "cvd_plot",
              fluidRow(
                box(title = "Cardiovascular Disease and Mortality", width = 16,
                    plotlyOutput("cvd_mortality_plot"),
                    div(class = "footer-text",
                        "Interact with the plot. You can select the different variables of the legend.")
                  )
              )
      ),
      
      # WHF and Hospitalization Plot Tab
      tabItem(tabName = "whf_plot",
              fluidRow(
                box(title = "Worsening Heart Failure and Hospitalizations", width = 16,
                    plotlyOutput("whf_hosp_plot"),
                    div(class = "footer-text",
                        "Interact with the plot. You can select the different variables of the legend.")
                    )
              )
      ),
      
      # Mortality Over Time Plot Tab
      tabItem(tabName = "mortality_plot",
              fluidRow(
                box(title = "Mortality Over Time by CVD", width = 16,
                    plotlyOutput("mortality_rate_plot"),
                    div(class = "footer-text",
                        "Interact with the plot. You can select the different variables of the legend.")
                    )
              )
      ),
      
      # Blood Pressure Relationship Plot Tab
      tabItem(tabName = "bp_plot",
              fluidRow(
                box(title = "Blood Pressure Relationship", width = 16,
                    plotlyOutput("bp_relationship_plot"),
                    div(class = "footer-text",
                        "Interact with the plot. You can select the different variables of the legend.")
                    )
              )
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  label(dig.df$AGE)     <- "Age"
  label(dig.df$SEX)     <- "Sex"
  label(dig.df$BMI)     <- "Body Mass Index"
  label(dig.df$KLEVEL)  <- "Serum Potassium Level"
  label(dig.df$CREAT)   <- "Serum Creatinine"
  label(dig.df$DIABP)   <- "Diastolic Blood Pressure"
  label(dig.df$SYSBP)   <- "Systolic Blood Pressure"
  label(dig.df$HYPERTEN) <- "Hypertension History"
  label(dig.df$CVD)     <- "Cardiovascular Disease (CVD) Hospitalisation"
  label(dig.df$WHF)     <- "Worsening Heart Failure (WHF) Hospitalisation"
  label(dig.df$DIG)     <- "Digoxin Toxicity Hospitalisation"
  label(dig.df$HOSP)    <- "Any Hospitalisation"
  label(dig.df$TRTMT)   <- "Treatment"
  
  #Table1 Summary
  output$summary_table <- renderUI({
    table1_output <- table1(~ AGE + SEX + BMI + KLEVEL + CREAT + DIABP + SYSBP + HYPERTEN + CVD + WHF + DIG + HOSP | TRTMT, data = dig.df)
    HTML(table1_output)
  })
  
  # Hospitalisation Bar Plot
  output$hospital_plot <- renderPlotly({
    hospitalizations <- ggplot(dig.df, aes(x = HOSP, fill = TRTMT)) +
      geom_bar(position = "dodge") +
      labs(title = "Hospitalisations by Treatment", x = "Hospitalized?", fill = "Treatment") +
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
  # Scatter Plot for month and mortality rate are both continuous variables
  output$bp_relationship_plot <- renderPlotly({
    plot3 <- ggplot(dig.df, aes(x = SYSBP, y = DIABP, colour = TRTMT, shape = TRTMT)) +
      geom_point(size = 1.2) +
      geom_smooth(method = "lm", formula = y ~ x, se = F) +
      labs(
        title = "Relationship between Systolic and Diastolic Blood Pressure",
        x = "Systolic Blood Pressure (mmHg)",
        y = "Diastolic Blood Pressure (mmHg)",
        color = "Treatment Type",
        shape = "Treatment Type"
      ) +
      scale_color_manual(values = c("Placebo" = "#000", "Treatment" = "#008B8B"))
    ggplotly(plot3)
  })
}

shinyApp(ui, server)

