<<<<<<< HEAD
=======
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
library(rsconnect)
>>>>>>> eb860c9f3d48688dcad763f63e11edce632ee8c4
library(shiny)

library(plotly)

library(dplyr)

library(ggplot2)

library(ggmosaic)

library(readr)
library(table1)

library(bslib)  # Bootstrap for styling



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



# UI

ui <- navbarPage(
  
  title = div("Clinical Trial Dashboard", style = "font-size: 28px; font-weight: bold;"),
  
  theme = bs_theme(bootswatch = "flatly"),
  
  
  
  tabPanel("Introduction",
           
           fluidRow(
             
             column(10, offset = 1,
                    
                    box(title = "Welcome to the DIG Trial Dashboard", width = 12,
                        
                        tags$p("Overview of the DIG trial with dataset description.")
                        
                    )
                    
             )
             
           )
           
  ),
<<<<<<< HEAD
  
  
  
  # Hospitalization Plot
  
  tabPanel("Hospitalization Plot",
           
           sidebarLayout(
             
             sidebarPanel(
               
               selectInput("hosp_trt", "Treatment", choices = c("All", "Placebo", "Treatment")),
               
               selectInput("hosp_cvd", "CVD Status", choices = c("All", "Yes", "No"))
               
             ),
             
             mainPanel(
               
               plotlyOutput("hospital_plot")
               
             )
             
           )
           
  ),
  
  
  
  # CVD and Mortality Plot
  
  tabPanel("CVD and Mortality",
           
           sidebarLayout(
             
             sidebarPanel(
               
               selectInput("cvd_trt", "Treatment", choices = c("All", "Placebo", "Treatment")),
               
               selectInput("cvd_status", "CVD Status", choices = c("All", "Yes", "No"))
               
             ),
             
             mainPanel(
               
               plotlyOutput("cvd_mortality_plot")
               
             )
             
           )
           
  ),
  
  
  
  # WHF and Hospitalization
  
  tabPanel("WHF and Hospitalization",
           
           sidebarLayout(
             
             sidebarPanel(
               
               selectInput("whf_trt", "Treatment", choices = c("All", "Placebo", "Treatment")),
               
               checkboxInput("whf_only", "Show WHF Only", value = FALSE)
               
             ),
             
             mainPanel(
               
               plotlyOutput("whf_hosp_plot")
               
             )
             
           )
           
  ),
  
  
  
  # Blood Pressure Relationship
  
  tabPanel("Blood Pressure Relationship",
           
           sidebarLayout(
             
             sidebarPanel(
               
               sliderInput("bp_age", "Age Range", min = 30, max = 90, value = c(40, 70)),
               
               selectInput("bp_trt", "Treatment", choices = c("All", "Placebo", "Treatment"))
               
             ),
             
             mainPanel(
               
               plotlyOutput("bp_relationship_plot")
               
             )
             
           )
           
=======
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
          border-radius: 10px;  
      }
      .box-title {
        color: #98F5FF;  
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
                box(title = "Hospitalizations by Treatment", width = 12,
                    plotlyOutput("hospital_plot"),
                    div(class = "footer-text",
                        "By clicking on the variable you want to observe, you can select the different options to isolate it and observe its data.")
                )
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
      ),
      # Blood Pressure Relationship Plot Tab
      tabItem(tabName = "bp_plot",
              fluidRow(
                box(title = "Blood Pressure Relationship", width = 12,
                    plotlyOutput("bp_relationship_plot"))
              )
      )
    )
>>>>>>> eb860c9f3d48688dcad763f63e11edce632ee8c4
  )
  
)



# Server

server <- function(input, output) {
  
<<<<<<< HEAD
  
  
  # Filtered Dataset for Hospitalization Plot
  
  filtered_hosp <- reactive({
    
    df <- dig.df
    
    if (input$hosp_trt != "All") {
      
      df <- df %>% filter(TRTMT == input$hosp_trt)
      
    }
    
    if (input$hosp_cvd != "All") {
      
      df <- df %>% filter(CVD == input$hosp_cvd)
      
    }
    
    df
    
  })
  
  
  
  # Hospitalization Plot
  
=======
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
>>>>>>> eb860c9f3d48688dcad763f63e11edce632ee8c4
  output$hospital_plot <- renderPlotly({
    
    hospitalizations <- ggplot(filtered_hosp(), aes(x = HOSP, fill = TRTMT)) +
      
      geom_bar(position = "dodge") +
<<<<<<< HEAD
      
      labs(title = "Hospitalizations by Treatment", x = "Hospitalized?", fill = "Treatment") +
      
=======
      labs(title = "Hospitalisations by Treatment", x = "Hospitalized?", fill = "Treatment") +
>>>>>>> eb860c9f3d48688dcad763f63e11edce632ee8c4
      scale_fill_manual(values = c("#FFAEB9", "#B03060"))
    
    
    
    ggplotly(hospitalizations)
    
  })
  
  
  
  # Filtered Dataset for CVD Plot
  
  filtered_cvd <- reactive({
    
    df <- dig.df
    
    if (input$cvd_trt != "All") {
      
      df <- df %>% filter(TRTMT == input$cvd_trt)
      
    }
    
    if (input$cvd_status != "All") {
      
      df <- df %>% filter(CVD == input$cvd_status)
      
    }
    
    df
    
  })
  
  
  
  # CVD and Mortality Plot
  
  output$cvd_mortality_plot <- renderPlotly({
    
    CVD_mortality <- ggplot(filtered_cvd()) +
      
      geom_mosaic(aes(x = product(CVD), fill = DEATH)) +
      
      facet_wrap(~TRTMT) +
      
      labs(title = "CVD and Mortality by Treatment") +
      
      scale_fill_manual(values = c("#FFF68F", "#4682B4"))
    
    
    
    ggplotly(CVD_mortality)
    
  })
  
  
  
  # Filtered WHF Plot
  
  output$whf_hosp_plot <- renderPlotly({
    
    df <- dig.df
    
    if (input$whf_trt != "All") {
      
      df <- df %>% filter(TRTMT == input$whf_trt)
      
    }
    
    if (input$whf_only) {
      
      df <- df %>% filter(WHF == "Yes")
      
    }
    
    
    
    WHF_HOSP <- ggplot(df, aes(x = HOSP, fill = WHF)) +
      
      geom_bar() +
      
      labs(title = "Hospitalizations for WHF by Treatment")
    
    
    
    ggplotly(WHF_HOSP)
    
  })
  
  
  
  # Filtered BP Plot
  
  output$bp_relationship_plot <- renderPlotly({
    
    df <- dig.df %>% filter(AGE >= input$bp_age[1] & AGE <= input$bp_age[2])
    
    if (input$bp_trt != "All") {
      
      df <- df %>% filter(TRTMT == input$bp_trt)
      
    }
    
    
    
    bp_plot <- ggplot(df, aes(x = SYSBP, y = DIABP, colour = TRTMT)) +
      
      geom_point() +
      
      labs(title = "BP Relationship by Age and Treatment")
    
    
    
    ggplotly(bp_plot)
    
  })
<<<<<<< HEAD
  
}



shinyApp(ui, server)
=======
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

>>>>>>> eb860c9f3d48688dcad763f63e11edce632ee8c4
