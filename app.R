library(shiny)

library(plotly)

library(dplyr)

library(ggplot2)

library(ggmosaic)

library(readr)

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
           
  )
  
)



# Server

server <- function(input, output) {
  
  
  
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
  
  output$hospital_plot <- renderPlotly({
    
    hospitalizations <- ggplot(filtered_hosp(), aes(x = HOSP, fill = TRTMT)) +
      
      geom_bar(position = "dodge") +
      
      labs(title = "Hospitalizations by Treatment", x = "Hospitalized?", fill = "Treatment") +
      
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
  
}



shinyApp(ui, server)