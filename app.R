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


shinyApp(ui, server)

