library(shinydashboard)
library(shiny)
library(lubridate)
library(DT)
library(scales)
library(readr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(scales)
library(ggrepel)
library(ggflags)
library(utils)
library(janitor)

# Importing data

#setwd("D:/GitHub/ECDC-COVID-19-Vaccines")

uptake_first_dose <- read_csv("europe_first_dose_final.csv")

# shiny app


ui <- dashboardPage (
  
  
  dashboardHeader(
    title = "COVID-19 Vaccines"
  ), # End of dashboardHeader
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("COVID-19", tabName = "vaccines", icon = icon(name = "syringe"))
    ) # End of sidebarMenu
  ), # End of dashboardSidebar
  
  
  dashboardBody(
    
    tabItems(
      tabItem(tabName = "vaccines",
              fluidRow(
                box(
                  title = "Uptake of first dose of vaccines (Percentage) till 2021/11/28",
                  width = 12,
                  DT::DTOutput("europe_first_dose_final")
                  
                ),
              ), # End of fluidRow
              fluidRow(
                box(
                  title = "Uptake of at Least One Dose in Adults (Percentage)",
                  width = 12,
                  plotOutput("one_dose_adults")
                )
              ) # End of fluidrow
      ) # End of tabItem
    ) # End of tabItems
  ) # End of dashboardBody
  
) # End of dashboardPage
  

server <- function(input, output, session) {
  
  output$europe_first_dose_final <- DT::renderDT({
    uptake_first_dose
    
  })
    
  output$one_dose_adults <- renderPlot({
    
    uptake_first_dose%>%
      filter(!is.na(Adults_over_18))%>%
      mutate(Adults_over_18 = Adults_over_18 / 100)%>%
      mutate(region = if_else(region == "EL", "GR", region))%>%
      mutate(country = fct_reorder(country, Adults_over_18))%>%
      ggplot(aes(x = country, y = Adults_over_18))+
      geom_col(aes(fill = Adults_over_18), width = .3)+
      scale_fill_gradientn(colors = c("red2", "yellow", "skyblue1"), breaks = c("0.2", "0.75", "0.90", "1"), guide = F)+
      scale_y_continuous(labels = percent_format(prefix = "", suffix = "%"))+
      geom_text(aes(label = round((Adults_over_18 * 100), 1), y = Adults_over_18 + 0.03), size = 4)+
      geom_flag(aes(country = str_to_lower(region)))+
      scale_country(guide = F)+
      coord_flip()+
      labs(x = "", 
           y = "",
           title = "till 2021/11/28",
           legend.position = "none")+
      theme_minimal()
    
  }) 
    
  
  
  
} # End of server

shinyApp(ui, server)
  
