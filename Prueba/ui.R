#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
#library(shinydashboard)
#library(shinydashboardPlus)
library(bs4Dash)
library(rJava)
library(xlsx)
library(highcharter)
library(dplyr)
library(viridisLite)
library(moments)
library(shinycssloaders)
#source("carga.R")


# Define UI for application that draws a histogram
dashboardPage(
  dashboardHeader(title = "Unidad de Urgencia HBV",titleWidth = 250),
  dashboardSidebar(width = 250,skin = "light",elevation = 2,status = "primary",
                   sidebarMenu(
                     id = "sidebar",
                     menuItem("Menu Principal",tabName="menu1",
                              icon=icon("layer-group"),
                              selected = TRUE),
                     menuItem("Segundo menu",tabName="menu2",
                              icon=icon("layer-group"),
                              selected = FALSE),
                    menuItem("Urgencias covid",tabName="menu3",
                              icon=icon("layer-group"),
                              selected = FALSE))),
  dashboardBody(
    tabItems(
      tabItem(tabName = "menu1",
    
    # Boxes need to be put in a row (or column)
    fluidRow(width=12,box(width = 10,title = "Grafico General",closable = FALSE,elevation = 2,
                          withSpinner(highchartOutput("grafico_general",height = "300px")),
                          status = "primary",headerBorder = FALSE,collapsible = FALSE),
             column(width = 2,
                    valueBox(width = 12,subtitle = "Media",value = textOutput("media_1"),color = "primary",icon = icon("check")),
                    valueBox(width = 12,subtitle = "Desviación estándar",value = textOutput("desvest_1"),color = "info",icon = icon("check")),
                    valueBox(width = 12,subtitle = "Asimetria",value = textOutput("asi_1"),color = "success",icon = icon("check"))
             )),
    
    fluidRow(width=12,box(width = 4,title = "Histograma",closable = FALSE,elevation = 2,withSpinner(highchartOutput("histograma_principal",height = "300px")),
                          status = "secondary",headerBorder = FALSE,collapsible = FALSE),
                      box(width = 4,title = "Atenciones de urgencia por causa",closable = FALSE,elevation = 2,withSpinner(highchartOutput("grafico_circular_1",height = "300px")),
                          status = "secondary",headerBorder = FALSE,collapsible = FALSE),
                      box(width = 4,title = "Atenciones de urgencia por edad",closable = FALSE,elevation = 2,withSpinner(highchartOutput("grafico_circular_2",height = "300px")),
                          status = "secondary",headerBorder = FALSE,collapsible = FALSE))
   
  ),
      tabItem(tabName = "menu2",
              fluidRow(width=12,box(width = 12,title = "Grafico Principal",closable = FALSE,elevation = 2,
                                    withSpinner(highchartOutput("grafico_principal",height = "300px")),
                                    status = "info",headerBorder = FALSE,collapsible = FALSE)),
    
    fluidRow(width=12,
             box(width = 12,title = "Histogramas",closable = FALSE,elevation = 2,withSpinner(highchartOutput("histogramas_principales",height = "300px")),
                 status = "info",headerBorder = FALSE,collapsible = FALSE)),
    

    fluidRow(width=12,
             box(width = 12,title = "Atenciones de urgencia por causa",closable = FALSE,elevation = 2,withSpinner(highchartOutput("grafico_barras_causa")),
                 status = "info",headerBorder = FALSE,collapsible = FALSE)
      ),
    
    fluidRow(width=12,
            box(width = 12,title = "Atenciones de urgencia por edad",closable = FALSE,elevation = 2,withSpinner(highchartOutput("grafico_barras_edad")),
                status = "info",headerBorder = FALSE,collapsible = FALSE)
      )
    ),

  
  tabItem(tabName = "menu3",
          fluidRow(width=12,box(width = 12,title = "Atenciones de urgencia Covid-19",closable = FALSE,elevation = 2,
                                withSpinner(highchartOutput("grafico_covid",height = "300px")),
                                status = "primary",headerBorder = FALSE,collapsible = FALSE)),
          fluidRow(width=12,box(width = 6,title = "Atenciones de urgencia Covid-19 por edad",closable = FALSE,elevation = 2,withSpinner(highchartOutput("grafico_circular_3",height = "300px")),
                       status = "secondary",headerBorder = FALSE,collapsible = FALSE))
          ))
  
))