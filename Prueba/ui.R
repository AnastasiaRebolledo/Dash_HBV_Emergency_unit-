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
#library(openxlsx)
library(highcharter)
library(dplyr)
library(viridisLite)
library(moments)
library(shinycssloaders)
#source("carga.R")
#library(shiny.worker)
library(promises)
library(future)
#library(shiny.fluent)
library(shiny.i18n)
library(scales)
source("traductor.R")


# Define UI for application that draws a histogram
dashboardPage(
  dashboardHeader(title = i18n$t("Dash Unidad de Urgencia HBV"),titleWidth = 350),
  dashboardSidebar(width = 350,skin = "light",elevation = 2,status = "primary",
                   sidebarMenu(
                     id = "sidebar",
                     menuItem(i18n$t("Menu Principal"),tabName = "menu1",icon=icon("laptop-medical"),
                                         menuSubItem(i18n$t("Menu Principal"),tabName="menu1_1",
                                         icon=icon("chart-simple"),
                                         selected = TRUE),
                                         menuSubItem(i18n$t("Predicción demanda"),tabName="menu1_2",
                                icon=icon("chart-line"),
                                selected = FALSE)),
                     menuItem(i18n$t("Estadisticas anuales"),tabName="menu2",
                              icon=icon("hospital"),
                              selected = FALSE),
                    menuItem(i18n$t("Urgencias covid"),tabName="menu3",
                              icon=icon("heart-pulse"),
                              selected = FALSE),
                    menuItem(i18n$t("Listas de espera"),tabName="menu4",
                             icon=icon("bed-pulse"),
                             selected = FALSE)
                    
                    # menuItem(i18n$t("Predicción demanda"),tabName="menu4",
                    #          icon=icon("layer-group"),
                    #          selected = FALSE)
                    )),
  dashboardBody(
    tabItems(
      tabItem(tabName = "menu1_1",
    
    # Boxes need to be put in a row (or column)
    fluidRow(width=12,box(width = 10,title = i18n$t("Demanda de atenciones de urgencia por día"),closable = FALSE,elevation = 2,
                          withSpinner(highchartOutput("grafico_general",height = "300px")),
                          status = "primary",headerBorder = FALSE,collapsible = FALSE),
             column(width = 2,
                    valueBox(width = 12,subtitle = i18n$t("Media"),value = textOutput("media_1"),color = "primary",icon = icon("check")),
                    valueBox(width = 12,subtitle = i18n$t("Desviación estándar"),value = textOutput("desvest_1"),color = "info",icon = icon("check")),
                    valueBox(width = 12,subtitle = i18n$t("Asimetría"),value = textOutput("asi_1"),color = "success",icon = icon("check"))
             )),
    
    
    fluidRow(width=12,box(width = 4,title = i18n$t("Histograma de atenciones diarias de urgencias"),closable = FALSE,elevation = 2,withSpinner(highchartOutput("histograma_principal",height = "300px")),
                          status = "secondary",headerBorder = FALSE,collapsible = FALSE),
                      box(width = 4,title = i18n$t("Atenciones de urgencia por causa"),closable = FALSE,elevation = 2,withSpinner(highchartOutput("grafico_circular_1",height = "300px")),
                          status = "secondary",headerBorder = FALSE,collapsible = FALSE),
                      box(width = 4,title = i18n$t("Atenciones de urgencia por edad"),closable = FALSE,elevation = 2,withSpinner(highchartOutput("grafico_circular_2",height = "300px")),
                          status = "secondary",headerBorder = FALSE,collapsible = FALSE))
   
  ),
  
  tabItem(tabName = "menu1_2",
          
                  fluidRow(width=12,box(width = 10,title = i18n$t("Demanda diaria real vs predicción red neuronal"),closable = FALSE,elevation = 2,
                                        withSpinner(highchartOutput("grafico_predicción",height = "300px")),
                                        status = "primary",headerBorder = FALSE,collapsible = FALSE),
                           column(width = 2,
                                  valueBox(width = 12,subtitle = i18n$t("Error Cuadrático Medio"),value = textOutput("MSE"),color = "primary",icon = icon("check")),
                                  valueBox(width = 12,subtitle = i18n$t("Raíz del Error Cuadrático Medio"),value = textOutput("RMSE"),color = "info",icon = icon("check")),
                                  valueBox(width = 12,subtitle = i18n$t("Error Absoluto Medio"),value = textOutput("mae"),color = "success",icon = icon("check"))
                           )),
                  fluidRow(width=12,box(width = 10,title = i18n$t("Demanda diaria real vs predicción gbm"),closable = FALSE,elevation = 2,
                                        withSpinner(highchartOutput("grafico_predicción_gbm",height = "300px")),
                                        status = "primary",headerBorder = FALSE,collapsible = FALSE),
                           column(width = 2,
                                  valueBox(width = 12,subtitle = i18n$t("Error Cuadrático Medio"),value = textOutput("MSE2"),color = "primary",icon = icon("check")),
                                  valueBox(width = 12,subtitle = i18n$t("Raíz del Error Cuadrático Medio"),value = textOutput("RMSE2"),color = "info",icon = icon("check")),
                                  valueBox(width = 12,subtitle = i18n$t("Error Absoluto Medio"),value = textOutput("mae2"),color = "success",icon = icon("check"))
                           ))),
          # fluidRow(width=12,box(width = 12,title = i18n$t("Demanda diaria real vs predicción"),closable = FALSE,elevation = 2,
          #                       withSpinner(highchartOutput("grafico_predicción",height = "300px")),
          #                       status = "primary",headerBorder = FALSE,collapsible = FALSE))
  
      tabItem(tabName = "menu2",
              fluidRow(width=12,box(width = 12,title = i18n$t("Demanda de atenciones de urgencia por día"),closable = FALSE,elevation = 2,
                                    withSpinner(highchartOutput("grafico_principal",height = "300px")),
                                    status = "info",headerBorder = FALSE,collapsible = FALSE)),
    
    fluidRow(width=12,
             box(width = 12,title = i18n$t("Histogramas de atenciones diarias de urgencias"),closable = FALSE,elevation = 2,withSpinner(highchartOutput("histogramas_principales",height = "300px")),
                 status = "info",headerBorder = FALSE,collapsible = FALSE)),
    

    fluidRow(width=12,
             box(width = 12,title = i18n$t("Atenciones de urgencia por causa"),closable = FALSE,elevation = 2,withSpinner(highchartOutput("grafico_barras_causa")),
                 status = "info",headerBorder = FALSE,collapsible = FALSE)
      ),
    
    fluidRow(width=12,
            box(width = 12,title = i18n$t("Atenciones de urgencia por edad"),closable = FALSE,elevation = 2,withSpinner(highchartOutput("grafico_barras_edad")),
                status = "info",headerBorder = FALSE,collapsible = FALSE)
      )
    ),

  
  tabItem(tabName = "menu3",
          fluidRow(width=12,box(width = 12,title = i18n$t("Demanda diaria casos con Covid-19"),closable = FALSE,elevation = 2,
                                withSpinner(highchartOutput("grafico_covid",height = "300px")),
                                status = "primary",headerBorder = FALSE,collapsible = FALSE)),
          fluidRow(width=12,box(width = 6,title = i18n$t("Atenciones de urgencia de casos con Covid-19 por edad"),closable = FALSE,elevation = 2,withSpinner(highchartOutput("grafico_circular_3",height = "300px")),
                       status = "secondary",headerBorder = FALSE,collapsible = FALSE),
                            box(width = 6,title = i18n$t("Histogramas de atenciones de urgencia casos covid-19 por edad"),closable = FALSE,elevation = 2,withSpinner(highchartOutput("histogramas_covid_edad",height = "300px")),
                                status = "secondary",headerBorder = FALSE,collapsible = FALSE))
          ),

  tabItem(tabName = "menu4",
          fluidRow(width=12,valueBox(width = 3,subtitle = i18n$t("Indice ocupacional 2020"),value = h2(textOutput("ind_ocu_2020")),color = "primary",icon = icon("check")),
                            valueBox(width = 3,subtitle = i18n$t("Promedio dias cama 2020"),value = h2(textOutput("prom_dias_cama_2020")),color = "info",icon = icon("check")),
                            valueBox(width = 3,subtitle = i18n$t("Promedio dias estadia 2020"),value = h2(textOutput("prom_dias_est_2020")),color = "success",icon = icon("check")),
                            box(width = 3,closable = FALSE,elevation = 2,headerBorder = FALSE,collapsible = FALSE)),
          fluidRow(width=12,box(width = 12,closable = FALSE,elevation = 2,headerBorder = FALSE,collapsible = FALSE,
                                withSpinner(highchartOutput("grafico_causas_muerte_2020",height = "500px")))))
  
  )
  
))