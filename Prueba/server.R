#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  load("datos_app.RData")
  
    
  
    output$grafico_general<-renderHighchart({
      
      highchart() %>%
        hc_add_series(name=i18n$t("Demanda"),data, type = "line",
                      hcaes(x = x1, y = demanda))  %>% hc_xAxis(type="datetime")
      })
    
    
    output$media_1<-renderText({
          mean(data$demanda[1:1436])
      })
    
    
    output$desvest_1<-renderText({
          sd(data$demanda[1:1436])
      })
    
    
    output$histograma_principal<-renderHighchart({
          hchart(density(data$demanda[1:1436]), type = "area", name = i18n$t("Demanda"))
      })
    
    
    output$grafico_circular_1<-renderHighchart({ 
          causa %>% hchart("pie",hcaes(x=X1,y=X6),name="Causas")
      })  
    
    
    output$grafico_circular_2<-renderHighchart({ 
          edad %>% hchart("pie",hcaes(x=X1,y=X6),name="Causas")
      })  
    
    
    output$grafico_principal<-renderHighchart({
    
      cols <- viridis(4)
      cols <- substr(cols, 0, 7)
      
      
      #Aca cambie la forma de realizar el grafico pero con la misma libreria highcharter
       highchart() %>%
        hc_add_series(name="2019",data_por_año, type = "line",
                      hcaes(x = x2, y = y1)) %>%
         hc_add_series(name="2020",data_por_año, type = "line",
                       hcaes(x = x2, y = y2_2)) %>%
         hc_add_series(name="2021",data_por_año, type = "line",
                       hcaes(x = x2, y = y3)) %>%
         hc_add_series(name="2022",data_por_año, type = "line",
                       hcaes(x = x2, y = y4)) %>% hc_xAxis(type="datetime") %>%
         hc_colors(cols) 
        
    })
    
    output$histogramas_principales<-renderHighchart({

      cols <- viridis(4)
      cols <- substr(cols, 0, 7)
      
      #Aca deberiamos hacer los histogramas
      hchart(density(data_por_año$y1), type = "area", 
                    name = "2019") %>%
      hc_add_series(density(data_por_año$y2_2), type = "area",
                    name = "2020") %>%
      hc_add_series(density(data_por_año$y3), type = "area",
                      name = "2021") %>%
      hc_add_series(density(data_por_año$y4[1:340]), type = "area",
                      name = "2022") %>%
      hc_colors(cols)
      
      
    })

    
    output$grafico_barras_causa<-renderHighchart({ 
      causas_por_año %>% hchart("column", hcaes(x = "Año", y = "Cantidad", group = "Tipo.de.causa"))
    })
    
    output$grafico_barras_edad<-renderHighchart({ 
      edad_por_año %>% hchart("column", hcaes(x = "Año", y = "Cantidad", group = "Edad"))
    })
    
    output$grafico_covid<-renderHighchart({
      
      highchart() %>%
        hc_add_series(name="demanda",covid, type = "line",
                      hcaes(x = x1, y = covid))  %>% hc_xAxis(type="datetime")
    })
    
    output$grafico_circular_3<-renderHighchart({ 
      tabla %>% hchart("pie",hcaes(x=X1,y=X2),name="Edades")
    }) 
    
    output$histogramas_covid_edad<-renderHighchart({
      
    hchart(density(covid_por_edad2$X1), type = "area", 
                      name = "Niños menores de 1 año") %>%
        hc_add_series(density(covid_por_edad2$X2), type = "area",
                      name = "Niños de 1 a 4 años") %>%
        hc_add_series(density(covid_por_edad2$X3), type = "area",
                      name = "Niños de 5 a 14 años") %>%
        hc_add_series(density(covid_por_edad2$X4), type = "area",
                      name = "Adultos de 15 a 64 años") %>%
        hc_add_series(density(covid_por_edad2$X5), type = "area",
                      name = "Adultos de 65 y más años")
      
    })
})
