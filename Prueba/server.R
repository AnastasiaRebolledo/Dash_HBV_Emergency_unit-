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
  
  
  
    
# menu 1 ####
    output$grafico_general<-renderHighchart({
      
      highchart(type = "stock") %>%
            hc_add_series(name=i18n$t("Demanda"),data, type = "line",
                      hcaes(x = x1, y = demanda))  %>% hc_xAxis(type="datetime")
      })
    
    
    output$media_1<-renderText({
          mean(data$demanda[1:1436])
      })
    
    output$desvest_1<-renderText({
          sd(data$demanda[1:1436])
      })
    
    output$asi_1<-renderText({
      skewness(data$demanda[1:1436])
    })
    

    output$histograma_principal<-renderHighchart({
          hchart(density(data$demanda[1:1436]), type = "area", name = i18n$t("Demanda"))
      })
    
    
    output$grafico_circular_1<-renderHighchart({ 
          causa$X1<-i18n$t(c("TOTAL CAUSAS SISTEMA RESPIRATORIO", "Covid-19", "TOTAL CAUSAS SISTEMA CIRCULATORIO", "TOTAL TRAUMATISMOS Y ENVENENAMIENTO", "TOTAL DEMÁS CAUSAS"))
          causa %>% hchart("pie",hcaes(x=X1,y=X6),name="Causas")
      })  
    
    
    output$grafico_circular_2<-renderHighchart({ 
          edad$X1<-i18n$t(c("Niños menores de 1 año","Niños de 1 a 4 años","Niños de 5 a 14 años","Adultos de 15 a 64 años","Adultos de 65 y más años"))
          edad %>% hchart("pie",hcaes(x=X1,y=X6),name="Causas")
      })  
    
    
    output$grafico_principal<-renderHighchart({
    
      cols <- viridis(4)
      cols <- substr(cols, 0, 7)
      
#menu 2 ####   
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
      causas_por_año$Tipo.de.causa<-i18n$t(c("TOTAL CAUSAS SISTEMA RESPIRATORIO", "Covid-19", "TOTAL CAUSAS SISTEMA CIRCULATORIO", "TOTAL TRAUMATISMOS Y ENVENENAMIENTO", "TOTAL DEMÁS CAUSAS"))
      causas_por_año %>% hchart("column", hcaes(x = Año, y = "Cantidad", group = "Tipo.de.causa")) %>%
      hc_xAxis(title = list(text = i18n$t("Año"))) %>% hc_yAxis(title = list(text = i18n$t("Cantidad")))
    })
    
    output$grafico_barras_edad<-renderHighchart({ 
      edad_por_año$Edad<-i18n$t(c("Niños menores de 1 año","Niños de 1 a 4 años","Niños de 5 a 14 años","Adultos de 15 a 64 años","Adultos de 65 y más años"))
      edad_por_año %>% hchart("column", hcaes(x = "Año", y = "Cantidad", group = "Edad")) %>%
      hc_xAxis(title = list(text = i18n$t("Año"))) %>% hc_yAxis(title = list(text = i18n$t("Cantidad")))

    })

    
# menu 3 ####    
    output$grafico_covid<-renderHighchart({
      
      highchart(type = "stock") %>%
        hc_add_series(name=i18n$t("Demanda"),data, type = "line",
                      hcaes(x = x1, y = demanda))  %>% hc_xAxis(type="datetime")%>%
        hc_add_series(name=i18n$t("Demanda casos covid"),covid, type = "line", 
                      hcaes(x = x1, y = covid))  %>% hc_xAxis(type="datetime") 
        
    })
    
    output$grafico_circular_3<-renderHighchart({ 
      tabla$X1<-i18n$t(c("Niños menores de 1 año","Niños de 1 a 4 años","Niños de 5 a 14 años","Adultos de 15 a 64 años","Adultos de 65 y más años"))
      tabla %>% hchart("pie",hcaes(x=X1,y=X2),name="Edades")
    }) 
    
    output$histogramas_covid_edad<-renderHighchart({
      
    hchart(density(covid_por_edad2$X1), type = "area", 
             name = i18n$t("Niños menores de 1 año")) %>%
        hc_add_series(density(covid_por_edad2$X2), type = "area",
                      name = i18n$t("Niños de 1 a 4 años")) %>%
        hc_add_series(density(covid_por_edad2$X3), type = "area",
                      name = i18n$t("Niños de 5 a 14 años")) %>%
        hc_add_series(density(covid_por_edad2$X4), type = "area",
                      name = i18n$t("Adultos de 15 a 64 años")) %>%
        hc_add_series(density(covid_por_edad2$X5), type = "area",
                      name = i18n$t("Adultos de 65 y más años"))
      
    })

# Menu 4 ###   
    output$grafico_predicción<-renderHighchart({
      
      highchart(type = "stock") %>%
        hc_add_series(name=i18n$t("Demanda"),data, type = "line",
                      hcaes(x = x1, y = demanda)) %>%
        hc_add_series(name=i18n$t("Predicción"),data, type = "line",
                      hcaes(x = x1, y = predict)) %>% hc_xAxis(type="datetime")
    })   
    
    output$MSE<-renderText({MSE})
    
    output$RMSE<-renderText({RMSE})
    
    output$mae<-renderText({mae})
    
    output$grafico_predicción_gbm<-renderHighchart({
      
      highchart(type = "stock") %>%
        hc_add_series(name=i18n$t("Demanda"),data, type = "line",
                      hcaes(x = x1, y = demanda)) %>%
        hc_add_series(name=i18n$t("Predicción"),data, type = "line",
                      hcaes(x = x1, y = predict_gbm)) %>% hc_xAxis(type="datetime")
    })   
    
    output$MSE2<-renderText({MSE2})
    
    output$RMSE2<-renderText({RMSE2})
    
    output$mae2<-renderText({mae2})
    
    output$grafico_predicción_gbm_eneyfeb<-renderHighchart({
      
      highchart(type = "stock") %>%
        hc_add_series(name=i18n$t("Demanda"),data2, type = "line",
                      hcaes(x = x1, y = demanda)) %>%
        hc_add_series(name=i18n$t("Predicción"),predicciónframe3, type = "line",
                      hcaes(x = x5, y = predict)) %>% hc_xAxis(type="datetime")
    })   
    
    ####### Listas de espera #######
    
    output$grafico_causas_muerte_2020<-renderHighchart({
      
      causas_muerte$Causas.de.muerte.2020<-i18n$t(c("Tumores","Enfermedades del sistema circulatorio","Enfermedades el sistema digestivo",
                                                    "Traumatismo,envenenamientos","Enfermedades del sistema respiratorio",
                                                    "Enfermedades del sistema nervioso central","Codigos para propositos especiales",
                                                    "Enf. Endocrinas,nutricionales y metabolicas","Trastornos mentales y del comportamiento",
                                                    "Sintomas,signos mal definidos","Enfermedades del sistema genitourinario",
                                                    "Ciertas enfermedades infecciosas y parasitarias",
                                                    "Enfermedades de la piel y del tejido subcutaneo","Otras"))
      highchart() %>%
        hc_chart(type = 'column') %>%
        hc_xAxis(categories = causas_muerte$Causas.de.muerte.2020) %>%
        hc_add_series(causas_muerte$Num, name = "Causas de muerte",dataLabels = list(enabled = TRUE)) %>%
        hc_exporting(enabled = TRUE) %>%
        hc_plotOptions(series = list(animation = FALSE))
      
            })  
    
    output$ind_ocu_2020<-renderText({scales::percent(indice_ocupacional_2020$X10,accuracy = 0.01)})
    output$prom_dias_cama_2020<-renderText({promedio_dias_cama_2020$X10})
    output$prom_dias_est_2020<-renderText({promedio_dias_estadia_2020$X10})
})
