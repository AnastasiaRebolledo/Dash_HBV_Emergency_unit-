x1<-seq(as.Date("2019-01-01"),as.Date("2022-12-31"),"day")
x2<-seq(as.Date("2022-01-01"),as.Date("2022-12-31"),"day")

y1<-read.xlsx(file="Urgencia2019.xlsx",sheetIndex = 1, rowIndex = 19, colIndex= 2:366
              , as.data.frame = TRUE, header = FALSE)
y2<-read.xlsx(file="Urgencia2020.xlsx",sheetIndex = 1, rowIndex = 19, colIndex= 2:367
              , as.data.frame = TRUE, header = FALSE)
y2_2<-read.xlsx(file="Urgencia2020(2).xlsx",sheetIndex = 1, rowIndex = 19, colIndex= 2:366
                    , as.data.frame = TRUE, header = FALSE)
y3<-read.xlsx(file="Urgencia2021.xlsx",sheetIndex = 1, rowIndex = 19, colIndex= 2:366
              , as.data.frame = TRUE, header = FALSE)
y4<-read.xlsx(file="Urgencia2022.xlsx",sheetIndex = 1, rowIndex = 19, colIndex= 2:366
              , as.data.frame = TRUE, header = FALSE)

y1<-t(y1)
y2<-t(y2)
y2_2<-t(y2_2)
y3<-t(y3)
y4<-t(y4)



### Set de datos para serie de tiempo ####

demanda<-rbind(y1,y2,y3,y4)
data<-data.frame(x1,demanda)

### set de datos para causas y años ####

causa<-read.xlsx(file="UrgenciaPorCausayEdad.xlsx",sheetIndex = 1, rowIndex = 4:8, colIndex= 1:6
                 , as.data.frame = TRUE, header = FALSE)
causas_por_año<-read.xlsx(file="UrgenciaPorCausayEdad.xlsx",sheetIndex = 2, rowIndex = 1:21, colIndex= 1:3
                 , as.data.frame = TRUE, header = TRUE)

edad<-read.xlsx(file="UrgenciaPorCausayEdad.xlsx",sheetIndex = 3, rowIndex = 4:8, colIndex= 1:6
                , as.data.frame = TRUE, header = FALSE)
edad_por_año<-read.xlsx(file="UrgenciaPorCausayEdad.xlsx",sheetIndex = 4, rowIndex = 1:21, colIndex= 1:3
                , as.data.frame = TRUE, header = TRUE)

### set de datos para comparar años ####
data_por_año<-data.frame(x2,y1,y2_2,y3,y4)

### carga de datos urgencias por covid 19 ####
y6<-read.xlsx(file="Urgencia2019.xlsx",sheetIndex = 2, rowIndex = 4, colIndex= 2:366
              , as.data.frame = TRUE, header = FALSE)
y7<-read.xlsx(file="Urgencia2020.xlsx",sheetIndex = 2, rowIndex = 4, colIndex= 2:367
              , as.data.frame = TRUE, header = FALSE)
y8<-read.xlsx(file="Urgencia2021.xlsx",sheetIndex = 2, rowIndex = 4, colIndex= 2:366
              , as.data.frame = TRUE, header = FALSE)
y9<-read.xlsx(file="Urgencia2022.xlsx",sheetIndex = 2, rowIndex = 4, colIndex= 2:366
              , as.data.frame = TRUE, header = FALSE)

### set de datos urgencia covid 19 ####
y6<-t(y6)
y7<-t(y7)
y8<-t(y8)
y9<-t(y9)

covid<-rbind(y6,y7,y8,y9)
covid<-data.frame(x1,covid)

### carga datos urgencia covid por edad ####
covid_2020<-read.xlsx(file="Urgencia2020(2).xlsx",sheetIndex = 1, rowIndex = c(68,69,109,110,150,151,191,192,232,233), colIndex= 2:366
              , as.data.frame = TRUE, header = FALSE)

covid_2021<-read.xlsx(file="Urgencia2021.xlsx",sheetIndex = 1, rowIndex = c(68,69,109,110,150,151,191,192,232,233), colIndex= 2:365
                       , as.data.frame = TRUE, header = FALSE)
 
covid_2022<-read.xlsx(file="Urgencia2022.xlsx",sheetIndex = 1, rowIndex = c(68,69,109,110,150,151,191,192,232,233), colIndex= 2:341
                       , as.data.frame = TRUE, header = FALSE)

covid_por_edad<-cbind(covid_2020,covid_2021,covid_2022)

covid_por_edad<-rowSums(covid_por_edad)

tabla<-matrix(data = 0,ncol = 2,nrow = 5)
tabla<-data.frame(tabla)
tabla[1,1]<-"Niños menores de 1 año"
tabla[2,1]<-"Niños de 1 a 4 años"
tabla[3,1]<-"Niños de 5 a 14 años"
tabla[4,1]<-"Adultos de 15 a 64 años"
tabla[5,1]<-"Adultos de 65 y más años"

tabla[1,2]<-sum(covid_por_edad[1:2])
tabla[2,2]<-sum(covid_por_edad[3:4])
tabla[3,2]<-sum(covid_por_edad[5:6])
tabla[4,2]<-sum(covid_por_edad[7:8])
tabla[5,2]<-sum(covid_por_edad[9:10])

