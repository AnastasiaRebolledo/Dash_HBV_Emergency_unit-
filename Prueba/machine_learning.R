
## fila 1 a 1169 corresponde a entrenamiento, 1170 hasta 1461 validación

#install.packages("h2o")
#install.packages("timetk")
#install.packages("dplyr")

library(h2o)
library(timetk)
library(dplyr)

#transformacion del mes segun el timetk
demanda.diaria<-data%>%tk_augment_timeseries_signature()

#limpiar los NA
demanda.diaria<-demanda.diaria%>%
  select_if(~!any(is.na(.)))%>%
  mutate_if(is.ordered,~as.character(.)%>%as.factor)

demanda.diaria<-cbind(data$demanda,demanda.diaria)
#colnames(demanda.diaria$`data$demanda`)<-c("demanda")

#generacion del set de datos train y valid
train<-demanda.diaria[1:1169,]
valid<-demanda.diaria[1170:1461,]

h2o.init(max_mem_size =  "1g",min_mem_size = "1g")

#transformar el data.frame en un formato h2o
train_h2o<-as.h2o(train)
valid_h2o<-as.h2o(valid)

dl <- h2o.deeplearning(x = 3:29,
                       y = "data$demanda",
                       training_frame = train_h2o,
                       hidden = c(200,200),
                       epochs = 1000,
                       activation = "Tanh",
                       seed = 23123)

predicción<-h2o.predict(dl,valid_h2o)
predicción<-as.data.frame(predicción)
compara<-data.frame(predicción,valid$`data$demanda`)

df <- data.frame(matrix(nrow = 1169, ncol = 1)) 
colnames(df) = c("predict")
predicciónframe<-rbind(df,predicción)