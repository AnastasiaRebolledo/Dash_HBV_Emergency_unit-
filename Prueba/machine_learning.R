lunes_inicio_sem1 <- as.POSIXct("2022-08-01 08:00:00")
lunes_termino_sem1 <- as.POSIXct("2022-08-01 20:00:00")

lunes_sem1<-data.frame("dias"=seq(from=lunes_inicio_sem1, to=lunes_termino_sem1, by="360 min"))

semana1<-rbind(lunes_sem1)

#install.packages("h2o")
#install.packages("timetk")
#install.packages("dplyr")

library(h2o)
library(timetk)
library(dplyr)

#transformacion del mes segun el timetk
mes<-semana1%>%tk_augment_timeseries_signature()

#limpiar los NA
mes<-mes%>%
  select_if(~!any(is.na(.)))%>%
  mutate_if(is.ordered,~as.character(.)%>%as.factor)

#generacion del set de datos train y valid
train<-mes[1:45,]
valid<-mes[46:60,]

h2o.init(max_mem_size =  "10g",min_mem_size = "1g")

#transformar el data.frame en un formato h2o
train_h2o<-as.h2o(train)
valid_h2o<-as.h2o(valid)

dl <- h2o.deeplearning(x = "variable x",
                       y = "variable y",
                       hidden = c(5,5),
                       epochs = 1000,
                       activation = "Tanh",
                       seed = 23123)

predic_train_bike_rn_est3<-h2o.predict(train_rn_model_bike_est3,valid_h2o)
predic_train_bike_rn_est3<-as.data.frame(predic_train_bike_rn_est3)
