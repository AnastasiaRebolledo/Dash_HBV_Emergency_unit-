
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
                       epochs = 1500,
                       activation = "Tanh",
                       seed = 23123)

predicción<-h2o.predict(dl,valid_h2o)
predicción<-as.data.frame(predicción)
compara<-data.frame(predicción,valid$`data$demanda`)

df <- data.frame(matrix(nrow = 1169, ncol = 1)) 
colnames(df) = c("predict")
predicciónframe<-rbind(df,predicción)
MSE<-dl@model$training_metrics@metrics$MSE
mae<-dl@model$training_metrics@metrics$mae

gbm<-h2o.gbm(x = 3:29,
             y = "data$demanda",
             training_frame = train_h2o,
             ntrees = 1000,
             learn_rate = 0.01,
             min_rows = 30)

MSE2<-gbm@model$training_metrics@metrics$MSE
mae2<-gbm@model$training_metrics@metrics$mae

gbm_params<-list(ntrees=c(50,100,150,200,250,300,350,400,450,500),
                 learn_rate=c(0.01,0.005,0.001),
                 min_rows=c(50,100,150))

criteria<-list(strategy='RandomDiscrete',max_models=100)

gbm_grid<-h2o.grid("gbm",
                   x = 3:29,
                   y = "data$demanda",
                   training_frame = train_h2o,
                   hyper_params = gbm_params,
                   search_criteria = criteria)
listado_gbm<-h2o.getGrid(grid_id = "Grid_GBM_train_sid_aa9e_5_model_R_1675282783287_5",sort_by = "MSE",decreasing = FALSE)
mejor_gbm<-h2o.getModel(listado_gbm@model_ids[[1]])

gbm_params_est3<-list(learn_rate=c(0.001,0.005,0.007,0.01,0.05,0.07,0.1,0.5,0.7),
                      sample_rate = c(0.7, 0.8, 1),
                      col_sample_rate = c(0.3, 0.7, 0.8, 1),
                      ntrees=c(30,40,50,60,70,100,150,200,250,500,550,600,650,700,1000),
                      max_depth=c(5,10,15,20,50,55,60,65,70,100,150,200,250,300,500,550,600,650,700,1000),
                      min_rows=c(1,5,10,15,20),
                      seed=c(1,10,50,100,200,500,700,1000,3000,5000,7000,10000),
                      learn_rate_annealing=c(0.99,0.989,0.985,0.983,0.98,0.94,0.92,0.89))
