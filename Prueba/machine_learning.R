
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

#colnames(demanda.diaria$`data$demanda`)<-c("demanda")

#generacion del set de datos train y valid ####
train<-demanda.diaria[1:1169,]
valid<-demanda.diaria[1170:1461,]

h2o.init(ip = "localhost",max_mem_size =  "10g",min_mem_size = "1g")

#transformar el data.frame en un formato h2o
train_h2o<-as.h2o(train)
valid_h2o<-as.h2o(valid)

## grid red neuronal ####

#criteria_rd<-list(strategy='RandomDiscrete',max_models=100)

#hiperparametros <- list(hidden = list(c(256,256),c(512, 512), c(256,256,256),c(512,512,512)),
#                        activation=c("Rectifier","Maxout","Tahn"),
#                        epochs=c(200,1500,2000),
#                        rate=c(0.001,0.7),
#                        rate_annealing=c(0.000001,0.0007),
#                        rate_decay=c(0.99,0.98))

#grid_dl <- h2o.grid(algorithm = "deeplearning",
#                    y = "demanda",
#                    x = 3:29,
#                    training_frame = train_h2o,
#                    hyper_params = hiperparametros,
#                    search_criteria = criteria_rd,
#                    seed = 123,
#                    grid_id = "grid_dl")

#resultados_grid <- h2o.getGrid(grid_id = "grid_dl",
#                               sort_by = "MSE",
#                               decreasing = FALSE)

dl <- h2o.deeplearning(x = 3:29,
                       y = "demanda",
                       training_frame = train_h2o,
                       hidden = c(256,256,256),
                       epochs = 3000,
                       activation = "Rectifier",
                       seed = 123,
                       rate = 0.7,
                       rate_annealing = 0.000001,
                       rate_decay = 0.99)

predicción<-h2o.predict(dl,valid_h2o)
predicción<-as.data.frame(predicción)

df <- data.frame(matrix(nrow = 1169, ncol = 1)) 
colnames(df) = c("predict")
predicciónframe<-rbind(df,predicción)
data<-cbind(data,predicciónframe)

MSE<-dl@model$training_metrics@metrics$MSE
RMSE<-dl@model$training_metrics@metrics$RMSE
mae<-dl@model$training_metrics@metrics$mae


## grid gbm ###

#criteria<-list(strategy='RandomDiscrete',max_models=150)

#gbm_params_est3<-list(learn_rate=c(0.001,0.005,0.007,0.01,0.05,0.07,0.1,0.5,0.7),
#                      sample_rate = c(0.7, 0.8, 1),
#                      col_sample_rate = c(0.3, 0.7, 0.8, 1),
#                      ntrees=c(30,40,50,60,70,100,150,200,250,500,550,600,650,700,1000),
#                      max_depth=c(5,10,15,20,50,55,60,65,70,100,150,200,250,300,500,550,600,650,700,1000),
#                      min_rows=c(1,5,10,15,20),
#                      seed=c(1,10,50,100,200,500,700,1000,3000,5000,7000,10000),
#                      learn_rate_annealing=c(0.99,0.989,0.985,0.983,0.98,0.94,0.92,0.89))

# gbm_grid<-h2o.grid("gbm",
#                   x = 3:29,
#                   y = "data$demanda",
#                   training_frame = train_h2o,
#                   hyper_params = gbm_params_est3,
#                   search_criteria = criteria)
#listado_gbm<-h2o.getGrid(grid_id = "Grid_GBM_train_sid_be46_1_model_R_1675282783287_11",sort_by = "MSE",decreasing = FALSE)
#mejor_gbm<-h2o.getModel(listado_gbm@model_ids[[1]])
#predicción_gbm<-as.data.frame(listado_gbm@model_ids[[1]])

#### entrenamiento gbm ####

gbm<-h2o.gbm(x = 3:29,
             y = "demanda",
             training_frame = train_h2o,
             ntrees = 150,
             learn_rate = 0.7,
             min_rows = 1,
             sample_rate = 0.8,
             learn_rate_annealing = 0.983,
             max_depth = 100,
             seed = 200)

MSE2<-gbm@model$training_metrics@metrics$MSE
RMSE2<-gbm@model$training_metrics@metrics$RMSE
mae2<-gbm@model$training_metrics@metrics$mae


predicción2<-h2o.predict(gbm,valid_h2o)
predicción2<-as.data.frame(predicción2)

colnames(df) = c("predict")
predicciónframe2<-rbind(df,predicción2)
colnames(predicciónframe2)=c("predict_gbm")
data<-cbind(data,predicciónframe2)

# predicción enero y febrero ##
x4<-seq(as.Date("2023-01-01"),as.Date("2023-02-28"),"day")
x4<-as.data.frame(x4)
columna <- data.frame(matrix(nrow = 59, ncol = 1)) 
colnames(columna) = c("demanda")
x4<-cbind(x4,columna)
x4<-x4%>%tk_augment_timeseries_signature()

#limpiar los NA
x4<-x4%>%
  select_if(~!any(is.na(.)))%>%
  mutate_if(is.ordered,~as.character(.)%>%as.factor)
h2o_x4<- as.h2o(x4)

predicción3<-h2o.predict(gbm,h2o_x4)
predicción3<-as.data.frame(predicción3)
x5<-seq(as.Date("2019-01-01"),as.Date("2023-02-28"),"day")
x5<-as.data.frame(x5)
dt<-data.frame(matrix(nrow = 1461, ncol = 1))
colnames(dt) = c("predict")
predicciónframe3<-rbind(dt,predicción3)
predicciónframe3<-cbind(x5,predicciónframe3)

gbm2<-h2o.gbm(x = 3:29,
             y = "demanda",
             training_frame = h2o_demandadiaira,
             ntrees = 150,
             learn_rate = 0.7,
             min_rows = 1,
             sample_rate = 0.8,
             learn_rate_annealing = 0.983,
             max_depth = 100,
             seed = 200)



