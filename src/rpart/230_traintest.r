rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

require("data.table")
require("rpart")


#Aqui se debe poner la carpeta de la computadora local
setwd("M:\\")  #Establezco el Working Directory

#cargo los datos
dataset  <- fread("./datasetsOri/paquete_premium_202009.csv")

ksemilla  <- 1021911  #Cambiar por la primer semilla de cada uno !
#divido en training/testing
set.seed( ksemilla )
fold  <- ifelse( runif( nrow(dataset) ) <  0.7, 1, 2 )

#genero el modelo
modelo  <- rpart("clase_ternaria ~ .",
                 data= dataset[ fold==1], #1 es training
                 xval= 0,
                 cp= -1,
                 maxdepth= 6 )


prediccion  <- predict( modelo, dataset[ fold==2] , type= "prob") #aplico el modelo

#prediccion es una matriz con TRES columnas, llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
#cada columna es el vector de probabilidades 

dataset[  , ganancia :=  ifelse( clase_ternaria=="BAJA+2", 48750, -1250 ) ]

dataset[ fold==2 , prob_baja2 := prediccion[, "BAJA+2"] ]
ganancia_test  <- dataset[ fold==2 & prob_baja2 > 0.025, sum(ganancia) ]
ganancia_test_normalizada  <-  ganancia_test / 0.3

ganancia_test_normalizada
