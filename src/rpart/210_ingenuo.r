#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

require("data.table")
require("rpart")

#Aqui se debe poner la carpeta de la computadora local
setwd("M:\\")  #Establezco el Working Directory

#cargo los datos
dtrain  <- fread("./datasetsOri/paquete_premium_202009.csv")
dapply  <- fread("./datasetsOri/paquete_premium_202011.csv")


for( profundidad  in  c(2,3,4,5,6,7,8,10,12,14,16,18,20,22,24,26,28,30) )
{
  #genero el modelo
  modelo  <- rpart("clase_ternaria ~ .",
                   data= dtrain,
                   xval= 0,
                   cp= -1,
                   maxdepth= profundidad )

  prediccion  <- predict( modelo, dapply , type= "prob") #aplico el modelo

  #prediccion es una matriz con TRES columnas, llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
  #cada columna es el vector de probabilidades 

  dapply[ , prob_baja2 := prediccion[, "BAJA+2"] ]
  dapply[ , Predicted  := as.numeric(prob_baja2 > 0.025) ]

  entrega  <- dapply[  , list(numero_de_cliente, Predicted) ] #genero la salida

  #genero el archivo para Kaggle
  fwrite( entrega, 
          file= paste0( "./kaggle/K110_h",  profundidad, ".csv"), 
          sep= "," )
}
