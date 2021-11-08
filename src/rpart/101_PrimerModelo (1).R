#Arbol elemental con libreria  rpart
require("data.table")
require("rpart")

#Aqui se debe poner la carpeta de la computadora local
setwd("M:\\")  #Establezco el Working Directory

#cargo los datos de 202009 que es donde voy a ENTRENAR el modelo
dtrain  <- fread("./datasetsOri/paquete_premium_202009.csv")

#genero el modelo
modelo  <- rpart("clase_ternaria ~ .",
                 data = dtrain,
                 xval=0,
                 cp=        -0.3, 
                 minsplit=  80,
                 minbucket=  1,
                 maxdepth=   8 )


#aplico al modelo  a los datos de 202011

#cargo los datos de 202011, que es donde voy a APLICAR el modelo
dapply  <- fread("./datasetsOri/paquete_premium_202011.csv")

prediccion  <- predict( modelo, dapply , type = "prob") #aplico el modelo

#prediccion es una matriz con TRES columnas, llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
#cada columna es el vector de probabilidades 

dapply[ , prob_baja2 := prediccion[, "BAJA+2"] ]
dapply[ , Predicted  := as.numeric(prob_baja2 > 0.025) ]

entrega  <- dapply[   , list(numero_de_cliente, Predicted) ] #genero la salida

#genero el archivo para Kaggle
fwrite( entrega, file="./kaggle/K101_001.csv", sep="," )
