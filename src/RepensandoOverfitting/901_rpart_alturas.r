#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rpart")

setwd("M:" )  #establezco la carpeta donde voy a trabajar
#cargo el dataset
dataset_entrenar  <- fread( "./datasetsOri/paquete_premium_202011.csv")

#dejo la clase binaria
dataset_entrenar[ , clase_binaria:= ifelse( clase_ternaria=="BAJA+2", "POS","NEG" ) ]
dataset_entrenar[ , clase_ternaria:= NULL ]

#cargo los datos donde aplico el modelo
dataset_aplicar  <- fread( "./datasetsOri/paquete_premium_202101.csv")


for( vmaxdepth  in 4:25 )
{

  #genero el modelo
  modelo  <- rpart(formula= "clase_binaria ~ . -mactivos_margen -mpasivos_margen",  
                   data= dataset_entrenar, 
                   model= TRUE, #quiero que me devuelva el modelo
                   xval= 0,
                   cp= 0,
                   minsplit= 5,
                   maxdepth=  vmaxdepth
                  )

  #aplico el modelo a los datos en donde entrene
  prediccion_202011  <- predict( modelo, dataset_entrenar, type = "prob")
  ganancia_202011 <-  sum(  (prediccion_202011[, "POS"] > 0.025) *
                            ifelse( dataset_entrenar$clase_binaria=="POS", 48750, -1250 ) )
                            
  cat( vmaxdepth, "\t", ganancia_202011, "\n" )

  prediccion_202101  <- predict( modelo, dataset_aplicar, type = "prob")

  prob_pos  <- prediccion_202101[, "POS"]
  estimulo  <- as.numeric(prob_pos > 0.025)

  entrega <-  as.data.table( list(  "numero_de_cliente"= dataset_aplicar$numero_de_cliente,
                                    "Predicted"=  estimulo ) )

  #genero el archivo para Kaggle
  fwrite( entrega, 
          file= paste0("./kaggle/altura_", vmaxdepth, ".csv"))

}


