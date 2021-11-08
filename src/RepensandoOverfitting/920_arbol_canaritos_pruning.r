#limpio la memoria
rm( list=ls() )
gc()

library("data.table")
library("rpart")
library("rpart.plot")


setwd("M:\\" )  #establezco la carpeta donde voy a trabajar


#cargo el dataset
dtrain  <- fread( "./datasetsOri/paquete_premium_202011.csv")
dapply  <- fread( "./datasetsOri/paquete_premium_202101.csv")

#uso esta semilla para los canaritos
set.seed(10219)
#agrego  30 canaritos
for( i in 1:30)  dtrain[ , paste0("canarito", i ) :=  runif( nrow(dtrain))]
for( i in 1:30)  dapply[ , paste0("canarito", i ) :=  runif( nrow(dapply))]


#Genero un arbol sin limite
modelo_original  <- rpart(formula= "clase_ternaria ~ . -mactivos_margen -mpasivos_margen",
                          data= dtrain,
                          xval= 0,
                          model= TRUE,
                          cp=        -1,
                          maxdepth=  30,  #lo dejo crecer lo mas posible
                          minsplit=   2,
                          minbucket=  1 )


#hago el pruning de los canaritos
#haciendo un hackeo a la estructura  modelo_original$frame
# -666 es un valor arbritrariamente negativo que jamas es generado por rpart
modelo_original$frame[ modelo_original$frame$var %like% "canarito", "complexity"] <- -666
modelo_pruned  <- prune(  modelo_original, -666 )

prediccion  <- predict( modelo_pruned, dapply, type = "prob")[,"BAJA+2"]

entrega  <-  as.data.table( list( "numero_de_cliente"= dapply$numero_de_cliente,
                                  "Predicted"= as.integer(  prediccion > 0.025 ) ) )

fwrite( entrega, paste0( "./kaggle/stopping_at_canaritos.csv"), sep="," ) 

pdf(file = "./work/stopping_at_canaritos.pdf", width=40, height=4)
prp(modelo_pruned, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)
dev.off()
