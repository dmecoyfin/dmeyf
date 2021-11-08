#limpio la memoria
rm( list=ls() )
gc()

library("data.table")
library("rpart")
library("rpart.plot")

setwd("M:" )  #establezco la carpeta donde voy a trabajar
#cargo el dataset
dataset  <- fread( "./datasetsOri/paquete_premium_202011.csv")


#uso esta semilla para los canaritos
set.seed(102191)

#agrego una variable canarito, random distribucion uniforme en el intervalo [0,1]
dataset[ ,  canarito1 :=  runif( nrow(dataset) ) ]

#agrego los siguientes canaritos
for( i in 13:30 ) dataset[ , paste0("canarito", i ) :=  runif( nrow(dataset)) ]


#Primero  veo como quedan mis arboles
modelo  <- rpart(formula= "clase_ternaria ~ . -mactivos_margen -mpasivos_margen",
                 data= dataset[,],
                 model= TRUE,
                 xval= 0,
                 cp= -1, 
                 minsplit= 200,
                 minbucket= 100,
                 maxdepth= 10)


pdf(file = "./work/arbol_canaritos_desconfiados.pdf", width=28, height=4)
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)
dev.off()
