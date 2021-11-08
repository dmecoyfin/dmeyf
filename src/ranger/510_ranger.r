#Se utiliza el algoritmo Random Forest, creado por Leo Breiman en el año 2001
#Una libreria que implementa Rando Forest se llama  ranger
#La libreria esta implementada en lenguaje C y corre en paralelo, utiliza TODOS los nucleos del procesador
#Leo Breiman provenia de la estadistica y tenia "horror a los nulos", con lo cual el algoritmo necesita imputar nulos antes


#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

require("data.table")
require("ranger")
require("randomForest")  #solo se usa para imputar nulos

#Aqui se debe poner la carpeta de la computadora local
setwd("~/buckets/b1/crudoB/")  #Establezco el Working Directory

#cargo los datos donde entreno
dtrain  <- fread("./datasetsOri/paquete_premium_202009.csv", stringsAsFactors= TRUE)

dtrain[ , clase_binaria := as.factor(ifelse( clase_ternaria=="BAJA+2", "POS", "NEG" )) ]
dtrain[ , clase_ternaria := NULL ]  #elimino la clase_ternaria, ya no la necesito

#imputo los nulos, ya que ranger no acepta nulos
#Leo Breiman, ¿por que le temias a los nulos?
dtrain  <- na.roughfix( dtrain )

#cargo los datos donde aplico el modelo
dapply  <- fread("./datasetsOri/paquete_premium_202011.csv", stringsAsFactors= TRUE)
dapply[ , clase_ternaria := NULL ]  #Elimino esta columna que esta toda en NA
dapply  <- na.roughfix( dapply )

#genero el modelo de Random Forest con la libreria ranger
param  <- list( "num.trees"=      500,  #cantidad de arboles
                "mtry"=             sqrt(ncol(dtrain)),  #cantidad de variables que evalua para hacer un split
                "min.node.size"=    1,  #hoja mas chica
                "max.depth"=        0   # 0 significa profundidad infinita
              )

set.seed(102191) #Establezco la semilla aleatoria

modelo  <- ranger( formula= "clase_binaria ~ .",
                   data=  dtrain, 
                   probability=   TRUE,  #para que devuelva las probabilidades
                   num.trees=     param$num.trees,
                   mtry=          param$mtry,
                   min.node.size= param$min.node.size,
                   max.depth=     param$max.depth
                 )

prediccion  <- predict( modelo, dapply )

#Genero la entrega para Kaggle
entrega  <- as.data.table( list( "numero_de_cliente"= dapply[  , numero_de_cliente],
                                 "Predicted"= as.numeric(prediccion$predictions[ ,"POS" ] > 0.025) ) ) #genero la salida

#genero el archivo para Kaggle
fwrite( entrega, 
        file="./kaggle/ranger_001.csv", 
        sep="," )
