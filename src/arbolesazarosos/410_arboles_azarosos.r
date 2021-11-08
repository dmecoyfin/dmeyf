#Ensemble de arboles de decision

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

require("data.table")
require("rpart")

#Aqui se debe poner la carpeta de la computadora local
setwd("~/buckets/b1/crudoB/")  #Establezco el Working Directory

#cargo los datos donde entreno
dtrain  <- fread("./datasetsOri/paquete_premium_202009.csv")

#cargo los datos donde aplico el modelo
dapply  <- fread("./datasetsOri/paquete_premium_202011.csv")


#Establezco cuales son los campos que puedo usar para la prediccion
campos_buenos  <- setdiff(  colnames(dtrain) ,  c("clase_ternaria") )

parametros  <-  list( "cp"=-1, "minsplit"=900,  "minbucket"=440, "maxdepth"=5 )

num_trees         <-  10    #voy a generar 10 arboles
feature_fraction  <-   0.5  #entreno cada arbol con solo 50% de las variables variables

set.seed(102191) #Establezco la semilla aleatoria

#inicializo en CERO el vector de las probabilidades en dapply
#Aqui es donde voy acumulando, sumando, las probabilidades
probabilidad_ensemble  <- rep( 0, nrow(dapply) )

for(  i in  1:num_trees ) #genero  num_trees arboles
{
  qty_campos_a_utilizar  <- as.integer( length(campos_buenos)* feature_fraction )
  campos_random  <- sample( campos_buenos, qty_campos_a_utilizar )
  
  #paso de un vector a un string con los elementos separados por un signo de "+"
  #este hace falta para la formula
  campos_random  <- paste( campos_random, collapse=" + ")

  #genero el modelo
  formulita  <- paste0( "clase_ternaria ~ ", campos_random )

  modelo  <- rpart( formulita,
                    data= dtrain,
                    xval= 0,
                    control= parametros )

  #aplico el modelo a los datos que no tienen clase
  prediccion  <- predict( modelo, dapply , type = "prob")

  #voy acumulando la probabilidad
  probabilidad_ensemble  <- probabilidad_ensemble +  prediccion[, "BAJA+2"]
}

#fue sumando las probabilidades, ahora hago el cociente por la cantidad de arboles
#o sea, calculo el promedio
probabilidad_ensemble  <- probabilidad_ensemble / num_trees

#Genero la entrega para Kaggle
entrega  <- as.data.table( list( "numero_de_cliente"= dapply[  , numero_de_cliente],
                                 "Predicted"= as.numeric(probabilidad_ensemble > 0.025) ) ) #genero la salida

#genero el archivo para Kaggle
fwrite( entrega, 
        file="./kaggle/arboles_azarosos_001.csv", 
        sep="," )
