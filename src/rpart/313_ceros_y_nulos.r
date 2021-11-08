#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")


#Aqui comienza el programa
setwd("~/buckets/b1/crudoB/")

datasetA  <- fread( "./datasetsOri/paquete_premium_202009.csv" )
datasetB  <- fread( "./datasetsOri/paquete_premium_202011.csv" )

options( digits=4 )

#Brutalmente un  for  !!
#La forma mas desprolija de mostrar datos 

campos_buenos  <- setdiff(  colnames( datasetA),  c("numero_de_cliente","foto_mes","clase_ternaria" ) )

for( campo in  campos_buenos )
{
  cat( "NA_ratio" , datasetA[  ,  sum(is.na(get(campo)))/.N ],  datasetB[  ,  sum(is.na(get(campo)))/.N ], sep="\t" )
  cat( "\t\t" )
  cat( "CERO_ratio"  , datasetA[  ,  sum(get(campo)==0, na.rm=TRUE)/.N ],  datasetB[  ,  sum(get(campo)==0, na.rm=TRUE)/.N ], sep="\t" )
  cat( "\t" )
  cat( campo, "\n" )
}

