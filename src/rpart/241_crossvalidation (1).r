#!/usr/bin/Rscript

#limpio la memoria
rm(list=ls())   #remove all objects
gc()            #garbage collection

require("data.table")
require("parallel")
require("rpart")

setwd( "M:\\" )

ksemillas  <- c(102191, 200177, 410551, 552581, 892237) #reemplazar por las propias semillas

#------------------------------------------------------------------------------

particionar  <- function( data, division, agrupa="", campo="fold", start=1, seed=NA )
{
  if( !is.na(seed) )   set.seed( seed )

  bloque  <- unlist( mapply(  function(x,y) { rep( y, x )} ,   division,  seq( from=start, length.out=length(division) )  ) )  

  data[ , (campo) :=  sample( rep( bloque, ceiling(.N/length(bloque))) )[1:.N],
        by= agrupa ]
}
#------------------------------------------------------------------------------

ArbolSimple  <- function( fold_test, data, param )
{
  #genero el modelo
  modelo  <- rpart("clase_ternaria ~ .", 
                   data= data[ fold != fold_test, ], #training  fold==1
                   xval= 0,
                   control= param )

  #aplico el modelo a los datos de testing, fold==2
  prediccion  <- predict( modelo, data[ fold==fold_test, ], type = "prob")

  prob_baja2  <- prediccion[, "BAJA+2"]

  ganancia_testing  <- sum(  data[ fold==fold_test ][ prob_baja2 >0.025,  ifelse( clase_ternaria=="BAJA+2", 48750, -1250 ) ] )

  return( ganancia_testing )
}
#------------------------------------------------------------------------------

ArbolesCrossValidation  <- function( data, param, qfolds, semilla )
{
  divi  <- rep( 1, qfolds )
  particionar( data, divi, seed=semilla )

  ganancias  <- mcmapply( ArbolSimple, 
                          seq(qfolds), # 1 2 3 4 5  
                          MoreArgs= list( data, param), 
                          SIMPLIFY= FALSE,
                          mc.cores= 1 )   #se puede subir a 5 si posee Linux o Mac OS

  #devuelvo la primer ganancia y el promedio
  return( mean( unlist( ganancias )) *  qfolds )   #aqui normalizo
}
#------------------------------------------------------------------------------

#cargo los datos donde voy a ENTRENAR el modelo
dataset  <- fread("./datasetsOri/paquete_premium_202009.csv")

#inicializo la tabla donde voy a dejar los resultados
tb_resultados  <- data.table( maxdepth=integer(), ganancia=numeric() )


for(  vmaxdepth in  c(4,5,6,7,6,9,10,11) )
{
  param_basicos  <- list( "cp"=-1, "maxdepth"= vmaxdepth )

  gan  <- ArbolesCrossValidation( dataset, 
                                  param_basicos, 
                                  qfolds= 5, # 5-fold cross validation
                                  semilla= ksemillas[1] )  #uso solo la primer semilla para particionar el dataset

  tb_resultados  <- rbind( tb_resultados, list( vmaxdepth, gan ) )
}

tb_resultados
