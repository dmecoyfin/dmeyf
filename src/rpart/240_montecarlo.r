#limpio la memoria
rm(list=ls())   #remove all objects
gc()            #garbage collection

require("data.table")
require("parallel")
require("rpart")

#setwd( "M:\\" )
setwd( "~/buckets/b1/crudoB/" )

ksemillas  <- c(102191, 200177, 410551, 552581, 892237) #reemplazar por las propias semillas
ksemilla_extra  <- 950009  #reemplazar por una elegida en el momento

#------------------------------------------------------------------------------

particionar  <- function( data,  division, agrupa="",  campo="fold", start=1, seed=NA )
{
  if( !is.na(seed) )   set.seed( seed )

  bloque  <- unlist( mapply(  function(x,y) { rep( y, x )} ,   division,  seq( from=start, length.out=length(division) )  ) )  

  data[ ,  (campo) :=  sample( rep( bloque, ceiling(.N/length(bloque))) )[1:.N],
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

ArbolEstimarGanancia  <- function( semilla, data, param )
{
  pct_test  <- 30/(30+70)
  particionar( data, division=c(70,30), agrupa="clase_ternaria", seed=semilla )

  ganancia_testing  <- ArbolSimple( 2, data, param )

  ganancia_testing_normalizada  <- ganancia_testing / pct_test   #normalizo la ganancia

  return( ganancia_testing_normalizada )
}
#------------------------------------------------------------------------------

ArbolesMontecarlo  <- function( data, param, semillas )
{
  ganancias  <- mcmapply( ArbolEstimarGanancia, 
                          semillas, 
                          MoreArgs= list( data, param), 
                          SIMPLIFY= FALSE,
                          mc.cores= 5 )  #se puede subir a 5 si posee Linux o Mac OS

  #devuelvo la primer ganancia y el promedio
  return( mean( unlist( ganancias ))  ) 
}
#------------------------------------------------------------------------------

#cargo los datos donde voy a ENTRENAR el modelo
dataset  <- fread("./datasetsOri/paquete_premium_202009.csv")

#inicializo la tabla donde voy a dejar los resultados
tb_resultados  <- data.table( maxdepth=integer(), ganancia1=numeric(), ganancia5=numeric()  )

for(  vmaxdepth in  c(4,5,6,7,8,9,10,11) )
{
  param_basicos  <- list( "cp"=-1, "minsplit"=20, "minbucket"=7,  "maxdepth"= vmaxdepth )

  gan1  <- ArbolesMontecarlo( dataset, param_basicos, ksemilla_extra )
  gan5  <- ArbolesMontecarlo( dataset, param_basicos, ksemillas )

  tb_resultados  <- rbind( tb_resultados, list( vmaxdepth, gan1, gan5 ) )
}

tb_resultados
