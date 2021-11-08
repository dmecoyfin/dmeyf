#Grid Search con Arboles de Decision

#limpio la memoria
rm(list=ls())   #remove all objects
gc()            #garbage collection

require("data.table")
require("rlist")
require("parallel")
require("rpart")

setwd( "M:\\" )

ksemillas  <- c(102191, 200177, 410551, 552581, 892237) #reemplazar por las propias semillas

#------------------------------------------------------------------------------

loguear  <- function( reg, arch=NA, folder="./work/", ext=".txt", verbose=TRUE )
{
  archivo  <- arch
  if( is.na(arch) )  archivo  <- paste0(  folder, substitute( reg), ext )

  if( !file.exists( archivo ) )  #Escribo los titulos
  {
    linea  <- paste0( "fecha\t", 
                      paste( list.names(reg), collapse="\t" ), "\n" )

    cat( linea, file=archivo )
  }

  linea  <- paste0( format(Sys.time(), "%Y%m%d %H%M%S"),  "\t",     #la fecha y hora
                    gsub( ", ", "\t", toString( reg ) ),  "\n" )

  cat( linea, file=archivo, append=TRUE )  #grabo al archivo

  if( verbose )  cat( linea )   #imprimo por pantalla
}
#------------------------------------------------------------------------------

particionar  <- function( data, division, agrupa="", campo="fold", start=1, seed=NA )
{
  if( !is.na( seed)  )   set.seed( seed )

  bloque  <- unlist( mapply(  function(x,y) { rep( y, x ) }, division, seq( from=start, length.out=length(division) )  ) )

  data[ , (campo) :=  sample( rep( bloque, ceiling(.N/length(bloque))) )[1:.N],
           by= agrupa ]
}
#------------------------------------------------------------------------------

ArbolSimple  <- function( fold_test, data, param )
{
  #genero el modelo
  modelo  <- rpart("clase_ternaria ~ .", 
                   data= data[ fold != fold_test, ],
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


for( vcp in c( -1, 0) ) 
for( vmaxdepth in  c(4,5,6,7,8,10,12,14,16) )
for( vminsplit in  c(2,4, 8, 10, 15, 20, 30, 50, 100, 150, 200, 300, 400 ) )
for( vminbucket  in  unique( as.integer(c(1,2,3,4,5,  vminsplit/10, vminsplit/5, vminsplit/3, vminsplit/2 )) ) )
{
  param_basicos  <- list( "cp"= vcp, 
                          "minsplit"= vminsplit,
                          "minbucket"= vminbucket,
                          "maxdepth"= vmaxdepth )

  gan  <- ArbolesCrossValidation( dataset,
                                  param_basicos,
                                  qfolds= 5, # 5-fold cross validation
                                  ksemillas[1] )  #uso solo la primer semilla para particionar el dataset

  E250  <- c( param_basicos,  list( "ganancia" = gan ) )
  loguear( E250 )
}

  param_basicos  <- list( "cp"= -1, 
                          "minsplit"= 10,
                          "minbucket"= 2,
                          "maxdepth"= 5 )
                          
                          




