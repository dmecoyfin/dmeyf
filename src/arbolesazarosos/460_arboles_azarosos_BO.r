#Optimizacion Bayesiana de hiperparametros de  ArbolesAzarosos
#funciona automaticamente con EXPERIMENTOS
#va generando incrementalmente salidas para kaggle

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rlist")
require("yaml")

require("rpart")
require("parallel")

#paquetes necesarios para la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")


#defino la carpeta donde trabajo
setwd( "~/buckets/b1/crudoB/"  )


kexperimento  <- NA   #NA si se corre la primera vez, un valor concreto si es para continuar procesando

kscript           <- "460_arboles_azarosos_BO"
karch_generacion  <- "./datasetsOri/paquete_premium_202009.csv"
karch_aplicacion  <- "./datasetsOri/paquete_premium_202011.csv"
kBO_iter    <-  150   #cantidad de iteraciones de la Optimizacion Bayesiana

hs  <- makeParamSet(
          makeNumericParam("cp"       ,         lower= -1   , upper=    0.1),
          makeIntegerParam("minsplit" ,         lower=  1L  , upper= 8000L),  #la letra L al final significa ENTERO
          makeIntegerParam("minbucket",         lower=  1L  , upper= 2000L),
          makeIntegerParam("maxdepth" ,         lower=  3L  , upper=   20L),
          makeNumericParam("feature_fraction" , lower=  0.1 , upper=    0.9),
          makeIntegerParam("num_trees" ,        lower=  2L  , upper=  100L),  #no me conforma que llegue solo a 100 ...
          forbidden = quote( minbucket > 0.5*minsplit ) )


ksemilla_azar  <- 102191  #Aqui poner la propia semilla
#------------------------------------------------------------------------------
#Funcion que lleva el registro de los experimentos

get_experimento  <- function()
{
  if( !file.exists( "./maestro.yaml" ) )  cat( file="./maestro.yaml", "experimento: 1000" )

  exp  <- read_yaml( "./maestro.yaml" )
  experimento_actual  <- exp$experimento

  exp$experimento  <- as.integer(exp$experimento + 1)
  Sys.chmod( "./maestro.yaml", mode = "0644", use_umask = TRUE)
  write_yaml( exp, "./maestro.yaml" )
  Sys.chmod( "./maestro.yaml", mode = "0444", use_umask = TRUE) #dejo el archivo readonly

  return( experimento_actual )
}
#------------------------------------------------------------------------------
#graba a un archivo los componentes de lista
#para el primer registro, escribe antes los titulos

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
#funcion para particionar, es la que Andres reemplaza con caret

particionar  <- function( data, division, agrupa="", campo="fold", start=1, seed=NA )
{
  if( !is.na( seed)  )   set.seed( seed )

  bloque  <- unlist( mapply(  function(x,y) { rep( y, x ) }, division, seq( from=start, length.out=length(division) )  ) )

  data[ , (campo) :=  sample( rep( bloque, ceiling(.N/length(bloque))) )[1:.N],
           by= agrupa ]
}
#------------------------------------------------------------------------------

ArbolesAzarosos_Simple  <- function( fold_test, data, param, pcampos_buenos )
{
  #genero el modelo

  set.seed(ksemilla_azar)

  qty_campos_a_utilizar  <- as.integer( length(pcampos_buenos) * param$feature_fraction )

  #inicializo en CERO el vector de las probabilidades en testing
  #Aqui es donde voy acumulando, sumando, las probabilidades
  probabilidad_ensemble  <- rep( 0, nrow(data[ fold==fold_test, ]) )

  for( i in 1:param$num_trees )
  {

    campos_random  <- sample( pcampos_buenos, qty_campos_a_utilizar )
    campos_random  <- paste( campos_random, collapse=" + ")
    formulita  <- paste0( "clase_ternaria ~ ", campos_random )

    modelo  <- rpart(formulita, 
                     data= data[ fold != fold_test, ],
                     xval= 0,
                     control= param )

    #aplico el modelo a los datos de testing
    prediccion  <- predict( modelo, data[ fold==fold_test, ], type = "prob")

    #Acumulo
    probabilidad_ensemble  <- probabilidad_ensemble +  prediccion[, "BAJA+2"]
  }
  
  #fue sumando las probabilidades, ahora hago el cociente por la cantidad de arboles
  #o sea, calculo el promedio
  probabilidad_ensemble  <- probabilidad_ensemble / param$num_trees

  ganancia_testing  <- sum(  data[ fold==fold_test ][ prediccion[, "BAJA+2"] >0.025,  ifelse( clase_ternaria=="BAJA+2", 48750, -1250 ) ] )

  return( ganancia_testing )
}
#------------------------------------------------------------------------------

ArbolesAzarosos_CrossValidation  <- function( data, param, pcampos_buenos, qfolds, pagrupa, semilla )
{
  divi  <- rep( 1, qfolds )
  particionar( data, divi, seed=semilla, agrupa=pagrupa )

  ganancias  <- mcmapply( ArbolesAzarosos_Simple, 
                          seq(qfolds), # 1 2 3 4 5  
                          MoreArgs= list( data, param, pcampos_buenos), 
                          SIMPLIFY= FALSE,
                          mc.cores= 5 )   #se puede subir a 5 si posee Linux o Mac OS
                                          #Se se usa Windows, obligatoriamente debe ser  1

  data[ , fold := NULL ]
  #devuelvo la primer ganancia y el promedio
  return( mean( unlist( ganancias )) *  qfolds )   #aqui normalizo la ganancia
}
#------------------------------------------------------------------------------
#esta funcion solo puede recibir los parametros que se estan optimizando
#el resto de los parametros se pasan como variables globales

EstimarGanancia_ArbolesAzarosos  <- function( x )
{
   campos_buenos  <- setdiff(  colnames(dataset) ,  c("clase_ternaria") )
   
   GLOBAL_iteracion  <<-  GLOBAL_iteracion + 1

   xval_folds  <- 5
   ganancia  <- ArbolesAzarosos_CrossValidation( dataset, 
                                                 param= x,
                                                 pcampos_buenos= campos_buenos,
                                                 qfolds= xval_folds, 
                                                 pagrupa= "clase_ternaria", 
                                                 semilla= ksemilla_azar )

   #si tengo una ganancia superadora, genero el archivo para Kaggle
   if(  ganancia > GLOBAL_ganancia_max )
   {
     GLOBAL_ganancia_max <<-  ganancia  #asigno la nueva maxima ganancia

     set.seed(ksemilla_azar)
     
     qty_campos_a_utilizar  <- as.integer( length(campos_buenos) * x$feature_fraction )
     #inicializo vector donde acumulo probabilidades
     probabilidad_ensemble  <- rep( 0, nrow(dapply) )

     for( i in 1:x$num_trees )
     {
       campos_random  <- sample( campos_buenos, qty_campos_a_utilizar )
       campos_random  <- paste( campos_random, collapse=" + ")
       
       formulita  <- paste0( "clase_ternaria ~ ", campos_random )

       modelo  <- rpart(formulita,
                        data= dataset,
                        xval= 0,
                        control= x )

       #genero el vector con la prediccion, la probabilidad de ser positivo
       prediccion  <- predict( modelo, dapply)

       probabilidad_ensemble  <- probabilidad_ensemble +  prediccion[, "BAJA+2"]
     }
     
     #fue sumando las probabilidades, ahora hago el cociente por la cantidad de arboles
     #o sea, calculo el promedio
     probabilidad_ensemble  <- probabilidad_ensemble / x$num_trees

     Predicted  <- ifelse( probabilidad_ensemble > 0.025, 1, 0 )

     entrega  <- as.data.table( list( "numero_de_cliente"=dapply$numero_de_cliente, 
                                      "Predicted"= Predicted)  )

     #genero el archivo para Kaggle
     fwrite( entrega, 
             file= paste0(kkaggle, GLOBAL_iteracion, ".csv" ),
             sep=  "," )
   }

   #logueo 
   xx  <- x
   xx$xval_folds  <-  xval_folds
   xx$ganancia  <- ganancia
   loguear( xx,  arch= klog )


   return( ganancia )
}
#------------------------------------------------------------------------------
#Aqui empieza el programa

if( is.na(kexperimento ) )   kexperimento <- get_experimento()  #creo el experimento

#en estos archivos quedan los resultados
kbayesiana  <- paste0("./work/E",  kexperimento, "_rpart.RDATA" )
klog        <- paste0("./work/E",  kexperimento, "_rpart_log.txt" )
kkaggle     <- paste0("./kaggle/E",kexperimento, "_", kscript, "_" )


GLOBAL_ganancia_max  <-  -Inf
GLOBAL_iteracion  <- 0

if( file.exists(klog) )
{
 tabla_log  <- fread( klog)
 GLOBAL_iteracion  <- nrow( tabla_log ) -1
 GLOBAL_ganancia_max  <-  tabla_log[ , max(ganancia) ]
}


#cargo los datasets
dataset  <- fread(karch_generacion)   #donde entreno
dapply  <- fread(karch_aplicacion)    #donde aplico el modelo

#Aqui comienza la configuracion de la Bayesian Optimization

configureMlr( show.learner.output = FALSE)

funcion_optimizar  <- EstimarGanancia_ArbolesAzarosos

#configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
#por favor, no desesperarse por lo complejo
obj.fun  <- makeSingleObjectiveFunction(
              fn=       funcion_optimizar,
              minimize= FALSE,   #estoy Maximizando la ganancia
              noisy=    TRUE,
              par.set=  hs,
              has.simple.signature = FALSE
             )

ctrl  <- makeMBOControl( save.on.disk.at.time= 600,  save.file.path= kbayesiana)
ctrl  <- setMBOControlTermination(ctrl, iters= kBO_iter )
ctrl  <- setMBOControlInfill(ctrl, crit= makeMBOInfillCritEI())

surr.km  <-  makeLearner("regr.km", predict.type= "se", covtype= "matern3_2", control= list(trace= TRUE))

#inicio la optimizacion bayesiana
if(!file.exists(kbayesiana)) {
  run  <- mbo(obj.fun, learner = surr.km, control = ctrl)
} else  run  <- mboContinue( kbayesiana )   #retomo en caso que ya exista


quit( save="no" )


