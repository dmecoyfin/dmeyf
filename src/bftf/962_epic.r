#Necesita para correr en Google Cloud
#128 GB de memoria RAM
#256 GB de espacio en el disco local
#8 vCPU

#clase_binaria2   1={BAJA+2,BAJA+1}    0={CONTINUA}
#Entrena en a union de VEINTE  meses de [201901, 202009] - { 202006 }  haciendo subsampling al 10% de los continua
#Testea en  { 202011 }
#estima automaticamente la cantidad de registros a enviar al medio de la meseta (en lugar de la prob de corte)

#Optimizacion Bayesiana de hiperparametros de  lightgbm
#funciona automaticamente con EXPERIMENTOS
#va generando incrementalmente salidas para kaggle

# WARNING  usted debe cambiar este script si lo corre en su propio Linux

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rlist")
require("yaml")

require("lightgbm")

#paquetes necesarios para la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")


#para poder usarlo en la PC y en la nube sin tener que cambiar la ruta
#cambiar aqui las rutas en su maquina
switch ( Sys.info()[['sysname']],
         Windows = { directory.root  <-  "M:\\" },   #Windows
         Darwin  = { directory.root  <-  "~/dm/" },  #Apple MAC
         Linux   = { directory.root  <-  "~/buckets/b1/" } #Google Cloud
       )
#defino la carpeta donde trabajo
setwd( directory.root )



kexperimento  <- NA   #NA si se corre la primera vez, un valor concreto si es para continuar procesando

kscript         <- "962_epic"

karch_dataset    <- "./datasets/dataset_epic_v952.csv.gz"

kapply_mes       <- c(202101)  #El mes donde debo aplicar el modelo

ktest_mes_hasta  <- 202011  #Esto es lo que uso para testing
ktest_mes_desde  <- 202011

ktrain_subsampling  <- 0.1   #el undersampling que voy a hacer de los continua

ktrain_mes_hasta    <- 202010  #Obviamente, solo puedo entrenar hasta 202011
ktrain_mes_desde    <- 201901
ktrain_meses_malos  <- c( 202006 )  #meses que quiero excluir del entrenamiento


kgen_mes_hasta    <- 202011   #La generacion final para Kaggle, sin undersampling
kgen_mes_desde    <- 201901


kBO_iter    <-  100   #cantidad de iteraciones de la Optimizacion Bayesiana

#Aqui se cargan los hiperparametros
hs <- makeParamSet( 
         makeNumericParam("learning_rate",    lower=    0.02 , upper=    0.1),
         makeNumericParam("feature_fraction", lower=    0.1  , upper=    1.0),
         makeIntegerParam("min_data_in_leaf", lower=  200L   , upper= 8000L),
         makeIntegerParam("num_leaves",       lower=  100L   , upper= 1024L)
        )

campos_malos  <- c()   #aqui se deben cargar todos los campos culpables del Data Drifting

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

particionar  <- function( data,  division, agrupa="",  campo="fold", start=1, seed=NA )
{
  if( !is.na(seed) )   set.seed( seed )

  bloque  <- unlist( mapply(  function(x,y) { rep( y, x )} ,   division,  seq( from=start, length.out=length(division) )  ) )  

  data[ ,  (campo) :=  sample( rep( bloque, ceiling(.N/length(bloque))) )[1:.N],
            by= agrupa ]
}
#------------------------------------------------------------------------------

HemiModelos  <- function( hparam )
{

  #Construyo el modelo sobre el fold=1
  dgeneracion1  <- lgb.Dataset( data=    data.matrix(  dataset[ generacion_final==1 & fold==1, campos_buenos, with=FALSE]),
                                label=   dataset[ generacion_final==1 & fold==1, clase01] )

  modelo_final1  <- lightgbm( data= dgeneracion1,
                              param= hparam,
                              verbose= -100 )

  rm( dgeneracion1 )  #borro y libero memoria
  gc()

  #Aplico el modelo al fold=2
  prediccion  <- predict( modelo_final1, data.matrix( dataset[ generacion_final==1 & fold==2, campos_buenos, with=FALSE]) )
  dataset[ generacion_final==1 & fold==2, prob := prediccion ]

  tb_modelitos[ dataset[ generacion_final==1 & fold==2], 
                on=c("numero_de_cliente","foto_mes"),  
                paste0( "E", kexperimento,"_", GLOBAL_iteracion ) := i.prob  ]


  #AHORA SOBRE LA OTRA MITAD -----------------

  #Construyo el modelo sobre el fold=2
  dgeneracion2  <- lgb.Dataset( data=    data.matrix(  dataset[ generacion_final==1 & fold==2, campos_buenos, with=FALSE]),
                                label=   dataset[ generacion_final==1 & fold==2, clase01]
                              )

  modelo_final2  <- lightgbm( data= dgeneracion2,
                              param= hparam,
                              verbose= -100
                            )
 
  rm( dgeneracion2 )  #borro y libero memoria
  gc()

  #Aplico el modelo al fold=1
  prediccion  <- predict( modelo_final2, data.matrix( dataset[ generacion_final==1 & fold==1, campos_buenos, with=FALSE]) )
  dataset[ generacion_final==1 & fold==1, prob := prediccion ]

  tb_modelitos[ dataset[ generacion_final==1 & fold==1], 
                on= c("numero_de_cliente","foto_mes"),  
                paste0( "E", kexperimento,"_", GLOBAL_iteracion ) := i.prob  ]

  dataset[  , prob := NULL ]

}
#------------------------------------------------------------------------------

FullModelo  <- function( hparam )
{
  #entreno sin undersampling
  dgeneracion  <- lgb.Dataset( data=    data.matrix(  dataset[ generacion_final==1 , campos_buenos, with=FALSE]),
                               label=   dataset[ generacion_final==1, clase01]
                             )

  modelo_final  <- lightgbm( data= dgeneracion,
                             param= hparam,
                             verbose= -100
                           )

  rm( dgeneracion )  #borro y libero memoria
  gc()

  #calculo la importancia de variables
  tb_importancia  <- lgb.importance( model= modelo_final )
  fwrite( tb_importancia, 
          file= paste0( kimp, "imp_", sprintf("%03d", GLOBAL_iteracion), ".txt"),
          sep="\t" )

  #Aplico sobre todo el dataset
  prediccion  <- predict( modelo_final, data.matrix( dataset[  , campos_buenos, with=FALSE]) )
  dataset[ , prob := prediccion ]
  tb_modelitos[ dataset, 
                on=c("numero_de_cliente","foto_mes"),  
                paste0( "E", kexperimento,"_", GLOBAL_iteracion ) := i.prob  ]

 #Fin primera pasada modelitos

  prediccion  <- predict( modelo_final, data.matrix( dapply[  , campos_buenos, with=FALSE]) )

  predsort  <- sort(prediccion, decreasing=TRUE)
  pos_corte  <- as.integer(hparam$ratio_corte*nrow(dapply))
  prob_corte <- predsort[ pos_corte ]
  Predicted  <- as.integer( prediccion > prob_corte )

  entrega  <- as.data.table( list( "numero_de_cliente"= dapply$numero_de_cliente, 
                                   "Predicted"= Predicted)  )

  #genero el archivo para Kaggle
  fwrite( entrega, 
          file= paste0(kkaggle, sprintf("%03d", GLOBAL_iteracion), ".csv" ),
          sep= "," )

  base  <- round( pos_corte / 500 ) * 500   - 3000
  evaluados  <- c( seq(from=base, to=pmax(base+6000,15000), by=500 ) , pos_corte )  
  evaluados  <- sort( evaluados )

  for(  pos  in  evaluados )
  {
    prob_corte  <-  predsort[ pos ]
    Predicted  <- as.integer( prediccion > prob_corte )

    entrega  <- as.data.table( list( "numero_de_cliente"= dapply$numero_de_cliente, 
                                     "Predicted"= Predicted)  )

    #genero el archivo para Kaggle
    fwrite( entrega, 
            file= paste0(kkagglemeseta, sprintf("%03d", GLOBAL_iteracion), 
                         "_",  sprintf( "%05d", pos) ,".csv" ),
            sep= "," )
  }

  rm( entrega, Predicted )
}
#------------------------------------------------------------------------------

VPOS_CORTE  <- c()

fganancia_lgbm_meseta  <- function(probs, datos) 
{
  vlabels  <- getinfo(datos, "label")
  vpesos   <- getinfo(datos, "weight")

  #solo sumo 48750 si vpesos > 1, hackeo 
  tbl  <- as.data.table( list( "prob"=probs, "gan"= ifelse( vlabels==1 & vpesos > 1, 48750, -1250 ) ) )

  setorder( tbl, -prob )
  tbl[ , posicion := .I ]
  tbl[ , gan_acum :=  cumsum( gan ) ]
  setorder( tbl, -gan_acum )   #voy por la meseta

  gan  <- mean( tbl[ 1:10,  gan_acum] )  #meseta de tamaño 10

  pos_meseta  <- tbl[ 1:10,  median(posicion)]
  VPOS_CORTE  <<- c( VPOS_CORTE, pos_meseta )

  return( list( "name"= "ganancia", 
                "value"=  gan,
                "higher_better"= TRUE ) )
}
#------------------------------------------------------------------------------

x  <- list( "learning_rate"= 0.02, 
            "feature_fraction"= 0.50,
            "min_data_in_leaf"= 4000,
            "num_leaves"= 600 )


EstimarGanancia_lightgbm  <- function( x )
{
  GLOBAL_iteracion  <<- GLOBAL_iteracion + 1

  gc()

  param_basicos  <- list( objective= "binary",
                          metric= "custom",
                          first_metric_only= TRUE,
                          boost_from_average= TRUE,
                          feature_pre_filter= FALSE,
                          verbosity= -100,
                          seed= 999983,
                          max_depth=  -1,         # -1 significa no limitar,  por ahora lo dejo fijo
                          min_gain_to_split= 0.0, #por ahora, lo dejo fijo
                          lambda_l1= 0.0,         #por ahora, lo dejo fijo
                          lambda_l2= 0.0,         #por ahora, lo dejo fijo
                          max_bin= 31,            #por ahora, lo dejo fijo
                          num_iterations= 9999,   #un numero muy grande, lo limita early_stopping_rounds
                          force_row_wise= TRUE    #para que los alumnos no se atemoricen con tantos warning
                        )

  #el parametro discolo, que depende de otro
  param_variable  <- list(  early_stopping_rounds= as.integer(50 + 1/x$learning_rate) )

  param_completo  <- c( param_basicos, param_variable, x )

  VPOS_CORTE  <<- c()
  set.seed( 999983 )
  modelo  <- lgb.train( data= dtrain,
                        valids= list( valid= dvalid ),
                        eval= fganancia_lgbm_meseta,
                        param= param_completo,
                        verbose= -100 )

  #unlist(modelo$record_evals$valid$ganancia$eval)[ modelo$best_iter ]

  #calculo la ganancia sobre los datos de testing
  prediccion  <- predict( modelo, data.matrix( dataset[ test==1, campos_buenos, with=FALSE]) )

  tb_test  <- as.data.table( list( "ganancia"=dataset[ test==1, ifelse(clase_ternaria=="BAJA+2", 48750, -1250)],
                                   "prob"= prediccion ) )

  setorder( tb_test, -prob )
  ganancia  <- tb_test[  1:   VPOS_CORTE[ modelo$best_iter ], sum( ganancia ) ]

  attr(ganancia,"extras" )  <- list("num_iterations"= modelo$best_iter)  #esta es la forma de devolver un parametro extra

  param_final  <- copy( param_completo )
  param_final["early_stopping_rounds"]  <- NULL
  param_final$num_iterations <- modelo$best_iter  #asigno el mejor num_iterations
  param_final$ratio_corte  <- VPOS_CORTE[ modelo$best_iter ] / nrow( dvalid )


  #si tengo una ganancia superadora, genero el archivo para Kaggle
  if( ganancia > GLOBAL_ganancia_max )
  {
    GLOBAL_ganancia_max  <<- ganancia  #asigno la nueva maxima ganancia a una variable GLOBAL, por eso el <<-

    if( GLOBAL_iteracion > 30 )
    {
      FullModelo( param_final )
      HemiModelos( param_final )
      fwrite( tb_modelitos, file= kmodelitos, sep= "," )
    }
  }

   #logueo 
   xx  <- param_final
   xx$iteracion_bayesiana  <- GLOBAL_iteracion
   xx$ganancia  <- ganancia  #le agrego la ganancia
   loguear( xx,  arch= klog )

   return( ganancia )
}
#------------------------------------------------------------------------------
#Aqui empieza el programa

if( is.na(kexperimento ) )   kexperimento <- get_experimento()  #creo el experimento

#en estos archivos quedan los resultados
dir.create( paste0( "./work/E",  kexperimento, "/" ) )     #creo carpeta del experimento dentro de work
dir.create( paste0( "./kaggle/E",  kexperimento, "/" ) )   #creo carpeta del experimento dentro de kaggle
dir.create( paste0( "./kaggle/E",  kexperimento, "/meseta/" ) )   #creo carpeta del experimento dentro de kaggle

kbayesiana    <- paste0("./work/E",  kexperimento, "/E",  kexperimento, "_", kscript, ".RDATA" )
klog          <- paste0("./work/E",  kexperimento, "/E",  kexperimento, "_", kscript, "_BOlog.txt" )
kimp          <- paste0("./work/E",  kexperimento, "/E",  kexperimento, "_", kscript, "_" )
kkaggle       <- paste0("./kaggle/E",kexperimento, "/E",  kexperimento, "_", kscript, "_" )
kkagglemeseta <- paste0("./kaggle/E",kexperimento, "/meseta/E",  kexperimento, "_", kscript, "_" )
kmodelitos    <- paste0("./modelitos/E", kexperimento, "_modelitos.csv.gz" )



#cargo el dataset que tiene los 36 meses
dataset  <- fread(karch_dataset)

#si ya existe el archivo log, traigo hasta donde llegue
if( file.exists(klog) )
{
  tabla_log  <- fread( klog)
  GLOBAL_iteracion  <- nrow( tabla_log ) -1
  GLOBAL_ganancia_max  <- tabla_log[ , max(ganancia) ]
  
  tb_modelitos  <- fread( kmodelitos )
} else {
  GLOBAL_iteracion  <- 0
  GLOBAL_ganancia_max  <- -Inf

  tb_modelitos  <- dataset[  ,  c("numero_de_cliente","foto_mes"), with=FALSE ]
  fwrite( tb_modelitos, file= kmodelitos, sep= "," )
}

#agrego un quinto de canaritos
for( i  in 1:(ncol(dataset)/5))  dataset[ , paste0("canarito", i ) :=  runif( nrow(dataset))]


#cargo los datos donde voy a aplicar el modelo
dapply  <- copy( dataset[  foto_mes %in% kapply_mes ] )


#creo la clase_binaria2   1={ BAJA+2,BAJA+1}  0={CONTINUA}
dataset[ , clase01:= ifelse( clase_ternaria=="CONTINUA", 0, 1 ) ]


particionar( dataset, division= c(1,1), agrupa= c("foto_mes","clase_ternaria" ), seed= ksemilla_azar )

dataset[    foto_mes>= kgen_mes_desde  &
            foto_mes<= kgen_mes_hasta & 
            !( foto_mes %in% ktrain_meses_malos ),
          generacion_final:= 1L ]  #donde entreno


#Defino los datos donde entreno, con subsampling de los CONTINUA
vector_azar  <- runif( nrow(dataset) )
dataset[    foto_mes>= ktrain_mes_desde  &
            foto_mes<= ktrain_mes_hasta & 
            !( foto_mes %in% ktrain_meses_malos ) & 
            ( clase01==1 | vector_azar < ktrain_subsampling ),  
          entrenamiento:= 1L ]  #donde entreno


#defino donde valido
dataset[    foto_mes>= ktest_mes_desde &
            foto_mes<= ktest_mes_hasta &
            fold== 1,
          validacion:= 1L ]  #donde entreno

#defino donde testeo
dataset[    foto_mes>= ktest_mes_desde &
            foto_mes<= ktest_mes_hasta &
            fold== 2,
          test:= 1L ]  #donde entreno



#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), 
                           c("clase_ternaria","clase01", "generacion_final", "entrenamiento", "validacion", "test", "fold", campos_malos) )

#dejo los datos en el formato que necesita LightGBM
#uso el weight como un truco ESPANTOSO para saber la clase real
dtrain  <- lgb.Dataset( data=    data.matrix(  dataset[ entrenamiento==1 , campos_buenos, with=FALSE]),
                        label=   dataset[ entrenamiento==1, clase01],
                        weight=  dataset[ entrenamiento==1, ifelse(clase_ternaria=="BAJA+2", 1.0000001, 1.0)] ,
                        free_raw_data= TRUE
                      )

dvalid  <- lgb.Dataset( data=    data.matrix(  dataset[validacion==1 , campos_buenos, with=FALSE]),
                        label=   dataset[ validacion==1, clase01],
                        weight=  dataset[ validacion==1, ifelse(clase_ternaria=="BAJA+2", 1.0000001, 1.0)] ,
                        free_raw_data= TRUE
                      )


#Aqui comienza la configuracion de la Bayesian Optimization

funcion_optimizar  <- EstimarGanancia_lightgbm   #la funcion que voy a maximizar

configureMlr( show.learner.output= FALSE)

#configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
#por favor, no desesperarse por lo complejo
obj.fun  <- makeSingleObjectiveFunction(
              fn=       funcion_optimizar, #la funcion que voy a maximizar
              minimize= FALSE,   #estoy Maximizando la ganancia
              noisy=    TRUE,
              par.set=  hs,     #definido al comienzo del programa
              has.simple.signature = FALSE   #paso los parametros en una lista
             )

ctrl  <- makeMBOControl( save.on.disk.at.time= 600,  save.file.path= kbayesiana)  #se graba cada 600 segundos
ctrl  <- setMBOControlTermination(ctrl, iters= kBO_iter )   #cantidad de iteraciones
ctrl  <- setMBOControlInfill(ctrl, crit= makeMBOInfillCritEI() )

#establezco la funcion que busca el maximo
surr.km  <- makeLearner("regr.km", predict.type= "se", covtype= "matern3_2", control= list(trace= TRUE))

#inicio la optimizacion bayesiana
if(!file.exists(kbayesiana)) {
  run  <- mbo(obj.fun, learner= surr.km, control= ctrl)
} else {
  run  <- mboContinue( kbayesiana )   #retomo en caso que ya exista
}



#apagado de la maquina virtual, pero NO se borra
system( "sleep 10  &&  sudo shutdown -h now", wait=FALSE)

#suicidio,  elimina la maquina virtual directamente
#system( "sleep 10  && 
#        export NAME=$(curl -X GET http://metadata.google.internal/computeMetadata/v1/instance/name -H 'Metadata-Flavor: Google') &&
#        export ZONE=$(curl -X GET http://metadata.google.internal/computeMetadata/v1/instance/zone -H 'Metadata-Flavor: Google') &&
#        gcloud --quiet compute instances delete $NAME --zone=$ZONE",
#        wait=FALSE )


quit( save="no" )


