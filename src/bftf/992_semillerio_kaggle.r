#Necesita para correr en Google Cloud
#64 GB de memoria RAM
#256 GB de espacio en el disco local
#10 vCPU

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rlist")
require("yaml")

require("lightgbm")

require("primes")  #para generar semillas

directory.root <- "~/buckets/b1/"
setwd( directory.root )

kexperimento  <- NA

kscript         <- "992_epic"
karch_dataset   <- "./datasets/dataset_epic_v952.csv.gz"  #el dataset que voy a utilizar
karch_realidad  <- "./datasets/realidad_202101.csv"  #el dataset que voy a utilizar

ktest_mes_hasta  <- 202101  #Esto es lo que uso para testing
ktest_mes_desde  <- 202101

kgen_mes_hasta   <- 202011  #hasta donde voy a entrenar
kgen_mes_desde   <- 201901  #desde donde voy a entrenar
kgen_meses_malos <- 202006  #el mes que voy a eliminar del entreanamiento

kgen_subsampling <- 1.0     #esto es NO hacer undersampling

campos_malos  <- c()   #aqui se deben cargar todos los campos culpables del Data Drifting

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
#Aqui empieza el programa

if( is.na(kexperimento ) )   kexperimento <- get_experimento()  #creo el experimento

#en estos archivos quedan los resultados
dir.create( paste0( "./work/E",  kexperimento, "/" ) )     #creo carpeta del experimento dentro de work

kresultados  <- paste0("./work/E",  kexperimento, "/E",  kexperimento, "_", kscript, ".txt" )  #archivo donde dejo el resultado


#cargo el dataset que tiene los 36 meses
dataset  <- fread(karch_dataset)

#cargo los datos donde voy a aplicar el modelo
dtest  <- copy( dataset[ foto_mes>= ktest_mes_desde &  foto_mes<= ktest_mes_hasta,  ] )
drealidad  <- fread(karch_realidad)
dtest[ drealidad, on="numero_de_cliente", Usage := i.Usage ]
dtest[ drealidad, on="numero_de_cliente", Predicted := i.Predicted ]
dtest[  , gan :=  ifelse( Predicted==1, 48750, -1250 ) ]


#creo la clase_binaria2   1={ BAJA+2,BAJA+1}  0={CONTINUA}
dataset[ , clase01:= ifelse( clase_ternaria=="CONTINUA", 0, 1 ) ]

#agrego la marca de lo que necesito
#SI hago undersampling de los CONTINUA
vector_azar  <- runif( nrow(dataset) )

dataset[    foto_mes>= kgen_mes_desde  &
            foto_mes<= kgen_mes_hasta  & 
            !( foto_mes %in% kgen_meses_malos ) &
            ( clase01==1 | vector_azar < kgen_subsampling ),
          generacion:= 1L ]  #donde genero el modelo

#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), 
                           c("clase_ternaria","clase01", "generacion", "test", campos_malos) )

#dejo los datos en el formato que necesita LightGBM
dtrain  <- lgb.Dataset( data=    data.matrix(  dataset[ generacion==1 , campos_buenos, with=FALSE]),
                        label=   dataset[ generacion==1, clase01],
                        free_raw_data= TRUE
                      )

rm( "dataset" )   #libero memoria para el dataset
gc()              #garbage collection


#Estos son los parametros que estan fijos 
param_basicos  <- list( objective= "binary",
                        metric= "custom",
                        first_metric_only= TRUE,
                        boost_from_average= TRUE,
                        feature_pre_filter= FALSE,
                        max_depth=  -1,         # -1 significa no limitar,  por ahora lo dejo fijo
                        min_gain_to_split= 0.0, #por ahora, lo dejo fijo
                        lambda_l1= 0.0,         #por ahora, lo dejo fijo
                        lambda_l2= 0.0,         #por ahora, lo dejo fijo
                        max_bin= 31,            #por ahora, lo dejo fijo
                        force_row_wise= TRUE    #para que los alumnos no se atemoricen con tantos warning
                       )


#Estos hiperparametros salieron de la optimizacion bayesiana del script 962
#ganancia  7706250  ( sobre la mitad de 202011 )
#hiperparametros encontrados en la iteracion bayesiana 41 de un total de 100 inteligentes
param_ganadores  <- list( "learning_rate"= 0.0289933062436432, 
                          "feature_fraction"= 0.914142998647527,
                          "min_data_in_leaf"= 367,
                          "num_leaves"= 455,
                          "num_iterations"= 461,
                          "ratio_corte"= 0.0465659156440689
                        )

#junto ambas listas de parametros en una sola
param_completo  <- c( param_basicos, param_ganadores )


#donde voy a guardar los resultados
tb_resultados  <- data.table( semilla= integer(),
                              subsamping= numeric(),
                              oficial= integer(),
                              meseta= integer(),
                              ganancia_Public= numeric(),
                              ganancia_Private= numeric()
                              )

set.seed( 102191 )   #dejo fija esta semilla
CANTIDAD_SEMILLAS  <- 500

#me genero un vector de semilla buscando numeros primos al azar
primos  <- generate_primes(min=100000, max=1000000)  #genero TODOS los numeros primos entre 100k y 1M
ksemillas  <- sample(primos)[ 1:CANTIDAD_SEMILLAS ]   #me quedo con CANTIDAD_SEMILLAS primos al azar
ksemillas  <- c( 999983, ksemillas )

for(  semillita  in  ksemillas )   #itero por las semillas
{
  gc()
  param_completo$seed  <- semillita   #asigno la semilla a esta corrida

  set.seed( semillita )
  #genero el modelo, los hiperparametros son siempre los mismos, la semilla CAMBIA
  modelo  <- lgb.train( data= dtrain,
                        param= param_completo )

  #aplico el modelo a los datos que elegi para testing  202011
  prediccion  <- predict( modelo, data.matrix( dtest[ , campos_buenos, with=FALSE]) )

  #creo una tabla con las probabilidades y la ganancia de ese registro
  tb_meseta  <- as.data.table( list( "prob"=prediccion,  
                                     "Usage"=  dtest[ , Usage],
                                     "gan"=  dtest[ , gan] ) )

  setorder( tb_meseta,  -prob )

  #calculo la ganancia  para el ratio de corte original
  pos_corte  <- as.integer( nrow(dtest)* param_completo$ratio_corte )

  ganancia_Public   <- tb_meseta[  1:pos_corte, sum(ifelse( Usage=="Public",  gan, 0)) ] / 0.3
  ganancia_Private  <- tb_meseta[  1:pos_corte, sum(ifelse( Usage=="Private", gan, 0)) ] / 0.7

  tb_resultados  <- rbind( tb_resultados, list( semillita, 
                                                kgen_subsampling,
                                                1,  #SI es el punto oficial
                                                pos_corte, 
                                                ganancia_Public,
                                                ganancia_Private) )  #agrego la ganancia estandar


  for( punto_meseta  in seq( 5000, 20000, by=500 ) )  #itero desde 5000 a 15000 , de a 500 
  {
    ganancia_Public   <- tb_meseta[  1:punto_meseta, sum(ifelse( Usage=="Public",  gan, 0)) ] / 0.3
    ganancia_Private  <- tb_meseta[  1:punto_meseta, sum(ifelse( Usage=="Private", gan, 0)) ] / 0.7

    tb_resultados  <- rbind( tb_resultados, list( semillita, 
                                                  kgen_subsampling, 
                                                  0,  #No es el punto oficial
                                                  punto_meseta, 
                                                  ganancia_Public,
                                                  ganancia_Private) )  #agrego la ganancia estandar
  }

  #en cada iteracion GRABO
  fwrite(  tb_resultados,
           file= kresultados,
           sep= "\t" )

}

