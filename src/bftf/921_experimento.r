#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
setwd("~/buckets/b1/")

script  <- 1

corrida <- list()

if( script==1 )
{
  corrida$arch_testing  <- "./work/E5013_981_epic.txt"
  corrida$arch_kaggle   <- "./work/E5016_991_epic.txt"
  corrida$arch_salida   <- "./work/900_dataset_comun.pdf"
  corrida$color_base    <- "blue"
}

if( script==2 )
{
  corrida$arch_testing  <- "./work/E5019_982_epic.txt"
  corrida$arch_kaggle   <- "./work/E5018_992_epic.txt"
  corrida$arch_salida   <- "./work/900_dataset_lagdelta.pdf"
  corrida$color_base    <- "red"
}


#leo los datasets
resultados_testing  <- fread(corrida$arch_testing)  #resultados del script  dataset simple
resultados_kaggle   <- fread(corrida$arch_kaggle)  #resultados del script  dataset lags y delta lags

#acomodo problemas en la corrida
resultados_testing  <-  resultados_testing[ semilla %in% unique( resultados_kaggle$semilla) ]
resultados_testing  <- unique( resultados_testing )

#divido por un millon para visualizar mas facil
resultados_testing[  , ganancia := ganancia/1e6 ]
resultados_kaggle[   , ganancia_Public  := ganancia_Public/1e6 ]
resultados_kaggle[   , ganancia_Private := ganancia_Private/1e6 ]

densidad_testing  <- density( resultados_testing[ oficial==1, ganancia ] )
densidad_Public   <- density( resultados_kaggle[  oficial==1, ganancia_Public ] )
densidad_Private  <- density( resultados_kaggle[  oficial==1, ganancia_Private ] )

gan_menor  <- pmin( resultados_testing[ oficial==1, min(ganancia) ],
                    resultados_kaggle[  oficial==1, min(ganancia_Public) ],
                    resultados_kaggle[  oficial==1, min(ganancia_Private) ] )

gan_mayor  <- pmax( resultados_testing[ oficial==1, max(ganancia) ],
                    resultados_kaggle[  oficial==1, max(ganancia_Public) ],
                    resultados_kaggle[  oficial==1, max(ganancia_Private) ] )

puntos_cantidad  <- length(resultados_kaggle[  oficial==1, ganancia_Private ])

los70  <- sample( 1:puntos_cantidad, 70 )


#----------------------------------------------------------
#Comienzo la impresion
pdf( corrida$arch_salida )   #escribo los resultados a un pdf



#----------------------------------------------------------
#Grafico las funciones de densidad de probabilidad

plot( densidad_testing,
      main= "Densidades 500 puntos",
      xlab= "Ganancia",
      ylab= "Probabilidad",
      xlim= c(gan_menor, gan_mayor),
      col= "darkgreen"
    )

lines( densidad_Public,  col= "blue" )
lines( densidad_Private, col= "red" )


legend("topright", 
       legend= c("Testing", "Public", "Private"),
       col= c("darkgreen", "blue", "red"),
       lty= c(1,1,1),
       pch= c(20,20,20), 
      )

#----------------------------------------------------------

#grafico  Private vs Public  todos los puntos
plot( x= resultados_kaggle[  oficial==1, ganancia_Public ],
      y= resultados_kaggle[  oficial==1, ganancia_Private ],
      main= paste0("Ganancias Private vs Public puntos " ,  puntos_cantidad),
      xlab= "Ganancia PUBLIC",
      ylab= "Ganancia PRIVATE",
      col= corrida$color_base,
      pch= 15
    )

#dibujo las medias
abline( v= mean(resultados_kaggle[  oficial==1, ganancia_Public ]) , col="darkgreen" )
abline( h= mean(resultados_kaggle[  oficial==1, ganancia_Private ]) , col="darkgreen" )

#----------------------------------------------------------

#grafico  Private vs Public  70 puntos

plot( x= resultados_kaggle[  oficial==1, ganancia_Public ][los70],
      y= resultados_kaggle[  oficial==1, ganancia_Private ][los70],
      main= "Ganancias Private vs Public  70 puntos",
      xlab= "Ganancia PUBLIC",
      ylab= "Ganancia PRIVATE",
      col= corrida$color_base,
      pch= 15
    )

#dibujo las medias
abline( v= mean(resultados_kaggle[  oficial==1, ganancia_Public ]) , col="darkgreen" )
abline( h= mean(resultados_kaggle[  oficial==1, ganancia_Private ]) , col="darkgreen" )

#----------------------------------------------------------

#grafico  Private vs Testing  todos los puntos
plot( x= resultados_testing[  oficial==1, ganancia ],
      y= resultados_kaggle[  oficial==1, ganancia_Private ],
      main= paste0("Ganancias Private vs Testing puntos " ,  puntos_cantidad),
      xlab= "Ganancia TESTING",
      ylab= "Ganancia PRIVATE",
      col= corrida$color_base,
      pch= 15
    )

#dibujo las medias
abline( v= mean(resultados_testing[ oficial==1, ganancia ]) , col="darkgreen" )
abline( h= mean(resultados_kaggle[  oficial==1, ganancia_Private ]) , col="darkgreen" )

#----------------------------------------------------------


#grafico  Private vs Testing  70 puntos
plot( x= resultados_testing[  oficial==1, ganancia ][los70],
      y= resultados_kaggle[  oficial==1, ganancia_Private ][los70],
      main= "Ganancias Private vs Testing  70 puntos",
      xlab= "Ganancia TESTING",
      ylab= "Ganancia PRIVATE",
      col= corrida$color_base,
      pch= 15
    )

#dibujo las medias
abline( v= mean(resultados_testing[ oficial==1, ganancia ]) , col="darkgreen" )
abline( h= mean(resultados_kaggle[  oficial==1, ganancia_Private ]) , col="darkgreen" )

#----------------------------------------------------------


#grafico  Public vs Testing  todos los puntos
plot( x= resultados_testing[  oficial==1, ganancia ],
      y= resultados_kaggle[  oficial==1, ganancia_Public ],
      main= paste0("Ganancias Public vs Testing puntos " ,  puntos_cantidad),
      xlab= "Ganancia TESTING",
      ylab= "Ganancia PUBLIC",
      col= corrida$color_base,
      pch= 15
    )

#dibujo las medias
abline( v= mean(resultados_testing[ oficial==1, ganancia ]) , col="darkgreen" )
abline( h= mean(resultados_kaggle[  oficial==1, ganancia_Public ]) , col="darkgreen" )

#----------------------------------------------------------

#grafico  Public vs Testing  70 puntos
plot( x= resultados_testing[  oficial==1, ganancia ][los70],
      y= resultados_kaggle[  oficial==1, ganancia_Public ][los70],
      main= "Ganancias Public vs Testing  70 puntos",
      xlab= "Ganancia TESTING",
      ylab= "Ganancia PUBLIC",
      col= corrida$color_base,
      pch= 15
    )

#dibujo las medias
abline( v= mean(resultados_testing[ oficial==1, ganancia ]) , col="darkgreen" )
abline( h= mean(resultados_kaggle[  oficial==1, ganancia_Public ]) , col="darkgreen" )

#----------------------------------------------------------

dev.off()  #dejo de graficar


wilcox.test(  resultados_testing1[ oficial==1, ganancia ][ 1:10],
              resultados_testing1[ oficial==1, ganancia ][11:20],
              paired= TRUE )

