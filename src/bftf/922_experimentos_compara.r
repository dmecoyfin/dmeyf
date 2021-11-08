#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
setwd("~/buckets/b1/")


corrida <- list()

corrida$arch_testing1  <- "./work/E5013_981_epic.txt"
corrida$arch_testing2  <- "./work/E5019_982_epic.txt"

corrida$arch_kaggle1  <- "./work/E5016_991_epic.txt"
corrida$arch_kaggle2  <- "./work/E5018_992_epic.txt"
corrida$arch_salida   <- "./work/900_dos_modelos.pdf"

#leo los datasets
resultados_testing1  <- fread( corrida$arch_testing1 )
resultados_testing2  <- fread( corrida$arch_testing2 )

resultados_kaggle1  <- fread( corrida$arch_kaggle1 )
resultados_kaggle2  <- fread( corrida$arch_kaggle2 )


#divido por un millon para visualizar mas facil
resultados_testing1[   , ganancia  := ganancia/1e6 ]
resultados_testing2[   , ganancia  := ganancia/1e6 ]

resultados_kaggle1[   , ganancia_Public  := ganancia_Public/1e6 ]
resultados_kaggle1[   , ganancia_Private := ganancia_Private/1e6 ]
resultados_kaggle2[   , ganancia_Public  := ganancia_Public/1e6 ]
resultados_kaggle2[   , ganancia_Private := ganancia_Private/1e6 ]

gan_min_testing  <- pmin( resultados_testing1[  oficial==1, min(ganancia) ],
                          resultados_testing2[  oficial==1, min(ganancia) ] )

gan_min_Public   <- pmin( resultados_kaggle1[  oficial==1, min(ganancia_Public) ],
                          resultados_kaggle2[  oficial==1, min(ganancia_Public) ] )

gan_min_Private  <- pmin( resultados_kaggle1[  oficial==1, min(ganancia_Private) ],
                          resultados_kaggle2[  oficial==1, min(ganancia_Private) ] )


gan_max_testing  <- pmin( resultados_testing1[  oficial==1, max(ganancia) ],
                          resultados_testing2[  oficial==1, max(ganancia) ] )

gan_max_Public   <- pmax( resultados_kaggle1[  oficial==1, max(ganancia_Public) ],
                          resultados_kaggle2[  oficial==1, max(ganancia_Public) ] )

gan_max_Private  <- pmax( resultados_kaggle1[  oficial==1, max(ganancia_Private) ],
                          resultados_kaggle2[  oficial==1, max(ganancia_Private) ] )


puntos_cantidad  <- length(resultados_kaggle2[  oficial==1, ganancia_Private ])

los70  <- sample( 1:puntos_cantidad, 70 )


mayores  <-  sum( resultados_kaggle2[  oficial==1, ganancia_Private ] >  resultados_kaggle1[  oficial==1, ganancia_Private ][1:200] )

superioridad  <-   mayores / 200

#----------------------------------------------------------
#Comienzo la impresion
pdf( corrida$arch_salida )   #escribo los resultados a un pdf


#grafico  Private vs Public  todos los puntos
plot( x= resultados_kaggle1[  oficial==1, ganancia_Public ][1:70],
      y= resultados_kaggle1[  oficial==1, ganancia_Private ][1:70],
      main= "Ganancias Private vs Public",
      xlab= "Ganancia PUBLIC",
      ylab= "Ganancia PRIVATE",
      xlim= c(gan_min_Public, gan_max_Public ),
      ylim= c(gan_min_Private, gan_max_Private ),
      col= "blue",
      pch= 15
    )

points( x= resultados_kaggle2[  oficial==1, ganancia_Public ][1:70],
        y= resultados_kaggle2[  oficial==1, ganancia_Private ][1:70],
        col= "red",
        pch= 15 
      )
      
#dibujo las medias
abline( v= mean(resultados_kaggle1[  oficial==1, ganancia_Public ]) , col="blue" )
abline( h= mean(resultados_kaggle1[  oficial==1, ganancia_Private ]) , col="blue" )

abline( v= mean(resultados_kaggle2[  oficial==1, ganancia_Public ]) , col="red" )
abline( h= mean(resultados_kaggle2[  oficial==1, ganancia_Private ]) , col="red" )


legend("topleft", 
       legend= c("Comun", "lag1+delta"),
       col= c("blue", "red"),
       lty= c(1,1),
       pch= c(20,20), 
      )

text( x= 25.7 ,
      y= 20.5 ,
      labels= paste0( "lag1+delta superador el ", superioridad ) )

#----------------------------------------------------------


#grafico  Private vs Testing  todos los puntos
plot( x= resultados_testing1[  oficial==1, ganancia ][1:70],
      y= resultados_kaggle1[  oficial==1, ganancia_Private ][1:70],
      main= "Ganancias Private vs Testing",
      xlab= "Ganancia TESTING",
      ylab= "Ganancia PRIVATE",
      xlim= c(gan_min_testing, gan_max_testing ),
      ylim= c(gan_min_Private, gan_max_Private ),
      col= "blue",
      pch= 15
    )

points( x= resultados_testing2[ oficial==1, ganancia ][1:70],
        y= resultados_kaggle2[  oficial==1, ganancia_Private ][1:70],
        col= "red",
        pch= 15 
      )

#dibujo las medias
abline( v= mean(resultados_testing1[  oficial==1, ganancia ]) , col="blue" )
abline( h= mean(resultados_kaggle1[  oficial==1, ganancia_Private ]) , col="blue" )

abline( v= mean(resultados_testing2[  oficial==1, ganancia ]) , col="red" )
abline( h= mean(resultados_kaggle2[  oficial==1, ganancia_Private ]) , col="red" )


legend("topleft", 
       legend= c("Comun", "lag1+delta"),
       col= c("blue", "red"),
       lty= c(1,1),
       pch= c(20,20), 
      )

text( x= 12.5 ,
      y= 20.5 ,
      labels= paste0( "lag1+delta superador el ", superioridad ) )


#----------------------------------------------------------

dev.off()  #dejo de graficar

