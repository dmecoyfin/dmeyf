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


#Sobre el mismo experimento
#Deberia dar que es lo mismo !
wilcox.test(  resultados_testing1[ oficial==1, ganancia ][  1:100],
              resultados_testing1[ oficial==1, ganancia ][101:200] )

wilcox.test(  resultados_testing1[ oficial==1, ganancia ][ 1:10],
              resultados_testing1[ oficial==1, ganancia ][11:20] )


#Sobre el experimento 1 y el experimento 2
#Deberia dar que son distintos
wilcox.test(  resultados_testing1[ oficial==1, ganancia ][  1:100],
              resultados_testing2[ oficial==1, ganancia ][101:200] )


#Hay solo 10 datos, pero debería darse cuenta que son distintos
resultados_testing1[ oficial==1, ganancia ][ 1:10]
resultados_testing2[ oficial==1, ganancia ][11:20]

wilcox.test(  resultados_testing1[ oficial==1, ganancia ][ 1:10],
              resultados_testing2[ oficial==1, ganancia ][11:20] )


#Hay muy pocos datos, solo 5, se va a confundir
resultados_testing1[ oficial==1, ganancia ][ 1:5]
resultados_testing2[ oficial==1, ganancia ][11:15]

wilcox.test(  resultados_testing1[ oficial==1, ganancia ][ 1:5],
              resultados_testing2[ oficial==1, ganancia ][11:15] )



#ALTERNATIVA  Test de Student
t.test( resultados_testing1[ oficial==1, ganancia ][ 1:5],
        resultados_testing2[ oficial==1, ganancia ][11:15],
        alternative = "less")   #el primero es menor al segundo

t.test( resultados_testing1[ oficial==1, ganancia ][ 1:100],
        resultados_testing2[ oficial==1, ganancia ][101:200],
        alternative = "less")
