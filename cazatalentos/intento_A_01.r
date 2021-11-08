#Intento de Solucion del desafio  15k
#que NO logra solucionarlo, una que falta una idea fundamental, una chispa, un Momento Eureka
#pero crea estructura sobre la cual trabajar

#limpio la memoria
rm( list=ls() )
gc()

require("data.table")

ftirar <- function( prob, qty )
{
  return(  sum( runif(qty) < prob ) )
}


#variables globales que usan las funciones gimnasio_xxxx
GLOBAL_jugadores <- c()
GLOBAL_tiros_total  <- 0

#Crea el juego
#a cada jugador se le pone un numero de 1 a 100 en la espalda
#debajo de ese numero esta el indice_de_enceste  que NO puede ser visto por el cazatalentos
gimnasio_init  <- function() 
{
  GLOBAL_jugadores  <<-  sample( c( (501:599 ) / 1000 , 0.7 ) )
  GLOBAL_tiros_total  <<- 0
}


#se le pasa un vector con los IDs de los jugadores y la cantidad de tiros a realizar
#devuelve en un vector cuantos aciertos tuvo cada jugador
gimnasio_tirar  <- function(  pids,  pcantidad )
{
  GLOBAL_tiros_total  <<-  GLOBAL_tiros_total + length( pids )*pcantidad
  res  <- mapply(  ftirar, GLOBAL_jugadores[pids], pcantidad )

  return( res )
}


#El cazatalentos decide a que jugador llevarse
#devuelve la cantidad de tiros libres y si le acerto al verdadero_mejor o no
gimnasio_veredicto  <- function( pid )
{
  return( list("tiros_total"= GLOBAL_tiros_total, 
               "acierto"=     as.integer( GLOBAL_jugadores[pid]==0.7) ))
}
#------------------------------------------------------------------------------


#Estrategia
#En la primer ronda se hace tirar 90 tiros libres a cada uno de los 100 jugadores ( se gastan 90000 tiros )
#Se eligen a la mejor mitad  ( se descarta a la peor mitad )
#En la segunda ronda, a la mejor mitad de la anterior ronda se los hace tirar 400 tiros a cada uno
#Se elige el mejor jugador de la segunda ronda

set.seed( 102191 )

#inicializo el juego
gimnasio_init()

#Esta el la planilla del cazatalentos
planilla_cazatalentos  <- data.table(  "id"=1:100 )


#Ronda 1  ------------------------------------------------------
#tiran los 100 jugadores es decir 1:100  90 tiros libres cada uno
ids_juegan1  <- 1:100   #los jugadores que participan en la ronda,

planilla_cazatalentos[ ids_juegan1,  tiros1 := 90 ]  #registro en la planilla que tiran 90 tiros

#Hago que tiren
resultado1  <- gimnasio_tirar( ids_juegan1, 90)
planilla_cazatalentos[ ids_juegan1,  aciertos1 := resultado1 ]  #registro en la planilla


#Ronda 2 -------------------------------------------------------
#los mejores 40 jugadores tiran 400 tiros cada uno
mediana  <- planilla_cazatalentos[ ids_juegan1, median(aciertos1) ]
ids_juegan2  <- planilla_cazatalentos[ ids_juegan1 ][aciertos1 >= mediana, id ]

planilla_cazatalentos[ ids_juegan2,  tiros2 := 400 ]  #registro en la planilla que tiran 400 tiros
resultado2  <- gimnasio_tirar( ids_juegan2, 400)
planilla_cazatalentos[ ids_juegan2,  aciertos2 := resultado2 ]  #registro en la planilla

#El cazatalentos toma una decision, elige al que mas aciertos tuvo en la ronda2
pos_mejor <-  planilla_cazatalentos[ , which.max(aciertos2) ]
id_mejor  <-  planilla_cazatalentos[ pos_mejor, id ]

#Finalmente, la hora de la verdadero_mejor
#Termino el juego
veredicto  <- gimnasio_veredicto( id_mejor )

veredicto


#El veredicto da que la estrategia seguida por el cazatalentos fue exitosa para este caso
#Le acerto al verdadero_mejor

#En el siguiente script veremos de hacer una Estimacion Montecarlo
#De 10000 veces que el entrenador sigue esta estrategia, cuantas realmente le acierta
