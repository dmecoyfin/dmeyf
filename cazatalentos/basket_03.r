require("data.table")

set.seed( 102191 )

#calcula cuantos encestes logra un jugador con indice de enceste prob que hace qyt tiros libres
ftirar <- function( prob, qty )
{
  return(  sum( runif(qty) < prob ) )
}

mejor     <-  0.7
peloton   <-  ( 501:599 ) / 1000
jugadores <-  c( mejor, peloton )

#veo que tiene el vector
jugadores


for( i in 1:10 )
{
   vaciertos <- mapply( ftirar, jugadores, 10 )  #cada jugador tira 10 tiros libres

   mejor <- which.max( vaciertos )

   aciertos_torneo <-  vaciertos[ mejor ]

   aciertos_segunda <- ftirar( jugadores[mejor], 10 )

   cat( aciertos_torneo, aciertos_segunda, "\n" )
}