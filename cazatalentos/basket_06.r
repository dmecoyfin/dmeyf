require("data.table")

set.seed( 102191 )

#calcula cuantos encestes logra un jugador con indice de enceste prob que hace qyt tiros libres
ftirar <- function( prob, qty )
{
  return(  sum( runif(qty) < prob ) )
}

jordan   <- 0.85
peloton  <- rep( 0.6, 99 )  #jugadores identicos

jugadores  <- c( jordan, peloton )

for( i in 1:10 )
{
   vaciertos  <- mapply( ftirar, jugadores, 100 )  #cada jugador tira 100 tiros libres

   mejor  <- which.max( vaciertos )

   aciertos_torneo  <-  vaciertos[ mejor ]

   aciertos_segunda  <- ftirar( jugadores[mejor], 100 )

   cat( mejor, aciertos_torneo, aciertos_segunda, "\n" )
}