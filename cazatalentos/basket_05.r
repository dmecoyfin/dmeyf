require("data.table")

set.seed( 102191 )

#calcula cuantos encestes logra un jugador con indice de enceste prob que hace qyt tiros libres
ftirar <- function( prob, qty )
{
  return(  sum( runif(qty) < prob ) )
}


jugadores  <- rep( 0.7, 100 )  #jugadores identicos

diferencias  <- c()

for( i in 1:9999 )
{
   vaciertos  <- mapply( ftirar, jugadores, 100 )  #cada jugador tira 100 tiros libres

   mejor  <- which.max( vaciertos )

   aciertos_torneo  <- vaciertos[ mejor ]

   aciertos_segunda  <- ftirar( jugadores[mejor], 100 )

   diferencias  <- c( diferencias, aciertos_torneo - aciertos_segunda )
}


mean( diferencias )