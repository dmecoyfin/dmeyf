require("data.table")

set.seed( 102191 )

#calcula cuantos encestes logra un jugador con indice de enceste prob que hace qyt tiros libres
ftirar <- function( prob, qty )
{
  return(  sum( runif(qty) < prob ) )
}

#defino los jugadores
mejor      <- 0.7
peloton    <- ( 501:599 ) / 1000
jugadores  <- c( mejor, peloton )

#veo que tiene el vector
jugadores

#hago que los 100 jugadores tiren 10 veces cada uno
mapply(  ftirar, jugadores, 10 )

primero_ganador <- 0

for( i in 1:10000 )  #diez mil experimentos
{
   vaciertos  <- mapply( ftirar, jugadores, 10 )  #10 tiros libres cada jugador
   
   mejor  <- which.max( vaciertos )
   if( mejor == 1 )  primero_ganador <- primero_ganador + 1
}

print(  primero_ganador )
