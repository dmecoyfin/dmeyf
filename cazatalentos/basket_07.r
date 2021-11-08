#intencionalmente el mejor jugador va al final de la lista de jugadores
#porque la funcion which.max() de R hace trampa
#si hay un empate ( dos máximos) se queda con el que esta primero en el vector

require("data.table")

set.seed( 102191 )

#calcula cuantos encestes logra un jugador con indice de enceste prob que hace qyt tiros libres
ftirar <- function( prob, qty )
{
  return(  sum( runif(qty) < prob ) )
}

#defino los jugadores
mejor      <-  0.7
peloton    <-  ( 501:599 ) / 1000
jugadores  <-  c( peloton, mejor ) #intencionalmente el mejor esta al final

#veo que tiene el vector
jugadores

#hago que los 100 jugadores tiren 10 veces cada uno
mapply(  ftirar, jugadores, 10 )


for(  tiros_libres  in c(10, 20, 50, 100, 200, 300, 400, 415, 500, 600, 700, 1000 ) )
{

  primero_ganador  <- 0

  for( i in 1:10000 )  #diez mil experimentos
  {
    vaciertos <- mapply( ftirar, jugadores, tiros_libres ) 
    mejor  <- which.max( vaciertos )
    if( mejor == 100 )  primero_ganador <- primero_ganador + 1
  }

  cat( tiros_libres, primero_ganador/10000, "\n" )

}

