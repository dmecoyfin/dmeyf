#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")


#cargo los datasets que voy a comparar
setwd("~/buckets/b1/crudoB/" )  #establezco la carpeta donde voy a trabajar


datasetA  <- fread( "./datasetsOri/paquete_premium_202009.csv" )
datasetB  <- fread( "./datasetsOri/paquete_premium_202011.csv" )

#los campos sobre los que voy a trabajar
campos_buenos <-  setdiff(  colnames( datasetA),  c("numero_de_cliente","foto_mes","clase_ternaria" ) )


pdf("./work/data_delta_01.pdf")

for( campo in  campos_buenos )
{
  cat( campo, "  " )

  tbl  <- datasetA[ , c("numero_de_cliente", campo),   with=FALSE ]
  tbl[  datasetB, on="numero_de_cliente",  futuro := get(paste0("i.",campo)) ]
  tbl[ , delta :=  futuro - get(campo)  ]

  drift  <- density( tbl[ !is.na(delta), delta ] , kernel="gaussian", na.rm=TRUE)

  qdrift  <- quantile(  tbl[ , delta ] , prob= c(0.05, 0.95), na.rm=TRUE )

  a1  <- qdrift[[1]]
  a2  <- qdrift[[2]]


  plot(drift, 
       col="blue",
       main= paste0("DELTA    ",  campo),
       xlim= c(a1, a2),
       lwd= 3
      )
 
  abline( v=0, col="darkgreen" )

  legend(  "topright",  
           legend=c("A", "B"),
           col=c("blue", "darkgreen"), lty=c(1,2))

}

dev.off()  #dejo de graficar


