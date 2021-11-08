# Mi pequeño frankenstein
# Un stacking básico! que ande, no que sea lindo. 

rm( list=ls() )
gc()

# Levatamos las librerías
library("data.table")
library("lightgbm")

# Levantamos los datos

setwd("/home/rstudio/repo/datasetsOri/")
# Poner el dataset del FE
ds_fe <- "paquete_premium_202009.csv"
ds <- fread(ds_fe, showProgress = FALSE)

# Armamos la clase
clase_binaria <- ifelse(ds$clase_ternaria == "BAJA+2", 1, 0)
ds$clase_ternaria <- NULL

######## 
# Empezamos con el stacking
########

# Van a:
# 1- Todos usar el mismos ds_fe
# 2- Todos van a ser LGBM con distintos parámetros

params_gbdt <- list( objective= "binary", max_bin= 15, min_data_in_leaf= 4000, learning_rate= 0.05 )
params_rf <- list(objective = "binary",  boosting_type = "rf", bagging_freq = 1, bagging_fraction = 0.66, feature_fraction = 0.4)
params_goss <- list(objective = "binary", learning_rate = 0.05, top_rate = 0.5, other_rate = 0.1, feature_fraction_bynode = 0.2, boosting_type = "goss")

# Los folds son para siempre! 
folds <- splitTools::create_folds(clase_binaria, k = 5, seed = 17)

### Para un solo modelo creamos una variables con datos "insesgados" solo con validación
# 
# 
# validation <- numeric(length(clase_binaria))
# for (f in folds) {
#  ds_train  <- lgb.Dataset( data=  data.matrix(ds[f]), label= clase_binaria[f] )
#  m <- lgb.train(ds_train, params = params_gbdt, verbose = -1)
#  validation[-f] <- predict(m,data.matrix(ds[-f]))
# }

## Armar una función donde, le paso los parámetros de mi lgbm, y me devuelva un vector con los scores de validación

val_scores_lgbm <- function(datos, target, params, folds) {
  
  validation <- numeric(length(target))
  for (f in folds) {
   # usamos 4 folds para entranar
   ds_train  <- lgb.Dataset( data=  data.matrix(datos[f]), label= target[f] )
   m <- lgb.train(ds_train, params = params, verbose = -1)
   # usamos el restante para generar el valor 
   validation[-f] <- predict(m,data.matrix(datos[-f]))
  }
  validation
}

m1_scores <- val_scores_lgbm(ds, clase_binaria, params_gbdt, folds)
m2_scores <- val_scores_lgbm(ds, clase_binaria, params_rf, folds)
m3_scores <- val_scores_lgbm(ds, clase_binaria, params_goss, folds)

# Cosas sucias que uno deja en el código! mal Ale, mal.
# cor(m1_scores, m2_scores)
# cor(m3_scores, m2_scores)
# cor(m1_scores, m3_scores)

### Como los metamodelamos?????

## Juntamos las variables al dataset, o no... who knows
# ds_meta <- cbind(m1_scores, m2_scores, m3_scores) 
ds_meta <- cbind(ds, m1_scores, m2_scores, m3_scores) 
# fwrite ( .... )


## tenemos un nuevo dataset como lo teníamos antes!
## a efectos prácticos hacemos un lgbm sencillo, pero no es lo que van a hacer ustedes!
params_simple <- list(objective = "binary")
ds_train_mm  <- lgb.Dataset( data= data.matrix(ds_meta), label= clase_binaria )
mm <- lgb.train(ds_train_mm, params = params_simple, verbose = -1)
# View(lgb.importance(mm))

### Como lo implementamos
# Construir los scores posta posta!

# Poner el dataset del FE para nov
ds_fe_nov <- "paquete_premium_202011.csv"
ds_nov <- fread(paste0(ds_fe_nov), showProgress = FALSE)
ds_nov$clase_ternaria <- NULL

ds_train  <- lgb.Dataset( data=  data.matrix(ds), label= clase_binaria )
m1 <- lgb.train(ds_train, params = params_gbdt, verbose = -1)
m2 <- lgb.train(ds_train, params = params_rf, verbose = -1)
m3 <- lgb.train(ds_train, params = params_goss, verbose = -1)

m1_scores <- predict(m1,data.matrix(ds_nov))
m2_scores <- predict(m2,data.matrix(ds_nov))
m3_scores <- predict(m3,data.matrix(ds_nov))

ds_nov <- cbind(ds_nov,m1_scores, m2_scores,m3_scores)

scores_finales <- predict(mm,data.matrix(ds_nov))



