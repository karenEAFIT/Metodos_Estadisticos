#Carga de información

library(kableExtra) 
library(readxl)
library(tidyverse)
datosp <- read_excel("Datos_definitivos.xlsx")
datosp <- datosp[,c("nit","periodo","costos","Proporcion","liquidez","PIB_Variacion","variacion_tas_int","variacion_cta_corr_nal","CHV_variacion","EC_Variacion","FIVI_Variacion","ICCV_Variacion","IVP_Variacion","IPVN_Variacion","Inflacion_Variacion","TRM_Variación","Tasa_Desempleo","IPC")]

#Exploración de datos

bxplot_gastos_dif = boxplot(datosp$Proporcion~datosp$periodo , 
                            main = "Proporción vs periodo",
                            xlab = "Periodos",
                            ylab = "Proporción",
                            boxwex = 0.5,col="dodgerblue")


#Outliers
impute_outliers <- function(x, removeNA = TRUE){
  quantiles <- quantile(x, c(0.05, 0.95), na.rm = removeNA)
  x[x<quantiles[1]] <- mean(x, na.rm = removeNA)
  x[x>quantiles[2]] <- median(x, na.rm = removeNA)
  x
}

datos_16   <- datosp %>%             # a partir de out.predict.garch  
  filter(datosp$periodo==2016)


out16 <- datos_16 %>%             # a partir de out.predict.garch  
  filter(datos_16$Proporcion <= quantile(datos_16$Proporcion, c(0.10)))


datos_17   <- datosp %>%             # a partir de out.predict.garch  
  filter(datosp$periodo==2017)

out17 <- datos_17 %>%             # a partir de out.predict.garch  
  filter(datos_17$Proporcion <= quantile(datos_17$Proporcion, c(0.10)))


datos_18   <- datosp %>%             # a partir de out.predict.garch  
  filter(datosp$periodo==2018)


out18 <- datos_18 %>%             # a partir de out.predict.garch  
  filter(datos_18$Proporcion <= quantile(datos_18$Proporcion, c(0.10)))


out16 = t(out16$nit)
out17 = t(out17$nit)
out18 = t(out18$nit)

outliers = distinct(as.data.frame(t(cbind(out16,out17,out18))))
names(outliers) = ("Nit_Out")


# Box plot sin outliers

`%ni%` <- Negate(`%in%`)
out = datosp$nit%ni%outliers$Nit_Out
datos_sop = datosp[out,]
boxplot(datos_sop$Proporcion~datos_sop$periodo,
        main = "Proporción vs periodo sin outliers",
        xlab = "Periodos",
        ylab = "Proporción",
        boxwex = 0.5,col="dodgerblue3")

# Se llevan a factor las variables periodo y nit

datos_sop$periodo = factor(datos_sop$periodo,
                           levels = c("2016","2017","2018"),
                           labels = c(1,2,3))
datos_sop$nit = factor(datos_sop$nit)


# Division entre test y entrenamiento

library(caTools)
set.seed(4)
nite = as.data.frame(unique(datos_sop$nit))
names(nite) = c("nit")
split = sample.split(nite$nit, SplitRatio = 0.8)
training_set_p = subset(nite, split == TRUE)
training_set_p = datos_sop$nit%in%training_set_p$nit
training_set_p = datos_sop[training_set_p,]
testing_set_p = subset(nite, split == FALSE)
testing_set_p = datos_sop$nit%in%testing_set_p$nit
testing_set_p = datos_sop[testing_set_p,]
periodo_test = c(3)
testing_p = testing_set_p$periodo%in%periodo_test
testing_set_ = testing_set_p[testing_p,]
periodo_train = c(1,2)
train_p = testing_set_p$periodo%in%periodo_train
train_p = testing_set_p[train_p,]
training_set_p = rbind(training_set_p,train_p)


cantidad <- as.data.frame(cbind(12,50))
names(cantidad) = c("Cantidad empresas testeo","Cantidad empresas entrenamiento")

# Modelamiento

library(nlme)
modelo1 <- lme(Proporcion ~ PIB_Variacion+Inflacion_Variacion, data = training_set_p, random = ~1 | nit )
modelo2 <- lme(log(Proporcion) ~ Tasa_Desempleo+TRM_Variación+liquidez, data = training_set_p, random = ~1 | nit )
modelo3 <- lme(Proporcion ~ Tasa_Desempleo+TRM_Variación+liquidez, data = training_set_p, random = ~1 | nit )
modelo4 <- lme(Proporcion ~ PIB_Variacion+Tasa_Desempleo, data = training_set_p, random = ~1 | nit )
modelo5 <- lme(Proporcion ~ Tasa_Desempleo+  IPC +liquidez, data = training_set_p, random = ~1 | nit )
modelo6 <- lme(Proporcion ~ PIB_Variacion+ liquidez+Tasa_Desempleo, data = training_set_p, random = ~1 | nit )
Modelo <-  rbind("Modelo1","Modelo2","Modelo3","Modelo4","Modelo5","Modelo6")

AIC_Modelo <- rbind(round(AIC(modelo1),2),
                    round(AIC(modelo2),2),
                    round(AIC(modelo3),2),
                    round(AIC(modelo4),2),
                    round(AIC(modelo5),2),
                    round(AIC(modelo6),2))

BIC_Modelo <- rbind(round(BIC(modelo1),2),
                    round(BIC(modelo2),2),
                    round(BIC(modelo3),2),
                    round(BIC(modelo4),2),
                    round(BIC(modelo5),2),
                    round(BIC(modelo6),2))

estructura<-rbind("Proporcion ~ PIB_Variacion+Inflacion_Variacion+(1|nit)",
                  "log(Proporcion) ~ Tasa_Desempleo+TRM_Variación+liquidez+(1|nit)",
                  "Proporcion ~ Tasa_Desempleo+TRM_Variación+liquidez+(1|nit)",
                  "Proporcion ~ PIB_Variacion+Tasa_Desempleo+(1|nit)",
                  "Proporcion ~ Tasa_Desempleo+ IPC +liquidez+(1|nit)",
                  "Proporcion ~ PIB_Variacion+ liquidez+Tasa_Desempleo+(1|nit)")

metricas <- as.data.frame(cbind(Modelo,estructura,AIC_Modelo,BIC_Modelo))
names(metricas) = c("Modelo","Estructura","AIC","BIC")


# Predicción

library(nlme)
library(MLmetrics)
prediccion1	=	predict(modelo1,testing_set_,level =1,allow.new.levels=T)
predichos1	=	cbind(testing_set_$Proporcion,as.data.frame(prediccion1))
names(predichos1)	=	c("Datos1","Prediccion1")
prediccion2	=	predict(modelo2,testing_set_,level =1,allow.new.levels=T)
predichos2	=	cbind(testing_set_$Proporcion,as.data.frame(prediccion2))
names(predichos2)	=	c("Datos2","Prediccion2")
prediccion3	=	predict(modelo3,testing_set_,level =1,allow.new.levels=T)
predichos3	=	cbind(testing_set_$Proporcion,as.data.frame(prediccion3))
names(predichos3)	=	c("Datos3","Prediccion3")
prediccion4	=	predict(modelo4,testing_set_,level =1,allow.new.levels=T)
predichos4	=	cbind(testing_set_$Proporcion,as.data.frame(prediccion4))
names(predichos4)	=	c("Datos4","Prediccion4")
prediccion5	=	predict(modelo5,testing_set_,level =1,allow.new.levels=T)
predichos5	=	cbind(testing_set_$Proporcion,as.data.frame(prediccion5))
names(predichos5)	=	c("Datos5","Prediccion5")
prediccion6	=	predict(modelo6,testing_set_,level =1,allow.new.levels=T)
predichos6	=	cbind(testing_set_$Proporcion,as.data.frame(prediccion6))
names(predichos6)	=	c("Datos6","Prediccion6")

MSE1<-MSE(predichos1$Prediccion1,predichos1$Datos1)
RMSE1<-RMSE(predichos1$Prediccion1,predichos1$Datos1)
MAPE1<-MAPE(predichos1$Prediccion1,predichos1$Datos1)
MSE2<-MSE(predichos2$Prediccion2,predichos2$Datos2)
RMSE2<-RMSE(predichos2$Prediccion2,predichos2$Datos2)
MAPE2<-MAPE(predichos2$Prediccion2,predichos2$Datos2)
MSE3<-MSE(predichos3$Prediccion3,predichos3$Datos3)
RMSE3<-RMSE(predichos3$Prediccion3,predichos3$Datos3)
MAPE3<-MAPE(predichos3$Prediccion3,predichos3$Datos3)
MSE4<-MSE(predichos4$Prediccion4,predichos4$Datos4)
RMSE4<-RMSE(predichos4$Prediccion4,predichos4$Datos4)
MAPE4<-MAPE(predichos4$Prediccion4,predichos4$Datos4)
MSE5<-MSE(predichos5$Prediccion5,predichos5$Datos5)
RMSE5<-RMSE(predichos5$Prediccion5,predichos5$Datos5)
MAPE5<-MAPE(predichos5$Prediccion5,predichos5$Datos5)
MSE6<-MSE(predichos6$Prediccion6,predichos6$Datos6)
RMSE6<-RMSE(predichos6$Prediccion6,predichos6$Datos6)
MAPE6<-MAPE(predichos6$Prediccion6,predichos6$Datos6)

RMSE <- rbind(round(RMSE1,6),
              round(RMSE2,6),
              round(RMSE3,6),
              round(RMSE4,6),
              round(RMSE5,6),
              round(RMSE6,6))

MSE <- rbind(round(MSE1,6),
             round(MSE2,6),
             round(MSE3,6),
             round(MSE4,6),
             round(MSE5,6),
             round(MSE6,6))

MAPE <- rbind(round(MAPE1,6),
              round(MAPE2,6),
              round(MAPE3,6),
              round(MAPE4,6),
              round(MAPE5,6),
              round(MAPE6,6))

metricas_prediccion <- as.data.frame(cbind(MSE,RMSE,MAPE))
names(metricas_prediccion) = c("MSE","RMSE","MAPE")
metricas_consolidadas <- as.data.frame(cbind(metricas,metricas_prediccion))

#Gráfico del modelo

plot(modelo4,type = c("p", "smooth"))

Fit <- fitted(modelo4)
Res <- residuals(modelo4, type = "normalized") 
par(mfrow = c(2, 2))
plot(Res ~ Fit, xlab = "Fitted values", ylab = "Residuals", main = "Residuals vs. fitted")
abline(h = 0)

hist(Res, main = "Histogram of residuals", xlab = "Residuals")
qqnorm(Res)
qqline(Res)


summary(modelo4)

#Recta de regresión ajustada

plot(modelo4, Proporcion ~ fitted(.) | nit, abline = c(0,1))

## Datos por departamento

library(readxl)
datos <- read_excel("Departamentos.xlsx")

# Escalamiento de variables

datos$pib_dep=scale(datos$pib_dep, center=100000) #PIB departamento
datos$tasa_desemp_dep=scale(datos$tasa_desemp_dep,center=10,5) #tasa desempleo departamento
datos$ipc_anio_dep=scale(datos$ipc_anio_dep,center=2) #IPC anual departamento
datos$ingr_corr_dep=scale(datos$ingr_corr_dep,center=1000000000) # ingresos corrientes por  departamento
datos$gastos_fun_dep=scale(datos$gastos_fun_dep,center=1000000000) # gastos de  funcionamiento departamento
datos$inflacion=scale(datos$inflacion,center=2) # inflación nacional anual
datos$tasa_int_prom=scale(datos$tasa_int_prom) # Tasa de  intermediación bancaria promedio por año
datos$ingresos_nal=scale(datos$ingresos_nal, center=1000000000) #Ingreso nacionales
datos$gastos_nal=scale(datos$gastos_nal, center=1000000000)
datos$TRM=scale(datos$TRM, center=2500)
datos$variacion_pib_dep=scale(datos$variacion_pib_dep, center=1)
datos$intereses_nal=scale(datos$intereses_nal, center=100)
datos$CHV=scale(datos$CHV, center=100000)# Cartera hipotecaria Millones de pesos a precios corrientes
datos$EC=scale(datos$EC, center=1000000)# Concreto pre mezclado métros cúbicos
datos$ECG=scale(datos$ECG, center=1000000)# Cemento Gris toneladas
datos$EC_Variacion=scale(datos$EC_Variacion, center=1) #Variacion concreto pre mezclado
datos$ELIC_UND=scale(datos$ELIC_UND, center=10000) #EDIFICACIÓN LICENCIAS DE CONSTRUCCIÓN - ELIC  Unidades
datos$ELIC_AREA=scale(datos$ELIC_AREA, center=1000000) # Licencias de contronstruccion área metros cuadrados
datos$ICC_Variacion=scale(datos$ICC_Variacion, center=1) #Variacion de indice de costos de la construccion de vivienda
datos$ICCV_Indice=scale(datos$ICCV_Indice, center=1) #Indice de costos de la construccion de vivienda
datos$FIVI_Variacion=scale(datos$FIVI_Variacion, center=1) #Variación vivienda nueva
datos$IVP_Indice=scale(datos$IVP_Indice, center=100) #Indice de Valoracion predial
datos$IPVN_Indice=scale(datos$IPVN_Indice, center=100) #Indice de precios de vivienda nueva
datos$IPP_Manufactura=scale(datos$IPP_Manufactura, center=100) #Indice de precios al productor


#Gráfico de perfiles

library(lattice)
xyplot(proporcion ~ periodo | Departamento , groups = nit, data = datos, type = "l", lty = 1) 


# Identificación de outliers
library(dplyr)
grupos <- group_by(datos, Departamento)
grupo <- summarise(grupos,
                   num = n()
)

library(tidyverse)

Departamentos   <- grupo %>%  
  filter(grupo$num>=12) #Se excluye todo lo mayor a 12 ya que cada nit tiene 3 periodos.

Departamentos = Departamentos$Departamento
Dep = datos$Departamento%in%Departamentos
datos_dep = datos[Dep,]

#Exploración de datos

bxplot_gastos_dif = boxplot(datos_dep$proporcion~datos_dep$Departamento , 
                            main = "Proporcion vs Departamento",
                            xlab = "Periodos",
                            ylab = "Proporcion",
                            boxwex = 0.5,col="blue")


# Outliers

library(tidyverse)

impute_outliers <- function(x, removeNA = TRUE){
  quantiles <- quantile(x, c(0.05, 0.95), na.rm = removeNA)
  x[x<quantiles[1]] <- mean(x, na.rm = removeNA)
  x[x>quantiles[2]] <- median(x, na.rm = removeNA)
  x
}

datos_16d   <- datos_dep %>%             # a partir de out.predict.garch  
  filter(datos_dep$periodo==2016)


out16d <- datos_16d %>%             # a partir de out.predict.garch  
  filter(datos_16d$proporcion <= quantile(datos_16d$proporcion, c(0.10)))


datos_17d   <- datos_dep %>%             # a partir de out.predict.garch  
  filter(datos_dep$periodo==2017)


out17d <- datos_17d %>%             # a partir de out.predict.garch  
  filter(datos_17d$proporcion <= quantile(datos_17d$proporcion, c(0.10)))


datos_18d   <- datos_dep %>%             # a partir de out.predict.garch  
  filter(datos_dep$periodo==2018)

out18d <- datos_18d %>%             # a partir de out.predict.garch  
  filter(datos_18d$proporcion <= quantile(datos_18d$proporcion, c(0.10)))


out16d = t(out16d$nit)
out17d = t(out17d$nit)
out18d = t(out18d$nit)

outliersd = distinct(as.data.frame(t(cbind(out16d,out17d,out18d))))
names(outliersd) = ("Nit_Out")


`%ni%` <- Negate(`%in%`)
outd = datos_dep$nit%ni%outliersd$Nit_Out
datos_deps = datos_dep[outd,]

#Datos sin outliers

boxplot(datos_deps$proporcion~datos_deps$Departamento,
        main = "Proporcion vs periodo",
        xlab = "Periodos",
        ylab = "Proporcion",
        boxwex = 0.5,col="blue")


# Se lleva a factor nit y periodos

datos_deps$nit = factor(datos_deps$nit)
datos_deps$Departamento = factor(datos_deps$Departamento)
datos_deps$periodo = factor(datos_deps$periodo,
                            levels = c("2016","2017","2018"),
                            labels = c(1,2,3))

# Division set de entrenamiento y test

library(caTools)
set.seed(4)

nite = as.data.frame(unique(datos_deps$nit))

names(nite) = c("nit")

splitd = sample.split(nite$nit, SplitRatio = 0.8)


training_set_pd = subset(nite, splitd == TRUE)
training_set_pd = datos_deps$nit%in%training_set_pd$nit
training_set_pd = datos_deps[training_set_pd,]


testing_set_pd = subset(nite, splitd == FALSE)
testing_set_pd = datos_deps$nit%in%testing_set_pd$nit
testing_set_pd = datos_deps[testing_set_pd,]

periodo_testd = c(3)
testing_pd = testing_set_pd$periodo%in%periodo_testd
testing_set_d = testing_set_pd[testing_pd,]

periodo_traind = c(1,2)
train_pd = testing_set_pd$periodo%in%periodo_traind
train_pd = testing_set_pd[train_pd,]

training_set_pd = rbind(training_set_pd,train_pd)

cantidad2 <- as.data.frame(cbind(6,34))
names(cantidad2) = c("Cantidad empresas testeo","Cantidad empresas entrenamiento")


# Modelamiento

library(nlme)

modelo1d <- lme(proporcion ~ Liquidez+tasa_desemp_dep+pib_dep+variacion_pib_dep+ipc_anio_dep+ingr_corr_dep+gastos_fun_dep+inflacion +tasa_int_prom+CHV+EC+ECG, data = training_set_pd, random = ~1|nit)

modelo2d <- lme(proporcion ~ tasa_desemp_dep+pib_dep+ingr_corr_dep, data = training_set_pd, random = ~1|nit)

modelo3d <- lme(proporcion ~ tasa_desemp_dep+pib_dep, data = training_set_pd, random = ~1|nit)

modelo4d <- lme(proporcion ~ tasa_desemp_dep+pib_dep+CHV , data = training_set_pd, random = ~1|nit)
AIC(modelo3d)

modelo5d <- lme(proporcion ~ CHV+EC+ECG, data = training_set_pd, random = ~1|nit)

modelo6d <- lme(proporcion ~ tasa_desemp_dep+pib_dep++ipc_anio_dep+inflacion, data = training_set_pd, random = ~1|nit)

Modelod <-  rbind("Modelo1d","Modelo2d","Modelo3d","Modelo4d","Modelo5d","Modelo6d")

AIC_Modelod <- rbind(round(AIC(modelo1d),2),
                     round(AIC(modelo2d),2),
                     round(AIC(modelo3d),2),
                     round(AIC(modelo4d),2),
                     round(AIC(modelo5d),2),
                     round(AIC(modelo6d),2))

BIC_Modelod <- rbind(round(BIC(modelo1d),2),
                     round(BIC(modelo2d),2),
                     round(BIC(modelo3d),2),
                     round(BIC(modelo4d),2),
                     round(BIC(modelo5d),2),
                     round(BIC(modelo6d),2))


estructurad<-rbind("Proporcion ~ liquidez+tasa_desemp_dep+pib_dep+variacion_pib_dep+ipc_anio_dep+ingr_corr_dep+gastos_fun_dep+inflacion +tasa_int_prom+CHV+EC+ECG+(1|nit)",
                   "Proporcion ~ tasa_desemp_dep+pib_dep+ingr_corr_dep+(1|nit)",
                   "Proporcion ~ tasa_desemp_dep+pib_dep+(1|nit)",
                   "Proporcion ~ PIB_Variacion+Tasa_Desempleo+(1|nit)",
                   "Proporcion ~ tasa_desemp_dep+pib_dep+CHV+(1|nit)",
                   "Proporcion ~ tasa_desemp_dep+pib_dep++ipc_anio_dep+inflacion+(1|nit)")

metricasd <- as.data.frame(cbind(Modelod,estructurad,AIC_Modelod,BIC_Modelod))
names(metricasd) = c("Modelo","Estructura","AIC","BIC")

#Resumen de modelo

summary(modelo3d)
anova(modelo3d)

#Prediccion

prediccion1d	=	predict(modelo1d,testing_set_pd,level =1,allow.new.levels=T)
names(prediccion1d) = c("prediccion")
prediccion2d	=	predict(modelo2d,testing_set_pd,level =1,allow.new.levels=T)
names(prediccion2d) = c("prediccion")
prediccion3d	=	predict(modelo3d,testing_set_pd,level =1,allow.new.levels=T)
names(prediccion3d) = c("prediccion")
prediccion4d	=	predict(modelo4d,testing_set_pd,level =1,allow.new.levels=T)
names(prediccion4d) = c("prediccion")
prediccion5d	=	predict(modelo5d,testing_set_pd,level =1,allow.new.levels=T)
names(prediccion5d) = c("prediccion")
prediccion6d	=	predict(modelo6d,testing_set_pd,level =1,allow.new.levels=T)
names(prediccion6d) = c("prediccion")

library(MLmetrics)

prediccion1d	=	predict(modelo1d,testing_set_d,level =1,allow.new.levels=T)
predichos1d	=	cbind(testing_set_d$proporcion,as.data.frame(prediccion1d))
names(predichos1d)	=	c("Datos1","Prediccion1")

prediccion2d	=	predict(modelo2d,testing_set_d,level =1,allow.new.levels=T)
predichos2d	=	cbind(testing_set_d$proporcion,as.data.frame(prediccion2d))
names(predichos2d)	=	c("Datos2","Prediccion2")

prediccion3d	=	predict(modelo3d,testing_set_d,level =1,allow.new.levels=T)
predichos3d	=	cbind(testing_set_d$proporcion,as.data.frame(prediccion3d))
names(predichos3d)	=	c("Datos3","Prediccion3")

prediccion4d	=	predict(modelo4d,testing_set_d,level =1,allow.new.levels=T)
predichos4d	=	cbind(testing_set_d$proporcion,as.data.frame(prediccion4d))
names(predichos4d)	=	c("Datos4","Prediccion4")

prediccion5d	=	predict(modelo5d,testing_set_d,level =1,allow.new.levels=T)
predichos5d	=	cbind(testing_set_d$proporcion,as.data.frame(prediccion5d))
names(predichos5d)	=	c("Datos5","Prediccion5")

prediccion6d	=	predict(modelo6d,testing_set_d,level =1,allow.new.levels=T)
predichos6d	=	cbind(testing_set_d$proporcion,as.data.frame(prediccion6d))
names(predichos6d)	=	c("Datos6","Prediccion6")


MSE1d<-MSE(predichos1d$Prediccion1,predichos1d$Datos1)
RMSE1d<-RMSE(predichos1d$Prediccion1,predichos1d$Datos1)
MAPE1d<-MAPE(predichos1d$Prediccion1,predichos1d$Datos1)

MSE2d<-MSE(predichos2d$Prediccion2,predichos2d$Datos2)
RMSE2d<-RMSE(predichos2d$Prediccion2,predichos2d$Datos2)
MAPE2d<-MAPE(predichos2d$Prediccion2,predichos2d$Datos2)

MSE3d<-MSE(predichos3d$Prediccion3,predichos3d$Datos3)
RMSE3d<-RMSE(predichos3d$Prediccion3,predichos3d$Datos3)
MAPE3d<-MAPE(predichos3d$Prediccion3,predichos3d$Datos3)

MSE4d<-MSE(predichos4d$Prediccion4,predichos4d$Datos4)
RMSE4d<-RMSE(predichos4d$Prediccion4,predichos4d$Datos4)
MAPE4d<-MAPE(predichos4d$Prediccion4,predichos4d$Datos4)

MSE5d<-MSE(predichos5d$Prediccion5,predichos5d$Datos5)
RMSE5d<-RMSE(predichos5d$Prediccion5,predichos5d$Datos5)
MAPE5d<-MAPE(predichos5d$Prediccion5,predichos5d$Datos5)

MSE6d<-MSE(predichos6d$Prediccion6,predichos6d$Datos6)
RMSE6d<-RMSE(predichos6d$Prediccion6,predichos6d$Datos6)
MAPE6d<-MAPE(predichos6d$Prediccion6,predichos6d$Datos6)

RMSEd <- rbind(round(RMSE1d,6),
               round(RMSE2d,6),
               round(RMSE3d,6),
               round(RMSE4d,6),
               round(RMSE5d,6),
               round(RMSE6d,6))

MSEd <- rbind(round(MSE1d,6),
              round(MSE2d,6),
              round(MSE3d,6),
              round(MSE4d,6),
              round(MSE5d,6),
              round(MSE6d,6))

MAPEd <- rbind(round(MAPE1d,6),
               round(MAPE2d,6),
               round(MAPE3d,6),
               round(MAPE4d,6),
               round(MAPE5d,6),
               round(MAPE6d,6))


metricas_predicciond <- as.data.frame(cbind(MSEd,RMSEd,MAPEd))
names(metricas_predicciond) = c("MSE","RMSE","MAPE")

metricas_consolidadasd <- as.data.frame(cbind(metricasd,metricas_predicciond))

# Gráfico del modelo

plot(modelo3d,type = c("p", "smooth"))


Fit <- fitted(modelo3d)
Res <- residuals(modelo3d, type = "normalized") 
par(mfrow = c(2, 2))
plot(Res ~ Fit, xlab = "Fitted values", ylab = "Residuals", main = "Residuals vs. fitted")
abline(h = 0)

hist(Res, main = "Histogram of residuals", xlab = "Residuals")
qqnorm(Res)
qqline(Res)
