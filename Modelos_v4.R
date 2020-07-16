##Cargar librerias requeridas


library("readxl")
library("knitr")
library("dplyr")
library("caret")
library("neuralnet")
library("ggplot2")
library("ROCR")
library("pROC")
library("caTools")
library("rpart")
library("rpart.plot")
library("rattle")
library("randomForest")

##=====================================##
## Comprensión del negocio
##=====================================##
#El esparcimiento de contenido noticioso falso es un fenómeno que ha venido en incremento en Costa Rica, utilizando 
#diferentes tipos de medios digitales como lo son sitios web, redes sociales (Facebook en especial) y WhatsApp para 
#su esparcimiento rápido entre la población. A nivel internacional existen diferentes tipos de iniciativas para 
#tratar de identificar contenido falso y clasificarlo como tal, pero en Costa Rica los esfuerzos han sido casi nulos, 
#a excepción del programa "No Coma Cuento" de La Nación y "DobleCheck" de la Universidad de Costa Rica, los cuales
#son iniciativas para desmentir noticias falsas utilizando métodos cómo búsqueda de fuentes, corroboración con involucrados,
#llamadas telefónicas y otros, siempre de manera manual. 
#Tampoco se cuentan con conjuntos de datos pre-procesados con los cuales se pueda trabajar por lo que es necesario
#empezar con la creación de los mismos y por medio de aplicación de algoritmos de miner??a de datos, se pretende en este
#trabajo poder validar la precisión de los mismos para la clasificación de noticias como verdaderas o falsas. 

##=====================================##
#Comprensión de los Datos
##=====================================##

#El conjunto de datos utilizado se recopiló durante los meses de Abril a Setiembre del 2019, pero puede contener
#noticias de años anteriores en algunos medios. 


#Cargar datos
setwd('C:\\Users\\gteno\\Google Drive (gtenorioa@ucenfotec.ac.cr)\\Proyecto de Investigación II\\R_scripts') #directorio
#setwd('F:\\Google Drive\\Maestria\\Proyecto de Investigaci?n II\\R_scripts') 


datos_eng <- read_excel('LIWC2015_ResultsENG.xlsx') #Conjunto de datos ingles
summary(datos_eng)
str(datos_eng)

datos_esp <-read_excel('LIWC2015_ResultsSPA.xlsx')  #Conjunto de datos español
summary(datos_esp)
str(datos_esp)


##=====================================##
#Preparacion de los Datos
##=====================================##

#convertir variables a factores
datos_eng$Clasificacion<-as.factor(datos_eng$Clasificacion)
datos_eng$MedioPeriodistico <-as.factor(datos_eng$MedioPeriodistico)
datos_eng$TipoSitio<-as.factor(datos_eng$TipoSitio)
str(datos_eng)

datos_esp$Clasificacion<-as.factor(datos_esp$Clasificacion)
datos_esp$MedioPeriodistico <-as.factor(datos_esp$MedioPeriodistico)
datos_esp$TipoSitio<-as.factor(datos_esp$TipoSitio)
str(datos_esp)

# Se establece el seed para garantizar obtener resultados consistentes.
set.seed(42)


#Limpieza variables con poca o cero varianza

#nearZeroVar(datos_eng[7:80]) #varianza cercana a cero
#nearZeroVar(datos_esp[7:69]) #SOlo para asegurarse que los mismos salen en el siguiente comando

#no estamos removiendo nada aun, solo viendo que valores no tienen varianza
#which(apply(datos_eng[7:80], 2, var) == 0) #cero varianza

#which(apply(datos_esp[7:69], 2, var) == 0) 



#Variables con la mitad de sus resultados la suma es 0. Para saber cuales se eliminaran 

#which(colMeans(datos_eng[7:80] == 0) > 0.5)
#which(colMeans(datos_esp[7:69] == 0) > 0.5)

#removemos esas variables del conjunto de datos. 

#str(datos_eng[6:69])
#datos_eng <-datos_eng[-nearZeroVar(datos_eng, saveMetrics = FALSE)]

#which(colMeans(datos_eng[5:59] == 0) > 0.5)

#datos_eng2<- datos_eng[, -which(colMeans(datos_eng[7:80] == 0) > 0.5)]
#str(datos_eng)


#datos_esp2<- datos_esp[, -which(colMeans(datos_esp[7:69] == 0) > 0.5)]
#str(datos_esp)

#datos_esp <-datos_esp[-nearZeroVar(datos_esp, saveMetrics = FALSE)]


#Visualizacion de variables.
#Cantidad de observaciones
barplot(table(datos_eng$Clasificacion), 
        main = 'Distribuci?n de las Noticias', 
        ylab = 'Observaciones', 
        xlab = 'Clasificaci?n',
        col = c('aquamarine3','blue'))



#Pairs
##Relaci?n entre variables resumen - excluyendo la variable WC en ingl?s

#Matriz de correlacion entre variabes resumen y variables de la dimension Linguistica
library(corrplot)
M2<-cor(datos_eng[c(7:20)])
head(round(M2,2))

layout(matrix(1:1, ncol = 2))
corrplot(M2, method = "number", tl.cex = 0.8)
layout(1)

#Matriz de correlacion entre variabes resumen, variables de la dimension Linguistica y signos de puntuaci?n
categoria1_ingles<-cor(datos_eng[c(7:20,55:60)])
head(round(categoria1_ingles,2))
layout(matrix(1:1, ncol = 2))
corrplot(categoria1_ingles, method = "number", tl.cex = 0.8)
layout(1)

categoria2_ingles<-cor(datos_eng[c(27,54)])
head(round(categoria2_ingles,2))
layout(matrix(1:1, ncol = 2))
corrplot(categoria2_ingles, method = "number", tl.cex = 0.8)
layout(1)

categoria3_ingles<-cor(datos_eng[c(28:32,33:38,39:42,43:48,49:51,52:53)])
head(round(categoria3_ingles,2))
layout(matrix(1:1, ncol = 2))
corrplot(categoria3_ingles, method = "number", tl.cex = 0.8)
layout(1)

pairs(datos_eng[c(7:13)], 
      main = 'Relaci?n entre variables resumen')

pairs(datos_eng[c(14:21)], 
      main = 'Relaci?n entre variables de dimensi?n Linguistica 1')

pairs(datos_eng[c(22:26)], 
      main = 'Relaci?n entre variables de dimensi?n Linguistica 2')

#Nota de la categoria procesos psicol?gicos solo se conserv? la variable negemo

pairs(datos_eng[c(28:32)], 
      main = 'Relaci?n entre variables de procesos sociales')

pairs(datos_eng[c(33:38)], 
      main = 'Relaci?n entre variables de procesos cognitivos')

pairs(datos_eng[c(39:42)], 
      main = 'Relaci?n entre variables de procesos de percepci?n')

pairs(datos_eng[c(43:48)], 
      main = 'Relaci?n entre variables de procesos de direcci?n')

pairs(datos_eng[c(49:51)], 
      main = 'Relaci?n entre variables de procesos de orientaci?n en el tiempo')

pairs(datos_eng[c(52:53)], 
      main = 'Relaci?n entre variables de relatividad')

#De lenguaje informal solo se conserv? la variable netspeak 54

pairs(datos_eng[c(55:60)], 
      main = 'Relaci?n entre variables de signos de puntuaci?n')


##Relaci?n entre variables resumen - excluyendo la variable WC en espannol

#Matriz de correlacion entre variabes resumen y variables de la dimension Luiguistica
library(corrplot)
M2<-cor(datos_esp[c(7:20)])
head(round(M2,2))

layout(matrix(1:1, ncol = 2))
corrplot(M2, method = "number", tl.cex = 0.8)
layout(1)

#Matriz de correlacion entre variabes resumen, variables de la dimension Linguistica, y los signos de puntuaci?n
library(corrplot)
M2<-cor(datos_esp[c(7:20,44:49)])
head(round(M,2))

layout(matrix(1:1, ncol = 2))
corrplot(M2, method = "number", tl.cex = 0.8)
layout(1)


pairs(datos_esp[c(7:9)], 
      main = 'Relaci?n entre variables resumen')

pairs(datos_esp[c(10:20)], 
      main = 'Relaci?n entre variables de dimensi?n Linguistica 1')

pairs(datos_esp[c(21:23)], 
      main = 'Relaci?n entre variables de procesos sociales')

pairs(datos_esp[c(24:28)], 
      main = 'Relaci?n entre variables de procesos psicol?gicos')

pairs(datos_esp[c(29:37)], 
      main = 'Relaci?n entre variables de procesos cognitivos')


pairs(datos_esp[c(38:41)], 
      main = 'Relaci?n entre variables de procesos de percepci?n')

#procesos de relatividad solo se conservo la variable Espacio #42
#procesos de preocupaciones personales solo se conservo la variable Dinero #43

pairs(datos_esp[c(44:49)], 
      main = 'Relaci?n entre variables de signos de puntuaci?n')

#Graficos

hist(datos_eng$WC, 
     main="Cantidad de palabras de los art?culos", 
     xlab="WC")

hist(datos_esp$WC, 
     main="Cantidad de palabras de los art?culos", 
     xlab="WC")
#Nos va a ayudar a describir el dataset. Distribucion de la cantidad de palabras. 

ggplot(datos_eng, aes(x=MedioPeriodistico, y=Tone)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  labs(title="Gráfico de Barras del Tono de los art??culos según el medio", 
       subtitle="Medio vs Emociones Positivas") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

#ordenar por valor del tono


ggplot(datos_esp, aes(x=MedioPeriodistico, y=EmoPos)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  labs(title="Gráfico de Barras de Emociones Positivas de los art??culos según el medio", 
       subtitle="Medio vs Emociones Positivas") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

ggplot(datos_esp, aes(x=MedioPeriodistico, y=EmoNeg)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  labs(title="Gráfico de Barras de Emociones Positivas de los art??culos según el medio", 
       subtitle="Medio vs Emociones Positivas") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))


ggplot(datos_eng, aes(x=MedioPeriodistico, y=WC)) + 
  geom_point(col="blue", size=3) +   # Dibuja los puntos
  geom_segment(aes(x=MedioPeriodistico, 
                   xend=MedioPeriodistico, 
                   y=min(WC), 
                   yend=max(WC)), 
               linetype="dashed", 
               size=0.1) +   #dibuja lineas punteadas
  labs(title="Gráfico Plot de puntos", 
       subtitle="Total de palabras por medio period??stico") +  
  coord_flip()

ggplot(datos_esp, aes(x=Clasificacion, y=WC)) + 
  geom_point(col="blue", size=3) +   # Dibuja los puntos
  geom_segment(aes(x=Clasificacion, 
                   xend=Clasificacion, 
                   y=min(WC), 
                   yend=max(WC)), 
               linetype="dashed", 
               size=0.1) +   #dibuja lineas punteadas
  labs(title="Gráfico Plot de puntos", 
       subtitle="Total de palabras por medio period??stico") +  
  coord_flip()


g <- ggplot(datos_esp, aes(Clasificacion, Articulo))
g + geom_boxplot(varwidth=T, fill="plum") + 
  labs(title="Gráfico de Cajas", 
       subtitle="Cantidad de Art??culos agrupado por Clasificación",
       x="Clasificación de la Noticia",
       y="Cantidad de Art??culos")


##=====================================##
#Modelado de los Datos
##=====================================##

#set.seed(nrow(datos))
#Dividir el conjunto de datos en uno de entrenamiento y otro de prueba utilizando Notas Finales
splt <- sample.split(datos_eng$Clasificacion, SplitRatio = 0.7)
splt1 <- sample.split(datos_esp$Clasificacion, SplitRatio = 0.7)

deng.entrenamiento <- datos_eng[splt, ]
deng.prueba <- datos_eng[!splt, ]

desp.entrenamiento <- datos_esp[splt1, ]
desp.prueba <- datos_esp[!splt1, ]


#deng.entrenamiento2 <- datos_eng2[splt, ]
#deng.prueba2 <- datos_eng2[!splt, ]

#desp.entrenamiento2 <- datos_esp2[splt, ]
#desp.prueba2 <- datos_esp2[!splt, ]

#Aplicar t?cnicas de balanceo de cargas - En este caso se aplica upSampling para garantizar que el n?mero de observaciones tanto las verdaderas
#como las sospechosas es el mismo
#deng.entrenamiento <- upSample(x = deng.entrenamiento[, -ncol(deng.entrenamiento)],
#                               y = deng.entrenamiento$Clasificacion)
#table(datos_eng$deng.entrenamiento)
#table(deng.entrenamiento$Clasificacion)
#summary(deng.entrenamiento)
##deng.prueba <- upSample(x = deng.prueba[, -ncol(deng.prueba)],
#                        y = deng.prueba$Clasificacion)
#table(datos_eng$deng.prueba)
#table(deng.prueba$Clasificacion)
#desp.entrenamiento <- upSample(x = desp.entrenamiento[, -ncol(desp.entrenamiento)],
#                               y = desp.entrenamiento$Clasificacion)
#table(desp.entrenamiento$Clasificacion)

#desp.prueba <- upSample(x = desp.prueba[, -ncol(desp.prueba)],
#                        y = desp.prueba$Clasificacion)
#table(desp.prueba$Clasificacion)


###Modelo Ingenuo
## Dado el balanceo de cargas el modelo ingenuo dar? una probabilidad de 50-50
#modelo.ingenuo1 <- rep(0,nrow(desp.prueba))
#table(desp.prueba$Clasificacion,modelo.ingenuo1)

#============================================#
#Regresion Logistica Conjunto Datos Ingles
library(caret)

set.seed(nrow(deng.entrenamiento))

#entrenar los datos ingles
#trainControl(method = "cv", number = 10) especifica que se realizar una validaci?n cruzada de 10 pliegues
t_control <- trainControl(method = "repeatedcv", sampling = "up", number = 10, repeats = 3)

resumen_metricas_total <- data.frame(
  Metrica=c("Sensibilidad","Especificidad","Valor Pred Pos", "Valor Pred Neg","Precision","Exhaustividad","F1","Prevalencia","Tasa de Detencion",
               "Prevalencia de Detencion","Precision Balanceada","Tasa de Error"), row.names=NULL)
resumen_metricas_ingles_regresion_logistica <- data.frame(
  Metrica=c("Sensibilidad","Especificidad","Valor Pred Pos", "Valor Pred Neg","Precision","Exhaustividad","F1","Prevalencia","Tasa de Detencion",
            "Prevalencia de Detencion","Precision Balanceada","Tasa de Error"), row.names=NULL)
resumen_metricas_ingles_bosques_aleatorios <- data.frame(
  Metrica=c("Sensibilidad","Especificidad","Valor Pred Pos", "Valor Pred Neg","Precision","Exhaustividad","F1","Prevalencia","Tasa de Detencion",
            "Prevalencia de Detencion","Precision Balanceada","Tasa de Error"), row.names=NULL)
resumen_metricas_espannol_regresion_logistica <- data.frame(
  Metrica=c("Sensibilidad","Especificidad","Valor Pred Pos", "Valor Pred Neg","Precision","Exhaustividad","F1","Prevalencia","Tasa de Detencion",
            "Prevalencia de Detencion","Precision Balanceada","Tasa de Error"), row.names=NULL)
resumen_metricas_espannol_bosques_aleatorios <- data.frame(
  Metrica=c("Sensibilidad","Especificidad","Valor Pred Pos", "Valor Pred Neg","Precision","Exhaustividad","F1","Prevalencia","Tasa de Detencion",
            "Prevalencia de Detencion","Precision Balanceada","Tasa de Error"), row.names=NULL)
resumen_metricas_ingles_svm <- data.frame(
  Metrica=c("Sensibilidad","Especificidad","Valor Pred Pos", "Valor Pred Neg","Precision","Exhaustividad","F1","Prevalencia","Tasa de Detencion",
            "Prevalencia de Detencion","Precision Balanceada","Tasa de Error"), row.names=NULL)
resumen_metricas_ingles_ad <- data.frame(
  Metrica=c("Sensibilidad","Especificidad","Valor Pred Pos", "Valor Pred Neg","Precision","Exhaustividad","F1","Prevalencia","Tasa de Detencion",
            "Prevalencia de Detencion","Precision Balanceada","Tasa de Error"), row.names=NULL)
resumen_metricas_espannol_svm <- data.frame(
  Metrica=c("Sensibilidad","Especificidad","Valor Pred Pos", "Valor Pred Neg","Precision","Exhaustividad","F1","Prevalencia","Tasa de Detencion",
            "Prevalencia de Detencion","Precision Balanceada","Tasa de Error"), row.names=NULL)
resumen_metricas_espannol_ad <- data.frame(
  Metrica=c("Sensibilidad","Especificidad","Valor Pred Pos", "Valor Pred Neg","Precision","Exhaustividad","F1","Prevalencia","Tasa de Detencion",
            "Prevalencia de Detencion","Precision Balanceada","Tasa de Error"), row.names=NULL)

#*****************************************************************************************************************************
#***************************************************** MODELOS EN INGLES *****************************************************
#*****************************************************************************************************************************
#############REGRESION LOGISTICA
#***************************************************** MODELO 1 **************************************************************
regresion_logistica_eng_m1 <- train(Clasificacion ~WC+WPS+Sixltr+pronoun+article+adverb+verb+QMark+Exclam+Apostro+auxverb+Funct, 
                                    data = deng.entrenamiento[c(2:2,6:80)], 
                                    method ="glm",
                             family ="binomial", trControl=t_control,preProcess = c("center", "scale"),
                             tuneLength = 10)

regresion_logistica_eng_m1
##Prediccion del conjunto de datos de prueba

prediccion_rl_eng_m1 <- predict(regresion_logistica_eng_m1, newdata = deng.prueba[c(2:2,6:80)])
prediccion_rl_eng_m1

#Matriz de confusion
cm_rl_eng_m1 <- confusionMatrix(prediccion_rl_eng_m1, deng.prueba$Clasificacion)
metrics_rl_eng_m1 <-cm_rl_eng_m1$byClass

metrics_rl_eng_m1["Sensitivity"]
table(metrics_rl_eng_m1)

resumen_metricas_lr_eng_m1 <- data.frame(keyName=names(metrics_rl_eng_m1), value=metrics_rl_eng_m1, row.names=NULL)
names(resumen_metricas_lr_eng_m1) <- c("Metrica", "M1 LR")

#Calculo del error
error_rate = round(mean(prediccion_rl_eng_m1 != deng.prueba$Clasificacion),2)
error_rate

resumen_error_rl_eng_m1 <- data.frame(Metrica=c("Tasa de Error"), value=c(error_rate), row.names=NULL)
names(resumen_error_rl_eng_m1) <- c("Metrica", "M1 LR")
resumen_error_rl_eng_m1

resumen_metricas_lr_eng_m1_with_error <- union(resumen_metricas_lr_eng_m1, resumen_error_rl_eng_m1)
resumen_metricas_lr_eng_m1_with_error

resumen_metricas_ingles_regresion_logistica <- cbind(resumen_metricas_ingles_regresion_logistica, resumen_metricas_lr_eng_m1_with_error[2])

#plot(regresion_logistica_eng_m1)

#*****************************************************************************************************************************
#**************************************************** MODELO 2 ***************************************************************
regresion_logistica_eng_m2 <- train(Clasificacion ~ adverb+Sixltr+negemo, data = deng.entrenamiento[c(2:2,6:80)], 
                                    method ="glm",
                                    family ="binomial", 
                                    trControl=t_control,preProcess = c("center", "scale"),
                                    tuneLength = 10)

regresion_logistica_eng_m2
##Prediccion del conjunto de datos de prueba

prediccion_rl_eng_m2 <- predict(regresion_logistica_eng_m2, newdata = deng.prueba[c(2:2,6:80)])
prediccion_rl_eng_m2

#Matriz de confusion
cm_rl_eng_m2 <-confusionMatrix(prediccion_rl_eng_m2, deng.prueba$Clasificacion)
metrics_rl_eng_m2 <-cm_rl_eng_m2$byClass

resumen_metricas_lr_eng_m2 <- data.frame(keyName=names(metrics_rl_eng_m2), value=metrics_rl_eng_m2, row.names=NULL)
names(resumen_metricas_lr_eng_m2) <- c("Metrica", "M2 LR")
resumen_metricas_lr_eng_m2[2]

#Calculo del error
error_rate = round(mean(prediccion_rl_eng_m2 != deng.prueba$Clasificacion),2)
error_rate

resumen_error_rl_eng_m2 <- data.frame(Metrica=c("Tasa de Error"), value=c(error_rate), row.names=NULL)
names(resumen_error_rl_eng_m2) <- c("Metrica", "M2 LR")
resumen_error_rl_eng_m2

resumen_metricas_lr_eng_m2_with_error <- union(resumen_metricas_lr_eng_m2, resumen_error_rl_eng_m2)
resumen_metricas_lr_eng_m2_with_error

resumen_metricas_ingles_regresion_logistica <- cbind(resumen_metricas_ingles_regresion_logistica, resumen_metricas_lr_eng_m2_with_error[2])

#*****************************************************************************************************************************
#**************************************************** MODELO 3 ***************************************************************
regresion_logistica_eng_m3 <- train(Clasificacion ~ QMark+informal+prep
, data = deng.entrenamiento[c(2:2,6:80)], 
                                    method ="glm",
                                    family ="binomial", 
                                    trControl=t_control,preProcess = c("center", "scale"),
                                    tuneLength = 10)

regresion_logistica_eng_m3
##Prediccion del conjunto de datos de prueba

prediccion_rl_eng_m3 <- predict(regresion_logistica_eng_m3, newdata = deng.prueba[c(2:2,6:80)])
prediccion_rl_eng_m3

#Matriz de confusion
cm_rl_eng_m3 <-confusionMatrix(prediccion_rl_eng_m3, deng.prueba$Clasificacion)

metrics_rl_eng_m3 <-cm_rl_eng_m3$byClass

resumen_metricas_lr_eng_m3 <- data.frame(keyName=names(metrics_rl_eng_m3), value=metrics_rl_eng_m3, row.names=NULL)
names(resumen_metricas_lr_eng_m3) <- c("Metrica", "M3 LR")
resumen_metricas_lr_eng_m3[2]

#Calculo del error
error_rate = round(mean(prediccion_rl_eng_m3 != deng.prueba$Clasificacion),2)
error_rate

resumen_error_rl_eng_m3 <- data.frame(Metrica=c("Tasa de Error"), value=c(error_rate), row.names=NULL)
names(resumen_error_rl_eng_m3) <- c("Metrica", "M3 LR")
resumen_error_rl_eng_m3

resumen_metricas_lr_eng_m3_with_error <- union(resumen_metricas_lr_eng_m3, resumen_error_rl_eng_m3)
resumen_metricas_lr_eng_m3_with_error

resumen_metricas_ingles_regresion_logistica <- cbind(resumen_metricas_ingles_regresion_logistica, resumen_metricas_lr_eng_m3_with_error[2])

#***************************************************** MODELO 4 **************************************************************
str(deng.entrenamiento2[c(2:2,5:60)])
regresion_logistica_eng_m4 <- train(Clasificacion ~., data = deng.entrenamiento[c(2:2,6:80)], method ="glm",
                                    family ="binomial", trControl=t_control,preProcess = c("center", "scale"),
                                    tuneLength = 10)

regresion_logistica_eng_m4
##Prediccion del conjunto de datos de prueba

prediccion_rl_eng_m4 <- predict(regresion_logistica_eng_m4, newdata = deng.prueba[c(2:2,6:80)])
prediccion_rl_eng_m4

#Matriz de confusion
cm_rl_eng_m4 <- confusionMatrix(prediccion_rl_eng_m4, deng.prueba$Clasificacion)
metrics_rl_eng_m4 <-cm_rl_eng_m4$byClass

metrics_rl_eng_m4["Sensitivity"]
table(metrics_rl_eng_m4)

resumen_metricas_lr_eng_m4 <- data.frame(keyName=names(metrics_rl_eng_m4), value=metrics_rl_eng_m4, row.names=NULL)
names(resumen_metricas_lr_eng_m4) <- c("Metrica", "M4 LR")

#Calculo del error
error_rate = round(mean(prediccion_rl_eng_m4 != deng.prueba$Clasificacion),2)
error_rate

resumen_error_rl_eng_m4 <- data.frame(Metrica=c("Tasa de Error"), value=c(error_rate), row.names=NULL)
names(resumen_error_rl_eng_m4) <- c("Metrica", "M4 LR")
resumen_error_rl_eng_m4

resumen_metricas_lr_eng_m4_with_error <- union(resumen_metricas_lr_eng_m4, resumen_error_rl_eng_m4)
resumen_metricas_lr_eng_m4_with_error

resumen_metricas_ingles_regresion_logistica <- cbind(resumen_metricas_ingles_regresion_logistica, resumen_metricas_lr_eng_m4_with_error[2])

#*****************************************************************************************************************************
#**************************************************** MODELO 5 ***************************************************************
regresion_logistica_eng_m5 <- train(Clasificacion ~ WC+Analytic+Clout+Authentic+Tone+WPS+Sixltr+Funct, data = deng.entrenamiento[c(2:2,6:80)], 
                                    method ="glm",
                                    family ="binomial", 
                                    trControl=t_control,preProcess = c("center", "scale"),
                                    tuneLength = 10)

regresion_logistica_eng_m5
##Prediccion del conjunto de datos de prueba

prediccion_rl_eng_m5 <- predict(regresion_logistica_eng_m5, newdata = deng.prueba[c(2:2,6:80)])
prediccion_rl_eng_m5

#Matriz de confusion
cm_rl_eng_m5 <-confusionMatrix(prediccion_rl_eng_m5, deng.prueba$Clasificacion)
metrics_rl_eng_m5 <-cm_rl_eng_m5$byClass

resumen_metricas_lr_eng_m5 <- data.frame(keyName=names(metrics_rl_eng_m5), value=metrics_rl_eng_m5, row.names=NULL)
names(resumen_metricas_lr_eng_m5) <- c("Metrica", "M5 LR")
resumen_metricas_lr_eng_m5[2]

#Calculo del error
error_rate = round(mean(prediccion_rl_eng_m5 != deng.prueba$Clasificacion),2)
error_rate

resumen_error_rl_eng_m5 <- data.frame(Metrica=c("Tasa de Error"), value=c(error_rate), row.names=NULL)
names(resumen_error_rl_eng_m5) <- c("Metrica", "M5 LR")
resumen_error_rl_eng_m5

resumen_metricas_lr_eng_m5_with_error <- union(resumen_metricas_lr_eng_m5, resumen_error_rl_eng_m5)
resumen_metricas_lr_eng_m5_with_error

resumen_metricas_ingles_regresion_logistica <- cbind(resumen_metricas_ingles_regresion_logistica, resumen_metricas_lr_eng_m5_with_error[2])

#*****************************************************************************************************************************
#**************************************************** MODELO 6 ***************************************************************
regresion_logistica_eng_m6 <- train(Clasificacion ~ WC+Analytic+Clout+Authentic+Tone+WPS+Sixltr+Funct+pronoun+article+prep+auxverb+adverb+conj+negate+verb+adj+compare+interrog+number+quant+AllPunc+Period+Colon+SemiC+QMark+Exclam+Dash+Apostro+Parenth
                                    , data = deng.entrenamiento[c(2:2,6:80)], 
                                    method ="glm",
                                    family ="binomial", 
                                    trControl=t_control,preProcess = c("center", "scale"),
                                    tuneLength = 10)

regresion_logistica_eng_m6
##Prediccion del conjunto de datos de prueba

prediccion_rl_eng_m6 <- predict(regresion_logistica_eng_m6, newdata = deng.prueba[c(2:2,6:80)])
prediccion_rl_eng_m6

#Matriz de confusion
cm_rl_eng_m6 <-confusionMatrix(prediccion_rl_eng_m6, deng.prueba$Clasificacion)

metrics_rl_eng_m6 <-cm_rl_eng_m6$byClass

resumen_metricas_lr_eng_m6 <- data.frame(keyName=names(metrics_rl_eng_m6), value=metrics_rl_eng_m6, row.names=NULL)
names(resumen_metricas_lr_eng_m6) <- c("Metrica", "M6 LR")
resumen_metricas_lr_eng_m6[2]

#Calculo del error
error_rate = round(mean(prediccion_rl_eng_m6 != deng.prueba$Clasificacion),2)
error_rate

resumen_error_rl_eng_m6 <- data.frame(Metrica=c("Tasa de Error"), value=c(error_rate), row.names=NULL)
names(resumen_error_rl_eng_m6) <- c("Metrica", "M6 LR")
resumen_error_rl_eng_m6

resumen_metricas_lr_eng_m6_with_error <- union(resumen_metricas_lr_eng_m6, resumen_error_rl_eng_m6)
resumen_metricas_lr_eng_m6_with_error

resumen_metricas_ingles_regresion_logistica <- cbind(resumen_metricas_ingles_regresion_logistica, resumen_metricas_lr_eng_m6_with_error[2])
#*****************************************************************************************************************************
#*****************************************************************************************************************************
#############BOSQUES ALEATORIOS
#***************************************************** MODELO 1 **************************************************************
#mtry: Number of variable is randomly collected to be sampled at each split time.
#ntree: Number of branches will grow after each time split.

bosques_aleatorios_eng_m1 <- train(Clasificacion ~WC+WPS+Sixltr+pronoun+article+adverb+verb+QMark+Exclam+Apostro+auxverb+Funct, 
                                   data = deng.entrenamiento[c(2:2,6:80)], 
                                   method = "rf",
                                   ntree = 10,
                                   trControl=t_control,
                                   tuneLength = 10)

bosques_aleatorios_eng_m1
##Prediccion del conjunto de datos de prueba

prediccion_ba_eng_m1 <- predict(bosques_aleatorios_eng_m1, newdata = deng.prueba[c(2:2,6:80)])
prediccion_ba_eng_m1

#Matriz de confusion
cm_ba_eng_m1 <-confusionMatrix(prediccion_ba_eng_m1, deng.prueba$Clasificacion)

metrics_ba_eng_m1 <-cm_ba_eng_m1$byClass

resumen_metricas_ba_eng_m1 <- data.frame(keyName=names(metrics_ba_eng_m1), value=metrics_ba_eng_m1, row.names=NULL)
names(resumen_metricas_ba_eng_m1) <- c("Metrica", "M1 BA")
resumen_metricas_ba_eng_m1[2]

#Calculo del error
error_rate = round(mean(prediccion_ba_eng_m1 != deng.prueba$Clasificacion),2)
error_rate

resumen_error_ba_eng_m1 <- data.frame(Metrica=c("Tasa de Error"), value=c(error_rate), row.names=NULL)
names(resumen_error_ba_eng_m1) <- c("Metrica", "M1 BA")
resumen_error_ba_eng_m1

resumen_metricas_ba_eng_m1_with_error <- union(resumen_metricas_ba_eng_m1, resumen_error_ba_eng_m1)
resumen_metricas_ba_eng_m1_with_error

resumen_metricas_ingles_bosques_aleatorios <- cbind(resumen_metricas_ingles_bosques_aleatorios, resumen_metricas_ba_eng_m1_with_error[2])

plot(bosques_aleatorios_eng_m1)

#Area bajo la curva
prediccionROC.log.eng <- prediction(prediccion_ba_eng_m1, as.numeric(deng.prueba$Clasificacion))
as.numeric(performance(prediccionROC.log.eng, "auc")@y.values) 

ROCR.log.eng <- performance(prediccionROC.log.eng, "tpr", "fpr")#prediction, x axys, y  axis
ROCR.log.eng 

plot(ROCR.bosque.eng,
     main = 'Curva ROC - Bosques aleatorios',
     colorize=TRUE,
     print.cutoffs.at=seq(0,1,by=0.1),
     text.adj = c(-0.2,1.7))

#install.packages("Metrics")
#library(Metrics)
pd <- predict(bosques_aleatorios_eng_m1 , deng.prueba[c(2:2,6:80)])
#mse(test$Clasificacion,pd)
#*****************************************************************************************************************************
#**************************************************** MODELO 2 ***************************************************************
bosques_aleatorios_eng_m2 <- train(Clasificacion ~ adverb+Sixltr+negemo, data = deng.entrenamiento[c(2:2,6:80)], 
                                   method = "rf",
                                   ntree = 10,
                                   trControl=t_control,
                                   tuneLength = 10)

bosques_aleatorios_eng_m2
##Prediccion del conjunto de datos de prueba

prediccion_ba_eng_m2 <- predict(bosques_aleatorios_eng_m2, newdata = deng.prueba[c(2:2,6:80)])
prediccion_ba_eng_m2

#Matriz de confusion
cm_ba_eng_m2 <-confusionMatrix(prediccion_ba_eng_m2, deng.prueba$Clasificacion)

metrics_ba_eng_m2 <-cm_ba_eng_m2$byClass

resumen_metricas_ba_eng_m2 <- data.frame(keyName=names(metrics_ba_eng_m2), value=metrics_ba_eng_m2, row.names=NULL)
names(resumen_metricas_ba_eng_m2) <- c("Metrica", "M2 BA")
resumen_metricas_ba_eng_m2[2]

#Calculo del error
error_rate = round(mean(prediccion_ba_eng_m2 != deng.prueba$Clasificacion),2)
error_rate

resumen_error_ba_eng_m2 <- data.frame(Metrica=c("Tasa de Error"), value=c(error_rate), row.names=NULL)
names(resumen_error_ba_eng_m2) <- c("Metrica", "M2 BA")
resumen_error_ba_eng_m2

resumen_metricas_ba_eng_m2_with_error <- union(resumen_metricas_ba_eng_m2, resumen_error_ba_eng_m2)
resumen_metricas_ba_eng_m2_with_error

resumen_metricas_ingles_bosques_aleatorios <- cbind(resumen_metricas_ingles_bosques_aleatorios, resumen_metricas_ba_eng_m2_with_error[2])

plot(bosques_aleatorios_eng_m2)
#*****************************************************************************************************************************
#**************************************************** MODELO 3 ***************************************************************
bosques_aleatorios_eng_m3 <- train(Clasificacion ~ QMark+informal+prep
                                   , data = deng.entrenamiento[c(2:2,6:80)],
                                   method = "rf",
                                   ntree = 10,
                                   trControl=t_control,
                                   tuneLength = 10)

bosques_aleatorios_eng_m3
##Prediccion del conjunto de datos de prueba

prediccion_ba_eng_m3 <- predict(bosques_aleatorios_eng_m3, newdata = deng.prueba[c(2:2,6:80)])
prediccion_ba_eng_m3

#Matriz de confusion
cm_ba_eng_m3 <-confusionMatrix(prediccion_ba_eng_m3, deng.prueba$Clasificacion)

metrics_ba_eng_m3 <-cm_ba_eng_m3$byClass

resumen_metricas_ba_eng_m3 <- data.frame(keyName=names(metrics_ba_eng_m3), value=metrics_ba_eng_m3, row.names=NULL)
names(resumen_metricas_ba_eng_m3) <- c("Metrica", "M3 BA")
resumen_metricas_ba_eng_m3[2]

#Calculo del error
error_rate = round(mean(prediccion_ba_eng_m3 != deng.prueba$Clasificacion),2)
error_rate

resumen_error_ba_eng_m3 <- data.frame(Metrica=c("Tasa de Error"), value=c(error_rate), row.names=NULL)
names(resumen_error_ba_eng_m3) <- c("Metrica", "M3 BA")
resumen_error_ba_eng_m3

resumen_metricas_ba_eng_m3_with_error <- union(resumen_metricas_ba_eng_m3, resumen_error_ba_eng_m3)
resumen_metricas_ba_eng_m3_with_error

resumen_metricas_ingles_bosques_aleatorios <- cbind(resumen_metricas_ingles_bosques_aleatorios, resumen_metricas_ba_eng_m3_with_error[2])

plot(bosques_aleatorios_eng_m3)
#***************************************************** MODELO 4 **************************************************************
#mtry: Number of variable is randomly collected to be sampled at each split time.
#ntree: Number of branches will grow after each time split.

bosques_aleatorios_eng_m4 <- train(Clasificacion ~., data = deng.entrenamiento[c(2:2,6:80)], 
                                    method = "rf",
                                    ntree = 10,
                                    trControl=t_control,
                                    tuneLength = 10)

bosques_aleatorios_eng_m4
##Prediccion del conjunto de datos de prueba

prediccion_ba_eng_m4 <- predict(bosques_aleatorios_eng_m1, newdata = deng.prueba[c(2:2,6:80)])
prediccion_ba_eng_m4

#Matriz de confusion
cm_ba_eng_m4 <-confusionMatrix(prediccion_ba_eng_m4, deng.prueba$Clasificacion)

metrics_ba_eng_m4 <-cm_ba_eng_m4$byClass

resumen_metricas_ba_eng_m4 <- data.frame(keyName=names(metrics_ba_eng_m4), value=metrics_ba_eng_m4, row.names=NULL)
names(resumen_metricas_ba_eng_m4) <- c("Metrica", "M4 BA")
resumen_metricas_ba_eng_m4[2]

#Calculo del error
error_rate = round(mean(prediccion_ba_eng_m4 != deng.prueba$Clasificacion),2)
error_rate

resumen_error_ba_eng_m4 <- data.frame(Metrica=c("Tasa de Error"), value=c(error_rate), row.names=NULL)
names(resumen_error_ba_eng_m4) <- c("Metrica", "M4 BA")
resumen_error_ba_eng_m4

resumen_metricas_ba_eng_m4_with_error <- union(resumen_metricas_ba_eng_m4, resumen_error_ba_eng_m4)
resumen_metricas_ba_eng_m4_with_error

resumen_metricas_ingles_bosques_aleatorios <- cbind(resumen_metricas_ingles_bosques_aleatorios, resumen_metricas_ba_eng_m4_with_error[2])

plot(bosques_aleatorios_eng_m4)

#Area bajo la curva
prediccionROC.log.eng <- prediction(prediccion_ba_eng_m4, as.numeric(deng.prueba$Clasificacion))
as.numeric(performance(prediccionROC.log.eng, "auc")@y.values) 

ROCR.log.eng <- performance(prediccionROC.log.eng, "tpr", "fpr")#prediction, x axys, y  axis
ROCR.log.eng 

plot(ROCR.bosque.eng,
     main = 'Curva ROC - Bosques aleatorios',
     colorize=TRUE,
     print.cutoffs.at=seq(0,1,by=0.1),
     text.adj = c(-0.2,1.7))

#install.packages("Metrics")
#library(Metrics)
pd <- predict(bosques_aleatorios_eng_m4 , deng.prueba[c(2:2,6:80)])
#mse(test$Clasificacion,pd)
#*****************************************************************************************************************************
#**************************************************** MODELO 5 ***************************************************************
bosques_aleatorios_eng_m5 <- train(Clasificacion ~ WC+Analytic+Clout+Authentic+Tone+WPS+Sixltr+Funct, data = deng.entrenamiento[c(2:2,6:80)], 
                                    method = "rf",
                                    ntree = 10,
                                    trControl=t_control,
                                    tuneLength = 10)

bosques_aleatorios_eng_m5
##Prediccion del conjunto de datos de prueba

prediccion_ba_eng_m5 <- predict(bosques_aleatorios_eng_m5, newdata = deng.prueba[c(2:2,6:80)])
prediccion_ba_eng_m5

#Matriz de confusion
cm_ba_eng_m5 <-confusionMatrix(prediccion_ba_eng_m5, deng.prueba$Clasificacion)

metrics_ba_eng_m5 <-cm_ba_eng_m5$byClass

resumen_metricas_ba_eng_m5 <- data.frame(keyName=names(metrics_ba_eng_m5), value=metrics_ba_eng_m5, row.names=NULL)
names(resumen_metricas_ba_eng_m5) <- c("Metrica", "M5 BA")
resumen_metricas_ba_eng_m5[2]

#Calculo del error
error_rate = round(mean(prediccion_ba_eng_m5 != deng.prueba$Clasificacion),2)
error_rate

resumen_error_ba_eng_m5 <- data.frame(Metrica=c("Tasa de Error"), value=c(error_rate), row.names=NULL)
names(resumen_error_ba_eng_m5) <- c("Metrica", "M5 BA")
resumen_error_ba_eng_m5

resumen_metricas_ba_eng_m5_with_error <- union(resumen_metricas_ba_eng_m5, resumen_error_ba_eng_m5)
resumen_metricas_ba_eng_m5_with_error

resumen_metricas_ingles_bosques_aleatorios <- cbind(resumen_metricas_ingles_bosques_aleatorios, resumen_metricas_ba_eng_m5_with_error[2])

plot(bosques_aleatorios_eng_m5)
#*****************************************************************************************************************************
#**************************************************** MODELO 6 ***************************************************************
bosques_aleatorios_eng_m6 <- train(Clasificacion ~ WC+Analytic+Clout+Authentic+Tone+WPS+Sixltr+Dic+Funct+pronoun+article+prep+auxverb+adverb+conj+negate+verb+adj+compare+interrog+number+quant+AllPunc+Period+Colon+SemiC+QMark+Exclam+Dash+Apostro+Parenth
                                    , data = deng.entrenamiento[c(2:2,6:80)],
                                    method = "rf",
                                    ntree = 10,
                                    trControl=t_control,
                                    tuneLength = 10)

bosques_aleatorios_eng_m6
##Prediccion del conjunto de datos de prueba

prediccion_ba_eng_m6 <- predict(bosques_aleatorios_eng_m3, newdata = deng.prueba[c(2:2,6:80)])
prediccion_ba_eng_m6

#Matriz de confusion
cm_ba_eng_m6 <-confusionMatrix(prediccion_ba_eng_m6, deng.prueba$Clasificacion)

metrics_ba_eng_m6 <-cm_ba_eng_m6$byClass

resumen_metricas_ba_eng_m6 <- data.frame(keyName=names(metrics_ba_eng_m6), value=metrics_ba_eng_m6, row.names=NULL)
names(resumen_metricas_ba_eng_m6) <- c("Metrica", "M6 BA")
resumen_metricas_ba_eng_m6[2]

#Calculo del error
error_rate = round(mean(prediccion_ba_eng_m6 != deng.prueba$Clasificacion),2)
error_rate

resumen_error_ba_eng_m6 <- data.frame(Metrica=c("Tasa de Error"), value=c(error_rate), row.names=NULL)
names(resumen_error_ba_eng_m6) <- c("Metrica", "M6 BA")
resumen_error_ba_eng_m6

resumen_metricas_ba_eng_m6_with_error <- union(resumen_metricas_ba_eng_m6, resumen_error_ba_eng_m6)
resumen_metricas_ba_eng_m6_with_error

resumen_metricas_ingles_bosques_aleatorios <- cbind(resumen_metricas_ingles_bosques_aleatorios, resumen_metricas_ba_eng_m6_with_error[2])

plot(bosques_aleatorios_eng_m6)
#*****************************************************************************************************************************
#*****************************************************************************************************************************
#############SUPPORT VECTOR MACHINES
#***************************************************** MODELO 1 **************************************************************
#grid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))

#svm_eng_m1 <- train(Clasificacion ~WC+WPS+Sixltr+pronoun+article+adverb+verb+QMark+Exclam+Apostro+auxverb+Funct,
#                    data = deng.entrenamiento[c(2:2,6:80)], 
#                    method = "svmLinear", 
#                    trControl=t_control,
#                    preProcess = c("center", "scale"),
#                    tuneGrid = grid,
#                    tuneLength = 10)

svm_eng_m1 <- train(Clasificacion ~WC+WPS+Sixltr+pronoun+article+adverb+verb+QMark+Exclam+Apostro+auxverb+Funct,
                    data = deng.entrenamiento[c(2:2,6:80)], 
                    method = "svmRadial", 
                    trControl=t_control,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)


#Resultado del modelo entrenado
svm_eng_m1
plot(svm_eng_m1)

##Prediccion del conjunto de datos de prueba

prediccion_svm_eng_m1 <- predict(svm_eng_m1, newdata = deng.prueba[c(2:2,6:80)])
prediccion_svm_eng_m1

#Matriz de confusion
cm_svm_eng_m1 <-confusionMatrix(prediccion_svm_eng_m1, deng.prueba$Clasificacion)
metrics_svm_eng_m1 <-cm_svm_eng_m1$byClass

resumen_metricas_svm_eng_m1 <- data.frame(keyName=names(metrics_svm_eng_m1), value=metrics_svm_eng_m1, row.names=NULL)
names(resumen_metricas_svm_eng_m1) <- c("Metrica", "M1 SVM")
resumen_metricas_svm_eng_m1[2]

#Calculo del error
error_rate = round(mean(prediccion_svm_eng_m1 != deng.prueba$Clasificacion),2)
error_rate

resumen_error_svm_eng_m1 <- data.frame(Metrica=c("Tasa de Error"), value=c(error_rate), row.names=NULL)
names(resumen_error_svm_eng_m1) <- c("Metrica", "M1 SVM")
resumen_error_svm_eng_m1

resumen_metricas_svm_eng_m1_with_error <- union(resumen_metricas_svm_eng_m1, resumen_error_svm_eng_m1)
resumen_metricas_svm_eng_m1_with_error

resumen_metricas_ingles_svm <- cbind(resumen_metricas_ingles_svm, resumen_metricas_svm_eng_m1_with_error[2])

#***************************************************** MODELO 2 **************************************************************
#svm_eng_m2 <- train(Clasificacion ~adverb+Sixltr+negemo,
#                    data = deng.entrenamiento[c(2:2,6:80)], 
#                    method = "svmLinear", 
#                    trControl=t_control,
#                    preProcess = c("center", "scale"),
#                   tuneLength = 10)

svm_eng_m2 <- train(Clasificacion ~adverb+Sixltr+negemo,
                    data = deng.entrenamiento[c(2:2,6:80)], 
                    method = "svmRadial", 
                    trControl=t_control,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)
#Resultado del modelo entrenado
svm_eng_m2
plot(svm_eng_m2)
##Prediccion del conjunto de datos de prueba

prediccion_svm_eng_m2 <- predict(svm_eng_m2, newdata = deng.prueba[c(2:2,6:80)])
prediccion_svm_eng_m2

#Matriz de confusion
cm_svm_eng_m2 <-confusionMatrix(prediccion_svm_eng_m2, deng.prueba$Clasificacion)
metrics_svm_eng_m2 <-cm_svm_eng_m2$byClass

resumen_metricas_svm_eng_m2 <- data.frame(keyName=names(metrics_svm_eng_m2), value=metrics_svm_eng_m2, row.names=NULL)
names(resumen_metricas_svm_eng_m2) <- c("Metrica", "M2 SVM")
resumen_metricas_svm_eng_m2[2]

#Calculo del error
error_rate = round(mean(prediccion_svm_eng_m2 != deng.prueba$Clasificacion),2)
error_rate

resumen_error_svm_eng_m2 <- data.frame(Metrica=c("Tasa de Error"), value=c(error_rate), row.names=NULL)
names(resumen_error_svm_eng_m2) <- c("Metrica", "M2 SVM")
resumen_error_svm_eng_m2

resumen_metricas_svm_eng_m2_with_error <- union(resumen_metricas_svm_eng_m2, resumen_error_svm_eng_m2)
resumen_metricas_svm_eng_m2_with_error

resumen_metricas_ingles_svm <- cbind(resumen_metricas_ingles_svm, resumen_metricas_svm_eng_m2_with_error[2])


#***************************************************** MODELO 3 **************************************************************
#svm_eng_m3 <- train(Clasificacion ~QMark+informal+prep,
#                    data = deng.entrenamiento[c(2:2,6:80)], 
#                    method = "svmLinear", 
#                    trControl=t_control,
#                    preProcess = c("center", "scale"),
#                    tuneGrid = grid,
#                    tuneLength = 10)

svm_eng_m3 <- train(Clasificacion ~QMark+informal+prep,
                    data = deng.entrenamiento[c(2:2,6:80)], 
                    method = "svmRadial", 
                    trControl=t_control,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)

#Resultado del modelo entrenado
svm_eng_m3
plot(svm_eng_m3)
##Prediccion del conjunto de datos de prueba

prediccion_svm_eng_m3 <- predict(svm_eng_m3, newdata = deng.prueba[c(2:2,6:80)])
prediccion_svm_eng_m3

#Matriz de confusion
cm_svm_eng_m3 <-confusionMatrix(prediccion_svm_eng_m3, deng.prueba$Clasificacion)
metrics_svm_eng_m3 <-cm_svm_eng_m3$byClass

resumen_metricas_svm_eng_m3 <- data.frame(keyName=names(metrics_svm_eng_m3), value=metrics_svm_eng_m3, row.names=NULL)
names(resumen_metricas_svm_eng_m3) <- c("Metrica", "M3 SVM")
resumen_metricas_svm_eng_m3[2]

#Calculo del error
error_rate = round(mean(prediccion_svm_eng_m3 != deng.prueba$Clasificacion),2)
error_rate

resumen_error_svm_eng_m3 <- data.frame(Metrica=c("Tasa de Error"), value=c(error_rate), row.names=NULL)
names(resumen_error_svm_eng_m3) <- c("Metrica", "M3 SVM")
resumen_error_svm_eng_m3

resumen_metricas_svm_eng_m3_with_error <- union(resumen_metricas_svm_eng_m3, resumen_error_svm_eng_m3)
resumen_metricas_svm_eng_m3_with_error

resumen_metricas_ingles_svm <- cbind(resumen_metricas_ingles_svm, resumen_metricas_svm_eng_m3_with_error[2])

#***************************************************** MODELO 4 **************************************************************
#svm_eng_m4 <- train(Clasificacion ~.,
#                    data = deng.entrenamiento[c(2:2,6:80)], 
#                    method = "svmLinear", 
#                    trControl=t_control,
#                    preProcess = c("center", "scale"),
#                    tuneGrid = grid,
#                    tuneLength = 10)

svm_eng_m4 <- train(Clasificacion ~.,
                    data = deng.entrenamiento[c(2:2,6:80)], 
                    method = "svmRadial", 
                    trControl=t_control,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)

#Resultado del modelo entrenado
svm_eng_m4
plot(svm_eng_m4)
##Prediccion del conjunto de datos de prueba

prediccion_svm_eng_m4 <- predict(svm_eng_m4, newdata = deng.prueba[c(2:2,6:80)])
prediccion_svm_eng_m4

#Matriz de confusion
cm_svm_eng_m4 <-confusionMatrix(prediccion_svm_eng_m4, deng.prueba$Clasificacion)
metrics_svm_eng_m4 <-cm_svm_eng_m4$byClass

resumen_metricas_svm_eng_m4 <- data.frame(keyName=names(metrics_svm_eng_m4), value=metrics_svm_eng_m4, row.names=NULL)
names(resumen_metricas_svm_eng_m4) <- c("Metrica", "M4 SVM")
resumen_metricas_svm_eng_m4[2]

#Calculo del error
error_rate = round(mean(prediccion_svm_eng_m4 != deng.prueba$Clasificacion),2)
error_rate

resumen_error_svm_eng_m4 <- data.frame(Metrica=c("Tasa de Error"), value=c(error_rate), row.names=NULL)
names(resumen_error_svm_eng_m4) <- c("Metrica", "M4 SVM")
resumen_error_svm_eng_m4

resumen_metricas_svm_eng_m4_with_error <- union(resumen_metricas_svm_eng_m4, resumen_error_svm_eng_m4)
resumen_metricas_svm_eng_m4_with_error

resumen_metricas_ingles_svm <- cbind(resumen_metricas_ingles_svm, resumen_metricas_svm_eng_m4_with_error[2])

#***************************************************** MODELO 5 **************************************************************
#svm_eng_m5 <- train(Clasificacion ~WC+Analytic+Clout+Authentic+Tone+WPS+Sixltr+Funct,
#                    data = deng.entrenamiento[c(2:2,6:80)], 
#                    method = "svmLinear", 
#                    trControl=t_control,
#                    preProcess = c("center", "scale"),
#                    tuneGrid = grid,
#                    tuneLength = 10)

svm_eng_m5 <- train(Clasificacion ~WC+Analytic+Clout+Authentic+Tone+WPS+Sixltr+Funct,
                    data = deng.entrenamiento[c(2:2,6:80)], 
                    method = "svmRadial", 
                    trControl=t_control,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)



#Resultado del modelo entrenado
svm_eng_m5
plot(svm_eng_m5)
##Prediccion del conjunto de datos de prueba

prediccion_svm_eng_m5 <- predict(svm_eng_m5, newdata = deng.prueba[c(2:2,6:80)])
prediccion_svm_eng_m5

#Matriz de confusion
cm_svm_eng_m5 <-confusionMatrix(prediccion_svm_eng_m5, deng.prueba$Clasificacion)
metrics_svm_eng_m5 <-cm_svm_eng_m5$byClass

resumen_metricas_svm_eng_m5 <- data.frame(keyName=names(metrics_svm_eng_m5), value=metrics_svm_eng_m5, row.names=NULL)
names(resumen_metricas_svm_eng_m5) <- c("Metrica", "M5 SVM")
resumen_metricas_svm_eng_m5[2]

#Calculo del error
error_rate = round(mean(prediccion_svm_eng_m5 != deng.prueba$Clasificacion),2)
error_rate

resumen_error_svm_eng_m5 <- data.frame(Metrica=c("Tasa de Error"), value=c(error_rate), row.names=NULL)
names(resumen_error_svm_eng_m5) <- c("Metrica", "M5 SVM")
resumen_error_svm_eng_m5

resumen_metricas_svm_eng_m5_with_error <- union(resumen_metricas_svm_eng_m5, resumen_error_svm_eng_m5)
resumen_metricas_svm_eng_m5_with_error

resumen_metricas_ingles_svm <- cbind(resumen_metricas_ingles_svm, resumen_metricas_svm_eng_m5_with_error[2])

#***************************************************** MODELO 6 **************************************************************

#svm_eng_m6 <- train(Clasificacion ~WC+Analytic+Clout+Authentic+Tone+WPS+Sixltr+Dic+Funct+pronoun+article+prep+auxverb+adverb
#                                    +conj+negate+verb+adj+compare+interrog+number+quant+AllPunc+Period+Colon+SemiC+QMark+Exclam+Dash+Apostro+Parenth,
#                    data = deng.entrenamiento[c(2:2,6:80)], 
#                    method = "svmLinear", 
#                    trControl=t_control,
#                    preProcess = c("center", "scale"),
#                    tuneLength = 10,
#                    tuneGrid = grid)

svm_eng_m6 <- train(Clasificacion ~WC+Analytic+Clout+Authentic+Tone+WPS+Sixltr+Dic+Funct+pronoun+article+prep+auxverb+adverb
                    +conj+negate+verb+adj+compare+interrog+number+quant+AllPunc+Period+Colon+SemiC+QMark+Exclam+Dash+Apostro+Parenth,
                    data = deng.entrenamiento[c(2:2,6:80)], 
                    method = "svmRadial", 
                    trControl=t_control,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)


#Resultado del modelo entrenado
svm_eng_m6
plot(svm_eng_m6)
##Prediccion del conjunto de datos de prueba

prediccion_svm_eng_m6 <- predict(svm_eng_m6, newdata = deng.prueba[c(2:2,6:80)])
prediccion_svm_eng_m6

#Matriz de confusion
cm_svm_eng_m6 <-confusionMatrix(prediccion_svm_eng_m6, deng.prueba$Clasificacion)
metrics_svm_eng_m6 <-cm_svm_eng_m6$byClass

resumen_metricas_svm_eng_m6 <- data.frame(keyName=names(metrics_svm_eng_m6), value=metrics_svm_eng_m6, row.names=NULL)
names(resumen_metricas_svm_eng_m6) <- c("Metrica", "M6 SVM")
resumen_metricas_svm_eng_m6[2]

#Calculo del error
error_rate = round(mean(prediccion_svm_eng_m6 != deng.prueba$Clasificacion),2)
error_rate

resumen_error_svm_eng_m6 <- data.frame(Metrica=c("Tasa de Error"), value=c(error_rate), row.names=NULL)
names(resumen_error_svm_eng_m6) <- c("Metrica", "M6 SVM")
resumen_error_svm_eng_m6

resumen_metricas_svm_eng_m6_with_error <- union(resumen_metricas_svm_eng_m6, resumen_error_svm_eng_m6)
resumen_metricas_svm_eng_m6_with_error

resumen_metricas_ingles_svm <- cbind(resumen_metricas_ingles_svm, resumen_metricas_svm_eng_m6_with_error[2])


#*****************************************************************************************************************************
#*****************************************************************************************************************************
#############ARBOLES DE DECISION
#***************************************************** MODELO 1 **************************************************************

arbol_eng_m1 <- train(Clasificacion ~WC+WPS+Sixltr+pronoun+article+adverb+verb+QMark+Exclam+Apostro+auxverb+Funct,
               data = deng.entrenamiento[c(2:2, 6:80)], 
               method = "rpart", 
               trControl=t_control,
               parms = list(split = "information"),
               tuneLength = 10)

#Resultado del modelo entrenado
arbol_eng_m1
plot(arbol_eng_m1)
fancyRpartPlot(arbol_eng_m1$finalModel)

##Prediccion del conjunto de datos de prueba

prediccion_ad_eng_m1 <- predict(arbol_eng_m1, newdata = deng.prueba[c(2:2,6:80)])
prediccion_ad_eng_m1

#Matriz de confusion
cm_ad_eng_m1 <-confusionMatrix(prediccion_ad_eng_m1, deng.prueba$Clasificacion)
metrics_ad_eng_m1 <-cm_ad_eng_m1$byClass

resumen_metricas_ad_eng_m1 <- data.frame(keyName=names(metrics_ad_eng_m1), value=metrics_ad_eng_m1, row.names=NULL)
names(resumen_metricas_ad_eng_m1) <- c("Metrica", "M1 AD")
resumen_metricas_ad_eng_m1[2]

#Calculo del error
error_rate = round(mean(prediccion_ad_eng_m1 != deng.prueba$Clasificacion),2)
error_rate

resumen_error_ad_eng_m1 <- data.frame(Metrica=c("Tasa de Error"), value=c(error_rate), row.names=NULL)
names(resumen_error_ad_eng_m1) <- c("Metrica", "M1 AD")
resumen_error_ad_eng_m1

resumen_metricas_ad_eng_m1_with_error <- union(resumen_metricas_ad_eng_m1, resumen_error_ad_eng_m1)
resumen_metricas_ad_eng_m1_with_error

resumen_metricas_ingles_ad <- cbind(resumen_metricas_ingles_ad, resumen_metricas_ad_eng_m1_with_error[2])

#***************************************************** MODELO 2 **************************************************************

arbol_eng_m2 <- train(Clasificacion ~adverb+Sixltr+negemo,
               data = deng.entrenamiento[c(2:2, 6:80)], 
               method = "rpart", 
               trControl=t_control,
               parms = list(split = "information"),
               tuneLength = 10)

#Resultado del modelo entrenado
arbol_eng_m2
plot(arbol_eng_m2)
fancyRpartPlot(arbol_eng_m2$finalModel)

##Prediccion del conjunto de datos de prueba

prediccion_ad_eng_m2 <- predict(arbol_eng_m2, newdata = deng.prueba[c(2:2,6:80)])
prediccion_ad_eng_m2

#Matriz de confusion
cm_ad_eng_m2 <-confusionMatrix(prediccion_ad_eng_m2, deng.prueba$Clasificacion)
metrics_ad_eng_m2 <-cm_ad_eng_m2$byClass

resumen_metricas_ad_eng_m2 <- data.frame(keyName=names(metrics_ad_eng_m2), value=metrics_ad_eng_m2, row.names=NULL)
names(resumen_metricas_ad_eng_m2) <- c("Metrica", "M2 AD")
resumen_metricas_ad_eng_m2[2]

#Calculo del error
error_rate = round(mean(prediccion_ad_eng_m2 != deng.prueba$Clasificacion),2)
error_rate

resumen_error_ad_eng_m2 <- data.frame(Metrica=c("Tasa de Error"), value=c(error_rate), row.names=NULL)
names(resumen_error_ad_eng_m2) <- c("Metrica", "M2 AD")
resumen_error_ad_eng_m2

resumen_metricas_ad_eng_m2_with_error <- union(resumen_metricas_ad_eng_m2, resumen_error_ad_eng_m2)
resumen_metricas_ad_eng_m2_with_error

resumen_metricas_ingles_ad <- cbind(resumen_metricas_ingles_ad, resumen_metricas_ad_eng_m2_with_error[2])
#***************************************************** MODELO 3 **************************************************************
arbol_eng_m3 <- train(Clasificacion ~QMark+informal+prep, data = deng.entrenamiento[c(2:2, 6:80)], 
               method = "rpart", 
               trControl=t_control,
               parms = list(split = "information"),
               tuneLength = 10)

#Resultado del modelo entrenado
arbol_eng_m3
plot(arbol_eng_m3)
fancyRpartPlot(arbol_eng_m3$finalModel)

##Prediccion del conjunto de datos de prueba

prediccion_ad_eng_m3 <- predict(arbol_eng_m3, newdata = deng.prueba[c(2:2,6:80)])
prediccion_ad_eng_m3

#Matriz de confusion
cm_ad_eng_m3 <-confusionMatrix(prediccion_ad_eng_m3, deng.prueba$Clasificacion)
metrics_ad_eng_m3 <-cm_ad_eng_m3$byClass

resumen_metricas_ad_eng_m3 <- data.frame(keyName=names(metrics_ad_eng_m3), value=metrics_ad_eng_m3, row.names=NULL)
names(resumen_metricas_ad_eng_m3) <- c("Metrica", "M3 AD")
resumen_metricas_ad_eng_m3[2]

#Calculo del error
error_rate = round(mean(prediccion_ad_eng_m3 != deng.prueba$Clasificacion),2)
error_rate

resumen_error_ad_eng_m3 <- data.frame(Metrica=c("Tasa de Error"), value=c(error_rate), row.names=NULL)
names(resumen_error_ad_eng_m3) <- c("Metrica", "M3 AD")
resumen_error_ad_eng_m3

resumen_metricas_ad_eng_m3_with_error <- union(resumen_metricas_ad_eng_m3, resumen_error_ad_eng_m3)
resumen_metricas_ad_eng_m3_with_error

resumen_metricas_ingles_ad <- cbind(resumen_metricas_ingles_ad, resumen_metricas_ad_eng_m3_with_error[2])

#***************************************************** MODELO 4 **************************************************************
arbol_eng_m4 <- train(Clasificacion ~., data = deng.entrenamiento[c(2:2, 6:80)], 
               method = "rpart", 
               trControl=t_control,
               parms = list(split = "information"),
               tuneLength = 10)

#Resultado del modelo entrenado
arbol_eng_m4
plot(arbol_eng_m4)
fancyRpartPlot(arbol_eng_m4$finalModel)

##Prediccion del conjunto de datos de prueba

prediccion_ad_eng_m4 <- predict(arbol_eng_m4, newdata = deng.prueba[c(2:2,6:80)])
prediccion_ad_eng_m4

#Matriz de confusion
cm_ad_eng_m4 <-confusionMatrix(prediccion_ad_eng_m4, deng.prueba$Clasificacion)
metrics_ad_eng_m4 <-cm_ad_eng_m4$byClass

resumen_metricas_ad_eng_m4 <- data.frame(keyName=names(metrics_ad_eng_m4), value=metrics_ad_eng_m4, row.names=NULL)
names(resumen_metricas_ad_eng_m4) <- c("Metrica", "M4 AD")
resumen_metricas_ad_eng_m4[2]

#Calculo del error
error_rate = round(mean(prediccion_ad_eng_m4 != deng.prueba$Clasificacion),2)
error_rate

resumen_error_ad_eng_m4 <- data.frame(Metrica=c("Tasa de Error"), value=c(error_rate), row.names=NULL)
names(resumen_error_ad_eng_m4) <- c("Metrica", "M4 AD")
resumen_error_ad_eng_m4

resumen_metricas_ad_eng_m4_with_error <- union(resumen_metricas_ad_eng_m4, resumen_error_ad_eng_m4)
resumen_metricas_ad_eng_m4_with_error

resumen_metricas_ingles_ad <- cbind(resumen_metricas_ingles_ad, resumen_metricas_ad_eng_m4_with_error[2])


#***************************************************** MODELO 5 **************************************************************
arbol_eng_m5 <- train(Clasificacion ~WC+Analytic+Clout+Authentic+Tone+WPS+Sixltr+Funct,
                      data = deng.entrenamiento[c(2:2, 6:80)], 
                      method = "rpart", 
                      trControl=t_control,
                      parms = list(split = "information"),
                      tuneLength = 10)

#Resultado del modelo entrenado
arbol_eng_m5
plot(arbol_eng_m5)
fancyRpartPlot(arbol_eng_m5$finalModel)

##Prediccion del conjunto de datos de prueba

prediccion_ad_eng_m5 <- predict(arbol_eng_m5, newdata = deng.prueba[c(2:2,6:80)])
prediccion_ad_eng_m5

#Matriz de confusion
cm_ad_eng_m5 <-confusionMatrix(prediccion_ad_eng_m5, deng.prueba$Clasificacion)
metrics_ad_eng_m5 <-cm_ad_eng_m5$byClass

resumen_metricas_ad_eng_m5 <- data.frame(keyName=names(metrics_ad_eng_m5), value=metrics_ad_eng_m5, row.names=NULL)
names(resumen_metricas_ad_eng_m5) <- c("Metrica", "M5 AD")
resumen_metricas_ad_eng_m5[2]

#Calculo del error
error_rate = round(mean(prediccion_ad_eng_m5 != deng.prueba$Clasificacion),2)
error_rate

resumen_error_ad_eng_m5 <- data.frame(Metrica=c("Tasa de Error"), value=c(error_rate), row.names=NULL)
names(resumen_error_ad_eng_m5) <- c("Metrica", "M5 AD")
resumen_error_ad_eng_m5

resumen_metricas_ad_eng_m5_with_error <- union(resumen_metricas_ad_eng_m5, resumen_error_ad_eng_m5)
resumen_metricas_ad_eng_m5_with_error

resumen_metricas_ingles_ad <- cbind(resumen_metricas_ingles_ad, resumen_metricas_ad_eng_m5_with_error[2])

#***************************************************** MODELO 6 **************************************************************
arbol_eng_m6 <- train(Clasificacion ~WC+Analytic+Clout+Authentic+Tone+WPS+Sixltr+Dic+Funct+pronoun+article+prep+auxverb+adverb+conj+negate+verb+adj+compare+interrog+number+quant+AllPunc+Period+Colon+SemiC+QMark+Exclam+Dash+Apostro+Parenth,
                data = deng.entrenamiento[c(2:2, 6:80)], 
                method = "rpart", 
                trControl=t_control,
                parms = list(split = "information"),
                tuneLength = 10)

#Resultado del modelo entrenado
arbol_eng_m6
plot(arbol_eng_m6)
fancyRpartPlot(arbol_eng_m6$finalModel)

##Prediccion del conjunto de datos de prueba

prediccion_ad_eng_m6 <- predict(arbol_eng_m6, newdata = deng.prueba[c(2:2,6:80)])
prediccion_ad_eng_m6

#Matriz de confusion
cm_ad_eng_m6 <-confusionMatrix(prediccion_ad_eng_m6, deng.prueba$Clasificacion)
metrics_ad_eng_m6 <-cm_ad_eng_m6$byClass

resumen_metricas_ad_eng_m6 <- data.frame(keyName=names(metrics_ad_eng_m6), value=metrics_ad_eng_m6, row.names=NULL)
names(resumen_metricas_ad_eng_m6) <- c("Metrica", "M6 AD")
resumen_metricas_ad_eng_m6[2]

#Calculo del error
error_rate = round(mean(prediccion_ad_eng_m6 != deng.prueba$Clasificacion),2)
error_rate

resumen_error_ad_eng_m6 <- data.frame(Metrica=c("Tasa de Error"), value=c(error_rate), row.names=NULL)
names(resumen_error_ad_eng_m6) <- c("Metrica", "M6 AD")
resumen_error_ad_eng_m6

resumen_metricas_ad_eng_m6_with_error <- union(resumen_metricas_ad_eng_m6, resumen_error_ad_eng_m6)
resumen_metricas_ad_eng_m6_with_error

resumen_metricas_ingles_ad <- cbind(resumen_metricas_ingles_ad, resumen_metricas_ad_eng_m6_with_error[2])

#*****************************************************************************************************************************
#***************************************************** MODELOS EN ESPANNOL *****************************************************
#*****************************************************************************************************************************
#############REGRESION LOGISTICA
#***************************************************** MODELO 1 **************************************************************
regresion_logistica_esp_m1 <- train(Clasificacion ~WC+WPS+Sixltr+TotPron+Articulo+Adverb+Verbos+QMark+Exclam+Apostro+VerbAux+Funct, data = desp.entrenamiento[c(2:2,6:69)], method ="glm",
                                    family ="binomial", trControl=t_control,preProcess = c("center", "scale"),
                                    tuneLength = 10)

regresion_logistica_esp_m1
##Prediccion del conjunto de datos de prueba

prediccion_rl_esp_m1 <- predict(regresion_logistica_esp_m1, newdata = desp.prueba[c(2:2,6:69)])
prediccion_rl_esp_m1

#Matriz de confusion
cm_rl_esp_m1 <- confusionMatrix(prediccion_rl_esp_m1, desp.prueba$Clasificacion)

metrics_rl_esp_m1 <-cm_rl_esp_m1$byClass

resumen_metricas_lr_esp_m1 <- data.frame(keyName=names(metrics_rl_esp_m1), value=metrics_rl_esp_m1, row.names=NULL)
names(resumen_metricas_lr_esp_m1) <- c("Metrica", "M1 LR")
resumen_metricas_lr_esp_m1[2]

#Calculo del error
error_rate = round(mean(prediccion_rl_esp_m1 != desp.prueba$Clasificacion),2)
error_rate

resumen_error_rl_esp_m1 <- data.frame(Metrica=c("Tasa de Error"), value=c(error_rate), row.names=NULL)
names(resumen_error_rl_esp_m1) <- c("Metrica", "M1 LR")
resumen_error_rl_esp_m1

resumen_metricas_lr_esp_m1_with_error <- union(resumen_metricas_lr_esp_m1, resumen_error_rl_esp_m1)
resumen_metricas_lr_esp_m1_with_error

resumen_metricas_espannol_regresion_logistica <- cbind(resumen_metricas_espannol_regresion_logistica, resumen_metricas_lr_esp_m1_with_error[2])
#*****************************************************************************************************************************
#**************************************************** MODELO 2 ***************************************************************
regresion_logistica_esp_m2 <- train(Clasificacion ~ Adverb+Sixltr+EmoNeg, data = desp.entrenamiento[c(2:2,6:69)], 
                                    method ="glm",
                                    family ="binomial", 
                                    trControl=t_control,preProcess = c("center", "scale"),
                                    tuneLength = 10)

regresion_logistica_esp_m2
##Prediccion del conjunto de datos de prueba

prediccion_rl_esp_m2 <- predict(regresion_logistica_esp_m2, newdata = desp.prueba[c(2:2,6:69)])
prediccion_rl_esp_m2

#Matriz de confusion
cm_rl_esp_m2 <- confusionMatrix(prediccion_rl_esp_m2, desp.prueba$Clasificacion)

metrics_rl_esp_m2 <-cm_rl_esp_m2$byClass

resumen_metricas_lr_esp_m2 <- data.frame(keyName=names(metrics_rl_esp_m2), value=metrics_rl_esp_m2, row.names=NULL)
names(resumen_metricas_lr_esp_m2) <- c("Metrica", "M2 LR")
resumen_metricas_lr_esp_m2[2]

#Calculo del error
error_rate = round(mean(prediccion_rl_esp_m2 != desp.prueba$Clasificacion),2)
error_rate

resumen_error_rl_esp_m2 <- data.frame(Metrica=c("Tasa de Error"), value=c(error_rate), row.names=NULL)
names(resumen_error_rl_esp_m2) <- c("Metrica", "M2 LR")
resumen_error_rl_esp_m2

resumen_metricas_lr_esp_m2_with_error <- union(resumen_metricas_lr_esp_m2, resumen_error_rl_esp_m2)
resumen_metricas_lr_esp_m2_with_error

resumen_metricas_espannol_regresion_logistica <- cbind(resumen_metricas_espannol_regresion_logistica, resumen_metricas_lr_esp_m2_with_error[2])
#*****************************************************************************************************************************
#**************************************************** MODELO 3 ***************************************************************
regresion_logistica_esp_m3 <- train(Clasificacion ~QMark+informal+Prepos
                                    , data = desp.entrenamiento[c(2:2,6:69)], 
                                    method ="glm",
                                    family ="binomial", 
                                    trControl=t_control,preProcess = c("center", "scale"),
                                    tuneLength = 10)

regresion_logistica_esp_m3
##Prediccion del conjunto de datos de prueba

prediccion_rl_esp_m3 <- predict(regresion_logistica_esp_m3, newdata = desp.prueba[c(2:2,6:69)])
prediccion_rl_esp_m3

#Matriz de confusion
cm_rl_esp_m3 <- confusionMatrix(prediccion_rl_esp_m3, desp.prueba$Clasificacion)

metrics_rl_esp_m3 <-cm_rl_esp_m2$byClass

resumen_metricas_lr_esp_m3 <- data.frame(keyName=names(metrics_rl_esp_m3), value=metrics_rl_esp_m3, row.names=NULL)
names(resumen_metricas_lr_esp_m3) <- c("Metrica", "M3 LR")
resumen_metricas_lr_esp_m3[2]

#Calculo del error
error_rate = round(mean(prediccion_rl_esp_m3 != desp.prueba$Clasificacion),2)
error_rate

resumen_error_rl_esp_m3 <- data.frame(Metrica=c("Tasa de Error"), value=c(error_rate), row.names=NULL)
names(resumen_error_rl_esp_m3) <- c("Metrica", "M3 LR")
resumen_error_rl_esp_m3

resumen_metricas_lr_esp_m3_with_error <- union(resumen_metricas_lr_esp_m3, resumen_error_rl_esp_m3)
resumen_metricas_lr_esp_m3_with_error

resumen_metricas_espannol_regresion_logistica <- cbind(resumen_metricas_espannol_regresion_logistica, resumen_metricas_lr_esp_m3_with_error[2])
#***************************************************** MODELO 4 **************************************************************
regresion_logistica_esp_m4 <- train(Clasificacion ~., data = desp.entrenamiento[c(2:2,6:69)], method ="glm",
                                    family ="binomial", trControl=t_control,preProcess = c("center", "scale"),
                                    tuneLength = 10)

regresion_logistica_esp_m4
##Prediccion del conjunto de datos de prueba

prediccion_rl_esp_m4 <- predict(regresion_logistica_esp_m4, newdata = desp.prueba[c(2:2,6:69)])
prediccion_rl_esp_m4

#Matriz de confusion
cm_rl_esp_m4 <- confusionMatrix(prediccion_rl_esp_m4, desp.prueba$Clasificacion)

metrics_rl_esp_m4 <-cm_rl_esp_m4$byClass

resumen_metricas_lr_esp_m4 <- data.frame(keyName=names(metrics_rl_esp_m4), value=metrics_rl_esp_m4, row.names=NULL)
names(resumen_metricas_lr_esp_m4) <- c("Metrica", "M4 LR")
resumen_metricas_lr_esp_m4[2]

#Calculo del error
error_rate = round(mean(prediccion_rl_esp_m4 != desp.prueba$Clasificacion),2)
error_rate

resumen_error_rl_esp_m4 <- data.frame(Metrica=c("Tasa de Error"), value=c(error_rate), row.names=NULL)
names(resumen_error_rl_esp_m4) <- c("Metrica", "M4 LR")
resumen_error_rl_esp_m4

resumen_metricas_lr_esp_m4_with_error <- union(resumen_metricas_lr_esp_m4, resumen_error_rl_esp_m4)
resumen_metricas_lr_esp_m4_with_error

resumen_metricas_espannol_regresion_logistica <- cbind(resumen_metricas_espannol_regresion_logistica, resumen_metricas_lr_esp_m4_with_error[2])
#*****************************************************************************************************************************
#**************************************************** MODELO 5 ***************************************************************
regresion_logistica_esp_m5 <- train(Clasificacion ~ WC+WPS+Sixltr+Funct, data = desp.entrenamiento[c(2:2,6:69)], 
                                    method ="glm",
                                    family ="binomial", 
                                    trControl=t_control,preProcess = c("center", "scale"),
                                    tuneLength = 10)

regresion_logistica_esp_m5
##Prediccion del conjunto de datos de prueba

prediccion_rl_esp_m5 <- predict(regresion_logistica_esp_m5, newdata = desp.prueba[c(2:2,6:69)])
prediccion_rl_esp_m5

#Matriz de confusion
cm_rl_esp_m5 <- confusionMatrix(prediccion_rl_esp_m5, desp.prueba$Clasificacion)

metrics_rl_esp_m5 <-cm_rl_esp_m5$byClass

resumen_metricas_lr_esp_m5 <- data.frame(keyName=names(metrics_rl_esp_m5), value=metrics_rl_esp_m5, row.names=NULL)
names(resumen_metricas_lr_esp_m5) <- c("Metrica", "M5 LR")
resumen_metricas_lr_esp_m5[2]

#Calculo del error
error_rate = round(mean(prediccion_rl_esp_m5 != desp.prueba$Clasificacion),2)
error_rate

resumen_error_rl_esp_m5 <- data.frame(Metrica=c("Tasa de Error"), value=c(error_rate), row.names=NULL)
names(resumen_error_rl_esp_m5) <- c("Metrica", "M5 LR")
resumen_error_rl_esp_m5

resumen_metricas_lr_esp_m5_with_error <- union(resumen_metricas_lr_esp_m5, resumen_error_rl_esp_m5)
resumen_metricas_lr_esp_m5_with_error

resumen_metricas_espannol_regresion_logistica <- cbind(resumen_metricas_espannol_regresion_logistica, resumen_metricas_lr_esp_m5_with_error[2])
#*****************************************************************************************************************************
#**************************************************** MODELO 6 ***************************************************************
regresion_logistica_esp_m6 <- train(Clasificacion ~WC+WPS+Sixltr+Dic+Funct+TotPron+PronPer+Articulo+Verbos+VerbAux+Pasado+Present+Futuro+Adverb+Prepos+Conjunc+Negacio+Cuantif+Numeros+Maldec+Subjuntiv+formal+informal+AllPunc+Period+Colon+SemiC+QMark+Exclam+Dash+Apostro+Parenth+OtherP
                                    , data = desp.entrenamiento[c(2:2,6:69)], 
                                    method ="glm",
                                    family ="binomial", 
                                    trControl=t_control,preProcess = c("center", "scale"),
                                    tuneLength = 10)

regresion_logistica_esp_m6
##Prediccion del conjunto de datos de prueba

prediccion_rl_esp_m6 <- predict(regresion_logistica_esp_m6, newdata = desp.prueba[c(2:2,6:69)])
prediccion_rl_esp_m6

#Matriz de confusion
cm_rl_esp_m6 <- confusionMatrix(prediccion_rl_esp_m3, desp.prueba$Clasificacion)

metrics_rl_esp_m6 <-cm_rl_esp_m2$byClass

resumen_metricas_lr_esp_m6 <- data.frame(keyName=names(metrics_rl_esp_m6), value=metrics_rl_esp_m6, row.names=NULL)
names(resumen_metricas_lr_esp_m6) <- c("Metrica", "M6 LR")
resumen_metricas_lr_esp_m6[2]

#Calculo del error
error_rate = round(mean(prediccion_rl_esp_m6 != desp.prueba$Clasificacion),2)
error_rate

resumen_error_rl_esp_m6 <- data.frame(Metrica=c("Tasa de Error"), value=c(error_rate), row.names=NULL)
names(resumen_error_rl_esp_m6) <- c("Metrica", "M6 LR")
resumen_error_rl_esp_m6

resumen_metricas_lr_esp_m6_with_error <- union(resumen_metricas_lr_esp_m6, resumen_error_rl_esp_m6)
resumen_metricas_lr_esp_m6_with_error

resumen_metricas_espannol_regresion_logistica <- cbind(resumen_metricas_espannol_regresion_logistica, resumen_metricas_lr_esp_m6_with_error[2])

#*****************************************************************************************************************************
#*****************************************************************************************************************************
#############BOSQUES ALEATORIOS
#***************************************************** MODELO 1 **************************************************************
#mtry: Number of variable is randomly collected to be sampled at each split time.
#ntree: Number of branches will grow after each time split.
set.seed(42)
bosques_aleatorios_esp_m1 <- train(Clasificacion ~WC+WPS+Sixltr+TotPron+Articulo+Adverb+Verbos+QMark+Exclam+Apostro+VerbAux+Funct, data = desp.entrenamiento[c(2:2,6:69)], 
                                   method = "rf",
                                   ntree = 10,
                                   trControl=t_control,
                                   tuneLength = 10)

bosques_aleatorios_esp_m1
##Prediccion del conjunto de datos de prueba

prediccion_ba_esp_m1 <- predict(bosques_aleatorios_esp_m1, newdata = desp.prueba[c(2:2,6:69)])
prediccion_ba_esp_m1

#Matriz de confusion
cm_ba_esp_m1 <-confusionMatrix(prediccion_ba_esp_m1, desp.prueba$Clasificacion)

metrics_ba_esp_m1 <-cm_ba_esp_m1$byClass

resumen_metricas_ba_esp_m1 <- data.frame(keyName=names(metrics_ba_esp_m1), value=metrics_ba_esp_m1, row.names=NULL)
names(resumen_metricas_ba_esp_m1) <- c("Metrica", "M1 BA")
resumen_metricas_ba_esp_m1[2]

#Calculo del error
error_rate = round(mean(prediccion_ba_esp_m1 != desp.prueba$Clasificacion),2)
error_rate

resumen_error_ba_esp_m1 <- data.frame(Metrica=c("Tasa de Error"), value=c(error_rate), row.names=NULL)
names(resumen_error_ba_esp_m1) <- c("Metrica", "M1 BA")
resumen_error_ba_esp_m1

resumen_metricas_ba_esp_m1_with_error <- union(resumen_metricas_ba_esp_m1, resumen_error_ba_esp_m1)
resumen_metricas_ba_esp_m1_with_error

resumen_metricas_espannol_bosques_aleatorios <- cbind(resumen_metricas_espannol_bosques_aleatorios, resumen_metricas_ba_esp_m1_with_error[2])

plot(bosques_aleatorios_esp_m1)

#Area bajo la curva
prediccionROC.log.esp <- prediction(prediccion_ba_esp_m1, as.numeric(desp.prueba$Clasificacion))
as.numeric(performance(prediccionROC.log.esp, "auc")@y.values) 

ROCR.log.esp <- performance(prediccionROC.log.esp, "tpr", "fpr")#prediction, x axys, y  axis
ROCR.log.esp 

plot(ROCR.bosque.esp,
     main = 'Curva ROC - Bosques aleatorios',
     colorize=TRUE,
     print.cutoffs.at=seq(0,1,by=0.1),
     text.adj = c(-0.2,1.7))

#install.packages("Metrics")
library(Metrics)
pd <- predict(bosques_aleatorios_esp_m1 , desp.prueba[c(2:2,6:69)])
#mse(test$Clasificacion,pd)
#*****************************************************************************************************************************
#**************************************************** MODELO 2 ***************************************************************
bosques_aleatorios_esp_m2 <- train(Clasificacion ~ Adverb+Sixltr+EmoNeg,
                                   data = desp.entrenamiento[c(2:2,6:69)],
                                   method = "rf",
                                   ntree = 10,
                                   trControl=t_control,
                                   tuneLength = 10)

bosques_aleatorios_esp_m2
##Prediccion del conjunto de datos de prueba

prediccion_ba_esp_m2 <- predict(bosques_aleatorios_esp_m2, newdata = desp.prueba[c(2:2,6:69)])
prediccion_ba_esp_m2

#Matriz de confusion
cm_ba_esp_m2 <-confusionMatrix(prediccion_ba_esp_m2, desp.prueba$Clasificacion)

metrics_ba_esp_m2 <-cm_ba_esp_m2$byClass

resumen_metricas_ba_esp_m2 <- data.frame(keyName=names(metrics_ba_esp_m2), value=metrics_ba_esp_m2, row.names=NULL)
names(resumen_metricas_ba_esp_m2) <- c("Metrica", "M2 BA")
resumen_metricas_ba_esp_m2[2]

#Calculo del error
error_rate = round(mean(prediccion_ba_esp_m2 != desp.prueba$Clasificacion),2)
error_rate

resumen_error_ba_esp_m2 <- data.frame(Metrica=c("Tasa de Error"), value=c(error_rate), row.names=NULL)
names(resumen_error_ba_esp_m2) <- c("Metrica", "M2 BA")
resumen_error_ba_esp_m2

resumen_metricas_ba_esp_m2_with_error <- union(resumen_metricas_ba_esp_m2, resumen_error_ba_esp_m2)
resumen_metricas_ba_esp_m2_with_error

resumen_metricas_espannol_bosques_aleatorios <- cbind(resumen_metricas_espannol_bosques_aleatorios, resumen_metricas_ba_esp_m2_with_error[2])

plot(bosques_aleatorios_esp_m2)
#*****************************************************************************************************************************
#**************************************************** MODELO 3 ***************************************************************
bosques_aleatorios_esp_m3 <- train(Clasificacion ~ QMark+informal+Prepos
                                   , data = desp.entrenamiento[c(2:2,6:69)],
                                   method = "rf",
                                   ntree = 10,
                                   trControl=t_control,
                                   tuneLength = 10)

bosques_aleatorios_esp_m3
##Prediccion del conjunto de datos de prueba

prediccion_ba_esp_m3 <- predict(bosques_aleatorios_esp_m3, newdata = desp.prueba[c(2:2,6:69)])
prediccion_ba_esp_m3

#Matriz de confusion
cm_ba_esp_m3 <-confusionMatrix(prediccion_ba_esp_m3, desp.prueba$Clasificacion)

metrics_ba_esp_m3 <-cm_ba_esp_m3$byClass

resumen_metricas_ba_esp_m3 <- data.frame(keyName=names(metrics_ba_esp_m3), value=metrics_ba_esp_m3, row.names=NULL)
names(resumen_metricas_ba_esp_m3) <- c("Metrica", "M3 BA")
resumen_metricas_ba_esp_m3[2]

#Calculo del error
error_rate = round(mean(prediccion_ba_esp_m3 != desp.prueba$Clasificacion),2)
error_rate

resumen_error_ba_esp_m3 <- data.frame(Metrica=c("Tasa de Error"), value=c(error_rate), row.names=NULL)
names(resumen_error_ba_esp_m3) <- c("Metrica", "M3 BA")
resumen_error_ba_esp_m3

resumen_metricas_ba_esp_m3_with_error <- union(resumen_metricas_ba_esp_m3, resumen_error_ba_esp_m3)
resumen_metricas_ba_esp_m3_with_error

resumen_metricas_espannol_bosques_aleatorios <- cbind(resumen_metricas_espannol_bosques_aleatorios, resumen_metricas_ba_esp_m3_with_error[2])

plot(bosques_aleatorios_esp_m3)
#*****************************************************************************************************************************
#**************************************************** MODELO 4 ***************************************************************
bosques_aleatorios_esp_m4 <- train(Clasificacion ~., data = desp.entrenamiento[c(2:2,6:69)], 
                                   method = "rf",
                                   ntree = 10,
                                   trControl=t_control,
                                   tuneLength = 10)

bosques_aleatorios_esp_m4
##Prediccion del conjunto de datos de prueba

prediccion_ba_esp_m4 <- predict(bosques_aleatorios_esp_m4, newdata = desp.prueba[c(2:2,6:69)])
prediccion_ba_esp_m4

#Matriz de confusion
cm_ba_esp_m4 <-confusionMatrix(prediccion_ba_esp_m4, desp.prueba$Clasificacion)

metrics_ba_esp_m4 <-cm_ba_esp_m4$byClass

resumen_metricas_ba_esp_m4 <- data.frame(keyName=names(metrics_ba_esp_m4), value=metrics_ba_esp_m4, row.names=NULL)
names(resumen_metricas_ba_esp_m4) <- c("Metrica", "M4 BA")
resumen_metricas_ba_esp_m4[2]

#Calculo del error
error_rate = round(mean(prediccion_ba_esp_m4 != desp.prueba$Clasificacion),2)
error_rate

resumen_error_ba_esp_m4 <- data.frame(Metrica=c("Tasa de Error"), value=c(error_rate), row.names=NULL)
names(resumen_error_ba_esp_m4) <- c("Metrica", "M4 BA")
resumen_error_ba_esp_m4

resumen_metricas_ba_esp_m4_with_error <- union(resumen_metricas_ba_esp_m4, resumen_error_ba_esp_m4)
resumen_metricas_ba_esp_m4_with_error

resumen_metricas_espannol_bosques_aleatorios <- cbind(resumen_metricas_espannol_bosques_aleatorios, resumen_metricas_ba_esp_m4_with_error[2])

plot(bosques_aleatorios_esp_m4)

#Area bajo la curva
prediccionROC.log.esp <- prediction(prediccion_ba_esp_m4, as.numeric(desp.prueba$Clasificacion))
as.numeric(performance(prediccionROC.log.esp, "auc")@y.values) 

ROCR.log.esp <- performance(prediccionROC.log.esp, "tpr", "fpr")#prediction, x axys, y  axis
ROCR.log.esp 

plot(ROCR.bosque.esp,
     main = 'Curva ROC - Bosques aleatorios',
     colorize=TRUE,
     print.cutoffs.at=seq(0,1,by=0.1),
     text.adj = c(-0.2,1.7))

#install.packages("Metrics")
library(Metrics)
pd <- predict(bosques_aleatorios_esp_m4 , desp.prueba[c(2:2,6:69)])
#mse(test$Clasificacion,pd)
#*****************************************************************************************************************************
#**************************************************** MODELO 5 ***************************************************************
bosques_aleatorios_esp_m5 <- train(Clasificacion ~ WC+WPS+Sixltr+Funct,
                                   ,data = desp.entrenamiento[c(2:2,6:69)],
                                   method = "rf",
                                   ntree = 10,
                                   trControl=t_control,
                                   tuneLength = 10)

bosques_aleatorios_esp_m5
##Prediccion del conjunto de datos de prueba

prediccion_ba_esp_m5 <- predict(bosques_aleatorios_esp_m5, newdata = desp.prueba[c(2:2,6:69)])
prediccion_ba_esp_m5

#Matriz de confusion
cm_ba_esp_m5 <-confusionMatrix(prediccion_ba_esp_m5, desp.prueba$Clasificacion)

metrics_ba_esp_m5 <-cm_ba_esp_m5$byClass

resumen_metricas_ba_esp_m5 <- data.frame(keyName=names(metrics_ba_esp_m5), value=metrics_ba_esp_m5, row.names=NULL)
names(resumen_metricas_ba_esp_m5) <- c("Metrica", "M5 BA")
resumen_metricas_ba_esp_m5[2]

#Calculo del error
error_rate = round(mean(prediccion_ba_esp_m5 != desp.prueba$Clasificacion),2)
error_rate

resumen_error_ba_esp_m5 <- data.frame(Metrica=c("Tasa de Error"), value=c(error_rate), row.names=NULL)
names(resumen_error_ba_esp_m5) <- c("Metrica", "M5 BA")
resumen_error_ba_esp_m5

resumen_metricas_ba_esp_m5_with_error <- union(resumen_metricas_ba_esp_m5, resumen_error_ba_esp_m5)
resumen_metricas_ba_esp_m5_with_error

resumen_metricas_espannol_bosques_aleatorios <- cbind(resumen_metricas_espannol_bosques_aleatorios, resumen_metricas_ba_esp_m5_with_error[2])

plot(bosques_aleatorios_esp_m5)
#*****************************************************************************************************************************
#**************************************************** MODELO 6 ***************************************************************
bosques_aleatorios_esp_m6 <- train(Clasificacion ~ WC+WPS+Sixltr+Dic+Funct+TotPron+PronPer+Articulo+Verbos+VerbAux+Pasado+Present+Futuro+Adverb+Prepos+Conjunc+Negacio+Cuantif+Numeros+Maldec+Subjuntiv+formal+informal+AllPunc+Period+Colon+SemiC+QMark+Exclam+Dash+Apostro+Parenth+OtherP
                                   , data = desp.entrenamiento[c(2:2,6:69)],
                                   method = "rf",
                                   ntree = 10,
                                   trControl=t_control,
                                   tuneLength = 10)

bosques_aleatorios_esp_m6
##Prediccion del conjunto de datos de prueba

prediccion_ba_esp_m6 <- predict(bosques_aleatorios_esp_m6, newdata = desp.prueba[c(2:2,6:69)])
prediccion_ba_esp_m6

#Matriz de confusion
cm_ba_esp_m6 <-confusionMatrix(prediccion_ba_esp_m6, desp.prueba$Clasificacion)

metrics_ba_esp_m6 <-cm_ba_esp_m6$byClass

resumen_metricas_ba_esp_m6 <- data.frame(keyName=names(metrics_ba_esp_m6), value=metrics_ba_esp_m6, row.names=NULL)
names(resumen_metricas_ba_esp_m6) <- c("Metrica", "M6 BA")
resumen_metricas_ba_esp_m6[2]

#Calculo del error
error_rate = round(mean(prediccion_ba_esp_m6 != desp.prueba$Clasificacion),2)
error_rate

resumen_error_ba_esp_m6 <- data.frame(Metrica=c("Tasa de Error"), value=c(error_rate), row.names=NULL)
names(resumen_error_ba_esp_m6) <- c("Metrica", "M6 BA")
resumen_error_ba_esp_m6

resumen_metricas_ba_esp_m6_with_error <- union(resumen_metricas_ba_esp_m6, resumen_error_ba_esp_m6)
resumen_metricas_ba_esp_m6_with_error

resumen_metricas_espannol_bosques_aleatorios <- cbind(resumen_metricas_espannol_bosques_aleatorios, resumen_metricas_ba_esp_m3_with_error[2])

plot(bosques_aleatorios_esp_m6)

#*****************************************************************************************************************************

#*****************************************************************************************************************************
#*****************************************************************************************************************************
#############SUPPORT VECTOR MACHINES
#***************************************************** MODELO 1 **************************************************************
#grid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))

#svm_esp_m1 <- train(Clasificacion ~WC+WPS+Sixltr+TotPron+Articulo+Adverb+Verbos+QMark+Exclam+Apostro+VerbAux+Funct,
#                    data = desp.entrenamiento[c(2:2,6:69)], 
#                    method = "svmLinear", 
#                    trControl=t_control,
#                    preProcess = c("center", "scale"),
#                    tuneGrid = grid,
#                    tuneLength = 10)

svm_esp_m1 <- train(Clasificacion ~WC+WPS+Sixltr+TotPron+Articulo+Adverb+Verbos+QMark+Exclam+Apostro+VerbAux+Funct,
                    data = desp.entrenamiento[c(2:2,6:69)], 
                    method = "svmRadial", 
                    trControl=t_control,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)


#Resultado del modelo entrenado
svm_esp_m1
plot(svm_esp_m1)

##Prediccion del conjunto de datos de prueba

prediccion_svm_esp_m1 <- predict(svm_esp_m1, newdata = desp.prueba[c(2:2,6:69)])
prediccion_svm_esp_m1

#Matriz de confusion
cm_svm_esp_m1 <-confusionMatrix(prediccion_svm_esp_m1, desp.prueba$Clasificacion)
metrics_svm_esp_m1 <-cm_svm_esp_m1$byClass

resumen_metricas_svm_esp_m1 <- data.frame(keyName=names(metrics_svm_esp_m1), value=metrics_svm_esp_m1, row.names=NULL)
names(resumen_metricas_svm_esp_m1) <- c("Metrica", "M1 SVM")
resumen_metricas_svm_esp_m1[2]

#Calculo del error
error_rate = round(mean(prediccion_svm_esp_m1 != desp.prueba$Clasificacion),2)
error_rate

resumen_error_svm_esp_m1 <- data.frame(Metrica=c("Tasa de Error"), value=c(error_rate), row.names=NULL)
names(resumen_error_svm_esp_m1) <- c("Metrica", "M1 SVM")
resumen_error_svm_esp_m1

resumen_metricas_svm_esp_m1_with_error <- union(resumen_metricas_svm_esp_m1, resumen_error_svm_esp_m1)
resumen_metricas_svm_esp_m1_with_error

resumen_metricas_espannol_svm <- cbind(resumen_metricas_espannol_svm, resumen_metricas_svm_esp_m1_with_error[2])

#***************************************************** MODELO 2 **************************************************************
#svm_esp_m2 <- train(Clasificacion ~Adverb+Sixltr+EmoNeg,
#                    data = desp.entrenamiento[c(2:2,6:69)], 
#                    method = "svmLinear", 
#                    trControl=t_control,
#                    preProcess = c("center", "scale"),
#                    tuneGrid = grid,
#                    tuneLength = 10)

svm_esp_m2 <- train(Clasificacion ~Adverb+Sixltr+EmoNeg,
                    data = desp.entrenamiento[c(2:2,6:69)], 
                    method = "svmRadial", 
                    trControl=t_control,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)

#Resultado del modelo entrenado
svm_esp_m2
plot(svm_esp_m2)

##Prediccion del conjunto de datos de prueba

prediccion_svm_esp_m2 <- predict(svm_esp_m2, newdata = desp.prueba[c(2:2,6:69)])
prediccion_svm_esp_m2

#Matriz de confusion
cm_svm_esp_m2 <-confusionMatrix(prediccion_svm_esp_m2, deng.prueba$Clasificacion)
metrics_svm_esp_m2 <-cm_svm_esp_m2$byClass

resumen_metricas_svm_esp_m2 <- data.frame(keyName=names(metrics_svm_esp_m2), value=metrics_svm_esp_m2, row.names=NULL)
names(resumen_metricas_svm_esp_m2) <- c("Metrica", "M2 SVM")
resumen_metricas_svm_esp_m2[2]

#Calculo del error
error_rate = round(mean(prediccion_svm_esp_m2 != desp.prueba$Clasificacion),2)
error_rate

resumen_error_svm_esp_m2 <- data.frame(Metrica=c("Tasa de Error"), value=c(error_rate), row.names=NULL)
names(resumen_error_svm_esp_m2) <- c("Metrica", "M2 SVM")
resumen_error_svm_esp_m2

resumen_metricas_svm_esp_m2_with_error <- union(resumen_metricas_svm_esp_m2, resumen_error_svm_esp_m2)
resumen_metricas_svm_esp_m2_with_error

resumen_metricas_espannol_svm <- cbind(resumen_metricas_espannol_svm, resumen_metricas_svm_esp_m2_with_error[2])

#***************************************************** MODELO 3 **************************************************************
#svm_esp_m3 <- train(Clasificacion ~QMark+informal+Prepos,
#                    data = desp.entrenamiento[c(2:2,6:69)], 
#                    method = "svmLinear", 
#                    trControl=t_control,
#                    preProcess = c("center", "scale"),
#                    tuneGrid = grid,
#                    tuneLength = 10)

svm_esp_m3 <- train(Clasificacion ~QMark+informal+Prepos,
                    data = desp.entrenamiento[c(2:2,6:69)], 
                    method = "svmRadial", 
                    trControl=t_control,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)


#Resultado del modelo entrenado
svm_esp_m3
plot(svm_esp_m3)
##Prediccion del conjunto de datos de prueba

prediccion_svm_esp_m3 <- predict(svm_esp_m3, newdata = desp.prueba[c(2:2,6:69)])
prediccion_svm_esp_m3

#Matriz de confusion
cm_svm_esp_m3 <-confusionMatrix(prediccion_svm_esp_m3, desp.prueba$Clasificacion)
metrics_svm_esp_m3 <-cm_svm_esp_m3$byClass

resumen_metricas_svm_esp_m3 <- data.frame(keyName=names(metrics_svm_esp_m3), value=metrics_svm_esp_m3, row.names=NULL)
names(resumen_metricas_svm_esp_m3) <- c("Metrica", "M3 SVM")
resumen_metricas_svm_esp_m3[2]

#Calculo del error
error_rate = round(mean(prediccion_svm_esp_m3 != desp.prueba$Clasificacion),2)
error_rate

resumen_error_svm_esp_m3 <- data.frame(Metrica=c("Tasa de Error"), value=c(error_rate), row.names=NULL)
names(resumen_error_svm_esp_m3) <- c("Metrica", "M3 SVM")
resumen_error_svm_esp_m3

resumen_metricas_svm_esp_m3_with_error <- union(resumen_metricas_svm_esp_m3, resumen_error_svm_esp_m3)
resumen_metricas_svm_esp_m3_with_error

resumen_metricas_espannol_svm <- cbind(resumen_metricas_espannol_svm, resumen_metricas_svm_esp_m3_with_error[2])

#***************************************************** MODELO 4 **************************************************************
#svm_esp_m4 <- train(Clasificacion ~.,
#                    data = desp.entrenamiento[c(2:2,6:69)], 
#                    method = "svmLinear", 
#                    trControl=t_control,
#                    preProcess = c("center", "scale"),
#                    tuneGrid = grid,
#                    tuneLength = 10)

svm_esp_m4 <- train(Clasificacion ~.,
                    data = desp.entrenamiento[c(2:2,6:69)], 
                    method = "svmRadial", 
                    trControl=t_control,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)


#Resultado del modelo entrenado
svm_esp_m4
plot(svm_esp_m4)
##Prediccion del conjunto de datos de prueba

prediccion_svm_esp_m4 <- predict(svm_esp_m4, newdata = desp.prueba[c(2:2,6:69)])
prediccion_svm_esp_m4

#Matriz de confusion
cm_svm_esp_m4 <-confusionMatrix(prediccion_svm_esp_m4, desp.prueba$Clasificacion)
metrics_svm_esp_m4 <-cm_svm_esp_m4$byClass

resumen_metricas_svm_esp_m4 <- data.frame(keyName=names(metrics_svm_esp_m4), value=metrics_svm_esp_m4, row.names=NULL)
names(resumen_metricas_svm_esp_m4) <- c("Metrica", "M4 SVM")
resumen_metricas_svm_esp_m4[2]

#Calculo del error
error_rate = round(mean(prediccion_svm_esp_m4 != desp.prueba$Clasificacion),2)
error_rate

resumen_error_svm_esp_m4 <- data.frame(Metrica=c("Tasa de Error"), value=c(error_rate), row.names=NULL)
names(resumen_error_svm_esp_m4) <- c("Metrica", "M4 SVM")
resumen_error_svm_esp_m4

resumen_metricas_svm_esp_m4_with_error <- union(resumen_metricas_svm_esp_m4, resumen_error_svm_esp_m4)
resumen_metricas_svm_esp_m4_with_error

resumen_metricas_espannol_svm <- cbind(resumen_metricas_espannol_svm, resumen_metricas_svm_esp_m4_with_error[2])

#***************************************************** MODELO 5 **************************************************************
#svm_esp_m5 <- train(Clasificacion ~WC+WPS+Sixltr+Funct,
#                    data = desp.entrenamiento[c(2:2,6:69)], 
#                    method = "svmLinear", 
#                    trControl=t_control,
#                    preProcess = c("center", "scale"),
#                    tuneGrid = grid,
#                    tuneLength = 10)

svm_esp_m5 <- train(Clasificacion ~WC+WPS+Sixltr+Funct,
                    data = desp.entrenamiento[c(2:2,6:69)], 
                    method = "svmRadial", 
                    trControl=t_control,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)



#Resultado del modelo entrenado
svm_esp_m5
plot(svm_esp_m5)
##Prediccion del conjunto de datos de prueba

prediccion_svm_esp_m5 <- predict(svm_esp_m5, newdata = desp.prueba[c(2:2,6:69)])
prediccion_svm_esp_m5

#Matriz de confusion
cm_svm_esp_m5 <-confusionMatrix(prediccion_svm_esp_m5, desp.prueba$Clasificacion)
metrics_svm_esp_m5 <-cm_svm_esp_m5$byClass

resumen_metricas_svm_esp_m5 <- data.frame(keyName=names(metrics_svm_esp_m5), value=metrics_svm_esp_m5, row.names=NULL)
names(resumen_metricas_svm_esp_m5) <- c("Metrica", "M5 SVM")
resumen_metricas_svm_esp_m5[2]

#Calculo del error
error_rate = round(mean(prediccion_svm_esp_m5 != desp.prueba$Clasificacion),2)
error_rate

resumen_error_svm_esp_m5 <- data.frame(Metrica=c("Tasa de Error"), value=c(error_rate), row.names=NULL)
names(resumen_error_svm_esp_m5) <- c("Metrica", "M5 SVM")
resumen_error_svm_esp_m5

resumen_metricas_svm_esp_m5_with_error <- union(resumen_metricas_svm_esp_m5, resumen_error_svm_esp_m5)
resumen_metricas_svm_esp_m5_with_error

resumen_metricas_espannol_svm <- cbind(resumen_metricas_espannol_svm, resumen_metricas_svm_esp_m5_with_error[2])
#***************************************************** MODELO 6 **************************************************************

#svm_esp_m6 <- train(Clasificacion ~WC+WPS+Sixltr+Dic+Funct+TotPron+PronPer+Articulo+Verbos+VerbAux+Pasado+Present+Futuro+Adverb+Prepos+Conjunc+Negacio+Cuantif+Numeros+Maldec+Subjuntiv+formal+informal+AllPunc+Period+Colon+SemiC+QMark+Exclam+Dash+Apostro+Parenth+OtherP,
#                    data = desp.entrenamiento[c(2:2,6:69)], 
#                    method = "svmLinear", 
#                    trControl=t_control,
#                    preProcess = c("center", "scale"),
#                    tuneLength = 10,
#                    tuneGrid = grid)

svm_esp_m6 <- train(Clasificacion ~WC+WPS+Sixltr+Dic+Funct+TotPron+PronPer+Articulo+Verbos+VerbAux+Pasado+Present+Futuro+Adverb+Prepos+Conjunc+Negacio+Cuantif+Numeros+Maldec+Subjuntiv+formal+informal+AllPunc+Period+Colon+SemiC+QMark+Exclam+Dash+Apostro+Parenth+OtherP,
                    data = desp.entrenamiento[c(2:2,6:69)], 
                    method = "svmRadial", 
                    trControl=t_control,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)



#Resultado del modelo entrenado
svm_esp_m6
plot(svm_esp_m6)
##Prediccion del conjunto de datos de prueba

prediccion_svm_esp_m6 <- predict(svm_esp_m6, newdata = desp.prueba[c(2:2,6:69)])
prediccion_svm_esp_m6

#Matriz de confusion
cm_svm_esp_m6 <-confusionMatrix(prediccion_svm_esp_m6, desp.prueba$Clasificacion)
metrics_svm_esp_m6 <-cm_svm_esp_m6$byClass

resumen_metricas_svm_esp_m6 <- data.frame(keyName=names(metrics_svm_esp_m6), value=metrics_svm_esp_m6, row.names=NULL)
names(resumen_metricas_svm_esp_m6) <- c("Metrica", "M6 SVM")
resumen_metricas_svm_esp_m6[2]

#Calculo del error
error_rate = round(mean(prediccion_svm_esp_m6 != desp.prueba$Clasificacion),2)
error_rate

resumen_error_svm_esp_m6 <- data.frame(Metrica=c("Tasa de Error"), value=c(error_rate), row.names=NULL)
names(resumen_error_svm_esp_m6) <- c("Metrica", "M6 SVM")
resumen_error_svm_esp_m6

resumen_metricas_svm_esp_m6_with_error <- union(resumen_metricas_svm_esp_m6, resumen_error_svm_esp_m6)
resumen_metricas_svm_esp_m6_with_error

resumen_metricas_espannol_svm <- cbind(resumen_metricas_espannol_svm, resumen_metricas_svm_esp_m6_with_error[2])


#*****************************************************************************************************************************
#*****************************************************************************************************************************
#############ARBOLES DE DECISION
#***************************************************** MODELO 1 **************************************************************

arbol_esp_m1 <- train(Clasificacion ~WC+WPS+Sixltr+TotPron+Articulo+Adverb+Verbos+QMark+Exclam+Apostro+VerbAux+Funct,
                      data = desp.entrenamiento[c(2:2, 6:69)], 
                      method = "rpart", 
                      trControl=t_control,
                      parms = list(split = "information"),
                      tuneLength = 10)

#Resultado del modelo entrenado
arbol_esp_m1
plot(arbol_esp_m1)
fancyRpartPlot(arbol_esp_m1$finalModel)

##Prediccion del conjunto de datos de prueba

prediccion_ad_esp_m1 <- predict(arbol_esp_m1, newdata = desp.prueba[c(2:2,6:69)])
prediccion_ad_esp_m1

#Matriz de confusion
cm_ad_esp_m1 <-confusionMatrix(prediccion_ad_esp_m1, desp.prueba$Clasificacion)
metrics_ad_esp_m1 <-cm_ad_esp_m1$byClass

resumen_metricas_ad_esp_m1 <- data.frame(keyName=names(metrics_ad_esp_m1), value=metrics_ad_esp_m1, row.names=NULL)
names(resumen_metricas_ad_esp_m1) <- c("Metrica", "M1 AD")
resumen_metricas_ad_esp_m1[2]

#Calculo del error
error_rate = round(mean(prediccion_ad_esp_m1 != desp.prueba$Clasificacion),2)
error_rate

resumen_error_ad_esp_m1 <- data.frame(Metrica=c("Tasa de Error"), value=c(error_rate), row.names=NULL)
names(resumen_error_ad_esp_m1) <- c("Metrica", "M1 AD")
resumen_error_ad_esp_m1

resumen_metricas_ad_esp_m1_with_error <- union(resumen_metricas_ad_esp_m1, resumen_error_ad_esp_m1)
resumen_metricas_ad_esp_m1_with_error

resumen_metricas_espannol_ad <- cbind(resumen_metricas_espannol_ad, resumen_metricas_ad_esp_m1_with_error[2])

#***************************************************** MODELO 2 **************************************************************

arbol_esp_m2 <- train(Clasificacion ~Adverb+Sixltr+EmoNeg,
                      data = desp.entrenamiento[c(2:2, 6:69)], 
                      method = "rpart", 
                      trControl=t_control,
                      parms = list(split = "information"),
                      tuneLength = 10)

#Resultado del modelo entrenado
arbol_esp_m2
plot(arbol_esp_m2)
fancyRpartPlot(arbol_esp_m2$finalModel)

##Prediccion del conjunto de datos de prueba

prediccion_ad_esp_m2 <- predict(arbol_esp_m2, newdata = desp.prueba[c(2:2,6:69)])
prediccion_ad_esp_m2

#Matriz de confusion
cm_ad_esp_m2 <-confusionMatrix(prediccion_ad_esp_m2, desp.prueba$Clasificacion)
metrics_ad_esp_m2 <-cm_ad_esp_m2$byClass

resumen_metricas_ad_esp_m2 <- data.frame(keyName=names(metrics_ad_esp_m2), value=metrics_ad_esp_m2, row.names=NULL)
names(resumen_metricas_ad_esp_m2) <- c("Metrica", "M2 AD")
resumen_metricas_ad_esp_m2[2]

#Calculo del error
error_rate = round(mean(prediccion_ad_esp_m2 != desp.prueba$Clasificacion),2)
error_rate

resumen_error_ad_esp_m2 <- data.frame(Metrica=c("Tasa de Error"), value=c(error_rate), row.names=NULL)
names(resumen_error_ad_esp_m2) <- c("Metrica", "M2 AD")
resumen_error_ad_esp_m2

resumen_metricas_ad_esp_m2_with_error <- union(resumen_metricas_ad_esp_m2, resumen_error_ad_esp_m2)
resumen_metricas_ad_esp_m2_with_error

resumen_metricas_espannol_ad <- cbind(resumen_metricas_espannol_ad, resumen_metricas_ad_esp_m2_with_error[2])
#***************************************************** MODELO 3 **************************************************************
arbol_esp_m3 <- train(Clasificacion ~QMark+informal+Prepos, 
                      data = desp.entrenamiento[c(2:2, 6:69)], 
                      method = "rpart", 
                      trControl=t_control,
                      parms = list(split = "information"),
                      tuneLength = 10)

#Resultado del modelo entrenado
arbol_esp_m3
plot(arbol_esp_m3)
fancyRpartPlot(arbol_esp_m3$finalModel)

##Prediccion del conjunto de datos de prueba

prediccion_ad_esp_m3 <- predict(arbol_esp_m3, newdata = desp.prueba[c(2:2,6:69)])
prediccion_ad_esp_m3

#Matriz de confusion
cm_ad_esp_m3 <-confusionMatrix(prediccion_ad_esp_m3, desp.prueba$Clasificacion)
metrics_ad_esp_m3 <-cm_ad_esp_m3$byClass

resumen_metricas_ad_esp_m3 <- data.frame(keyName=names(metrics_ad_esp_m3), value=metrics_ad_esp_m3, row.names=NULL)
names(resumen_metricas_ad_esp_m3) <- c("Metrica", "M3 AD")
resumen_metricas_ad_esp_m3[2]

#Calculo del error
error_rate = round(mean(prediccion_ad_esp_m3 != desp.prueba$Clasificacion),2)
error_rate

resumen_error_ad_esp_m3 <- data.frame(Metrica=c("Tasa de Error"), value=c(error_rate), row.names=NULL)
names(resumen_error_ad_esp_m3) <- c("Metrica", "M3 AD")
resumen_error_ad_esp_m3

resumen_metricas_ad_esp_m3_with_error <- union(resumen_metricas_ad_esp_m3, resumen_error_ad_esp_m3)
resumen_metricas_ad_esp_m3_with_error

resumen_metricas_espannol_ad <- cbind(resumen_metricas_espannol_ad, resumen_metricas_ad_esp_m3_with_error[2])


#***************************************************** MODELO 4 **************************************************************
arbol_esp_m4 <- train(Clasificacion ~., data = desp.entrenamiento[c(2:2, 6:69)], 
                      method = "rpart", 
                      trControl=t_control,
                      parms = list(split = "information"),
                      tuneLength = 10)

#Resultado del modelo entrenado
arbol_esp_m4
plot(arbol_esp_m4)
fancyRpartPlot(arbol_esp_m4$finalModel)

##Prediccion del conjunto de datos de prueba

prediccion_ad_esp_m4 <- predict(arbol_esp_m4, newdata = desp.prueba[c(2:2,6:69)])
prediccion_ad_esp_m4

#Matriz de confusion
cm_ad_esp_m4 <-confusionMatrix(prediccion_ad_esp_m4, desp.prueba$Clasificacion)
metrics_ad_esp_m4 <-cm_ad_esp_m4$byClass

resumen_metricas_ad_esp_m4 <- data.frame(keyName=names(metrics_ad_esp_m4), value=metrics_ad_esp_m4, row.names=NULL)
names(resumen_metricas_ad_esp_m4) <- c("Metrica", "M4 AD")
resumen_metricas_ad_esp_m4[2]

#Calculo del error
error_rate = round(mean(prediccion_ad_esp_m4 != desp.prueba$Clasificacion),2)
error_rate

resumen_error_ad_esp_m4 <- data.frame(Metrica=c("Tasa de Error"), value=c(error_rate), row.names=NULL)
names(resumen_error_ad_esp_m4) <- c("Metrica", "M4 AD")
resumen_error_ad_esp_m4

resumen_metricas_ad_esp_m4_with_error <- union(resumen_metricas_ad_esp_m4, resumen_error_ad_esp_m4)
resumen_metricas_ad_esp_m4_with_error

resumen_metricas_espannol_ad <- cbind(resumen_metricas_espannol_ad, resumen_metricas_ad_esp_m4_with_error[2])


#***************************************************** MODELO 5 **************************************************************
arbol_esp_m5 <- train(Clasificacion ~WC+WPS+Sixltr+Funct,
                      data = desp.entrenamiento[c(2:2, 6:69)], 
                      method = "rpart", 
                      trControl=t_control,
                      parms = list(split = "information"),
                      tuneLength = 10)

#Resultado del modelo entrenado
arbol_esp_m5
plot(arbol_esp_m5)
fancyRpartPlot(arbol_esp_m5$finalModel)

##Prediccion del conjunto de datos de prueba

prediccion_ad_esp_m5 <- predict(arbol_esp_m5, newdata = desp.prueba[c(2:2,6:69)])
prediccion_ad_esp_m5

#Matriz de confusion
cm_ad_esp_m5 <-confusionMatrix(prediccion_ad_esp_m5, desp.prueba$Clasificacion)
metrics_ad_esp_m5 <-cm_ad_esp_m5$byClass

resumen_metricas_ad_esp_m5 <- data.frame(keyName=names(metrics_ad_esp_m5), value=metrics_ad_esp_m5, row.names=NULL)
names(resumen_metricas_ad_esp_m5) <- c("Metrica", "M5 AD")
resumen_metricas_ad_esp_m5[2]

#Calculo del error
error_rate = round(mean(prediccion_ad_esp_m5 != desp.prueba$Clasificacion),2)
error_rate

resumen_error_ad_esp_m5 <- data.frame(Metrica=c("Tasa de Error"), value=c(error_rate), row.names=NULL)
names(resumen_error_ad_esp_m5) <- c("Metrica", "M5 AD")
resumen_error_ad_esp_m5

resumen_metricas_ad_esp_m5_with_error <- union(resumen_metricas_ad_esp_m5, resumen_error_ad_esp_m5)
resumen_metricas_ad_esp_m5_with_error

resumen_metricas_espannol_ad <- cbind(resumen_metricas_espannol_ad, resumen_metricas_ad_esp_m5_with_error[2])

#***************************************************** MODELO 6 **************************************************************
arbol_esp_m6 <- train(Clasificacion ~WC+WPS+Sixltr+Dic+Funct+TotPron+PronPer+Articulo+Verbos+VerbAux+Pasado+Present+Futuro+Adverb+Prepos+Conjunc+Negacio+Cuantif+Numeros+Maldec+Subjuntiv+formal+informal+AllPunc+Period+Colon+SemiC+QMark+Exclam+Dash+Apostro+Parenth+OtherP,
                      data = desp.entrenamiento[c(2:2, 6:69)], 
                      method = "rpart", 
                      trControl=t_control,
                      parms = list(split = "information"),
                      tuneLength = 10)

#Resultado del modelo entrenado
arbol_esp_m6
plot(arbol_esp_m6)
fancyRpartPlot(arbol_esp_m6$finalModel)

##Prediccion del conjunto de datos de prueba

prediccion_ad_esp_m6 <- predict(arbol_esp_m6, newdata = desp.prueba[c(2:2,6:69)])
prediccion_ad_esp_m6

#Matriz de confusion
cm_ad_esp_m6 <-confusionMatrix(prediccion_ad_esp_m6, desp.prueba$Clasificacion)
metrics_ad_esp_m6 <-cm_ad_esp_m6$byClass

resumen_metricas_ad_esp_m6 <- data.frame(keyName=names(metrics_ad_esp_m6), value=metrics_ad_esp_m6, row.names=NULL)
names(resumen_metricas_ad_esp_m6) <- c("Metrica", "M6 AD")
resumen_metricas_ad_esp_m6[2]

#Calculo del error
error_rate = round(mean(prediccion_ad_esp_m6 != desp.prueba$Clasificacion),2)
error_rate

resumen_error_ad_esp_m6 <- data.frame(Metrica=c("Tasa de Error"), value=c(error_rate), row.names=NULL)
names(resumen_error_ad_esp_m6) <- c("Metrica", "M6 AD")
resumen_error_ad_esp_m6

resumen_metricas_ad_esp_m6_with_error <- union(resumen_metricas_ad_esp_m6, resumen_error_ad_esp_m6)
resumen_metricas_ad_esp_m6_with_error

resumen_metricas_espannol_ad <- cbind(resumen_metricas_espannol_ad, resumen_metricas_ad_esp_m6_with_error[2])


#***********************************************************************#
#Red Neuronal ESP ---- Modelo 1



model.matrix(~Clasificacion, data=desp.entrenamiento)

desp.matrix.m1<-model.matrix(~Clasificacion+WC+WPS+Sixltr+TotPron+Articulo+Adverb+Verbos+QMark+Exclam+Apostro+VerbAux+Funct, 
                             data=desp.entrenamiento[c(2:2,6:69)])
str(data.frame(desp.matrix.m1))

desp.prueba.matrix.m1<-model.matrix(~Clasificacion+WC+WPS+Sixltr+TotPron+Articulo+Adverb+Verbos+QMark+Exclam+Apostro+VerbAux+Funct, 
                                    data=desp.prueba[c(2:2,6:69)])

colnames(desp.matrix.m1)<-make.names(colnames(desp.matrix.m1))
colnames(desp.prueba.matrix.m1)<-make.names(colnames(desp.prueba.matrix.m1))


nmodel.m1 <-neuralnet(Clasificacion1 ~ WC+WPS+Sixltr+TotPron+Articulo+Adverb+Verbos+QMark+Exclam+Apostro+VerbAux+Funct,
                      data = desp.matrix.m1,
                      hidden = c(8,3),
                      linear.output=FALSE)

summary(nmodel.m1)
plot(nmodel.m1)


nmodel2.m1 <-neuralnet(Clasificacion1 ~ WC+WPS+Sixltr+TotPron+Articulo+Adverb+Verbos+QMark+Exclam+Apostro+VerbAux+Funct,
                       data = desp.matrix.m1,
                       hidden = 2,
                       linear.output=FALSE)

summary(nmodel2.m1)
plot(nmodel2.m1)
#predicciones
predicciones1_nmodel.m1 <- compute(nmodel.m1, desp.prueba.matrix.m1[, c('WC','WPS','Sixltr','TotPron','Articulo','Adverb','Verbos','QMark','Exclam','Apostro','VerbAux','Funct')])
predicciones2_nmodel.m1 <- compute(nmodel2.m1, desp.prueba.matrix.m1[, c('WC','WPS','Sixltr','TotPron','Articulo','Adverb','Verbos','QMark','Exclam','Apostro','VerbAux','Funct')])
ls(predicciones1_nmodel.m2)
head(predicciones1_nmodel.m2$net.result)
dim(predicciones1_nmodel.m2$net.result)

ls(predicciones2_nmodel)
head(predicciones2_nmodel.m1$net.result)
dim(predicciones2_nmodel.m1$net.result)

##resultados

table(desp.prueba.matrix.m1[, 'Clasificacion1'], predicciones1_nmodel.m1$net.result >= 0.5)
table(desp.prueba.matrix.m1[, 'Clasificacion1'], predicciones2_nmodel.m1$net.result >= 0.5)

#Exactitud #TP+TN / TOTAL
(51+54)/nrow(desp.prueba)  #red1  61%
(36+70)/nrow(desp.prueba)  #red2  58%

#****************************************************************************************************************
#****************************************************************************************************************
#Red Neuronal ESP ---- Modelo 2
model.matrix(~Clasificacion, data=desp.entrenamiento)

desp.matrix.m2<-model.matrix(~Clasificacion+Adverb+Sixltr+EmoNeg, 
                             data=desp.entrenamiento[c(2:2,6:69)])
str(data.frame(desp.matrix.m2))

desp.prueba.matrix.m2<-model.matrix(~Clasificacion+Adverb+Sixltr+EmoNeg, 
                                    data=desp.prueba[c(2:2,6:69)])

colnames(desp.matrix.m2)<-make.names(colnames(desp.matrix.m2))
colnames(desp.prueba.matrix.m2)<-make.names(colnames(desp.prueba.matrix.m2))


nmodel.m2 <-neuralnet(Clasificacion1 ~ Adverb+Sixltr+EmoNeg,
                      data = desp.matrix.m2,
                      #hidden = 3,
                      linear.output=FALSE)

summary(nmodel.m2)
plot(nmodel.m2)


nmodel2.m2 <-neuralnet(Clasificacion1 ~ Adverb+Sixltr+EmoNeg,
                       data = desp.matrix.m2,
                       hidden = 2,
                       linear.output=FALSE)

summary(nmodel2.m2)
plot(nmodel2.m2)
#predicciones
predicciones1_nmodel.m2 <- compute(nmodel.m2, desp.prueba.matrix.m2[, c('Adverb','Sixltr','EmoNeg')])
predicciones2_nmodel.m2 <- compute(nmodel2.m2, desp.prueba.matrix.m2[, c('Adverb','Sixltr','EmoNeg')])
ls(predicciones1_nmodel.m2)
head(predicciones1_nmodel.m2$net.result)
dim(predicciones1_nmodel.m2$net.result)

ls(predicciones2_nmodel)
head(predicciones2_nmodel.m2$net.result)
dim(predicciones2_nmodel.m2$net.result)

##resultados

table(desp.prueba.matrix.m2[, 'Clasificacion1'], predicciones1_nmodel.m2$net.result >= 0.5)
table(desp.prueba.matrix.m2[, 'Clasificacion1'], predicciones2_nmodel.m2$net.result >= 0.5)

#Exactitud #TP+TN / TOTAL
(15+88)/nrow(desp.prueba)  #red1  56%
(23+83)/nrow(desp.prueba)  #red2  58%

#****************************************************************************************************************
#****************************************************************************************************************
#Red Neuronal ESP ---- Modelo 3
model.matrix(~Clasificacion, data=desp.entrenamiento)

desp.matrix.m3<-model.matrix(~Clasificacion+QMark+informal+Prepos, 
                             data=desp.entrenamiento[c(2:2,6:69)])
str(data.frame(desp.matrix.m3))

desp.prueba.matrix.m3<-model.matrix(~Clasificacion+QMark+informal+Prepos, 
                                    data=desp.prueba[c(2:2,6:69)])

colnames(desp.matrix.m3)<-make.names(colnames(desp.matrix.m3))
colnames(desp.prueba.matrix.m3)<-make.names(colnames(desp.prueba.matrix.m3))


nmodel.m3 <-neuralnet(Clasificacion1 ~ QMark+informal+Prepos,
                      data = desp.matrix.m3,
                      hidden = 3,
                      linear.output=FALSE)

summary(nmodel.m3)
plot(nmodel.m3)


nmodel2.m3 <-neuralnet(Clasificacion1 ~ QMark+informal+Prepos,
                       data = desp.matrix.m3,
                       hidden = 2,
                       linear.output=FALSE)

summary(nmodel2.m3)
plot(nmodel2.m3)
#predicciones
predicciones1_nmodel.m3 <- compute(nmodel.m3, desp.prueba.matrix.m3[, c('QMark','informal','Prepos')])
predicciones2_nmodel.m3 <- compute(nmodel2.m3, desp.prueba.matrix.m3[, c('QMark','informal','Prepos')])
ls(predicciones1_nmodel.m3)
head(predicciones1_nmodel.m3$net.result)
dim(predicciones1_nmodel.m3$net.result)

ls(predicciones2_nmodel)
head(predicciones2_nmodel.m3$net.result)
dim(predicciones2_nmodel.m3$net.result)

##resultados

table(desp.prueba.matrix.m3[, 'Clasificacion1'], predicciones1_nmodel.m3$net.result >= 0.5)
table(desp.prueba.matrix.m3[, 'Clasificacion1'], predicciones2_nmodel.m3$net.result >= 0.5)

#Exactitud #TP+TN / TOTAL
(22+70)/nrow(desp.prueba)  #red1  50%
(24+80)/nrow(desp.prueba)  #red2  57%

#****************************************************************************************************************
#Red Neuronal ESP ---- Modelo 4
model.matrix(~Clasificacion, data=desp.entrenamiento)

desp.matrix.m4<-model.matrix(~., 
                             data=desp.entrenamiento[c(2:2,6:69)])
str(data.frame(desp.matrix.m4))

desp.prueba.matrix.m4<-model.matrix(~., 
                                    data=desp.prueba[c(2:2,6:69)])

colnames(desp.matrix.m4)<-make.names(colnames(desp.matrix.m4))
colnames(desp.prueba.matrix.m4)<-make.names(colnames(desp.prueba.matrix.m4))


nmodel.m4 <-neuralnet(Clasificacion1 ~ .,
                      data = desp.matrix.m4,
                      hidden = 3,
                      linear.output=FALSE)

summary(nmodel.m4)
plot(nmodel.m4)


nmodel2.m4 <-neuralnet(Clasificacion1 ~ .,
                       data = desp.matrix.m4,
                       hidden = 5,
                       linear.output=FALSE)

summary(nmodel2.m4)
plot(nmodel2.m4)
#predicciones
predicciones1_nmodel.m4 <- compute(nmodel.m4, desp.prueba.matrix.m4)
predicciones2_nmodel.m4 <- compute(nmodel2.m4, desp.prueba.matrix.m4)
ls(predicciones1_nmodel.m4)
head(predicciones1_nmodel.m4$net.result)
dim(predicciones1_nmodel.m4$net.result)

ls(predicciones2_nmodel)
head(predicciones2_nmodel.m4$net.result)
dim(predicciones2_nmodel.m4$net.result)

##resultados

table(desp.prueba.matrix.m4[, 'Clasificacion1'], predicciones1_nmodel.m4$net.result >= 0.5)
table(desp.prueba.matrix.m4[, 'Clasificacion1'], predicciones2_nmodel.m4$net.result >= 0.5)

#Exactitud #TP+TN / TOTAL
(20+85)/nrow(desp.prueba)  #red1  58%
(22+88)/nrow(desp.prueba)  #red2  60%

#Precision positiva #TP / TOTAL POSITIVOS
18 / (18+12) #red 1   60%
48 / (48+35) #red 2   57%


#Valor precision negativa
70/ (54+70) #red 1
80 / (80+62)

#*************************************************************#
#Red Neuronal ESP ---- Modelo 5
model.matrix(~Clasificacion, data=desp.entrenamiento)

desp.matrix.m5<-model.matrix(~Clasificacion+WC+WPS+Sixltr+Funct, 
                          data=desp.entrenamiento[c(2:2,6:69)])
str(data.frame(desp.matrix.m5))

desp.prueba.matrix.m5<-model.matrix(~Clasificacion+WC+WPS+Sixltr+Funct, 
                                 data=desp.prueba[c(2:2,6:69)])

colnames(desp.matrix.m5)<-make.names(colnames(desp.matrix.m5))
colnames(desp.prueba.matrix.m5)<-make.names(colnames(desp.prueba.matrix.m5))


nmodel.m5 <-neuralnet(Clasificacion1 ~ WC+WPS+Sixltr+Funct,
                   data = desp.matrix.m5,
                   hidden = 3,
                   linear.output=FALSE)

summary(nmodel.m5)
plot(nmodel.m5)


nmodel2.m5 <-neuralnet(Clasificacion1 ~ WC+WPS+Sixltr+Funct,
                    data = desp.matrix.m5,
                    hidden = 2,
                    linear.output=FALSE)

summary(nmodel2.m5)
plot(nmodel2.m5)
#predicciones
predicciones1_nmodel.m5 <- compute(nmodel.m5, desp.prueba.matrix.m5[, c('WC','WPS','Sixltr','Funct')])
predicciones2_nmodel.m5 <- compute(nmodel2.m5, desp.prueba.matrix.m5[, c('WC','WPS','Sixltr','Funct')])
ls(predicciones1_nmodel.m5)
head(predicciones1_nmodel.m5$net.result)
dim(predicciones1_nmodel.m5$net.result)

ls(predicciones2_nmodel)
head(predicciones2_nmodel.m5$net.result)
dim(predicciones2_nmodel.m5$net.result)

##resultados

table(desp.prueba.matrix.m5[, 'Clasificacion1'], predicciones1_nmodel.m5$net.result >= 0.5)
table(desp.prueba.matrix.m5[, 'Clasificacion1'], predicciones2_nmodel.m5$net.result >= 0.5)

#Exactitud #TP+TN / TOTAL
(1+95)/nrow(desp.prueba)  #red1  55%
(12+90)/nrow(desp.prueba)  #red2  59%

#Precision positiva #TP / TOTAL POSITIVOS
18 / (18+12) #red 1   60%
48 / (48+35) #red 2   57%


#Valor precision negativa
70/ (54+70) #red 1
80 / (80+62)




#Red Neuronal ESP ---- Modelo 6
model.matrix(~Clasificacion, data=desp.entrenamiento)

desp.matrix.m6<-model.matrix(~Clasificacion+WC+WPS+Sixltr+Dic+Funct+TotPron+PronPer+Articulo+Verbos+VerbAux+Pasado+Present+Futuro+Adverb+Prepos+Conjunc+Negacio+Cuantif+Numeros+Maldec+Subjuntiv+formal+informal+AllPunc+Period+Colon+SemiC+QMark+Exclam+Dash+Apostro+Parenth+OtherP, 
                          data=desp.entrenamiento[c(2:2,6:69)])
str(data.frame(desp.matrix.m6))

desp.prueba.matrix.m6<-model.matrix(~Clasificacion+WC+WPS+Sixltr+Dic+Funct+TotPron+PronPer+Articulo+Verbos+VerbAux+Pasado+Present+Futuro+Adverb+Prepos+Conjunc+Negacio+Cuantif+Numeros+Maldec+Subjuntiv+formal+informal+AllPunc+Period+Colon+SemiC+QMark+Exclam+Dash+Apostro+Parenth+OtherP, 
                                 data=desp.prueba[c(2:2,6:69)])

colnames(desp.matrix.m6)<-make.names(colnames(desp.matrix.m6))
colnames(desp.prueba.matrix.m6)<-make.names(colnames(desp.prueba.matrix.m6))


nmodel.m6 <-neuralnet(Clasificacion1 ~ WC+WPS+Sixltr+Dic+Funct+TotPron+PronPer+Articulo+Verbos+VerbAux+Pasado+Present+Futuro+Adverb+Prepos+Conjunc+Negacio+Cuantif+Numeros+Maldec+Subjuntiv+formal+informal+AllPunc+Period+Colon+SemiC+QMark+Exclam+Dash+Apostro+Parenth+OtherP,
                   data = desp.matrix.m6,
                   hidden = 3,
                   linear.output=FALSE)
                   
summary(nmodel.m6)
plot(nmodel.m6)


nmodel2.m6 <-neuralnet(Clasificacion1 ~ WC+WPS+Sixltr+Dic+Funct+TotPron+PronPer+Articulo+Verbos+VerbAux+Pasado+Present+Futuro+Adverb+Prepos+Conjunc+Negacio+Cuantif+Numeros+Maldec+Subjuntiv+formal+informal+AllPunc+Period+Colon+SemiC+QMark+Exclam+Dash+Apostro+Parenth+OtherP,
                   data = desp.matrix.m6,
                   hidden = 5,
                   linear.output=FALSE)

summary(nmodel2.m6)
plot(nmodel2.m6)
#predicciones
predicciones1_nmodel.m6 <- compute(nmodel.m6, desp.prueba.matrix.m6[, c('WC','WPS','Sixltr','Dic','Funct','TotPron','PronPer','Articulo','Verbos','VerbAux','Pasado','Present',
                                                                 'Futuro','Adverb','Prepos','Conjunc','Negacio','Cuantif','Numeros','Maldec','Subjuntiv','formal','informal','AllPunc','Period','Colon','SemiC','QMark','Exclam','Dash','Apostro','Parenth','OtherP')])
predicciones2_nmodel.m6 <- compute(nmodel2.m6, desp.prueba.matrix.m6[, c('WC','WPS','Sixltr','Dic','Funct','TotPron','PronPer','Articulo','Verbos','VerbAux','Pasado','Present',
                                                               'Futuro','Adverb','Prepos','Conjunc','Negacio','Cuantif','Numeros','Maldec','Subjuntiv','formal','informal','AllPunc','Period','Colon','SemiC','QMark','Exclam','Dash','Apostro','Parenth','OtherP')])
ls(predicciones1_nmodel.m6)
head(predicciones1_nmodel.m6$net.result)
dim(predicciones1_nmodel.m6$net.result)

ls(predicciones2_nmodel.m6)
head(predicciones2_nmodel.m6$net.result)
dim(predicciones2_nmodel.m6$net.result)

##resultados

table(desp.prueba.matrix.m6[, 'Clasificacion1'], predicciones1_nmodel.m6$net.result >= 0.5)
table(desp.prueba.matrix.m6[, 'Clasificacion1'], predicciones2_nmodel.m6$net.result >= 0.5)

#Exactitud #TP+TN / TOTAL
(29+76)/nrow(desp.prueba)  #red1  55%
(29+71)/nrow(desp.prueba)  #red2  58%

#Precision positiva #TP / TOTAL POSITIVOS
18 / (18+12) #red 1   60%
31 / (31+20) #red 2   60%

#Valor precision negativa
70/ (54+70) #red 1
80 / (80+62)

#-----------------------------------------------------------
#Red Neuronal ENG - Modelo 1
model.matrix(~Clasificacion, data=deng.entrenamiento)

deng.matrix.m1<-model.matrix(~Clasificacion+WC+WPS+Sixltr+pronoun+article+adverb+verb+QMark+Exclam+Apostro+auxverb+Funct, 
                          data=deng.entrenamiento[c(2:2,6:80)])
str(data.frame(deng.matrix.m1))

deng.prueba.matrix.m1<-model.matrix(~Clasificacion+WC+WPS+Sixltr+pronoun+article+adverb+verb+QMark+Exclam+Apostro+auxverb+Funct, 
                                 data=deng.prueba[c(2:2,6:80)])

colnames(deng.matrix.m1)<-make.names(colnames(deng.matrix.m1))
colnames(deng.prueba.matrix.m1)<-make.names(colnames(deng.prueba.matrix.m1))


nmodel_eng.m1 <-neuralnet(Clasificacion1 ~ WC+WPS+Sixltr+pronoun+article+adverb+verb+QMark+Exclam+Apostro+auxverb+Funct,
                       data = deng.matrix.m1,
                       hidden = 3,
                       linear.output=FALSE)

summary(nmodel_eng.m1)
plot(nmodel_eng.m1)


nmodel2_eng.m1 <-neuralnet(Clasificacion1 ~ WC+WPS+Sixltr+pronoun+article+adverb+verb+QMark+Exclam+Apostro+auxverb+Funct,
                        data = deng.matrix.m1,
                        hidden = 5,
                        linear.output=FALSE)

summary(nmodel2_eng.m1)
plot(nmodel2_eng.m1)
#predicciones
predicciones1_nmodel_eng.m1 <- compute(nmodel_eng.m1, deng.prueba.matrix.m1[, c('WC','WPS','Sixltr','pronoun','article','adverb','verb','QMark','Exclam','Apostro','auxverb','Funct')])
predicciones2_nmodel_eng.m1 <- compute(nmodel2_eng.m1, deng.prueba.matrix.m1[, c('WC','WPS','Sixltr','pronoun','article','adverb','verb','QMark','Exclam','Apostro','auxverb','Funct')])
ls(predicciones1_nmodel_eng.m1)
head(predicciones1_nmodel_eng.m1$net.result)
dim(predicciones1_nmodel_eng.m1$net.result)

ls(predicciones2_nmodel_eng.m1)
head(predicciones2_nmodel_eng.m1$net.result)
dim(predicciones2_nmodel_eng.m1$net.result)

##resultados

table(deng.prueba.matrix.m1[, 'Clasificacion1'], predicciones1_nmodel_eng.m1$net.result >= 0.5)
table(deng.prueba.matrix.m1[, 'Clasificacion1'], predicciones2_nmodel_eng.m1$net.result >= 0.5)

#Exactitud
#Verdaderos Positivos + Verdaderos negativos / Numero total de ejemplos

(29+79)/nrow(deng.prueba)  #red1   59%
(21+80)/nrow(deng.prueba)  #red2  55%  
#------------------------------------------------------------------------------------
#Red Neuronal ENG - Modelo 2
model.matrix(~Clasificacion, data=deng.entrenamiento)

deng.matrix.m2<-model.matrix(~Clasificacion+adverb+Sixltr+negemo, 
                             data=deng.entrenamiento[c(2:2,6:80)])
str(data.frame(deng.matrix.m2))

deng.prueba.matrix.m2<-model.matrix(~Clasificacion+adverb+Sixltr+negemo, 
                                    data=deng.prueba[c(2:2,6:80)])

colnames(deng.matrix.m2)<-make.names(colnames(deng.matrix.m2))
colnames(deng.prueba.matrix.m2)<-make.names(colnames(deng.prueba.matrix.m2))


nmodel_eng.m2 <-neuralnet(Clasificacion1 ~ adverb+Sixltr+negemo,
                          data = deng.matrix.m2,
                          #hidden = 3,
                          linear.output=FALSE)

summary(nmodel_eng.m2)
plot(nmodel_eng.m2)


nmodel2_eng.m2 <-neuralnet(Clasificacion1 ~ adverb+Sixltr+negemo,
                           data = deng.matrix.m2,
                           hidden = 2,
                           linear.output=FALSE)

summary(nmodel2_eng.m2)
plot(nmodel2_eng.m2)
#predicciones
predicciones1_nmodel_eng.m2 <- compute(nmodel_eng.m2, deng.prueba.matrix.m2[, c('adverb','Sixltr','negemo')])
predicciones2_nmodel_eng.m2 <- compute(nmodel2_eng.m2, deng.prueba.matrix.m2[, c('adverb','Sixltr','negemo')])
ls(predicciones1_nmodel_eng.m2)
head(predicciones1_nmodel_eng.m2$net.result)
dim(predicciones1_nmodel_eng.m2$net.result)

ls(predicciones2_nmodel_eng.m2)
head(predicciones2_nmodel_eng.m2$net.result)
dim(predicciones2_nmodel_eng.m2$net.result)

##resultados

table(deng.prueba.matrix.m2[, 'Clasificacion1'], predicciones1_nmodel_eng.m2$net.result >= 0.5)
table(deng.prueba.matrix.m2[, 'Clasificacion1'], predicciones2_nmodel_eng.m2$net.result >= 0.5)

#Exactitud
#Verdaderos Positivos + Verdaderos negativos / Numero total de ejemplos

(28+73)/nrow(deng.prueba)  #red1   55%
(26+74)/nrow(deng.prueba)  #red2   55%  
#---------------------------------------------------------------------------------
#Red Neuronal ENG - Modelo 3
model.matrix(~Clasificacion, data=deng.entrenamiento)

deng.matrix.m3<-model.matrix(~Clasificacion+QMark+informal+prep, 
                             data=deng.entrenamiento[c(2:2,6:80)])
str(data.frame(deng.matrix.m3))

deng.prueba.matrix.m3<-model.matrix(~Clasificacion+QMark+informal+prep, 
                                    data=deng.prueba[c(2:2,6:80)])

colnames(deng.matrix.m3)<-make.names(colnames(deng.matrix.m3))
colnames(deng.prueba.matrix.m3)<-make.names(colnames(deng.prueba.matrix.m3))


nmodel_eng.m3 <-neuralnet(Clasificacion1 ~ QMark+informal+prep,
                          data = deng.matrix.m3,
                          #hidden = 3,
                          linear.output=FALSE)

summary(nmodel_eng.m3)
plot(nmodel_eng.m3)


nmodel2_eng.m3 <-neuralnet(Clasificacion1 ~ QMark+informal+prep,
                           data = deng.matrix.m3,
                           hidden = 2,
                           linear.output=FALSE)

summary(nmodel2_eng.m3)
plot(nmodel2_eng.m3)
#predicciones
predicciones1_nmodel_eng.m3 <- compute(nmodel_eng.m3, deng.prueba.matrix.m3[, c('QMark','informal','prep')])
predicciones2_nmodel_eng.m3 <- compute(nmodel2_eng.m3, deng.prueba.matrix.m3[, c('QMark','informal','prep')])
ls(predicciones1_nmodel_eng.m3)
head(predicciones1_nmodel_eng.m3$net.result)
dim(predicciones1_nmodel_eng.m3$net.result)

ls(predicciones2_nmodel_eng.m3)
head(predicciones2_nmodel_eng.m3$net.result)
dim(predicciones2_nmodel_eng.m3$net.result)

##resultados

table(deng.prueba.matrix.m3[, 'Clasificacion1'], predicciones1_nmodel_eng.m3$net.result >= 0.5)
table(deng.prueba.matrix.m3[, 'Clasificacion1'], predicciones2_nmodel_eng.m3$net.result >= 0.5)

#Exactitud
#Verdaderos Positivos + Verdaderos negativos / Numero total de ejemplos

(7+93)/nrow(deng.prueba)  #red1   55%
(1+95)/nrow(deng.prueba)  #red2   53%

#---------------------------------------------------------------------------------
#Red Neuronal ENG - Modelo 4
model.matrix(~Clasificacion, data=deng.entrenamiento)

deng.matrix.m4<-model.matrix(~., 
                             data=deng.entrenamiento[c(2:2,6:80)])
str(data.frame(deng.matrix.m4))

deng.prueba.matrix.m4<-model.matrix(~., 
                                    data=deng.prueba[c(2:2,6:80)])

colnames(deng.matrix.m4)<-make.names(colnames(deng.matrix.m4))
colnames(deng.prueba.matrix.m4)<-make.names(colnames(deng.prueba.matrix.m4))


nmodel_eng.m4 <-neuralnet(Clasificacion1 ~ .,
                          data = deng.matrix.m4,
                          hidden = 3,
                          linear.output=FALSE)

summary(nmodel_eng.m4)
plot(nmodel_eng.m4)


nmodel2_eng.m4 <-neuralnet(Clasificacion1 ~ .,
                           data = deng.matrix.m4,
                           hidden = 5,
                           linear.output=FALSE)

summary(nmodel2_eng.m4)
plot(nmodel2_eng.m4)
#predicciones
predicciones1_nmodel_eng.m4 <- compute(nmodel_eng.m4, deng.prueba.matrix.m4)
predicciones2_nmodel_eng.m4 <- compute(nmodel2_eng.m4, deng.prueba.matrix.m4)
ls(predicciones1_nmodel_eng.m4)
head(predicciones1_nmodel_eng.m4$net.result)
dim(predicciones1_nmodel_eng.m4$net.result)

ls(predicciones2_nmodel_eng.m4)
head(predicciones2_nmodel_eng.m4$net.result)
dim(predicciones2_nmodel_eng.m4$net.result)

##resultados

table(deng.prueba.matrix.m4[, 'Clasificacion1'], predicciones1_nmodel_eng.m4$net.result >= 0.5)
table(deng.prueba.matrix.m4[, 'Clasificacion1'], predicciones2_nmodel_eng.m4$net.result >= 0.5)

#Exactitud
#Verdaderos Positivos + Verdaderos negativos / Numero total de ejemplos

(19+87)/nrow(deng.prueba)  #red1   58%
(14+91)/nrow(deng.prueba)  #red2   58%
#-----------------------------------------------------------------------------------
#Red Neuronal ENG - Modelo 5
model.matrix(~Clasificacion, data=deng.entrenamiento)

deng.matrix.m5<-model.matrix(~Clasificacion+WC+Analytic+Clout+Authentic+Tone+WPS+Sixltr+Funct, 
                             data=deng.entrenamiento[c(2:2,6:80)])
str(data.frame(deng.matrix.m5))

deng.prueba.matrix.m5<-model.matrix(~Clasificacion+WC+Analytic+Clout+Authentic+Tone+WPS+Sixltr+Funct, 
                                    data=deng.prueba[c(2:2,6:80)])

colnames(deng.matrix.m5)<-make.names(colnames(deng.matrix.m5))
colnames(deng.prueba.matrix.m5)<-make.names(colnames(deng.prueba.matrix.m5))


nmodel_eng.m5 <-neuralnet(Clasificacion1 ~ WC+Analytic+Clout+Authentic+Tone+WPS+Sixltr+Funct,
                          data = deng.matrix.m5,
                          hidden = c(6,6,6,6,6,6),
                          linear.output=FALSE)

summary(nmodel_eng.m5)
plot(nmodel_eng.m5)


nmodel2_eng.m5 <-neuralnet(Clasificacion1 ~ WC+Analytic+Clout+Authentic+Tone+WPS+Sixltr+Funct,
                           data = deng.matrix.m5,
                           hidden = c(3,3),
                           linear.output=FALSE)

summary(nmodel2_eng.m5)
plot(nmodel2_eng.m5)
#predicciones
predicciones1_nmodel_eng.m5 <- compute(nmodel_eng.m5, deng.prueba.matrix.m5[,c('WC','Analytic','Clout','Authentic','Tone','WPS','Sixltr','Funct')])
predicciones2_nmodel_eng.m5 <- compute(nmodel2_eng.m5, deng.prueba.matrix.m5[,c('WC','Analytic','Clout','Authentic','Tone','WPS','Sixltr','Funct')])
ls(predicciones1_nmodel_eng.m5)
head(predicciones1_nmodel_eng.m5$net.result)
dim(predicciones1_nmodel_eng.m5$net.result)

ls(predicciones2_nmodel_eng.m5)
head(predicciones2_nmodel_eng.m5$net.result)
dim(predicciones2_nmodel_eng.m5$net.result)

##resultados

table(deng.prueba.matrix.m5[, 'Clasificacion1'], predicciones1_nmodel_eng.m5$net.result >= 0.5)
table(deng.prueba.matrix.m5[, 'Clasificacion1'], predicciones2_nmodel_eng.m5$net.result >= 0.5)

#Exactitud
#Verdaderos Positivos + Verdaderos negativos / Numero total de ejemplos

(20+80)/nrow(deng.prueba)  #red1   58%
(42+65)/nrow(deng.prueba)  #red2   59%
#--------------------------------------------------------------
#Red Neuronal ENG - Modelo 6
model.matrix(~Clasificacion, data=deng.entrenamiento)

deng.matrix.m6<-model.matrix(~Clasificacion+WC+Analytic+Clout+Authentic+Tone+WPS+Sixltr+Dic+Funct+pronoun+article+prep+auxverb+adverb+conj+negate+verb+adj+compare+interrog+number+quant+AllPunc+Period+Colon+SemiC+QMark+Exclam+Dash+Apostro+Parenth, 
                          data=deng.entrenamiento[c(2:2,6:80)])
str(data.frame(deng.matrix.m6))

deng.prueba.matrix.m6<-model.matrix(~Clasificacion+WC+Analytic+Clout+Authentic+Tone+WPS+Sixltr+Dic+Funct+pronoun+article+prep+auxverb+adverb+conj+negate+verb+adj+compare+interrog+number+quant+AllPunc+Period+Colon+SemiC+QMark+Exclam+Dash+Apostro+Parenth, 
                                 data=deng.prueba[c(2:2,6:80)])

colnames(deng.matrix.m6)<-make.names(colnames(deng.matrix.m6))
colnames(deng.prueba.matrix.m6)<-make.names(colnames(deng.prueba.matrix.m6))


nmodel_eng.m6 <-neuralnet(Clasificacion1 ~ WC+Analytic+Clout+Authentic+Tone+WPS+Sixltr+Dic+Funct+pronoun+article+prep+auxverb+adverb+conj+negate+verb+adj+compare+interrog+number+quant+AllPunc+Period+Colon+SemiC+QMark+Exclam+Dash+Apostro+Parenth,
                   data = deng.matrix.m6,
                   hidden = 3,
                   linear.output=FALSE)

summary(nmodel_eng.m6)
plot(nmodel_eng.m6)


nmodel2_eng.m6 <-neuralnet(Clasificacion1 ~ WC+Analytic+Clout+Authentic+Tone+WPS+Sixltr+Dic+Funct+pronoun+article+prep+auxverb+adverb+conj+negate+verb+adj+compare+interrog+number+quant+AllPunc+Period+Colon+SemiC+QMark+Exclam+Dash+Apostro+Parenth,
                    data = deng.matrix.m6,
                    hidden = 5,
                    linear.output=FALSE)

summary(nmodel2_eng.m6)
plot(nmodel2_eng.m6)
#predicciones
predicciones1_nmodel_eng.m6 <- compute(nmodel_eng.m6, deng.prueba.matrix.m6[, c('WC','Analytic','Clout','Authentic','Tone','WPS','Sixltr','Dic','Funct','pronoun','article','prep','auxverb','adverb','conj','negate','verb','adj','compare','interrog','number','quant','AllPunc','Period','Colon','SemiC','QMark','Exclam','Dash','Apostro','Parenth')])
predicciones2_nmodel_eng.m6 <- compute(nmodel2_eng.m6, deng.prueba.matrix.m6[, c('WC','Analytic','Clout','Authentic','Tone','WPS','Sixltr','Dic','Funct','pronoun','article','prep','auxverb','adverb','conj','negate','verb','adj','compare','interrog','number','quant','AllPunc','Period','Colon','SemiC','QMark','Exclam','Dash','Apostro','Parenth')])
ls(predicciones1_nmodel_eng.m6)
head(predicciones1_nmodel_eng.m6$net.result)
dim(predicciones1_nmodel_eng.m6$net.result)

ls(predicciones2_nmodel_eng.m6)
head(predicciones2_nmodel_eng.m6$net.result)
dim(predicciones2_nmodel_eng.m6$net.result)

##resultados

table(deng.prueba.matrix.m6[, 'Clasificacion1'], predicciones1_nmodel_eng.m6$net.result >= 0.5)
table(deng.prueba.matrix.m6[, 'Clasificacion1'], predicciones2_nmodel_eng.m6$net.result >= 0.5)

#Exactitud
#Verdaderos Positivos + Verdaderos negativos / Numero total de ejemplos

(88+21)/nrow(deng.prueba)  #red1  60%
(31+77)/nrow(deng.prueba)  #red2  59% 


#Precision
9 / (9+12) #red 1
44 / (44+38) #red 2


#****************************************************************************************************************
resumen_metricas_ingles_regresion_logistica
resumen_metricas_ingles_bosques_aleatorios
resumen_metricas_espannol_regresion_logistica
resumen_metricas_espannol_bosques_aleatorios
resumen_metricas_ingles_svm
resumen_metricas_ingles_ad
resumen_metricas_espannol_svm
resumen_metricas_espannol_ad
