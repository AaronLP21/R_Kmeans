"
Programa que por medio k-means hace clusters para agrupar a
medicos asociados a JARDIANZA en el primer semestre de 2019
para ver su potencial y/o capacidad digital.
La base que ocuparemos es el modelo que hizo ESPANIA y lo modificamos
Las diferencias son ... de nuestro modelo, solo le dan peso a los correo
y después hice una caden ade Markov para pronosticar estados futuros de 
los phy a traves de una matriz de transicion

@Author : Aaron Lopez Pedraza
@Version: 1
@Date   : 20/08/2020
"
# Bibliotecas,directorio de trabajo, datos y semilla -------------------------------------------

install.packages("tidyverse", dependencies = TRUE)
install.packages("NbClust")
install.packages("cluster")
install.packages("factoextra")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("rlang")
rlang:::rlang_is_string
install.packages("devtools")
devtools::install_github("r-lib/rlang")
install.packages("scales")
remotes::update_packages("rlang")
install.packages("rlang", type = "source")
install.packages("MD5")
install.packages("rlang",type =remotes::update_packages())
install.packages("corrplot")
install.packages('readxl')   
install.packages("rio")
install.packages("openxlsx")
install.packages("scales")
library(readxl)
library(rio)
library(openxlsx)
library(corrplot)
library(MD5)
library(scales)
library(rlang)
library(tidyverse)
library(cluster)
library(factoextra)
library(ggplot2)
library(NbClust)
library(tidyr)


# DATOS -------------------------------------------------------------------

setwd("//eu.boehringer.com/users/mex/users2/lopezped/Documents/Digital_PHY_2019_Clustering/JDZ_PILOTO_1/Nuestro_modelo/Modelo por semestres/Medicos_unicos_data_para_leer_R")

dataset_1 <- read_xlsx('datos_modelo_1er_sem.xlsx',sheet = 'con_details_para_leer_R_1_sem')
#no vamos a escalar para que no haya tema



#-------------------------------Validamos que no haya NA's
dataset_1 <- na.omit(dataset_1)
is.na(dataset_1)
str(dataset_1)
summary(dataset_1)

# OPTIMO DE CLUSTERS ------------------------------------------------------

#estimar el número de clusters
set.seed(8)
wss <- (nrow(dataset_1)-1)*sum(apply(dataset_1,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(dataset_1, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")
#4 o 7 ; 5 o 9



#elbow graph
set.seed(10)
vek<-kmeans(dataset_1,centers = 1)$betweens
for (i in 2:15) {
  vek[i]<-kmeans(dataset_1,centers = i)$betweens
}#sacamos las y de la gráfica

plot(1:15,vek,type = "o",main = 'Optimal number of clusters',xlab = "Clusters",
     ylab = "Betweens groups sum of squares",
     col="blue2")
#5 

#el optimo, para indicar con una linea roja, NO HAY GRAFICA, NO COORIO
x<-5
y<-vek[x]
points(x,y,type = "h",col='red')


#Optimo de clusters via funciones
set.seed(8)
fviz_nbclust(dataset_1, kmeans, method = "wss",k.max = 15)#4 o 8
fviz_nbclust(dataset_1, kmeans, method = "silhouette")
fviz_nbclust(dataset_1, kmeans, method = "gap_stat")

# 30 PRUEBAS, No corre porque es demasiada data
resnumclust<-NbClust(dataset_1, distance = "euclidean", min.nc=2, max.nc=15, method = "kmeans", index = "alllong")
fviz_nbclust(resnumclust<-NbClust(dataset_ES_scale, distance = "euclidean", min.nc=2, max.nc=15, method = "kmeans", index = "alllong"))


# CLUSTERS ----------------------------------------------------------------

#Dado el optimo de clusters:= 5
set.seed(10)
#View(dataset_1)
resultados<-kmeans(dataset_1,centers = 5)

clusters <- as.vector(resultados$cluster)

#Distribucion de los CLUSTERS y almacenamiento (Ier semestre)
table(clusters)
resumen <- table(clusters)
class(resumen)
matriz_tabla <- as.data.frame(resumen)
names(matriz_tabla)[names(matriz_tabla) == "Freq"] <- "Physicians_1st_sem"
class(matriz_tabla)
view(matriz_tabla)
matriz_tabla

#valores por cluster
export(matriz_tabla,"valores_clus_1st_sem.xlsx")
getwd()#recordamos a donde se exporto

#los centros
centros <- as.data.frame(resultados$centers)
View(centros)# NO normalizados, esto te dice sus caracteristicas promedios
export(centros,"clusters_centers_1st_sem.xlsx")

# EXPORTAMOS RESULTADOS de los datos con sus respectivos clusters ---------------------------------------------------

#concatenamos
dataset_con_CL <- cbind(dataset_1,clusters)
#exportamos
export(dataset_con_CL,"Clustered_phy_1st_sem.xlsx")


#CLUSTERS DE EXPOSICION VS AFINIDAD coloreados
plot(dataset_con_CL$DIGITAL_EXPOSITION,dataset_con_CL$DIGITAL_AFFINITY,
     col=dataset_con_CL$clusters,main = "Jardianz clustered physicians",
     xlab = "Digital channels exposition",ylab = "Digital Affinity")

#vemos cual es cada cluster.
plot(dataset_con_CL$DIGITAL_EXPOSITION,dataset_con_CL$DIGITAL_AFFINITY,
     col=dataset_con_CL$clusters==5,main = "Jardianz clustered physicians",
     xlab = "Digital channels exposition",ylab = "Digital Affinity")


# legend("topleft",c("C1","C2","C3","C4","C5"),cex=.1,col=dataset_con_CL$clusters,
#        pch=c(1,2),box.col="black", title="sample types",
#        text.font=4,  bg='lightblue')








#-------------segundo semestre

# DATOS 

setwd("//eu.boehringer.com/users/mex/users2/lopezped/Documents/Digital_PHY_2019_Clustering/JDZ_PILOTO_1/Nuestro_modelo/Modelo por semestres/Medicos_unicos_data_para_leer_R")

dataset_2 <- read_xlsx('datos_modelo_2do_sem.xlsx',sheet = 'con_details_para_leer_R_2_sem')
#no vamos a escalar para que no haya tema



#-------------------------------Validamos que no haya NA's
dataset_2 <- na.omit(dataset_2)
is.na(dataset_2)
str(dataset_2)
summary(dataset_2)

# OPTIMO DE CLUSTERS ------------------------------------------------------

#estimar el número de clusters
set.seed(10)
wss <- (nrow(dataset_2)-1)*sum(apply(dataset_2,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(dataset_2, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")
# 5 o 6



#elbow graph
set.seed(10)
vek2<-kmeans(dataset_2,centers = 1)$betweens
for (i in 2:15) {
  vek2[i]<-kmeans(dataset_2,centers = i)$betweens
}#sacamos las y de la gráfica

plot(1:15,vek2,type = "o",main = 'Optimal number of clusters',xlab = "Clusters",
     ylab = "Betweens groups sum of squares",
     col="blue2")
#5

#el optimo, para indicar con una linea roja, NO HAY GRAFICA, NO COORIO
x2<-5
y2<-vek2[x2]
points(x2,y2,type = "h",col='red')


#Optimo de clusters via funciones
set.seed(8)
fviz_nbclust(dataset_2, kmeans, method = "wss",k.max = 15)#
fviz_nbclust(dataset_2, kmeans, method = "silhouette")
fviz_nbclust(dataset_2, kmeans, method = "gap_stat")

# 30 PRUEBAS, No corre porque es demasiada data
resnumclust<-NbClust(dataset_2, distance = "euclidean", min.nc=2, max.nc=15, method = "kmeans", index = "alllong")
fviz_nbclust(resnumclust<-NbClust(dataset_2, distance = "euclidean", min.nc=2, max.nc=15, method = "kmeans", index = "alllong"))


# CLUSTERS ----------------------------------------------------------------

#Dado el optimo de clusters:= 5
set.seed(10)
#View(dataset_2)
resultados2<-kmeans(dataset_2,centers = 5)

clusters2 <- as.vector(resultados2$cluster)

#Distribucion de los CLUSTERS y almacenamiento (Ier semestre)
table(clusters2)
resumen2 <- table(clusters2)
class(resumen2)
matriz_tabla2 <- as.data.frame(resumen2)
names(matriz_tabla2)[names(matriz_tabla2) == "Freq"] <- "Physicians_2nd_sem"
class(matriz_tabla2)
view(matriz_tabla2)
matriz_tabla2

#valores por cluster
export(matriz_tabla2,"valores_clus_2nd_sem.xlsx")
getwd()#recordamos a donde se exporto

#los centros
centros2 <- as.data.frame(resultados2$centers)
View(centros2)# NO normalizados, esto te dice sus caracteristicas promedios
export(centros2,"clusters_centers_2nd_sem.xlsx")

# EXPORTAMOS RESULTADOS de los datos con sus respectivos clusters ---------------------------------------------------

#concatenamos
dataset_con_CL2 <- cbind(dataset_2,clusters2)
#exportamos
export(dataset_con_CL2,"Clustered_phy_2nd_sem.xlsx")


#CLUSTERS DE EXPOSICION VS AFINIDAD coloreados
plot(dataset_con_CL2$DIGITAL_EXPOSITION,dataset_con_CL2$DIGITAL_AFFINITY,
     col=dataset_con_CL2$clusters,main = "Jardianz clustered physicians",
     xlab = "Digital channels exposition",ylab = "Digital Affinity")


#CLUSTERS DE EXPOSICION VS AFINIDAD coloreados
plot(dataset_con_CL2$DIGITAL_EXPOSITION,dataset_con_CL2$DIGITAL_AFFINITY,
     col=dataset_con_CL2$clusters==5,main = "Jardianz clustered physicians",
     xlab = "Digital channels exposition",ylab = "Digital Affinity")


# legend("topleft",c("C1","C2","C3","C4","C5"),cex=.1,col=dataset_con_CL$clusters,
#        pch=c(1,2),box.col="black", title="sample types",
#        text.font=4,  bg='lightblue')



#----------------------------------MATRIZ DE TRANSICION


"
1. Extraer los vectores de ambos clusters
2. Homogenizar las categorías
3. Hacer el table (histograma) de 1er_sem -> 2do_sem
(esta te dice como se movieron)
4. dividir la table entre rowSum (ya tenemos la MATRIZ DE TRANSICION)
5. pasar esa tabla a matriz o data frame

"
#tamanio de los clustres
tamanio_cl1 <- as.vector(resultados$size)
tamanio_cl2 <- as.vector(resultados2$size)

#Las frecuencias de los clusters
clusters  #1er_sem
clusters2 #2do_sem
#los exportamos:
export(clusters,"clusters_sem1_19.xlsx")
export(clusters2,"clusters_sem2_19.xlsx")


cl1 <- as.character(clusters)
cl2 <- as.character(clusters2)

" asi sera la trasnformacion de los nombres:
#---------1ER_SEM:
I   <- 2
II  <- 1
III <- 4
IV  <- 5
V   <- 3

#---------2DO_SEM:
I   <- 1
II  <- 2
III <- 5
IV  <- 4
V   <- 3

"
cl1_trans <- vector(mode = "character",length = 11783)
cl2_trans <- vector(mode = "character",length = 11783)

#CREAMOS LAS FUNCIONES PARA HACER LAS TRANSFORMACIONES:

" La funcion debe hacer esto:

for (i in 1:length(cl1)) {
ifelse(cl1[i]=="1",cl1_trans[i] <- "II",ifelse(cl1[i]=="2",cl1_trans[i] <- "I",ifelse(cl1[i]=="3",cl1_trans[i] <- "V",ifelse(cl1[i]=="4",cl1_trans[i] <- "III",ifelse(cl1[i]=="5",cl1_trans[i] <- "IV")))))
}
"

renombrar_clusters1 <- function(vector_clusters,vector_clusters_renombrados){
  
  for (i in 1:11783) {
    ifelse(vector_clusters[i]=="1",vector_clusters_renombrados[i] <- "II",ifelse(vector_clusters[i]=="2",vector_clusters_renombrados[i] <- "I",ifelse(vector_clusters[i]=="3",vector_clusters_renombrados[i] <- "V",ifelse(vector_clusters[i]=="4",vector_clusters_renombrados[i] <- "III",ifelse(vector_clusters[i]=="5",vector_clusters_renombrados[i] <- "IV")))))
  }  
  return(vector_clusters_renombrados)
}

#EL CRITERIO DE TRANSFORMACION ES DIFERENTE, ENTONCES:
renombrar_clusters2 <- function(vector_clusters,vector_clusters_renombrados){
  
  for (i in 1:11783) {
    ifelse(vector_clusters[i]=="1",vector_clusters_renombrados[i] <- "I",ifelse(vector_clusters[i]=="2",vector_clusters_renombrados[i] <- "II",ifelse(vector_clusters[i]=="3",vector_clusters_renombrados[i] <- "V",ifelse(vector_clusters[i]=="4",vector_clusters_renombrados[i] <- "IV",ifelse(vector_clusters[i]=="5",vector_clusters_renombrados[i] <- "III")))))
  }  
  return(vector_clusters_renombrados)
}


#LAS APLICAMOS:
cl1_trans <- renombrar_clusters1(cl1,cl1_trans)
cl2_trans <- renombrar_clusters2(cl2,cl2_trans)


#VALIDAMOS QUE LA TRANSFORMACION SE HIZO ADECUADMENTE:
revision_transformacion <- cbind(cl1,cl1_trans)
View(revision_transformacion)

revision_transformacion2 <- cbind(cl2,cl2_trans)
View(revision_transformacion2)


#COMO SE MOVIERON LOS PHY DE SEM1 A SEM2, aun como tabla
transicion <- table(cl1_trans,cl2_trans)

matriz_transicion <- transicion/rowSums(transicion)
matriz_transicion#la matriz con las probas

#renombramos las categorias
segmentos <- c("laggards","resistant","early adopters", "innovators","digital lovers")
a <- c(1877,672,234,40,3)
b <- c(502,789,288,68,3)
c <- c(268,566,414,123,9)
d <- c(293,684,1289,637,91)
e <- c(61,211,723,1135,803)
transicion_mat <- cbind(a,b,c,d,e)
colnames(transicion_mat) <- segmentos
rownames(transicion_mat) <- segmentos


##################################
transicion_mat#como se distribuyeron lo medicos
##################################

transicion_mat<-as.data.frame(transicion_mat) 
View(transicion_mat)
export(transicion_mat,"transicion_phy.xlsx")#exportados

transicion_mat <- as.matrix(transicion_mat)

#la matriz de transicion ya con probas y matriz; NO TABLA
transicion_mat_proba <- transicion_mat/rowSums(transicion_mat)
transicion_mat_proba

#CONVERTIMOS A DATA FRAME PARA EXPORTAR
transicion_mat_proba <- as.data.frame(transicion_mat_proba)
View(transicion_mat_proba)
export(transicion_mat_proba,"matriz_de_transiciones_con_probas.xlsx")


#vamos a pronosticar phy a los segmentos para los proximos 4 semestres

pronostico_segmentos <- matrix(nrow = 5,ncol = 5)
pronostico_segmentos[,1] <- tamanio_cl2
colnames(pronostico_segmentos) <- c("2019_2do_sem","2020_1er_sem","2020_2do_sem","2021_1er_sem","2021_2do_sem")
rownames(pronostico_segmentos) <- segmentos
pronostico_segmentos

#pronosticamos ocupando la matriz de transicion pero como matriz para multiplicar
transicion_mat_proba <- as.matrix(transicion_mat_proba)

#hacemos la multiplicacion de la matriz de trans por el vector de elementos
#de los segmentos al final del anio 2019 y pronosticamos 2 anios

for (i in 2:5){
  pronostico_segmentos[,i] <- pronostico_segmentos[,i-1]%*%transicion_mat_proba
}

#redondeamos para que no haya fracciones de medicos

for (i in 1:5) {
  for (j in 1:5) {
    pronostico_segmentos[i,j] <- round(pronostico_segmentos[i,j],0)
  }
}

View(pronostico_segmentos)#matriz donde se almacenan los pronosticos

pronostico_segmentos <- as.data.frame(pronostico_segmentos)
export(pronostico_segmentos,"pronostico_de_segmentos_2_anios.xlsx")


#para cambiar lamatriz porque no me habianentendido

View(transicion_mat_proba)

transicion_mat_proba#objeto original
matriz_trans_modificada <- transicion_mat_proba

#mod
matriz_trans_modificada[1,1] <- transicion_mat_proba[3,4]
matriz_trans_modificada[2,2] <- transicion_mat_proba[3,3]
matriz_trans_modificada[3,3] <- transicion_mat_proba[3,2]
matriz_trans_modificada[4,4] <- transicion_mat_proba[3,1]
matriz_trans_modificada[5,5] <- transicion_mat_proba[3,5]

matriz_trans_modificada[3,1] <- transicion_mat_proba[1,1]
matriz_trans_modificada[3,2] <- transicion_mat_proba[2,2]
matriz_trans_modificada[3,3] <- transicion_mat_proba[3,3]
matriz_trans_modificada[3,4] <- transicion_mat_proba[4,4]
matriz_trans_modificada[3,5] <- transicion_mat_proba[5,5]


View(matriz_trans_modificada)

export(matriz_trans_modificada,"matriz_cuchareada.xlsx")





# CORRELACIONES -----------------------------------------------------------
"
Me pidieron encontrar insights de de cada clusters a traves de la matriz
de correlaciones

"
View(dataset_con_CL2)#data que tiene los phy clusterizados

"filtramos los phy por segmento (cluster)
1 <- laggard
2 <- resistant
5 <- early adopters
4 <- innovators
3 <- digital lovers

"
#filtramos los datos

ds_laggards <- filter(dataset_con_CL2,clusters2==1)
ds_resistant <- filter(dataset_con_CL2,clusters2==2)
ds_digital_lovers <- filter(dataset_con_CL2,clusters2==3)
ds_innovators <- filter(dataset_con_CL2,clusters2==4)
ds_early_adopters <- filter(dataset_con_CL2,clusters2==5)


install.packages("propagate")
library(propagate)

#creamos las matrices d ecorrelacion para cada uno
#ncol(ds_laggards)
#corr_laggards <- bigcor(ds_laggards,size =46,fun = "cor") #no funciono

corr_laggards <- cor(ds_laggards)#matriz de correlaciones
view(corr_laggards)
class(corr_laggards)
colnames(corr_laggards)
rownames(corr_laggards)
export(corr_laggards,"matriz_correlaciones_laggards.xlsx")

corr_resistant <- cor(ds_resistant)#matriz de correlaciones
export(corr_resistant,"matriz_correlaciones_resistant.xlsx")

corr_early_adopters <- cor(ds_early_adopters)#matriz de correlaciones
export(corr_early_adopters,"matriz_correlaciones_early_adopters.xlsx")

corr_innovators <- cor(ds_innovators)#matriz de correlaciones
export(corr_innovators,"matriz_correlaciones_innovators.xlsx")

corr_digital_lovers <- cor(ds_digital_lovers)#matriz de correlaciones
export(corr_digital_lovers,"matriz_correlaciones_digital_lovers.xlsx")


#Por lo pronto, esto termino.
