### disciplina - modelagem de  nicho ecológico: teoria e pratica ###
### ppg ecologia e biodiversidade - unesp 2017 ###

# Thadeu Sobral de Souza - thadeusobral@gmail.com 
# Maurício Humberto Vancine - mauricio.vancine@gmail.com

###-----------------------------------------------------------------------------------------###
### 3. script ensemble ### 
###-----------------------------------------------------------------------------------------###


# 1. limpara a memoria e carregar os pacotes 
# limpar o workspace e aumentar a memoria para o r
rm(list = ls())
memory.limit(size = 10000000000000) 

# instalar e carregar pacotes
# install.packages(c("raster", "rgdal", "vegan"), dep = T)

# carregar pacotes
library(raster) # manejo de arquivos sig 
library(rgdal) # manejo de arquivos sig
library(vegan) # diversas analises multivariadas

# verificar pacotes carregados
search()


###------------------------------------------------------------------------------------------------------###

# diretorio
setwd("D:/90_aulas_montadas/_disciplina_enm_R_unesp_2017/scripts_r/03_saidas_enm")

# importando as tabelas com as avaliacoes
eval.bioclim <- read.table ("zEval_CCSM_Bioclim_B.balansae.txt")
eval.Gower <- read.table ("zEval_CCSM_Gower_B.balansae.txt")
eval.Maha <- read.table ("zEval_CCSM_Maha_B.balansae.txt")
eval.Maxent <- read.table ("zEval_CCSM_Maxent_B.balansae.txt")
eval.SVM <- read.table ("zEval_CCSM_SVM_B.balansae.txt")


###------------------------------------------------------------------------------------------------------###

### ensemble por frequencia - parte 1 ###

# bioclim
# importar os .asc
bioc1 <- raster ("CCSM_Bioclim_0k_B.balansae1.asc")
bioc2 <- raster ("CCSM_Bioclim_0k_B.balansae2.asc")
bioc3 <- raster ("CCSM_Bioclim_0k_B.balansae3.asc")
bioc4 <- raster ("CCSM_Bioclim_0k_B.balansae4.asc")
bioc5 <- raster ("CCSM_Bioclim_0k_B.balansae5.asc")

bioc211 <- raster ("CCSM_Bioclim_21k_B.balansae1.asc")
bioc212 <- raster ("CCSM_Bioclim_21k_B.balansae2.asc")
bioc213 <- raster ("CCSM_Bioclim_21k_B.balansae3.asc")
bioc214 <- raster ("CCSM_Bioclim_21k_B.balansae4.asc")
bioc215 <- raster ("CCSM_Bioclim_21k_B.balansae5.asc")

bioc61 <- raster ("CCSM_Bioclim_6k_B.balansae1.asc")
bioc62 <- raster ("CCSM_Bioclim_6k_B.balansae2.asc")
bioc63 <- raster ("CCSM_Bioclim_6k_B.balansae3.asc")
bioc64 <- raster ("CCSM_Bioclim_6k_B.balansae4.asc")
bioc65 <- raster ("CCSM_Bioclim_6k_B.balansae5.asc")

# soma dos mapas e corte do threshold 0 
bioclim0k <- sum(bioc1 >= eval.bioclim[1, 1], 
        bioc2 >= eval.bioclim[2, 1], 
        bioc3 >= eval.bioclim[3, 1], 
        bioc4 >= eval.bioclim[4, 1], 
        bioc5 >= eval.bioclim[5, 1])

# plot
plot(bioclim0k)

# exportando o .asc da soma por frequencia
writeRaster (bioclim0k, "Bioclim_0k.asc", format="ascii")

# bioclim 21k
bioclim21k <- sum(bioc211 >= eval.bioclim[1, 1], 
         bioc212 >= eval.bioclim[2, 1], 
         bioc213 >= eval.bioclim[3, 1], 
         bioc214 >= eval.bioclim[4, 1], 
         bioc215 >= eval.bioclim[5, 1])
plot(bioclim21k)
writeRaster (bioclim21k, "Bioclim_21k.asc", format="ascii")

# bioclim 6k
bioclim6k <- sum(bioc61 >= eval.bioclim[1, 1], 
         bioc62 >= eval.bioclim[2, 1], 
         bioc63 >= eval.bioclim[3, 1], 
         bioc64 >= eval.bioclim[4, 1], 
         bioc65 >= eval.bioclim[5, 1])
plot(bioclim6k)
writeRaster (bioclim6k, "Bioclim_6k.asc", format = "ascii")

###------------------------------------------------------------------------------------------------------###

# gower
# importar
bioc1 <- raster ("CCSM_Gower_0k_B.balansae1.asc")
bioc2 <- raster ("CCSM_Gower_0k_B.balansae2.asc")
bioc3 <- raster ("CCSM_Gower_0k_B.balansae3.asc")
bioc4 <- raster ("CCSM_Gower_0k_B.balansae4.asc")
bioc5 <- raster ("CCSM_Gower_0k_B.balansae5.asc")

bioc211 <- raster ("CCSM_Gower_21K_B.balansae1.asc")
bioc212 <- raster ("CCSM_Gower_21K_B.balansae2.asc")
bioc213 <- raster ("CCSM_Gower_21K_B.balansae3.asc")
bioc214 <- raster ("CCSM_Gower_21K_B.balansae4.asc")
bioc215 <- raster ("CCSM_Gower_21K_B.balansae5.asc")

bioc61 <- raster ("CCSM_Gower_6K_B.balansae1.asc")
bioc62 <- raster ("CCSM_Gower_6K_B.balansae2.asc")
bioc63 <- raster ("CCSM_Gower_6K_B.balansae3.asc")
bioc64 <- raster ("CCSM_Gower_6K_B.balansae4.asc")
bioc65 <- raster ("CCSM_Gower_6K_B.balansae5.asc")

# soma
Gower0k <- sum(bioc1 >= eval.Gower[1, 1], 
         bioc2 >= eval.Gower[2, 1], 
         bioc3 >= eval.Gower[3, 1], 
         bioc4 >= eval.Gower[4, 1], 
         bioc5 >= eval.Gower[5, 1])
plot(Gower0k)
writeRaster (Gower0k, "Gower_0k.asc", format="ascii")

# gower 21k
Gower21k <- sum(bioc211 >= eval.Gower[1, 1], 
         bioc212 >= eval.Gower[2, 1], 
         bioc213 >= eval.Gower[3, 1], 
         bioc214 >= eval.Gower[4, 1], 
         bioc215 >= eval.Gower[5, 1])
plot(Gower21k)
writeRaster (Gower21k, "Gower_21k.asc", format="ascii")

# gower 6k
Gower6k <- sum(bioc61 >= eval.Gower[1, 1], 
         bioc62 >= eval.Gower[2, 1], 
         bioc63 >= eval.Gower[3, 1], 
         bioc64 >= eval.Gower[4, 1], 
         bioc65 >= eval.Gower[5, 1])
plot(Gower6k)
writeRaster (Gower6k, "Gower_6k.asc", format="ascii")

###------------------------------------------------------------------------------------------------------###

# mahalanobis

# importar
bioc1 <- raster ("CCSM_Maha_0k_B.balansae1.asc")
bioc2 <- raster ("CCSM_Maha_0k_B.balansae2.asc")
bioc3 <- raster ("CCSM_Maha_0k_B.balansae3.asc")
bioc4 <- raster ("CCSM_Maha_0k_B.balansae4.asc")
bioc5 <- raster ("CCSM_Maha_0k_B.balansae5.asc")

bioc211 <- raster ("CCSM_Maha_21K_B.balansae1.asc")
bioc212 <- raster ("CCSM_Maha_21K_B.balansae2.asc")
bioc213 <- raster ("CCSM_Maha_21K_B.balansae3.asc")
bioc214 <- raster ("CCSM_Maha_21K_B.balansae4.asc")
bioc215 <- raster ("CCSM_Maha_21K_B.balansae5.asc")

bioc61 <- raster ("CCSM_Maha_6K_B.balansae1.asc")
bioc62 <- raster ("CCSM_Maha_6K_B.balansae2.asc")
bioc63 <- raster ("CCSM_Maha_6K_B.balansae3.asc")
bioc64 <- raster ("CCSM_Maha_6K_B.balansae4.asc")
bioc65 <- raster ("CCSM_Maha_6K_B.balansae5.asc")

# soma
Maha0k <- sum(bioc1 >= eval.Maha[1, 1], 
         bioc2 >= eval.Maha[2, 1], 
         bioc3 >= eval.Maha[3, 1], 
         bioc4 >= eval.Maha[4, 1], 
         bioc5 >= eval.Maha[5, 1])
plot(Maha0k)
writeRaster (Maha0k, "Maha_0k.asc", format="ascii")

# maha 21k
Maha21k <- sum(bioc211 >= eval.Maha[1, 1], 
         bioc212 >= eval.Maha[2, 1], 
         bioc213 >= eval.Maha[3, 1], 
         bioc214 >= eval.Maha[4, 1], 
         bioc215 >= eval.Maha[5, 1])
plot(Maha21k)
writeRaster (Maha21k, "Maha_21k.asc", format="ascii")

# maha 6k
Maha6k <- sum(bioc61 >= eval.Maha[1, 1], 
         bioc62 >= eval.Maha[2, 1], 
         bioc63 >= eval.Maha[3, 1], 
         bioc64 >= eval.Maha[4, 1], 
         bioc65 >= eval.Maha[5, 1])
plot(Maha6k)
writeRaster (Maha6k, "Maha_6k.asc", format="ascii")

###------------------------------------------------------------------------------------------------------###

# maxent

# importar
bioc1 <- raster ("CCSM_Maxent_0k_B.balansae1.asc")
bioc2 <- raster ("CCSM_Maxent_0k_B.balansae2.asc")
bioc3 <- raster ("CCSM_Maxent_0k_B.balansae3.asc")
bioc4 <- raster ("CCSM_Maxent_0k_B.balansae4.asc")
bioc5 <- raster ("CCSM_Maxent_0k_B.balansae5.asc")

bioc211 <- raster ("CCSM_Maxent_21K_B.balansae1.asc")
bioc212 <- raster ("CCSM_Maxent_21K_B.balansae2.asc")
bioc213 <- raster ("CCSM_Maxent_21K_B.balansae3.asc")
bioc214 <- raster ("CCSM_Maxent_21K_B.balansae4.asc")
bioc215 <- raster ("CCSM_Maxent_21K_B.balansae5.asc")

bioc61 <- raster ("CCSM_Maxent_6K_B.balansae1.asc")
bioc62 <- raster ("CCSM_Maxent_6K_B.balansae2.asc")
bioc63 <- raster ("CCSM_Maxent_6K_B.balansae3.asc")
bioc64 <- raster ("CCSM_Maxent_6K_B.balansae4.asc")
bioc65 <- raster ("CCSM_Maxent_6K_B.balansae5.asc")

# soma
Maxent0k <- sum(bioc1 >= eval.Maxent[1, 1], 
         bioc2 >= eval.Maxent[2, 1], 
         bioc3 >= eval.Maxent[3, 1], 
         bioc4 >= eval.Maxent[4, 1], 
         bioc5 >= eval.Maxent[5, 1])
plot(Maxent0k)
writeRaster (Maxent0k, "Maxent_0k.asc", format="ascii")

# maxent 21k
Maxent21k <- sum(bioc211 >= eval.Maxent[1, 1], 
         bioc212 >= eval.Maxent[2, 1], 
         bioc213 >= eval.Maxent[3, 1], 
         bioc214 >= eval.Maxent[4, 1], 
         bioc215 >= eval.Maxent[5, 1])
plot(Maxent21k)
writeRaster (Maxent21k, "Maxent_21k.asc", format="ascii")

# maxent 6k
Maxent6k <- sum(bioc61 >= eval.Maxent[1, 1], 
         bioc62 >= eval.Maxent[2, 1], 
         bioc63 >= eval.Maxent[3, 1], 
         bioc64 >= eval.Maxent[4, 1], 
         bioc65 >= eval.Maxent[5, 1])
plot(Maxent6k)
writeRaster (Maxent6k, "Maxent_6k.asc", format="ascii")

###------------------------------------------------------------------------------------------------------###

# svm
bioc1 <- raster ("CCSM_SVM_0k_B.balansae1.asc")
bioc2 <- raster ("CCSM_SVM_0k_B.balansae2.asc")
bioc3 <- raster ("CCSM_SVM_0k_B.balansae3.asc")
bioc4 <- raster ("CCSM_SVM_0k_B.balansae4.asc")
bioc5 <- raster ("CCSM_SVM_0k_B.balansae5.asc")

bioc211 <- raster ("CCSM_SVM_21K_B.balansae1.asc")
bioc212 <- raster ("CCSM_SVM_21K_B.balansae2.asc")
bioc213 <- raster ("CCSM_SVM_21K_B.balansae3.asc")
bioc214 <- raster ("CCSM_SVM_21K_B.balansae4.asc")
bioc215 <- raster ("CCSM_SVM_21K_B.balansae5.asc")

bioc61 <- raster ("CCSM_SVM_6K_B.balansae1.asc")
bioc62 <- raster ("CCSM_SVM_6K_B.balansae2.asc")
bioc63 <- raster ("CCSM_SVM_6K_B.balansae3.asc")
bioc64 <- raster ("CCSM_SVM_6K_B.balansae4.asc")
bioc65 <- raster ("CCSM_SVM_6K_B.balansae5.asc")

SVM0k <- sum(bioc1 >= eval.SVM[1, 1], 
        bioc2 >= eval.SVM[2, 1], 
        bioc3 >= eval.SVM[3, 1], 
        bioc4 >= eval.SVM[4, 1], 
        bioc5 >= eval.SVM[5, 1])
plot(SVM0k)
writeRaster (SVM0k, "SVM_0k.asc", format="ascii")

# svm 21k
SVM21k <- sum(bioc211 >= eval.SVM[1, 1], 
         bioc212 >= eval.SVM[2, 1], 
         bioc213 >= eval.SVM[3, 1], 
         bioc214 >= eval.SVM[4, 1], 
         bioc215 >= eval.SVM[5, 1])
plot(SVM21k)
writeRaster (SVM21k, "SVM_21k.asc", format="ascii")

# svm 6k
SVM6k <- sum(bioc61 >= eval.SVM[1, 1], 
        bioc62 >= eval.SVM[2, 1], 
        bioc63 >= eval.SVM[3, 1], 
        bioc64 >= eval.SVM[4, 1], 
        bioc65 >= eval.SVM[5, 1])
plot(SVM6k)
writeRaster (SVM6k, "SVM_6k.asc", format="ascii")


###------------------------------------------------------------------------------------------------------###


### ensamble por frequencia - parte 2 ###

# somando todos os algoritmos
# 0k
m1 <- raster ("Bioclim_0k.asc")
m2 <- raster ("Gower_0k.asc")
m3 <- raster ("Maha_0k.asc")
m4 <- raster ("Maxent_0k.asc")
m5 <- raster ("SVM_0k.asc")

ensemble <- m1 + m2 + m3 + m4 + m5
plot(ensemble)
writeRaster (ensemble, "ensemble_0k.asc", format="ascii")

# 21k
m1 <- raster ("Bioclim_21k.asc")
m2 <- raster ("Gower_21k.asc")
m3 <- raster ("Maha_21k.asc")
m4 <- raster ("Maxent_21k.asc")
m5 <- raster ("SVM_21k.asc")

ensemble21k <- m1 + m2 + m3 + m4 + m5
plot(ensemble21k)
writeRaster (ensemble21k, "ensemble_21k.asc", format="ascii")

# 6k
m1 <- raster ("Bioclim_6k.asc")
m2 <- raster ("Gower_6k.asc")
m3 <- raster ("Maha_6k.asc")
m4 <- raster ("Maxent_6k.asc")
m5 <- raster ("SVM_6k.asc")

ensemble6k <- m1 + m2 + m3 + m4 + m5
plot(ensemble6k)
writeRaster (ensemble6k, "ensemble_6k.asc", format="ascii")


# dividindo por 25 para ter um mapa de 0 a 1 
ensemble0kcut <- ensemble/25
writeRaster (ensemble0kcut, "ensemble0k_0_1.asc", format="ascii")

ensemble6kcut <-ensemble6k/25
writeRaster (ensemble6kcut, "ensemble6k_0_1.asc", format="ascii")

ensemble21kcut <- ensemble21k/25
writeRaster (ensemble21kcut, "ensemble21k_0_1.asc", format="ascii")

par(mfrow = c(1, 3))
plot(ensemble0kcut, main = "Atual")
plot(ensemble6kcut, main = "Holoceno")
plot(ensemble21kcut, main = "LGM")


###------------------------------------------------------------------------------------------------------###


### ensemble por media - parte 1 ###

# colocando todos os loops em um arquivo único

# bioclim
Bioclim0k1 <- raster("CCSM_Bioclim_0k_B.balansae1.asc")
Bioclim0k2 <- raster ("CCSM_Bioclim_0k_B.balansae2.asc")
Bioclim0k3 <- raster ("CCSM_Bioclim_0k_B.balansae3.asc")
Bioclim0k4 <- raster("CCSM_Bioclim_0k_B.balansae4.asc")
Bioclim0k5 <- raster ("CCSM_Bioclim_0k_B.balansae5.asc")

Bioclim6k1 <- raster("CCSM_Bioclim_6k_B.balansae1.asc")
Bioclim6k2 <- raster ("CCSM_Bioclim_6k_B.balansae2.asc")
Bioclim6k3 <- raster ("CCSM_Bioclim_6k_B.balansae3.asc")
Bioclim6k4 <- raster("CCSM_Bioclim_6k_B.balansae4.asc")
Bioclim6k5 <- raster ("CCSM_Bioclim_6k_B.balansae5.asc")

Bioclim21k1 <- raster("CCSM_Bioclim_21k_B.balansae1.asc")
Bioclim21k2 <- raster ("CCSM_Bioclim_21k_B.balansae2.asc")
Bioclim21k3 <- raster ("CCSM_Bioclim_21k_B.balansae3.asc")
Bioclim21k4 <- raster("CCSM_Bioclim_21k_B.balansae4.asc")
Bioclim21k5 <- raster ("CCSM_Bioclim_21k_B.balansae5.asc")


Bioclim0k1 <- values (Bioclim0k1)
Bioclim0k2 <- values (Bioclim0k2)
Bioclim0k3 <- values (Bioclim0k3)
Bioclim0k4 <- values (Bioclim0k4)
Bioclim0k5 <- values (Bioclim0k5)

Bioclim6k1 <- values (Bioclim6k1)
Bioclim6k2 <- values (Bioclim6k2)
Bioclim6k3 <- values (Bioclim6k3)
Bioclim6k4 <- values (Bioclim6k4)
Bioclim6k5 <- values (Bioclim6k5)

Bioclim21k1 <- values (Bioclim21k1)
Bioclim21k2 <- values (Bioclim21k2)
Bioclim21k3 <- values (Bioclim21k3)
Bioclim21k4 <- values (Bioclim21k4)
Bioclim21k5 <- values (Bioclim21k5)

Bioclim0k <-cbind (Bioclim0k1, Bioclim0k2, Bioclim0k3, Bioclim0k4, Bioclim0k5)
dim(Bioclim0k) 
Bioclim6k <-cbind (Bioclim6k1, Bioclim6k2, Bioclim6k3, Bioclim6k4, Bioclim6k5)
dim(Bioclim6k) 
Bioclim21k <-cbind (Bioclim21k1, Bioclim21k2, Bioclim21k3, Bioclim21k4, Bioclim21k5)
dim(Bioclim21k)     


# gower
Gower0k1 <- raster ("CCSM_Gower_0k_B.balansae1.asc")
Gower0k2 <- raster ("CCSM_Gower_0k_B.balansae2.asc")
Gower0k3 <- raster ("CCSM_Gower_0k_B.balansae3.asc")
Gower0k4 <- raster ("CCSM_Gower_0k_B.balansae4.asc")
Gower0k5 <- raster ("CCSM_Gower_0k_B.balansae5.asc")

Gower6k1 <- raster ("CCSM_Gower_6k_B.balansae1.asc")
Gower6k2 <- raster ("CCSM_Gower_6k_B.balansae2.asc")
Gower6k3 <- raster ("CCSM_Gower_6k_B.balansae3.asc")
Gower6k4 <- raster ("CCSM_Gower_6k_B.balansae4.asc")
Gower6k5 <- raster ("CCSM_Gower_6k_B.balansae5.asc")

Gower21k1 <- raster ("CCSM_Gower_21k_B.balansae1.asc")
Gower21k2 <- raster ("CCSM_Gower_21k_B.balansae2.asc")
Gower21k3 <- raster ("CCSM_Gower_21k_B.balansae3.asc")
Gower21k4 <- raster ("CCSM_Gower_21k_B.balansae4.asc")
Gower21k5 <- raster ("CCSM_Gower_21k_B.balansae5.asc")

Gower0k1 <- values (Gower0k1)
Gower0k2 <- values (Gower0k2)
Gower0k3 <- values (Gower0k3)
Gower0k4 <- values (Gower0k4)
Gower0k5 <- values (Gower0k5)

Gower6k1 <- values (Gower6k1)
Gower6k2 <- values (Gower6k2)
Gower6k3 <- values (Gower6k3)
Gower6k4 <- values (Gower6k4)
Gower6k5 <- values (Gower6k5)

Gower21k1 <- values (Gower21k1)
Gower21k2 <- values (Gower21k2)
Gower21k3 <- values (Gower21k3)
Gower21k4 <- values (Gower21k4)
Gower21k5 <- values (Gower21k5)

Gower0k <- cbind(Gower0k1, Gower0k2, Gower0k3, Gower0k4, Gower0k5)
dim(Gower0k)
Gower6k <- cbind(Gower6k1, Gower6k2, Gower6k3, Gower6k4, Gower6k5)
dim(Gower6k)
Gower21k <- cbind(Gower21k1, Gower21k2, Gower21k3, Gower21k4, Gower21k5)
dim(Gower21k)


# mahanalobis
Maha0k1 <- raster ("CCSM_Maha_0k_B.balansae1.asc")
Maha0k2 <- raster ("CCSM_Maha_0k_B.balansae2.asc")
Maha0k3 <- raster ("CCSM_Maha_0k_B.balansae3.asc")
Maha0k4 <- raster ("CCSM_Maha_0k_B.balansae4.asc")
Maha0k5 <- raster ("CCSM_Maha_0k_B.balansae5.asc")

Maha6k1 <- raster ("CCSM_Maha_6k_B.balansae1.asc")
Maha6k2 <- raster ("CCSM_Maha_6k_B.balansae2.asc")
Maha6k3 <- raster ("CCSM_Maha_6k_B.balansae3.asc")
Maha6k4 <- raster ("CCSM_Maha_6k_B.balansae4.asc")
Maha6k5 <- raster ("CCSM_Maha_6k_B.balansae5.asc")

Maha21k1 <- raster ("CCSM_Maha_21k_B.balansae1.asc")
Maha21k2 <- raster ("CCSM_Maha_21k_B.balansae2.asc")
Maha21k3 <- raster ("CCSM_Maha_21k_B.balansae3.asc")
Maha21k4 <- raster ("CCSM_Maha_21k_B.balansae4.asc")
Maha21k5 <- raster ("CCSM_Maha_21k_B.balansae5.asc")

Maha0k1 <- values (Maha0k1)
Maha0k2 <- values (Maha0k2)
Maha0k3 <- values (Maha0k3)
Maha0k4 <- values (Maha0k4)
Maha0k5 <- values (Maha0k5)

Maha6k1 <- values (Maha6k1)
Maha6k2 <- values (Maha6k2)
Maha6k3 <- values (Maha6k3)
Maha6k4 <- values (Maha6k4)
Maha6k5 <- values (Maha6k5)

Maha21k1 <- values (Maha21k1)
Maha21k2 <- values (Maha21k2)
Maha21k3 <- values (Maha21k3)
Maha21k4 <- values (Maha21k4)
Maha21k5 <- values (Maha21k5)

Maha0k <- cbind(Maha0k1, Maha0k2, Maha0k3, Maha0k4, Maha0k5 )
dim(Maha0k)
Maha6k <- cbind(Maha6k1, Maha6k2, Maha6k3, Maha6k4, Maha6k5)
dim(Maha6k)
Maha21k <- cbind(Maha21k1, Maha21k2, Maha21k3, Maha21k4, Maha21k5)
dim(Maha21k)


# maxent
Maxent0k1 <- raster ("CCSM_Maxent_0k_B.balansae1.asc")
Maxent0k2 <- raster ("CCSM_Maxent_0k_B.balansae2.asc")
Maxent0k3 <- raster ("CCSM_Maxent_0k_B.balansae3.asc")
Maxent0k4 <- raster ("CCSM_Maxent_0k_B.balansae4.asc")
Maxent0k5 <- raster ("CCSM_Maxent_0k_B.balansae5.asc")

Maxent6k1 <- raster ("CCSM_Maxent_6k_B.balansae1.asc")
Maxent6k2 <- raster ("CCSM_Maxent_6k_B.balansae2.asc")
Maxent6k3 <- raster ("CCSM_Maxent_6k_B.balansae3.asc")
Maxent6k4 <- raster ("CCSM_Maxent_6k_B.balansae4.asc")
Maxent6k5 <- raster ("CCSM_Maxent_6k_B.balansae5.asc")

Maxent21k1 <- raster ("CCSM_Maxent_21k_B.balansae1.asc")
Maxent21k2 <- raster ("CCSM_Maxent_21k_B.balansae2.asc")
Maxent21k3 <- raster ("CCSM_Maxent_21k_B.balansae3.asc")
Maxent21k4 <- raster ("CCSM_Maxent_21k_B.balansae4.asc")
Maxent21k5 <- raster ("CCSM_Maxent_21k_B.balansae5.asc")

Maxent0k1 <- values (Maxent0k1)
Maxent0k2 <- values (Maxent0k2)
Maxent0k3 <- values (Maxent0k3)
Maxent0k4 <- values (Maxent0k4)
Maxent0k5 <- values (Maxent0k5)

Maxent6k1 <- values (Maxent6k1)
Maxent6k2 <- values (Maxent6k2)
Maxent6k3 <- values (Maxent6k3)
Maxent6k4 <- values (Maxent6k4)
Maxent6k5 <- values (Maxent6k5)

Maxent21k1 <- values (Maxent21k1)
Maxent21k2 <- values (Maxent21k2)
Maxent21k3 <- values (Maxent21k3)
Maxent21k4 <- values (Maxent21k4)
Maxent21k5 <- values (Maxent21k5)

Maxent0k <- cbind(Maxent0k1, Maxent0k2, Maxent0k3, Maxent0k4, Maxent0k5 )
dim(Maxent0k)
Maxent6k <- cbind(Maxent6k1, Maxent6k2, Maxent6k3, Maxent6k4, Maxent6k5 )
dim(Maxent0k)
Maxent21k <- cbind(Maxent21k1, Maxent21k2, Maxent21k3, Maxent21k4, Maxent21k5 )
dim(Maxent0k)


# svm
SVM0k1 <- raster ("CCSM_SVM_0k_B.balansae1.asc")
SVM0k2 <- raster ("CCSM_SVM_0k_B.balansae2.asc")
SVM0k3 <- raster ("CCSM_SVM_0k_B.balansae3.asc")
SVM0k4 <- raster ("CCSM_SVM_0k_B.balansae4.asc")
SVM0k5 <- raster ("CCSM_SVM_0k_B.balansae5.asc")

SVM6k1 <- raster ("CCSM_SVM_6k_B.balansae1.asc")
SVM6k2 <- raster ("CCSM_SVM_6k_B.balansae2.asc")
SVM6k3 <- raster ("CCSM_SVM_6k_B.balansae3.asc")
SVM6k4 <- raster ("CCSM_SVM_6k_B.balansae4.asc")
SVM6k5 <- raster ("CCSM_SVM_6k_B.balansae5.asc")

SVM21k1 <- raster ("CCSM_SVM_21k_B.balansae1.asc")
SVM21k2 <- raster ("CCSM_SVM_21k_B.balansae2.asc")
SVM21k3 <- raster ("CCSM_SVM_21k_B.balansae3.asc")
SVM21k4 <- raster ("CCSM_SVM_21k_B.balansae4.asc")
SVM21k5 <- raster ("CCSM_SVM_21k_B.balansae5.asc")

SVM0k1 <- values (SVM0k1)
SVM0k2 <- values (SVM0k2)
SVM0k3 <- values (SVM0k3)
SVM0k4 <- values (SVM0k4)
SVM0k5 <- values (SVM0k5)

SVM6k1 <- values (SVM6k1)
SVM6k2 <- values (SVM6k2)
SVM6k3 <- values (SVM6k3)
SVM6k4 <- values (SVM6k4)
SVM6k5 <- values (SVM6k5)

SVM21k1 <- values (SVM21k1)
SVM21k2 <- values (SVM21k2)
SVM21k3 <- values (SVM21k3)
SVM21k4 <- values (SVM21k4)
SVM21k5 <- values (SVM21k5)

SVM0k <- cbind(SVM0k1, SVM0k2, SVM0k3, SVM0k4, SVM0k5 )
dim(SVM0k)
SVM6k <- cbind(SVM6k1, SVM6k2, SVM6k3, SVM6k4, SVM6k5 )
dim(SVM6k)
SVM21k <- cbind(SVM21k1, SVM21k2, SVM21k3, SVM21k4, SVM21k5 )
dim(SVM21k)


###------------------------------------------------------------------------------------------------------###


### ensemble por media - parte 2 ###

# media
head(Bioclim0k)

Bioclim.0k.mean <- apply(Bioclim0k, 1, mean)
Gower.0k.mean <- apply(Gower0k, 1, mean)
Maha.0k.mean <- apply(Maha0k, 1, mean)
Maxent.0k.mean <- apply(Maxent0k, 1, mean)
SVM.0k.mean <- apply(SVM0k, 1, mean)

Bioclim.6k.mean <- apply(Bioclim6k, 1, mean)
Gower.6k.mean <- apply(Gower6k, 1, mean)
Maha.6k.mean <- apply(Maha6k, 1, mean)
Maxent.6k.mean <- apply(Maxent6k, 1, mean)
SVM.6k.mean <- apply(SVM6k, 1, mean)

Bioclim.21k.mean <- apply(Bioclim21k, 1, mean)
Gower.21k.mean <- apply(Gower21k, 1, mean)
Maha.21k.mean <- apply(Maha21k, 1, mean)
Maxent.21k.mean <- apply(Maxent21k, 1, mean)
SVM.21k.mean <- apply(SVM21k, 1, mean)

# organizar os valores das medias dos algoritmos em um arquivo
Output0k <- cbind(Bioclim = Bioclim.0k.mean, Gower = Gower.0k.mean, Maha = Maha.0k.mean, 
			SVM= SVM.0k.mean, Maxent = Maxent.0k.mean)
head(Output0k)

Output6k <- cbind(Bioclim = Bioclim.6k.mean, Gower = Gower.6k.mean, Maha = Maha.6k.mean, 
			SVM = SVM.6k.mean, Maxent = Maxent.6k.mean)
head(Output6k)

Output21k <- cbind(Bioclim = Bioclim.21k.mean, Gower = Gower.21k.mean, Maha = Maha.21k.mean, 
			SVM = SVM.21k.mean, Maxent = Maxent.21k.mean)
head(Output21k)

# conferindo a dimensao
dim(Output0k)
dim(Output6k)
dim(Output21k)

# inserindo coordenadas geograficas aos outputs
setwd("D:/90_aulas_montadas/_disciplina_enm_R_unesp_2017/scripts_r/01_dados")
env <- raster ("CCSM_0k_am_bio02.asc")
coords <- xyFromCell(env, 1:ncell(env))
Output0k <- cbind(coords, Output0k)
Output6k <- cbind(coords, Output6k)
Output21k <- cbind(coords, Output21k)

head(Output0k)

# excluindo NAs
Output0k <- na.omit(Output0k)
Output21k <- na.omit(Output21k)
Output6k <- na.omit(Output6k)
nrow(Output0k)
nrow(Output6k)
nrow(Output21k)

# coercao em dataframe
Output0k <- as.data.frame(Output0k)
head(Output0k)
Output6k <- as.data.frame(Output6k)
Output21k <- as.data.frame(Output21k)

# transformando em arquivos espacializados - grid
gridded(Output0k) <- ~x + y
gridded(Output6k) <- ~x + y
gridded(Output21k) <- ~x + y

# transformando em arquivos stack raster
output.0k <- stack(Output0k)
output.6k <- stack(Output6k)
output.21k <- stack(Output21k)

# plot
par(mfrow = c(2, 3))
plot(output.0k)
plot(output.6k)
plot(output.21k)

# salvando os arquivos
setwd("C:/Users/leec/Dropbox/disciplina_enm_R_unicamp_2016/scripts_r/saidas_enm")
write.table (Output0k, "Output0k.txt")
write.table (Output6k, "Output6k.txt")
write.table (Output21k, "Output21k.txt")

###------------------------------------------------------------------------------------------------------###

