### script preparacao e selecao das variaveis ambientais ### 

# Thadeu Sobral de Souza - thadeusobral@gmail.com 
# Maurício Humberto Vancine - mauricio.vancine@gmail.com

###-----------------------------------------------------------------------------------------###

# 1. limpara a memoria e carregar os pacotes 
# limpar o workspace e aumentar a memoria para o r
rm(list = ls())
memory.limit(size = 10000000000000) 

# instalar e carregar pacotes
# install.packages(c("raster", "rgdal", "corrplot", "RStoolbox", "vegan", "psych"), dep = T)

# carregar pacotes
library(raster) # manejo de arquivos sig 
library(rgdal) # manejo de arquivos sig
library(corrplot) # graficos de correlacao
library(RStoolbox) # pca de arquivos raster
library(vegan) # diversas analises multivariadas
library(psych) # analise fatorial

# verificar pacotes carregados
search()

###-----------------------------------------------------------------------------------------###

# 2. importar os dados
# diretorio
setwd("D:/90_aulas_montadas/_disciplina_enm_R_unesp_2017/scripts_r/01_dados")
getwd()

# listar o nome dos arquivos no diretorio com um padrao
asc <- list.files(patt = ".asc")
asc

# selecionar o nome dos arquivos especificos
pres <- grep("0k", asc, value = T)
pres

hol <- grep("6k", asc, value = T)
hol

lgm <- grep("21k", asc, value = T)
lgm

# carregar os arquivos .asc em uma variavel rasterstack e renomea-los
pres.s <- stack(pres)
pres.s

names(pres.s)
names(pres.s) <- c(paste0("pres_", "bio0", 1:9), paste0("pres_", "bio", 10:19))
names(pres.s)

plot(pres.s)
plot(pres.s[[1]])

# bioclim - descricao
# http://www.worldclim.org/bioclim

hol.s <- stack(hol)
hol.s
names(hol.s) <- c(paste0("hol_", "bio0", 1:9), paste0("hol_", "bio", 10:19))
plot(hol.s)

lgm.s <- stack(lgm)
lgm.s
names(lgm.s) <- c(paste0("lgm_", "bio0", 1:9), paste0("lgm_", "bio", 10:19))
plot(lgm.s)


###-----------------------------------------------------------------------------------------###

# 3. cortar raster para a area de interesse - mascara
# 3.1. limite de interesse
# extensao
am <- extent(c(-90, -34, -60, 15)) # xmin, xmax, ymin, ymax
plot(am, xlab = "long", ylab = "lat", type = "n")
plot(pres.s[[1]], add = T)
plot(am, cex.lab = 1.3, col = "red", add = T)

# corte
pres.am <- crop(pres.s, am)
pres.am
pres.s

plot(pres.s[[1]])
plot(am, col = "red", add = T)

par(mfrow = c(1, 2))
plot(pres.s$pres_bio01)
plot(pres.am$pres_bio01)
dev.off()

hol.am <- crop(hol.s, am)
hol.am
plot(hol.am)

lgm.am <- crop(lgm.s, am)
lgm.am
plot(lgm.am)


# 3.2 a partir de um shapefile
# importar shapefile
br <- shapefile("brasil_gcs_wgs84.shp")
br
plot(br, col = "gray", axes = T)

# ajuste ao limite
ma <- mask(pres.s, br)
ma
plot(ma)
plot(ma[[1]])
plot(br, add = T)

# ajuste da extensao
cr <- crop(pres.s, br)
cr
plot(cr)
plot(cr[[1]])
plot(br, add = T)

# ajustar o limite e a extesao
ma <- mask(pres.s, br)
cr <- crop(ma, br)
plot(cr[[1]])
plot(br, add = T)

cr <- crop(pres.s, br)
ma <- mask(cr, br)
plot(ma[[1]])
plot(br, add = T)

# juntos
pres.br <- mask(crop(pres.s, br), br)
plot(pres.br)

hol.br <- mask(crop(hol.s, br), br)
plot(hol.br)

lgm.br <- mask(crop(lgm.s, br), br)
plot(lgm.br)


###-----------------------------------------------------------------------------------------###

4. extrair os valores das celulas
# extraindo valores dos rasters cortados
pres.am.v <- values(pres.am)
head(pres.am.v)
head(pres.am.v, 10)
head(pres.am.v, 50)

# contando o numero de linhas
nrow(pres.am.v)

# dimensao
dim(pres.am.v)

# omitir os NAs
pres.am.v.na <- na.omit(pres.am.v)
head(pres.am.v.na, 50)
dim(pres.am.v.na)

# perguntar se ha NAs
any(is.na(pres.am.v.na))

###-----------------------------------------------------------------------------------------###

# 5.correlacao

# criar pasta e definir diretorio para analise exploratoria - correlacao
dir.create("analise_selecao_variaveis")  # criar uma pasta no diretorio

setwd("./analise_selecao_variaveis")  # mudar o diretorio para a pasta criada

dir.create("correlacao")  # criar pasta no diretorio da pasta criada

setwd("./correlacao") # mudar o diretorio para a pasta criada, da pasta criada

getwd() 

# tabela da correlacao
corr <- cor(pres.am.v.na)
corr
round(corr, 2) # arredondamento dos valores para dois valores decimais
abs(round(corr, 2)) # arredondamento e valor absoluto
ifelse(corr >= 0.7, "Sim", "Não") # sim ou nao
ifelse(corr >= 0.7, 1, 0) # 1 ou 0

# exportar tabela com a correlacao
write.table(abs(round(corr, 2)), "cor_pres.xls", row.names = T, sep = "\t")
write.table(ifelse(corr >= 0.7, "Sim", "Não"), "cor_pres_afirmacao.xls", row.names = T, 
		sep = "\t")

# plot da correlacao
corrplot(corr, type = "lower", diag = F, tl.srt = 45, mar = c(3, 0.5, 2, 1),
	   title = "Correlações entre variáveis Bioclimáticas")

# apenas azul
corrplot(abs(corr), type = "lower", diag = F, tl.srt = 45, mar = c(3, 0.5, 2, 1),
	   title = "Correlações entre variáveis Bioclimáticas")

# apenas vermelho
corrplot(-1 * (abs(corr)), type = "lower", diag = F, tl.srt = 45, mar = c(3, 0.5, 2, 1),
	   title = "Correlações entre variáveis Bioclimáticas")

# exportar figura na pasta do diretorio
tiff("cor_ma.tif", width = 18, height = 18, units = "cm", res = 300, compression = "lzw")

corrplot(abs(corr), type = "lower", diag = F, tl.srt = 45, mar = c(3, 0.5, 2, 1),
	   title = "Correlações entre variáveis Bioclimáticas")

dev.off()

###-----------------------------------------------------------------------------------------###

# 6. pca

# criar pasta e definir diretorio para analise exploratoria - pca
setwd("..") # voltar uma pasta no diretorio
getwd() # conferir o diretorio
dir.create("pca") # criar pasta no diretorio
setwd("./pca") # mudar o diretorio para a pasta criada
getwd() # conferir o diretorio

# 6.1. pca para escolher variaveis
# pca do pacote "stats"
# pca com normalizacao interna
pca <- prcomp(pres.am.v.na, scale = T)
pca

# contribuicao de cada eixo (eigenvalues - autovalores)
summary(pca)

# grafico de barras com as contribuicoes
par(mar = c(3, 5, 5, 2))
screeplot(pca, main = "Contribuição de cada PC", ylab = "Autovalores", cex.lab = 1.3)
abline(h = 1, col = "red", lty = 2)

tiff("screeplot.tif", wi = 18, he = 18, un = "cm", res = 300, comp = "lzw")
par(mar = c(3, 5, 5, 2))
screeplot(pca, main = "Contribuição de cada PC", ylab = "Autovalores", cex.lab = 1.3)
abline(h = 1, col = "red", lty = 2)
dev.off()

# valores de cada eixo (eigenvectors - autovetores - escores)
pca$x
dim(pca$x)

# relacao das variaveis com cada eixo (loadings - cargas)
pca$rotation[, 1:5]
round(pca$rotation[, 1:5], 2)
abs(round(pca$rotation[, 1:5], 2))

# exportar tabela com a contribuicao
write.table(abs(round(pca$rotation[, 1:5], 2)), "contr_pca.xls", row.names = T, sep = "\t")

# plot
biplot(pca, var.axes = T, xlabs = rep("o", nrow(pca$x)), ylabs = paste0("bio", 1:19), cex = .8,
	 expand = 1.2, xlab = "PC1 (43.52%)", ylab = "PC2 (23.51%)", main = "PCA Bioclimáticas América do Sul", 
	 xlim = c(-.03, .04))

# 6.2. pca como novas variaveis
# pca dos raster
pca.am <- rasterPCA(pres.am, spca = T) 
pca.am

# contribuicao dos componentes
summary(pca.am$model)
summary(pca)

# grafico de barras com as contribuicoes
par(mar = c(3, 5, 5, 2))
screeplot(pca.am$model, main = "Contribuição de cada PC", ylab = "Autovalores", cex.lab = 1.3)
abline(h = 1, col = "red", lty = 2)

# comparacao
par(mfrow = c(1, 2))
screeplot(pca.am$model, main = "Contribuição de cada PC", ylab = "Autovalores", cex.lab = 1.3)
abline(h = 1, col = "red", lty = 2)

screeplot(pca, main = "Contribuição de cada PC", ylab = "Autovalores", cex.lab = 1.3)
abline(h = 1, col = "red", lty = 2)

tiff("screeplot_raster.tif", wi = 18, he = 18, un = "cm", res = 300, comp = "lzw")
par(mar = c(3, 5, 5, 2))
screeplot(pca.am$model, main = "Contribuição de cada PC", ylab = "Autovalores", cex.lab = 1.3)
abline(h = 1, col = "red", lty = 2)
dev.off()

# plot das pcs como novas variaveis
plot(pca.am$map)

plot(pca.am$map[[1:5]])


# exportar as novas variaveis
# exportar apenas uma variavel
writeRaster(pca.am$map[[1]], "pc1_e.asc", format = "ascii")
writeRaster(pca.am$map[[2]], "pc2_e.asc", format = "ascii")
writeRaster(pca.am$map[[3]], "pc3_e.asc", format = "ascii")
writeRaster(pca.am$map[[4]], "pc4_e.asc", format = "ascii")
writeRaster(pca.am$map[[5]], "pc5_e.asc", format = "ascii")

# comando for
print(1)
print(2)
print(3)
print(4)
print(5)

for(i in 1:5){
  print(i)}

for(i in 1:5000){
  print(i)}

# exportar as cinco variaveis
for(i in 1:5){
  writeRaster(pca.am$map[[i]], paste0("pc", i, "_e.asc"), format = "ascii", overwrite = T)}

###-----------------------------------------------------------------------------------------###

# 7. analise fatorial

# criar pasta e definir diretorio para analise exploratoria - fatorial
setwd("..") 
getwd() 
dir.create("fatorial") 
setwd("./fatorial") 
getwd() 

# analises preliminares de possibilidade de uso da analise fatorial
# kmo e bartlett
KMO(cor(pres.am.v.na)) # deve ser acima de 0.5
cortest.bartlett(cor(pres.am.v.na), n = nrow(pres.am.v.na)) # deve ser significativo (p < 0.05)

# numero de eixos - semelhante a pca
# screeplot
fa <- fa.parallel(pres.am.v.na, fa = "fa", fm = "ml") # sugere 5 eixos
fa

# exportar screeplot
tiff("screeplot_fatorial.tif", wi = 18, he = 18, un = "cm", res = 300, comp = "lzw")
fa.parallel(pres.am.v.na, fm = "ml", fa = "fa") 
dev.off()

# fatorial
fa.am <- fa(pres.am.v.na, nfactors = 5, rotate = "varimax", fm = "ml")
am.loadings <- loadings(fa.am)
am.loadings

abs(round(am.loadings, 2))

# exportar tabela dos resultados
write.table(abs(round(am.loadings, 2)), "as_loadings.xls", row.names = T, sep = "\t")

# bios escolhidas
# bio02, bio04, bio10, bio16, bio17

# significado das bios
# BIO1 = Temperatura media anual
# BIO2 = Variacao da media diurna (media por mes (temp max - temp min))
# BIO3 = Isotermalidade (BIO2/BIO7) (* 100)
# BIO4 = Sazonalidade da temperatura (desvio padrao deviation *100)
# BIO5 = Temperatura maxima do mes mais quente
# BIO6 = Temperatura minima do mes mais frio
# BIO7 = Variacao da temperatura anual (BIO5-BIO6)
# BIO8 = Temperatura media do trimestre mais chuvoso
# BIO9 = Temperatura media do trimestre mais seco
# BIO10 = Temperatura media do trimestre mais quente
# BIO11 = Temperatura media do trimestre mais frio
# BIO12 = Precipitacao anual
# BIO13 = Precipitacao do mes mais chuvoso
# BIO14 = Precipitacao do mes mais seco
# BIO15 = Sazonalidade da precipitacao (coeficiente de variacao)
# BIO16 = Precipitacao do trimestre mais chuvoso
# BIO17 = Precipitacao do trimestre mais seco
# BIO18 = Precipitacao do trimestre mais quente
# BIO19 = Precipitacao do trimestre mais frio

###-----------------------------------------------------------------------------------------###

# 8. exportar as variaveis escolhidas
# bios escolhidas
# bio02, bio04, bio10, bio16, bio17

pres.am
names(pres.am)

lista <- c(02, 04, 10, 16, 17)

pres.am[[01]]

# diretorios de saida
setwd("D:/90_aulas_montadas/_disciplina_enm_R_unesp_2017/scripts_r/01_dados")

# presente
for(i in lista){
  writeRaster(pres.am[[i]], ifelse(i < 10, paste0("CCSM_0k_am_bio0", i, ".asc"), 
		  paste0("CCSM_0k_am_bio", i, ".asc")), format = "ascii")}

# holoceno
for(i in lista){
  writeRaster(hol.am[[i]], ifelse(i < 10, paste0("CCSM_6k_am_bio0", i, ".asc"), 
		  paste0("CCSM_6k_am_bio", i, ".asc")), format = "ascii")}

# lgm
for(i in lista){
  writeRaster(lgm.am[[i]], ifelse(i < 10, paste0("CCSM_21k_am_bio0", i, ".asc"), 
		  paste0("CCSM_21k_am_bio", i, ".asc")), format = "ascii")}

###-----------------------------------------------------------------------------------------###


