### paper - barbeiros no parana ###

# andrade et al. 2017

# Maurício Humberto Vancine - mauricio.vancine@gmail.com
# 10/01/2017

### analise  ###

###-----------------------------------------------------------------------------------------###

# limpar o workspace e aumentar a memoria para o r
rm(list = ls())
gc() 
memory.limit(size = 10000000000) 

# instalar e carregar pacotes
if(!require(pacman)) install.packages("pacman")
pacman::p_load(raster, data.table)

###-----------------------------------------------------------------------------------------###

# diretorio
setwd("D:/butterflies/modelos_borboletas/modelos/ecoland")

###-----------------------------------------------------------------------------------------###


# import data
enm.clim <- raster("clima_richness.tif")
plot(enm.clim)
enm.lands <- raster("landscape_richness.tif")
plot(enm.lands)
enm.clim.v <- values(enm.clim)
enm.land.v <- values(enm.lands)
###-----------------------------------------------------------------------------------------###

# graficos

da <- data.table(clim = enm.clim.v, lands = enm.land.v)
da.na <- na.omit(da)
da.na
write.table(da.na, 'table_suit_climate_land_.txt')

min(da.na$clim)
min(da.na$lands)

# plot
par(mar = c(5, 5, 2, 2))
plot(da.na$clim, da.na$lands, type = "n",
     ylim = c(0, 150), 
	   xlim = c(0, 150),
     xlab = "Climate Richness", 
	   ylab = "Landscape Richness",
     col.axis = 'grey30',					
     cex.lab = 1.6,												
     cex.axis = 1.2, 
     las = 1)

abline(h = .25 * max(da.na$clim), v = .25 * max(da.na$lands), col = "gray", lty = 2)
abline(h = .75 * max(da.na$clim), v = .75 * max(da.na$lands), col = "gray", lty = 2)


h <- da.na[da.na$clim >= .75 * max(da.na$clim) & 
             da.na$lands >= .75 * max(da.na$lands), ]
points(h$clim, h$lands, col = adjustcolor("red", .01))

h.c_m.l <- da.na[da.na$clim >= 0.75 * max(da.na$clim) & 
                   da.na$lands >= 0.25 * max(da.na$lands) & 
                   da.na$lands < 0.75 * max(da.na$lands), ]
points(h.c_m.l$clim, h.c_m.l$lands, col = adjustcolor("dark green", .01))

m.c_h.l <- da.na[da.na$clim >= 0.25 * max(da.na$clim) & 
                   da.na$clim < 0.75 * max(da.na$clim) & 
                   da.na$lands >= 0.75 * max(da.na$lands), ]
points(m.c_h.l$clim, m.c_h.l$lands, col = adjustcolor("dark orange", .01))

m.c_m.l <- da.na[da.na$clim >= 0.25 * max(da.na$clim) & 
                   da.na$clim < 0.75 * max(da.na$clim) & 
                   da.na$lands >= 0.25 * max(da.na$lands) & 
                   da.na$lands < 0.75 * max(da.na$lands), ]
points(m.c_m.l$clim, m.c_m.l$lands, col = adjustcolor("green", .01))

l.c_h.l <- da.na[da.na$clim < 0.25 * max(da.na$clim) & 
                   da.na$lands >= 0.75 * max(da.na$lands), ]
points(l.c_h.l$clim, l.c_h.l$lands, col = adjustcolor("yellow", .01))

h.c_l.l <- da.na[da.na$clim >= 0.75 * max(da.na$clim) & 
                   da.na$lands < 0.25 * max(da.na$lands), ]
points(h.c_l.l$clim, h.c_l.l$lands, col = adjustcolor("chocolate4", .01))

l.c_m.l <- da.na[da.na$clim < 0.25 * max(da.na$clim) & 
                   da.na$lands >= 0.25 * max(da.na$lands) & 
                   da.na$lands < 0.75 * max(da.na$lands), ]
points(l.c_m.l$clim, l.c_m.l$lands, col = adjustcolor("blue", .01))

m.c_l.l <- da.na[da.na$clim >= 0.25 * max(da.na$clim) & 
                   da.na$clim >= 0.25 * max(da.na$clim) & 
                   da.na$lands < 0.25 * max(da.na$lands), ]
points(m.c_l.l$clim, m.c_l.l$lands, col = adjustcolor("blue", .01))

l <- da.na[da.na$clim < 0.25 * max(da.na$clim) & 
             da.na$lands < 0.25 * max(da.na$lands), ]
points(l$clim, l$lands, col = adjustcolor("blue", .01))


# tabela
co <- data.table(id = 1:ncell(enm.clim), cl = enm.clim[], la = enm.lands[])
co.na <- na.omit(co)

co.na$col <- NA
co.na

:nrow(co.na)

for(i in 1:50){
  if(co.na[i, co.na$cl >= .75 * max(co.na$cl) & 
           co.na$la >= .75 * max(co.na$la)]){co.na$col <- "red"}
  
  else if(co.na[i, co.na$cl >= 0.75 * max(co.na$cl) & 
                co.na$la >= 0.25 * max(co.na$la) & 
                co.na$la < 0.75 * max(co.na$la)]){co.na$col <- "dark green"}
  
  else if(co.na[i, co.na$cl >= 0.25 * max(co.na$cl) & 
                co.na$cl < 0.75 * max(co.na$cl) & 
                co.na$la >= 0.75 * max(co.na$la)]){co.na$col <- "orange"}
  
  else if(co.na[i, co.na$cl >= 0.25 * max(co.na$cl) & 
                co.na$cl < 0.75 * max(co.na$cl) & 
                co.na$la >= 0.25 * max(co.na$la) & 
                co.na$la < 0.75 * max(co.na$la)]){co.na$col <- "yellow"}
  
  else if(co.na[i, co.na$cl >= 0.75 * max(co.na$cl) & 
                co.na$la < 0.25 * max(co.na$la)]){co.na$col <- "chocolate4"}
  
  else if(co.na[i, co.na$cl < 0.25 * max(co.na$cl) & 
                co.na$la >= 0.25 * max(co.na$la) & 
                co.na$la < 0.75 * max(co.na$la)]){co.na$col <- "blue"}}
     

co.na

# mapa
r <- raster()
values(r) <- rnorm(ncell(r))

plot(r, col = c("black", "gray"))





