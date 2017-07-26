### script ecoland butterflies ###

# Maurício Humberto Vancine - mauricio.vancine@gmail.com
# 28/06/2017

###---------------------------------------------------------------------------###

# 1. clear memory and load packages 
# clear workspace and increase memory
rm(list = ls())
gc()
memory.limit(size = 1.75e13) 

# packages
if(!require("pacman")) install.packages("pacman")
pacman::p_load(raster, rgdal, data.table, colorRamps, ggplot2, hexbin)

# verify packages
search()

###---------------------------------------------------------------------------###

# diretorio 
setwd("D:/butterflies/02_enms/02_modelos/ecoland")

# enm climate
re <- raster('clima_richness_lands.tif')
plot(re)

re[is.nan(re)] <- NA

# enm landscaep
rl <- raster('landscape_richness.tif')
plot(rl)

###------------------------------------------------------------------###

# ## classification in 9
# 
# # values
# da <- data.table(id = 1:ncell(re), cl = re[], la = rl[], col = "NA", val = 0)
# da
# dim(da)
# 
# id.na <- as.matrix(da[which(is.na(da$cl)), 1])
# str(id.na)
# head(id.na)
# 
# da.s.na <- da[!id.na]
# da.s.na
# dim(da.s.na)
# head(da.s.na)
# 
# da.c.na <- da[id.na]
# da.c.na
# dim(da.c.na)
# 
# 
# # classification
# system.time(
# for(i in 1:nrow(da.s.na)){
#   if(da.s.na[i, 2] <= .25 * max(da.s.na$cl, na.rm = T) & 
#      da.s.na[i, 3] <= .25 * max(da.s.na$la, na.rm = T)){
#     da.s.na[i, 4] <- "blue"
#     da.s.na[i, 5] <- 0} 
#   
#   else if(da.s.na[i, 2] <= .25 * max(da.s.na$cl, na.rm = T) & 
#           da.s.na[i, 3] > .25 * max(da.s.na$la, na.rm = T) & da.s.na[i, 3] <= .75 * max(da.s.na$la, na.rm = T)){  
#     da.s.na[i, 4] <- "cyan"
#     da.s.na[i, 5] <- .125} 
#   
#   else if(da.s.na[i, 2] > .25 * max(da.s.na$cl, na.rm = T) & da.s.na[i, 2] <= .75 * max(da.s.na$cl, na.rm = T) & 
#           da.s.na[i, 3] <= .25* max(da.s.na$la, na.rm = T)){  
#     da.s.na[i, 4] <- "cyan4"
#     da.s.na[i, 5] <- .25} 
#   
#   else if(da.s.na[i, 2] > .25 * max(da.s.na$cl, na.rm = T) & da.s.na[i, 2] <= .75 * max(da.s.na$cl, na.rm = T) & 
#           da.s.na[i, 3] > .25 * max(da.s.na$la, na.rm = T) & da.s.na[i, 3] <= .75* max(da.s.na$la, na.rm = T)){  
#     da.s.na[i, 4] <- "green"
#     da.s.na[i, 5] <- .375} 
#   
#   else if(da.s.na[i, 2] > .75 * max(da.s.na$cl, na.rm = T) & 
#           da.s.na[i, 3] <= .25* max(da.s.na$la, na.rm = T)){  
#     da.s.na[i, 4] <- "chocolate"
#     da.s.na[i, 5] <- .5} 
#   
#   else if(da.s.na[i, 2] <= .25 * max(da.s.na$cl, na.rm = T) & 
#           da.s.na[i, 3] > .75* max(da.s.na$la, na.rm = T)){  
#     da.s.na[i, 4] <- "yellow"
#     da.s.na[i, 5] <- .625} 
#   
#   else if(da.s.na[i, 2] > .25 * max(da.s.na$cl, na.rm = T) & da.s.na[i, 2] <= .75 * max(da.s.na$cl, na.rm = T) & 
#           da.s.na[i, 3] > .75* max(da.s.na$la, na.rm = T)){  
#     da.s.na[i, 4] <- "orange"
#     da.s.na[i, 5] <- .75}
#   
#   else if(da.s.na[i, 2] > .75 * max(da.s.na$cl, na.rm = T) & 
#           da.s.na[i, 3] > .25 * max(da.s.na$la, na.rm = T) & da.s.na[i, 3] <= .75 * max(da.s.na$la, na.rm = T)){  
#     da.s.na[i, 4] <- "dark green"
#     da.s.na[i, 5] <- .875}
#   
#   else if(da.s.na[i, 2] > .75 * max(da.s.na$cl, na.rm = T) & 
#           da.s.na[i, 3] > .75 * max(da.s.na$la, na.rm = T)){  
#     da.s.na[i, 4] <- "red"
#     da.s.na[i, 5] <- 1}})
# 
# da.s.na
# head(da.s.na)
# write.table (da.s.na, 'ecoland_table.txt')
# 
# da.s.na <- read.table('ecoland_table_naomit.txt')
# 
# da.eco <- rbind(da.s.na, da.c.na)
# da.eco <- da.eco[order(id)]
# da.eco
# head(da.eco)
# write.table(da.eco, "ecolanda_table_all_AF.txt")

###------------------------------------------------------------------###

# importar tabela
da <- fread("ecolanda_table_all_AF.txt")
da


# lat-long
co <- data.table(xyFromCell(rl, 1:ncell(rl)), rl[])
co

colnames(co) <- c("long", "lat", "val")
co

# data
da.c <- data.table(co, da)
da.c

any(is.na(da.c$val) == is.na(da.c$V5))

da.na <- na.omit(da.c)
da.na

colnames(da.na) <- c("long", "lat", "val", "id", "cell", "cl", "la", "col", "val")
da.na

# plot
par(mar = c(5, 5, 2, 2))

plot(da.na$cl, da.na$la,
     type = "n",
     xlim = c(0, 150),
     ylim = c(0, 150),
     xlab = "Climate Richness", 
     ylab = "Landscape Richness",
     col.axis = 'grey30',					
     cex.lab = 1.6,												
     cex.axis = 1.2, 
     las = 1)

# smoothScatter(da.na.10[, 6:7], nrpoints = 0, add = T, 
# colramp = colorRampPalette(c("gray100", "gray60")))

points(da.na$cl, da.na$la, col = adjustcolor(da.na$col, .01), 
       pch = 20, cex = .8)

abline(h = .25 * max(da.na$la) , v = .25 * max(da.na$cl), 
       col = "gray30", lty = 2)
abline(h = .75 * max(da.na$la) , v = .75 * max(da.na$cl), 
       col = "gray30", lty = 2)


# map
da.na.map <- da.na[, c(1, 2, 8, 9)]
da.na.map

gridded(da.na.map) <- ~long + lat
da.na.map

ra <- stack(da.na.map)
ra

plot(ra[[2]])


col <- unique(da.na.map$col)
col

plot(ra[[2]])

writeRaster(ra[[2]], "ecoland_butterflies.tif", format = "GTiff")

barplot(table(da.na[, 9]) / nrow(da.na), ylim = c(0, .35),
        col = c("cyan", "green", "yellow", "orange", "dark green", "red") )