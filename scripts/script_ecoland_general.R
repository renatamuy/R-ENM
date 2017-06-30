### script ecoland general ###

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

# limit
br <- getData("GADM", country = "BRA", level = 0)
br

# enm climate
re <- raster(res = 1, xmn = -75, xmx = -33, ymn = -35, ymx = 6)
re[] <- rbeta(ncell(re), 2, 2)
re.b <- mask(re, br)

plot(re.b, col = matlab.like2(100))
plot(br, add = T)

# enm landscaep
rl <- raster(res = 1, xmn = -75, xmx = -33, ymn = -35, ymx = 6)
rl[] <- rbeta(ncell(rl), 5, 2)
rl.b <- mask(rl, br)

plot(rl.b, col = magenta2green(100))
plot(br, add = T)

###---------------------------------------------------------------------------###

## classification in 4 

# values
da <- data.table(id = 1:ncell(re.b), cl = re.b[], la = rl.b[], col = "NA", val = 0)
da

summary(da$cl)

id.na <- as.matrix(da[which(is.na(da$cl)), 1])
str(id.na)
head(id.na)

da.s.na <- da[!id.na]
da.s.na
dim(da.s.na)
head(da.s.na)

da.c.na <- da[id.na]
da.c.na$val <- NA 
da.c.na
dim(da.c.na)

# classification
for(i in 1:nrow(da.s.na)){
  
  if(da.s.na[i, 2] <= .5 & da.s.na[i, 3] <= .5){
    da.s.na[i, 4] <- "blue"
    da.s.na[i, 5] <- 0} 
  else if(da.s.na[i, 2] <= .5 & da.s.na[i, 3] > .5){  
    da.s.na[i, 4] <- "green"
    da.s.na[i, 5] <- .25} 
  else if(da.s.na[i, 2] > .5 & da.s.na[i, 3] <= .5){  
    da.s.na[i, 4] <- "orange"
    da.s.na[i, 5] <- .75} 
  else if(da.s.na[i, 2] > .5 & da.s.na[i, 3] > .5){  
    da.s.na[i, 4] <- "red"
    da.s.na[i, 5] <- 1}}

da.s.na

da.eco <- rbind(da.s.na, da.c.na)
da.eco <- da.eco[order(id)]
da.eco


# plot
par(mar = c(5, 5, 2, 2))
plot(da.eco$cl, da.eco$la, type = "n",
     xlim = c(0, 1),
     ylim = c(0, 1),
     xlab = "Climate suitability", 
     ylab = "Landscape suitability",
     col.axis = 'grey30',					
     cex.lab = 1.6,												
     cex.axis = 1.2, 
     las = 1)

smoothScatter(da.eco[, 2:3], nrpoints = 0, 
              colramp = colorRampPalette(c("white", "gray50")), add = T)

points(da.eco$cl, da.eco$la, col = da.eco$col, pch = 20, cex = .8)

abline(h = .5 , v = .5, col = "gray30", lty = 2)


# map
reco <- re.b
reco

reco[] <- da.eco$val

length(da.eco$val)

plot(reco, col = c("blue", "green", "orange", "red"))
plot(br, add = T)


###---------------------------------------------------------------------------###

## classification in 9

# values
da <- data.table(id = 1:ncell(re.b), cl = re.b[], la = rl.b[], col = "NA", val = 0)
da

id.na <- as.matrix(da[which(is.na(da$cl)), 1])
str(id.na)
head(id.na)

da.s.na <- da[!id.na]
da.s.na
dim(da.s.na)
head(da.s.na)

da.c.na <- da[id.na]
da.c.na$val <- NA 
da.c.na
dim(da.c.na)

# classification
for(i in 1:nrow(da.s.na)){
  
  if(da.s.na[i, 2] <= .25 & 
     da.s.na[i, 3] <= .25){
    da.s.na[i, 4] <- "blue"
    da.s.na[i, 5] <- 0} 
  
  else if(da.s.na[i, 2] <= .25 & 
          da.s.na[i, 3] > .25 & da.s.na[i, 3] <= .75){  
    da.s.na[i, 4] <- "cyan"
    da.s.na[i, 5] <- .125} 
  
  else if(da.s.na[i, 2] > .25 & da.s.na[i, 2] <= .75 & 
          da.s.na[i, 3] <= .25){  
    da.s.na[i, 4] <- "cyan4"
    da.s.na[i, 5] <- .25} 
  
  else if(da.s.na[i, 2] > .25 & da.s.na[i, 2] <= .75 & 
          da.s.na[i, 3] > .25 & da.s.na[i, 3] <= .75){  
    da.s.na[i, 4] <- "green"
    da.s.na[i, 5] <- .375} 
  
  else if(da.s.na[i, 2] > .75 & 
          da.s.na[i, 3] <= .25){  
    da.s.na[i, 4] <- "chocolate"
    da.s.na[i, 5] <- .5} 
  
  else if(da.s.na[i, 2] <= .25 & 
          da.s.na[i, 3] > .75){  
    da.s.na[i, 4] <- "yellow"
    da.s.na[i, 5] <- .625} 
  
  else if(da.s.na[i, 2] > .25 & da.s.na[i, 2] <= .75 & 
          da.s.na[i, 3] > .75){  
    da.s.na[i, 4] <- "orange"
    da.s.na[i, 5] <- .75}
  
  else if(da.s.na[i, 2] > .75 & 
          da.s.na[i, 3] > .25 & da.s.na[i, 3] <= .75){  
    da.s.na[i, 4] <- "dark green"
    da.s.na[i, 5] <- .875}
  
  else if(da.s.na[i, 2] > .75 & 
          da.s.na[i, 3] > .75){  
    da.s.na[i, 4] <- "red"
    da.s.na[i, 5] <- 1}}

da.s.na

da.eco <- rbind(da.s.na, da.c.na)
da.eco <- da.eco[order(id)]
da.eco

# plot
par(mar = c(5, 5, 2, 2))
plot(da.eco$cl, da.eco$la, type = "n",
     xlim = c(0, 1),
     ylim = c(0, 1),
     xlab = "Climate suitability", 
     ylab = "Landscape suitability",
     col.axis = 'grey30',					
     cex.lab = 1.6,												
     cex.axis = 1.2, 
     las = 1)

smoothScatter(da.eco[, 2:3], nrpoints = 0, add = T, 
              colramp = colorRampPalette(c("white", "gray50")))

points(da.eco$cl, da.eco$la, col = da.eco$col, pch = 20, cex = .8)

abline(h = .25 , v = .25, col = "gray30", lty = 2)
abline(h = .75 , v = .75, col = "gray30", lty = 2)


# map
reco <- re.b
reco

reco[] <- da.eco$val

length(da.eco$val)

plot(reco, col = c("blue", "cyan", "cyan4", "green", "chocolate", "yellow", "orange", 
                   "dark green", "red"))
plot(br, add = T)


###---------------------------------------------------------------------------###

