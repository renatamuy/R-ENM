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
pacman::p_load(raster, rgdal, dismo, data.table, colorRamps)

# verify packages
search()

###---------------------------------------------------------------------------###

# limite
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

## classificacao em 4 quadrantes

# values
da <- data.table(id = 1:ncell(re), cl = re[], la = rl[], col = "NA")
da

# classificacao 
for(i in 1:nrow(da)){
  if(da[i, 2] <= .5 & da[i, 3] <= .5){
    da[i, 4] <- "blue"} 
  else if(da[i, 2] <= .5 & da[i, 3] > .5){  
    da[i, 4] <- "green"} 
  else if(da[i, 2] > .5 & da[i, 3] <= .5){  
    da[i, 4] <- "orange"} 
  else if(da[i, 2] > .5 & da[i, 3] > .5){  
    da[i, 4] <- "red"}}

da

###---------------------------------------------------------------------------###

## classificacao em 9 quadrantes

# values
da <- data.table(id = 1:ncell(re), cl = re[], la = rl[], col = "NA")
da

# classificacao 
for(i in 1:nrow(da)){
  if(da[i, 2] <= .5 & da[i, 3] <= .5){
    da[i, 4] <- "blue"} 
  else if(da[i, 2] <= .5 & da[i, 3] > .5){  
    da[i, 4] <- "green"} 
  else if(da[i, 2] > .5 & da[i, 3] <= .5){  
    da[i, 4] <- "orange"} 
  else if(da[i, 2] > .5 & da[i, 3] > .5){  
    da[i, 4] <- "red"}}
da

###---------------------------------------------------------------------------###

