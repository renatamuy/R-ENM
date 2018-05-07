# memory
rm(list = ls())
memory.limit(size = 1.75e13) 

# instalar e carregar pacotes
if(!require(pacman)) install.packages("pacman")
pacman::p_load(BIEN, ape, maps, sp)

vignette("BIEN")

Xanthium_strumarium <- BIEN_occurrence_species(species = "Xanthium strumarium")
head(Xanthium_strumarium)
str(Xanthium_strumarium)

Xanthium_strumarium_full <- BIEN_occurrence_species(species = "Xanthium strumarium",
                                                    cultivated = T,
                                                    only.new.world = F,
                                                    all.taxonomy = T,
                                                    native.status = T,
                                                    observation.type = T,
                                                    political.boundaries = T)
str(Xanthium_strumarium_full)

Xanthium_strumarium_full$datasource

# Make a quick map to plot our points on
map('world',fill=T , col= "grey") 

#Plot the points from the full query in red
points(cbind(Xanthium_strumarium_full$longitude,Xanthium_strumarium_full$latitude),col="red",pch=20,cex=1) 

# Plot the points from the default query in blue
points(cbind(Xanthium_strumarium$longitude,Xanthium_strumarium$latitude),col="blue",pch=20,cex=1) 


sp <- BIEN_occurrence_species(species = "Peperomia gardneriana",
                               only.new.world = F,
                               all.taxonomy = T,
                               native.status = T,
                               observation.type = T,
                               political.boundaries = T)
head(sp)

colnames(sp)

sp$datasource

library(raster)

br <- getData("GADM", country = "BRA", level = 0)
plot(br, col = "gray", axes = T)
points(sp$longitude, sp$latitude, col = "red", pch = 20, cex = 1) 

