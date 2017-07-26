### script occurrencences - spocc ###

# Maurício Humberto Vancine - mauricio.vancine@gmail.com
# 25/07/2017

###---------------------------------------------------------------------------###

# 1. clear memory and load packages 
# clear workspace and increase memory
rm(list = ls())
gc()
memory.limit(size = 1.75e13) 

# packages
if(!require("pacman")) install.packages("pacman")
pacman::p_load(spocc)

# verify packages
search()

###---------------------------------------------------------------------------###

## data from one source

# search
re <- occ(query = "Vitreorana uranoscopa", from = "gbif")
re

# occurrence data
re$gbif

# metadata
re$gbif$meta

# type
re$gbif$meta$type

# optional
re$gbif$meta$opts

# data
re$gbif$data

# data of specie
re$gbif$data$Vitreorana_uranoscopa

###---------------------------------------------------------------------------###

## data from multiple sources
re.m <- occ(query = "Vitreorana uranoscopa", from = c("gbif", "ecoengine"))
re.m

###---------------------------------------------------------------------------###

## data of multiples species from multiple sources
sp <- c("Vitreorana uranoscopa")

ba <- c("gbif", "ebird", "ecoengine", "bison", "antweb", "vertnet", "idigbio", "inat",
        "obis", "ala")

re.a <- occ(query = sp, from = ba)
re.a

da.gbif <- na.omit(cbind(re.a$gbif$data$Vitreorana_uranoscopa$name, 
                         re.a$gbif$data$Vitreorana_uranoscopa$longitude,
                         re.a$gbif$data$Vitreorana_uranoscopa$latitude))
da.gbif

da.inat <- na.omit(cbind(re.a$inat$data$Vitreorana_uranoscopa$name, 
                         re.a$inat$data$Vitreorana_uranoscopa$longitude,
                         re.a$inat$data$Vitreorana_uranoscopa$latitude))
da.inat

da.vert <- na.omit(cbind(re.a$vertnet$data$Vitreorana_uranoscopa$name, 
                         re.a$vertnet$data$Vitreorana_uranoscopa$longitude,
                         re.a$vertnet$data$Vitreorana_uranoscopa$latitude))
da.vert

da.idig <- na.omit(cbind(re.a$idigbio$data$Vitreorana_uranoscopa$name, 
                         re.a$idigbio$data$Vitreorana_uranoscopa$longitude,
                         re.a$idigbio$data$Vitreorana_uranoscopa$latitude))
da.idig

da <- data.frame(rbind(da.gbif, da.inat, da.vert, da.idig))
colnames(da) <- c("sp", "long", "lat")
da$long <- as.numeric(da$long)
da$lat <- as.numeric(da$lat)
str(da)

plot(da$long, da$lat, pch = 20)

# maps
map_leaflet(dat)







