### script download occurrences - spocc ###

# Maurício Humberto Vancine - mauricio.vancine@gmail.com
# 25/07/2017

###---------------------------------------------------------------------------###

## memory
rm(list = ls())
gc()
memory.limit(size = 1.75e13) 

## packages
if(!require("pacman")) install.packages("pacman")
pacman::p_load(spocc, ggmap)

# verify
search()

###---------------------------------------------------------------------------###

## data from one base and one specie
# search
re <- occ(query = "Vitreorana uranoscopa", from = "gbif")
re

# data of specie
re$gbif$data$Vitreorana_uranoscopa

###---------------------------------------------------------------------------###

## download from multiple bases and multiple species
sp <- c("Vitreorana uranoscopa")

ba <- c("gbif", "ebird", "ecoengine", "bison", "antweb", "vertnet", "idigbio", "inat",
        "obis", "ala")

re.a <- occ(query = sp, from = ba)
re.a

###---------------------------------------------------------------------------###

## preparate data - seleting name, longitude, latitude and bases
# gbif
da.gbif <- na.omit(cbind(re.a$gbif$data$Vitreorana_uranoscopa$name, 
                         re.a$gbif$data$Vitreorana_uranoscopa$longitude,
                         re.a$gbif$data$Vitreorana_uranoscopa$latitude, 
                         re.a$gbif$data$Vitreorana_uranoscopa$prov))

# bison
da.bison <- na.omit(cbind(re.a$bison$data$Vitreorana_uranoscopa$name, 
                          re.a$bison$data$Vitreorana_uranoscopa$longitude,
                          re.a$bison$data$Vitreorana_uranoscopa$latitude,
                          re.a$bison$data$Vitreorana_uranoscopa$prov))

# inat
da.inat <- na.omit(cbind(re.a$inat$data$Vitreorana_uranoscopa$name, 
                         re.a$inat$data$Vitreorana_uranoscopa$longitude,
                         re.a$inat$data$Vitreorana_uranoscopa$latitude,
                         re.a$inat$data$Vitreorana_uranoscopa$prov))

# ebird
da.ebird <- na.omit(cbind(re.a$ebird$data$Vitreorana_uranoscopa$name, 
                          re.a$ebird$data$Vitreorana_uranoscopa$longitude,
                          re.a$ebird$data$Vitreorana_uranoscopa$latitude,
                          re.a$ebird$data$Vitreorana_uranoscopa$prov))

# ecoengine
da.ecoengine <- na.omit(cbind(re.a$ecoengine$data$Vitreorana_uranoscopa$name, 
                              re.a$ecoengine$data$Vitreorana_uranoscopa$longitude,
                              re.a$ecoengine$data$Vitreorana_uranoscopa$latitude,
                              re.a$ecoengine$data$Vitreorana_uranoscopa$prov))

# antweb
da.antweb <- na.omit(cbind(re.a$antweb$data$Vitreorana_uranoscopa$name, 
                           re.a$antweb$data$Vitreorana_uranoscopa$longitude,
                           re.a$antweb$data$Vitreorana_uranoscopa$latitude,
                           re.a$antweb$data$Vitreorana_uranoscopa$prov))

# vertnet
da.vertnet <- na.omit(cbind(re.a$vertnet$data$Vitreorana_uranoscopa$name, 
                         re.a$vertnet$data$Vitreorana_uranoscopa$longitude,
                         re.a$vertnet$data$Vitreorana_uranoscopa$latitude,
                         re.a$vertnet$data$Vitreorana_uranoscopa$prov))

# idigbio
da.idigbio <- na.omit(cbind(re.a$idigbio$data$Vitreorana_uranoscopa$name, 
                         re.a$idigbio$data$Vitreorana_uranoscopa$longitude,
                         re.a$idigbio$data$Vitreorana_uranoscopa$latitude, 
                         re.a$idigbio$data$Vitreorana_uranoscopa$prov))

# obis 
da.obis <- na.omit(cbind(re.a$obis$data$Vitreorana_uranoscopa$name, 
                         re.a$obis$data$Vitreorana_uranoscopa$longitude,
                         re.a$obis$data$Vitreorana_uranoscopa$latitude,
                         re.a$obis$data$Vitreorana_uranoscopa$prov))

# ala
da.ala <- na.omit(cbind(re.a$ala$data$Vitreorana_uranoscopa$name, 
                        re.a$ala$data$Vitreorana_uranoscopa$longitude,
                        re.a$ala$data$Vitreorana_uranoscopa$latitude,
                        re.a$ala$data$Vitreorana_uranoscopa$prov))


###---------------------------------------------------------------------------###

# data
da <- data.frame(rbind(da.gbif, da.bison, da.inat, da.ebird, da.ecoengine,  
                       da.antweb, da.vertnet, da.idigbio, da.obis, da.ala))

colnames(da) <- c("sp", "longitude", "latitude", "base")
da$longitude <- as.numeric(as.character(da$longitude))
da$latitude <- as.numeric(as.character(da$latitude))
str(da)

head(da)

setwd("E:/github/enmR/occurrence")

write.table(da, "occurrence_vitreorana_uranoscopa.txt", sep = "\t", quote = F, 
            row.names = F)

###---------------------------------------------------------------------------###

# map
# af <- get_map(location = c(-60, 0, -30, -40), zoom = 5)

af <- get_map(location = c(min(da$longitude), max(da$latitude), 
                           max(da$longitude), min(da$latitude)), zoom = 5)

ggmap(af, extent = "panel") +
  geom_point(data = da, aes(x = longitude, y = latitude), size = 3.3) + 
  geom_point(data = da, aes(x = longitude, y = latitude), size = 1.9, color = "red") +
  ggtitle(expression("Occurrences of" ~ italic("Vitreorana uranoscopa")))

ggsave("map_vitreorana_uranoscopa.tiff", dpi = 300)

###---------------------------------------------------------------------------###
