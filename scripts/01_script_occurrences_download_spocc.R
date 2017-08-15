### script download occurrences - spocc ###

# Mauricio Humberto Vancine - mauricio.vancine@gmail.com
# 25/07/2017

###---------------------------------------------------------------------------###

## memory
rm(list = ls())
gc()
memory.limit(size = 1.75e13) 

## packages
pacman::p_load(spocc, data.table, dplyr, ggmap)

# verify
search()

###---------------------------------------------------------------------------###

# import list of species
setwd("E:/github/enmR/occurrence")

sp <- fread("")

## download from multiple bases and multiple species
sp <- c("Vitreorana uranoscopa")

ba <- c("gbif", "ebird", "ecoengine", "bison", "antweb", "vertnet", 
        "idigbio", "inat", "obis", "ala")

re <- occ(query = sp, from = ba)
re

###---------------------------------------------------------------------------###

## preparate data - seleting name, longitude, latitude and bases

da <- data.table()

for(i in sp){
  
  for(j in 1:length(ba)){
    
    for(k in 1:length(sp)){
      
      if(dim(re.a[[j]][[2]][[k]]) == 0){print(paste0("Without data for ", ba[j]))
      
      } else{
        
        da.b <- data.table(re.a[[i]][[2]][[j]]$name,
                           re.a[[i]][[2]][[j]]$longitude,
                           re.a[[i]][[2]][[j]]$latitude,
                           re.a[[i]][[2]][[j]]$prov)
        
        da <- rbind(da, da.b, fill = T)
        
        colnames(da) <- c("sp", "longitude", "latitude", "base")
        da$longitude <- as.numeric(as.character(da$longitude))
        da$latitude <- as.numeric(as.character(da$latitude))
        
        fwrite(da, paste0("occurrence_", vitreorana_uranoscopa, ".txt"), sep = "\t", quote = F, 
               row.names = F)
        





# map
# af <- get_map(location = c(-60, 0, -30, -40), zoom = 5)

af <- get_map(location = c(min(da$longitude), max(da$latitude), 
                           max(da$longitude), min(da$latitude)), zoom = 5)

ggmap(af, extent = "panel") +
  geom_point(data = da, aes(x = longitude, y = latitude, fill = base), 
             shape = 21, size = 3) +
  theme(legend.position = c(0.9, 0.2),
        legend.background = element_rect(color = "black", fill = "grey90", 
                                         size = 1, linetype = "solid")) +
  scale_fill_discrete(name = "Bases") + 
  ggtitle(expression("Occurrences of" ~ italic("Vitreorana uranoscopa")))

ggsave("map_vitreorana_uranoscopa.tiff", dpi = 300)

###---------------------------------------------------------------------------###
