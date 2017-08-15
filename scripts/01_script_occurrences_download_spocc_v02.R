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

## import list of species
setwd("E:/github/enmR/occurrence")

sp <- fread("")
sp

sp <- c("Haddadus binotatus", "Macrogenioglottus alipioi", "Myersiella microps",
        "Proceratophyrus boiei", "Vitreorana eurygnatha", "Vitreorana uranoscopa")

###---------------------------------------------------------------------------###

## bases
ba <- c("gbif", "ebird", "ecoengine", "bison", "antweb", "vertnet", 
        "idigbio", "inat", "obis", "ala")

###---------------------------------------------------------------------------###

## download data

# empty data
da <- data.table()


# for
for(i in sp){
  
  re <- occ(query = sp, from = ba)
  
  for(j in 1:length(ba)){
    
      if(dim(re[[j]][[2]][[1]]) == 0){print(paste0("Without data for ", ba[j]))
      
      } else{
        
        da.b <- data.table(re[[j]][[2]][[1]]$name,
                           re[[j]][[2]][[1]]$longitude,
                           re[[j]][[2]][[1]]$latitude,
                           re[[j]][[2]][[1]]$prov)
        
        da <- rbind(da, da.b, fill = T)}}
        
        colnames(da) <- c("sp", "longitude", "latitude", "base")
        da$longitude <- as.numeric(as.character(da$longitude))
        da$latitude <- as.numeric(as.character(da$latitude))
        
        fwrite(da, paste0("occurrence_", sub(" ", "_", i), ".txt"), 
               sep = "\t", quote = F, row.names = F)
        
        # map
        af <- get_map(location = c(-60, 0, -30, -40), zoom = 5)
        
        ggmap(af, extent = "panel") +
          geom_point(data = da, aes(x = longitude, y = latitude, fill = base), 
                     shape = 21, size = 3) +
          theme(legend.position = c(0.9, 0.2),
                legend.background = element_rect(color = "black", fill = "grey90", 
                                                 size = 1, linetype = "solid")) +
          scale_fill_discrete(name = "Bases") + 
          ggtitle(paste0("Occurrences of ", i))
        
        ggsave(paste0("map_", i, ".tiff"), dpi = 600)}

###---------------------------------------------------------------------------###
