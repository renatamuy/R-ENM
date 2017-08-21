### script download occurrences - spocc ###

# Mauricio Humberto Vancine - mauricio.vancine@gmail.com
# 25/07/2017

###---------------------------------------------------------------------------###

## memory
rm(list = ls())
gc()
memory.limit(size = 1.75e13) 

## packages
pacman::p_load(spocc, data.table, dplyr, stringr, ggmap)

# verify
search()

###---------------------------------------------------------------------------###

## import list of species
setwd("E:/github/enmR/occurrence")

# sp <- fread("")
# sp

sp <- c("Haddadus binotatus", "Macrogenioglottus alipioi", "Myersiella microps",
        "Proceratophrys boiei", "Vitreorana eurygnatha", "Vitreorana uranoscopa")

###---------------------------------------------------------------------------###

## bases
ba <- c("gbif", "ebird", "ecoengine", "bison", "antweb", "vertnet", 
        "idigbio", "inat", "obis", "ala")

###---------------------------------------------------------------------------###

## download data
# map
af <- get_map(location = c(-60, 0, -30, -40), zoom = 5)

# for
for(i in sp){
  
  re <- occ(query = i, from = ba, has_coords = T, limit = 5000)
  
  print(i)
  
  da <- data.table()
  
  for(j in 1:length(ba)){
    
    if(dim(re[[j]][[2]][[1]]) == 0){print(paste("Without data for", i, "in", ba[j]))
      
    } else{
      
      print(paste("Yep! There is data for", i, "in", ba[j]))
      
      da.b <- data.table(re[[j]][[2]][[1]]$name,
                         re[[j]][[2]][[1]]$longitude,
                         re[[j]][[2]][[1]]$latitude,
                         re[[j]][[2]][[1]]$prov)
      
      da <- rbind(da, da.b, fill = T)}}
  
  colnames(da) <- c("sp", "long", "lat", "base")
  da$long <- as.numeric(as.character(da$long))
  da$lat <- as.numeric(as.character(da$lat))
  
  da.na <- na.omit(da)
  da.na$sp <- str_to_lower(sub(" ", "_", i))
  
  fwrite(da.na, paste0("occurrence_spocc_", str_to_lower(sub(" ", "_", i)), ".txt"), 
         sep = "\t", quote = F, row.names = F)
  
  
  # map
  ggmap(af, extent = "panel") +
    geom_point(data = da, aes(x = long, y = lat, fill = base), 
               shape = 21, size = 3) +
    theme(legend.position = c(0.9, 0.2),
          legend.background = element_rect(color = "black", fill = "grey90", 
                                           size = 1, linetype = "solid")) +
    scale_fill_discrete(name = "Bases") + 
    ggtitle(paste0("Occurrences of ", i))
  
  ggsave(paste0("map_", str_to_lower(sub(" ", "_", i)), ".tiff"), dpi = 600)}

###---------------------------------------------------------------------------###
