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
af <- get_map(location = c(-65, 0, -30, -40), zoom = 5)

# for
for(i in sp[1]){
  
  print(paste0("Species --", i, "--"))
  re <- occ(query = i, from = ba, has_coords = T, limit = 10000)
  
  if(length(occ2df(re)) == 0){
    print(paste0("Sorry, no data for synonymies --", j, "--"))
    
  } else{
    da <- data.table(occ2df(re)[1], 
                     sp_enm = str_to_lower(sub(" ", "_", i)), 
                     occ2df(re)[-1])
    
    colnames(da) <- c("sp", "sp_enm", "long", "lat", "base", "date", "key")
    
    da.d <- distinct(da, long, lat, .keep_all = T)
    
    da.d$long <- as.numeric(da.d$long)
    da.d$lat <- as.numeric(da.d$lat)
    
    fwrite(da.d, paste0("occurrence_spocc_", str_to_lower(sub(" ", "_", i)), ".csv"))
    
    # map
    ggmap(af, extent = "panel") +
      geom_point(data = da.d, aes(x = long, y = lat, fill = factor(base)), 
                 shape = 21, size = 3, alpha = .5) +
      theme(legend.position = c(0.9, 0.2),
            legend.background = element_rect(color = "black", fill = "grey90", 
                                             size = 1, linetype = "solid")) +
      scale_fill_discrete(name = "Bases") + 
      ggtitle(paste0("Occurrences of ", i))
    
    ggsave(paste0("map_", str_to_lower(sub(" ", "_", i)), ".tiff"), dpi = 300)
  }
}

###---------------------------------------------------------------------------###
