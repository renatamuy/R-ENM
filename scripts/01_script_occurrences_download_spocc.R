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

## list of species
sp <- c("Haddadus binotatus", "Macrogenioglottus alipioi", "Myersiella microps",
        "Proceratophrys boiei", "Vitreorana eurygnatha", "Vitreorana uranoscopa")

###---------------------------------------------------------------------------###

## bases
ba <- c("gbif", "ebird", "ecoengine", "bison", "antweb", "vertnet", 
        "idigbio", "inat", "obis", "ala")

###---------------------------------------------------------------------------###

## download data
# for
for(i in sp){
  
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

      fwrite(da.d, paste0("occurrence_spocc_", str_to_lower(sub(" ", "_", i)), ".csv"))
      
    }
}

###---------------------------------------------------------------------------###
