### script download occurrences - data bases ###

# Mauricio Humberto Vancine - mauricio.vancine@gmail.com
# 25/07/2017

### rgbif ###

###---------------------------------------------------------------------------###

## memory
rm(list = ls())
gc()
memory.limit(size = 1.75e13) 

## packages
pacman::p_load(rgbif, ggmap, data.table)

# verify
search()

###---------------------------------------------------------------------------###

## download
# search
se <- as.data.table(occ_search(scientificName = "Vitreorana uranoscopa", return = "data", 
                               limit = occ_count(taxonKey = name_backbone(name = "Vitreorana uranoscopa")$speciesKey)))
se$decimal

str(se)

# export
fwrite(se, "data_gbif.txt", sep = "\t")

###---------------------------------------------------------------------------###

# map
af <- get_map(location = c(min(na.omit(se$decimalLongitude)), max(na.omit(se$decimalLatitude)), 
                           max(na.omit(se$decimalLongitude)), min(na.omit(se$decimalLatitude))), zoom = 5)

ggmap(af, extent = "panel") +
  geom_point(data = se, aes(x = decimalLongitude, y = decimalLatitude), size = 2.7, col = "red") +
  geom_point(data = se, aes(x = decimalLongitude, y = decimalLatitude), size = 3, pch = 1) +
  theme(legend.position = c(0.9, 0.2),
        legend.background = element_rect(color = "black", fill = "grey90", 
                                         size = 1, linetype = "solid")) +
  ggtitle(expression("Occurrences of" ~ italic("Vitreorana uranoscopa")))

ggsave("map_vitreorana_uranoscopa.tiff", dpi = 300)