### script cleaning specieslink data ###

# Maurício Humberto Vancine - mauricio.vancine@
# 17/08/2017

###---------------------------------------------------------------------###

# memory
rm(list = ls())
gc()
memory.limit(size = 1.75e13)

# packages 
if(!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, dplyr, stringr, lettercase, stringi)

###---------------------------------------------------------------------###

### 1. data download in site
# http://www.splink.org.br/index > open search form > taxonomy > class = amphibia

###---------------------------------------------------------------------###

## data 
setwd("E:/mega/_dissertacao_v02/03_gaps/01_data/00_ocurrences/specieslink/01_data")

da <- fread("speciesLink_all_30301_a2017_m07_d29_h16_m20_s02.txt")
da

###---------------------------------------------------------------------###

## preparate data

# take species from column 'species' and put into column 'scientifcname'

### esta levando um milhão de anos... Preciso resolver isso....

for(i in 1:nrow(da)){
  
  print(i)
  
  if(da[i, ]$scientificname == ""){
    
    gen <- str_cap_words(str_split(da[i, ]$species, " ")[[1]][1])

    spe <- str_split(da[i, ]$species, " ")[[1]][2]
    
    da[i, ]$scientificname <- paste(gen, spe)
    }
}

table(da$scientificname)

# select species with two words (genus and epithet)
da.sp <- da[str_count(da$scientificname, "\\w+") == 2, ]

table(da.sp$scientificname)

# remove incert
da.sp.in <- da.sp[!c(grep("sp$", da.sp$scientificname),
                    grep("sp.$", da.sp$scientificname),
                    grep("sp1$",da.sp$scientificname),
                    grep("sp1.$",da.sp$scientificname),
                    grep("sp2$",da.sp$scientificname),
                    grep("sp2.$",da.sp$scientificname),
                    grep("sp3$",da.sp$scientificname),
                    grep("sp3.$",da.sp$scientificname),
                    grep("sp4$",da.sp$scientificname),
                    grep("sp4.$",da.sp$scientificname),
                    grep("gr.$", da.sp$scientificname),
                    grep("aff.$", da.sp$scientificname),
                    grep("cf.$", da.sp$scientificname),
                    grep("/", da.sp$scientificname),
                    grep("[?]", da.sp$scientificname),
                    grep("[(]", da.sp$scientificname)), ]

table(da.sp.in$scientificname)

# correct
da.sp.in.co <- da.sp.in

# "[Â]"
grep("[Â]", da.sp.in.co$scientificname, value = T)

da.sp.in.co$scientificname <- sub("[Â]", "", da.sp.in.co$scientificname)
grep("[Â]", da.sp.in.co$scientificname, value = T)

# "[*]"
grep("[*]", da.sp.in.co$scientificname, value = T)

da.sp.in.co$scientificname <- sub("[*]", "", da.sp.in.co$scientificname)
grep("[*]", da.sp.in.co$scientificname, value = T)

# "\""
grep("\"", da.sp.in.co$scientificname, value = T)

da.sp.in.co$scientificname <- sub("\"", "", da.sp.in.co$scientificname)
grep("\"", da.sp.in.co$scientificname, value = T)

# "[\\]"
grep("[\\]", da.sp.in.co$scientificname, value = T)

da.sp.in.co$scientificname <- sub("[\\]", "", da.sp.in.co$scientificname)
grep("[\\]", da.sp.in.co$scientificname, value = T)

ta <- data.table(table(da.sp.in.co$scientificname))

fwrite(ta, "ver.csv")

###---------------------------------------------------------------------###

## export species
sp <- unique(da.sp.in.co$scientificname)
sp

for(j in sp){
  occ.sp <-  da.sp.in.co[da.sp.in.co$scientificname %in% j]
  fwrite(occ.sp, paste0(sub(" ", "_", str_lower(j)),".csv"))
  }

###---------------------------------------------------------------------###


