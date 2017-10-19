### script join functional data amphibians atlantic forest ###

# Maurício Humberto Vancine - mauricio.vancine@gmail

###---------------------------------------------------------------------###

# memory
rm(list = ls())
gc()
memory.limit(size = 1.75e13)

# packages 
if(!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, stringr)

###---------------------------------------------------------------------###

## directory
setwd("E:/mega/_diversidade_funcional_anfibios_ma/bases/01_raw")

###---------------------------------------------------------------------###

## data

# species list from haddad et al. 2013
sp.ha <- fread("amphibians_haddad_etal_2013.txt")
sp.ha <- sp.ha[order(sp)]
sp.ha$valid_names <- "a"
sp.ha

# species list from vancine et al.
sp.va <- fread("amphibians_vancine_etal.txt")
sp.va <- sp.va[order(sp)]
sp.va$valid_names <- "a"
sp.va

# species list from segalla et al. 2016
sp.se <- fread("amphibians_segalla_etal_2016.txt")
sp.se <- sp.se[order(sp)]
sp.se$valid_names <- "a"
sp.se

# frost
sp.fr <- fread("synonymes_amphibia_frost.csv")
sp.fr <- sp.fr[, .(synonymies, valid_name)]
sp.fr <- sp.fr[order(synonymies)]
sp.fr

###---------------------------------------------------------------------###

## valid names
# species list from haddad et al. 2013
sp.ha.nv <- setDT(sp.fr, key = "synonymies")[.(sp.ha$sp)]

fwrite(sp.ha.nv, "amphibians_valid_name_haddad_etal_2013.csv")


# species list from vancine et al.
sp.va.nv <- setDT(sp.fr, key = "synonymies")[.(sp.va$sp)]

fwrite(sp.va.nv, "amphibians_valid_name_vancine_etal_2018.csv")


# species list from segalla et al. 2016
# for(i in 1:nrow(sp.se)){
#   sp.se[i, 3] <- unique(sp.fr[synonymies == sp.se[i, ]$sp, valid_name])
# }
# 
# sp.se[i, ]

sp.se.nv <- setDT(sp.fr, key = "synonymies")[.(sp.se$sp)]
sp.se.nv
 
fwrite(sp.se.nv, "amphibians_valid_name_segalla_etal_2016.csv")


###---------------------------------------------------------------------###

## synonymies
# amphibians atlantic forest
sp.af <- fread("amphibians_af.txt")
sp.af

sp.af.sy <- setDT(sp.fr, key = "valid_name")[.(sp.af$valid_names)]
sp.af.sy

fwrite(sp.af.sy, "amphibians_af_syn.csv")

###---------------------------------------------------------------------###

