### script amphibians synonymys frost ###

# Maurício Humberto Vancine - mauricio.vancine@

###  amphibians synonymys frost	###

###---------------------------------------------------------------------###

# memory
rm(list = ls())
gc()
memory.limit(size = 1.75e13)

# packages 
if(!require("pacman")) install.packages("pacman")
pacman::p_load(rvest, data.table, stringr)

###---------------------------------------------------------------------###

# data
da <- data.table()

# html
for(i in seq(10, 8360, 10)){
 
 # http of search
 url <- paste0("http://research.amnh.org/vz/herpetology/amphibia/amphib/basic_search/(offset)/", i,
        "/(query)/*")
 
 # page
 pg <- read_html(url)
 
 # link
 li <- html_nodes(pg, "a") %>%
  html_attr("href")
 
 # species
 sp <- grep("-", grep("Amphibia", li, value = T), value = T)

 
 for(j in sp){
  
  # page of specie
  pg.sp <- read_html(paste0("http://research.amnh.org", j))
  
  # names
  na.p <- html_nodes(pg.sp, "title") %>%
   html_text()
  
  na.c <- sub("  Amphibian Species of the World", "", str_replace(na.p, "[|]", ""))
  
  na.s <- paste(str_split(na.c, boundary("word"))[[1]][1], 
                str_split(na.c, boundary("word"))[[1]][2])
  
  na.a <- tolower(sub(" ", "_", na.s))
  
  na <- data.frame(name = na.c, name_j = na.s, name_s = na.a)
  na
  
  # synonymies
  sy <- html_node(pg.sp, "div.synonymy") %>%
   html_nodes("d") %>%
   html_text()
  sy
  
  te <- cbind.data.frame(na, synonymes = sy)
  
  fwrite(te, paste0(na.a, ".txt"), sep = "\t")
 
  da <- rbind(da, te)}}


fwrite(da, "synonymes_amphibia_frost.txt", sep = "\t")

###---------------------------------------------------------------------###











