### script amphibians synonymys frost ###

# Mauricio Humberto Vancine - mauricio.vancine@gmail.com
# Pablo - 

###---------------------------------------------------------------------###

# memory
rm(list = ls())
gc()
memory.limit(size = 1.75e13)

# packages 
if(!require("pacman")) install.packages("pacman")
pacman::p_load(rvest, data.table)

###---------------------------------------------------------------------###
###				amphibians synonymys frost				###
###---------------------------------------------------------------------###

# species 
sp <- NULL

# html
for(i in seq(10, 8340, 10)[1:2]){
  
  url <- paste0("http://research.amnh.org/vz/herpetology/amphibia/amphib/basic_search/(offset)/", i,
                "/(query)/*")

  li <- getHTMLLinks(url)
  
  sp <- c(sp, grep("-", grep("Amphibia", li, value = T), value = T))}

  
  for(j in sp){
    pg <- read_html(paste0("http://research.amnh.org", j))
      
    no <- pg %>%
      html_nodes("p") %>%
      html_text()}
    no

    class(no)

###---------------------------------------------------------------------###











