### script amphibians synonymys frost ###

# Mauricio Humberto Vancine - mauricio.vancine@gmail.com

###---------------------------------------------------------------------###

# memory
rm(list = ls())
gc()
memory.limit(size = 1.75e13)

# packages 
if(!require("pacman")) install.packages("pacman")
pacman::p_load(rvest, data.table)

###---------------------------------------------------------------------###
###				               amphibians synonymys frost	              			###
###---------------------------------------------------------------------###


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
    na <- html_nodes(pg.sp, "h2") %>%
      html_text() %>%
      '['(3)
    
    na.c <- sub("\n            ", "", sub("\n                    ", "", na), na)
    na.c
   
    # synonymies
    sy <- html_node(pg.sp, "div.synonymy") %>%
      html_nodes("b") %>%
      html_text()
    sy}}
    

###---------------------------------------------------------------------###











