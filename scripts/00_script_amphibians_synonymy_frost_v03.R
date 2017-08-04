### script amphibians synonymys frost ###

# Maurício Humberto Vancine - mauricio.vancine@

###  amphibians synonymys frost	###

###---------------------------------------------------------------------###

# memory
rm(list = ls())
gc()
memory.limit(size = 1.75e13)

# packages 
if(!require(pacman)) install.packages("pacman")
pacman::p_load(rvest, data.table, stringr, dplyr)

###---------------------------------------------------------------------###

## data
da <- data.table()


## frost
# http of search
url <- "http://research.amnh.org/vz/herpetology/amphibia/index.php//Amphibia"

# page
pg <- read_html(url)

# link
fr <- html_nodes(pg, "a") %>%
  html_attr("href")

# order
or <- grep("/Amphibia/", fr, value = T)


# for order
for(i in or){
  
  # http of search
  url <- paste0("http://research.amnh.org", i)
  
  # page
  pg <- read_html(url)
  
  # link
  li <- html_nodes(pg, "a") %>%
    html_attr("href")
  
  
  # superfamily
  if(grep("dea$", li, value = T) != 0){
    
    sup <- grep("dea$", li, value = T)
    
    # http of search
    url <- paste0("http://research.amnh.org", sup)
    
    # page
    pg <- read_html(url)
    
    # link
    li <- html_nodes(pg, "a") %>%
      html_attr("href")
    
    # genus
    ge <- grep("us$", li, value = T)
    
    for(j in ge){
      
      # http of search
      url <- paste0("http://research.amnh.org", j)
      
      # page
      pg <- read_html(url)
      
      # link
      li <- html_nodes(pg, "a") %>%
        html_attr("href")
      
      # species
      sp <- grep(j, grep("-", li, value = T), value = T)
      
      # data
      te <- data.table(cbind(specie = last(last(strsplit(sp, "/"))), 
                             link = paste0("http://research.amnh.org/", sp)))
      da <- rbind(da, te)
          
      print(last(last(strsplit(sp, "/"))))}
    

    } else {
      
      # family
      fa <- grep("dae$", li, value = T)
      
      
      # for to family
      for(k in fa){
      
      # http of search
      url <- paste0("http://research.amnh.org", k)
      
      # page
      pg <- read_html(url)
      
      # link
      li <- html_nodes(pg, "a") %>%
        html_attr("href")
      
      
      # subfamily
      if(grep("inae$", li, value = T) != 0){
        
        sub <- grep("inae$", li, value = T)
        
        for(k in sub){
        
        # http of search
        url <- paste0("http://research.amnh.org", k)
        
        # page
        pg <- read_html(url)
        
        # link
        li <- html_nodes(pg, "a") %>%
          html_attr("href")
        
        # genus
        ge <- grep("us$", li, value = T)
        
        for(j in ge){
          
          # http of search
          url <- paste0("http://research.amnh.org", j)
          
          # page
          pg <- read_html(url)
          
          # link
          li <- html_nodes(pg, "a") %>%
            html_attr("href")
          
          # species
          sp <- grep(j, grep("-", li, value = T), value = T)
          
          # data
          te <- data.table(cbind(specie = last(last(strsplit(sp, "/"))), 
                                 link = paste0("http://research.amnh.org/", sp)))
          da <- rbind(da, te)
          
          print(last(last(strsplit(sp, "/"))))}
        
        
      } else {
      
      
      # genus
      ge <- grep(k, li, value = T)
      
      if(length(ge) == 0){
        te <- data.table(cbind(species = last(last(strsplit(url, "/"))), links = url))
        da <- rbind(da, te)
        print(last(last(strsplit(url, "/"))))
        
      } else{
        
        for(k in 1:length(sp)){
          te <- data.table(cbind(specie = last(last(strsplit(sp[[k]], "/"))), 
                                 link = paste0("http://research.amnh.org/", sp[[k]])))
          da <- rbind(da, te)
          
          print(last(last(strsplit(sp[[k]], "/"))))}}}}}
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # for order
  for(j in or){
    
    # http of search
    url <- paste0("http://research.amnh.org", j)
    
    # page
    pg <- read_html(url)
    
    # link
    li <- html_nodes(pg, "a") %>%
      html_attr("href")
    li
    
      # genus
      ge <- grep("/Anura/", li, value = T)}
    
    
    # for genus
    for(j in ge){
      
      # http of search
      url <- paste0("http://research.amnh.org", j)
      
      # page
      pg <- read_html(url)
      
      # link
      li <- html_nodes(pg, "a") %>%
        html_attr("href")
      
      # species
      sp <- grep("-", grep("/Anura/", li, value = T), value = T)
      
      if(length(sp) == 0){
        te <- data.table(cbind(species = last(last(strsplit(url, "/"))), links = url))
        da <- rbind(da, te)
        print(last(last(strsplit(url, "/"))))
        
      } else{
        
        for(k in 1:length(sp)){
          te <- data.table(cbind(specie = last(last(strsplit(sp[[k]], "/"))), 
                                 link = paste0("http://research.amnh.org/", sp[[k]])))
          da <- rbind(da, te)
          
          print(last(last(strsplit(sp[[k]], "/"))))}}}}
    
    













