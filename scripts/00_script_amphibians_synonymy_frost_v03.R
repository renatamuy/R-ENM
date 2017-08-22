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
da <- data.table(class = "Amphibia", order = "", superfamily = "", family = "",
                 subfamily = "", genus = "", species = "", link = "", synonymy = "", 
                 publication = "")

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
or

# for order
for(i in or){
  
  # http of search
  url.or <- paste0("http://research.amnh.org", or[1])
  
  # page
  pg <- read_html(url.or)
  
  # link
  li.in.or <- html_nodes(pg, "a") %>%
    html_attr("href")
  
  ### superfamily
  if(grep("dea$", li.in.or, value = T) != 0){
    
    li.sup <- grep("dea$", li.in.or, value = T)
    sup <- last(str_split(li.sup, boundary("word"))[[1]])
    
    # http of search
    url.sup <- paste0("http://research.amnh.org", li.sup)
    
    # page
    pg.in.sup <- read_html(url.sup)
    
    # link
    li.in.sup <- html_nodes(pg.in.sup, "a") %>%
      html_attr("href")
    
    ### familia
    li.sup.fa <- grep("dae$", li.in.sup, value = T) 
    sup.fa <- last(do.call(rbind.data.frame, str_split(li.sup.fa, boundary("word"))))
    
    ### genus
    li.sup.ge <- grep("us$", li.in.sup, value = T) 
    sup.ge <- last(do.call(rbind.data.frame, str_split(li.sup.ge, boundary("word"))))
    
    ### species
    for(j in li.sup.ge){
      
      # http of search
      url.sup.sp <- paste0("http://research.amnh.org", j)
    
      # page
      pg.in.sup.sp <- read_html(url.sup.sp)
    
      # link
      li.in.sup.sp <- html_nodes(pg.in.sup.sp, "a") %>%
        html_attr("href")
      
      li.sp <- grep("-", grep("Amphibia", li.in.sup.sp, value = T), value = T)
      
      
      # page of specie
      pg.sp <- read_html(paste0("http://research.amnh.org", li.sp))
      
      # names
      na.p <- html_nodes(pg.sp, "title") %>%
        html_text()
      
      na.c <- sub("  Amphibian Species of the World", "", str_replace(na.p, "[|]", ""))
      
      na.s <- paste(str_split(na.c, boundary("word"))[[1]][1], 
                    str_split(na.c, boundary("word"))[[1]][2])
      
      na.a <- tolower(sub(" ", "_", na.s))
      
      na <- data.table(specie = na.s, species_total = na.c, article = , 
                       link = li.sp, name = na.a)
      na
      
      # synonymies
      sy <- html_node(pg.sp, "div.synonymy") %>%
        html_nodes("d") %>%
        html_text()
      sy
      
      if(length(sy) == 0){
        te <- cbind.data.frame(na, synonymes = NA)
      
      } else{
        da <- data.table(na, synonymes = sy)
      }
    }
      
    
  } else{
    
    ### family
    li.fa <- grep("dae$", li.in.or, value = T)
    fa <- last(do.call(rbind.data.frame, str_split(li.fa, boundary("word"))))
    
    li.fam <- sort(c(li.fa, li.sup.fa))
    fam <- sort(c(as.character(fa), as.character(sup.fa)))
    
    
    for(k in li.fam){
      
    # http of search
    url.sup <- paste0("http://research.amnh.org", k)
    
    # page
    pg.in.sup <- read_html(url.sup)
    
    # link
    li.in.sup <- html_nodes(pg.in.sup, "a") %>%
      html_attr("href")
    
    ### familia
    li.sup.fa <- grep("dae$", li.in.sup, value = T) 
    sup.fa <- last(do.call(rbind.data.frame, str_split(li.sup.fa, boundary("word"))))
    
    
  }

}
    
    
  
  
  