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

### frost
## species number
# http of search
url.n.sp <- "http://research.amnh.org/vz/herpetology/amphibia/index.php//Amphibia"

# page
pg.n.sp <- read_html(url.n.sp)

# link
n.sp <- html_nodes(pg.nu, "h3")[4] %>%
  html_text() %>%
  str_extract(patt = "[0-9]+")
n.sp

###---------------------------------------------------------------------###

## data
# http of search
url <- "http://research.amnh.org/vz/herpetology/amphibia/index.php//Amphibia"

# page
pg <- read_html(url)

# link
fr <- html_nodes(pg, "a") %>%
  html_attr("href")

# order
li.or <- grep("/Amphibia/", fr, value = T)
li.or

or <- as.character(last(do.call(rbind.data.frame, 
                                str_split(li.or, boundary("word")))))
or

## data
da <- data.table()
da

# for order
for(i in 1:length(li.or)){
  
  # http of search
  url.or <- paste0("http://research.amnh.org", li.or[i])
  
  # page
  pg <- read_html(url.or)
  
  # link
  li.in.or <- html_nodes(pg, "a") %>%
    html_attr("href")
  
  ### superfamily
  if(grep("dea$", li.in.or, value = T) != 0){
    
    li.sup <- grep("dea$", li.in.or, value = T)
    sup <- as.character(last(str_split(li.sup, boundary("word"))[[1]]))
    
    # http of search
    url.sup <- paste0("http://research.amnh.org", li.sup)
    
    # page
    pg.in.sup <- read_html(url.sup)
    
    # link
    li.in.sup <- html_nodes(pg.in.sup, "a") %>%
      html_attr("href")
    
    ### familia
    li.sup.fa <- grep("dae$", li.in.sup, value = T) 
    sup.fa <- as.character(last(do.call(rbind.data.frame, 
                                        str_split(li.sup.fa, boundary("word")))))
  
    ### genus
    li.sup.ge <- grep("us$", li.in.sup, value = T) 
    sup.ge <- as.character(last(do.call(rbind.data.frame, 
                                        str_split(li.sup.ge, boundary("word")))))
    
    ### species
    for(j in 1:length(li.sup.ge)){
      
      # http of search
      url.sup.sp <- paste0("http://research.amnh.org", li.sup.ge[j])
    
      # page
      pg.in.sup.sp <- read_html(url.sup.sp)
    
      # link
      li.in.sup.sp <- html_nodes(pg.in.sup.sp, "a") %>%
        html_attr("href")
      
      li.sp <- grep("-", grep("Amphibia", li.in.sup.sp, value = T), value = T)
      sp <- sub("-", " ", last(str_split(li.sp, "[/]")[[1]]))
      
      da <- rbind(da, data.table(cbind(class = "Amphibia", order = or[i], 
                                       superfamily = sup, family = NA, 
                                       subfamily = NA, genus = sup.ge[j], 
                                       species = sp, link = li.sp)))
      print(da[j, ]$species)
    
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
    
    
  
  
  