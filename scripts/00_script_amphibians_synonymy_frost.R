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
pacman::p_load(rvest, data.table, stringr, plyr, dplyr)

###---------------------------------------------------------------------###

### frost
## species number
# http of search
url.n.sp <- "http://research.amnh.org/vz/herpetology/amphibia/index.php//Amphibia"

# page
pg.n.sp <- read_html(url.n.sp)

# link
n.sp <- html_nodes(pg.n.sp, "h3")[4] %>%
  html_text() %>%
  str_extract(patt = "[0-9]+")
n.sp

###---------------------------------------------------------------------###

## search number
# http of search
url.n.se <- paste0("http://research.amnh.org/vz/herpetology/amphibia/index.php//amphib/basic_search/(query)/*")

# page
pg.n.se <- read_html(url.n.se)

# link
n.se <- html_nodes(pg.n.se, "a") %>%
  html_attr("href") %>%
  last() %>%
  str_extract(patt = "[0-9]+")
n.se

###---------------------------------------------------------------------###

## synonymies
# data
da <- data.table()

# for
for(i in seq(10, n.se, 10)){
  
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
 sp
 
 for(j in sp){
  
  # page of specie
  pg.sp <- read_html(paste0("http://research.amnh.org", j))
  
  # taxonomy
  ta <- html_node(pg.sp, "div#taxonomy_nav") %>%
    html_nodes("span") %>%
    html_text() %>%
    str_split("[:]") %>%
    ldply() %>%
    t() %>%
    trimws()
  
  ta.na <- str_to_lower(ta[1, ])
  ta.da <- ta[2, ]

  # synonymies
  sy.na <- html_node(pg.sp, "div.synonymy") %>%
    html_nodes("b") %>%
    html_text()
  
  sy.in <- html_node(pg.sp, "div.synonymy") %>%
    html_nodes("p") %>%
    html_text()
  
  if(length(sy.na) == 0){
    da.sy <- data.frame(synonymies = NA, reference = sy.in, 
                        link = paste0("http://research.amnh.org", j))
    
  } else{
    da.sy <- data.frame(synonymies = sy.na, reference = sy.in, 
                      link = paste0("http://research.amnh.org", j))
  }
  
  # data
  ta <- data.table(matrix(rep(ta.da, each = nrow(da.sy)), nrow(da.sy)))
  colnames(ta) <- ta.na
  
  da <- bind_rows(da, cbind(ta, da.sy))
  
  }
  }

# export
fwrite(da, "synonymes_amphibia_frost.csv", na = "NA")

###---------------------------------------------------------------------###
