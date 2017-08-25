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
pacman::p_load(rvest, data.table, stringr, plyr)

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
  
  colnames(ta) <- ta[1, ]
  ta <- ta[2, ]
  ta
  
  # synonymies
  sy.na <- html_node(pg.sp, "div.synonymy") %>%
    html_nodes("b") %>%
    html_text()
  sy.na
  
  sy.re <- html_node(pg.sp, "div.synonymy") %>%
    html_nodes("p") %>%
    html_nodes("a") %>%
    html_text()
  sy.re
  
  eng.na <- html_nodes(pg.sp, "h3") %>%
    html_node("p") %>%
    html_text()
  eng.na
  
  inf <- in.pg[1]
  
  dis <- re.in[2]
  
  
  }
 

  
  # information
  in.pg <- html_nodes(pg.sp, "p") %>%
    html_text()
  in.pg
  
  inf <- re.in[1]
  
  dis <- re.in[2]
  
  re.li <- html_nodes(pg.sp, "a") %>%
    html_attr("href")
  re.li <- grep("node", re.li, value = T)
  
  re.pg <- read_html(paste0("http://research.amnh.org", re.li))
  
  re <- html_nodes(re.sp, "div#aswContent") %>%
    html_text()
  
  re <- sub(" \n", "", sub("\n    Bibliography\n    ", "", re))
  

  }}


fwrite(da, "synonymes_amphibia_frost.csv", na = "NA")




###---------------------------------------------------------------------###


na.pg <- html_nodes(pg.sp, "title") %>%
  html_text()

na.de <- sub("  Amphibian Species of the World", "", str_replace(na.pg, "[|]", ""))

na <- paste(str_split(na.de, boundary("word"))[[1]][1], 
            str_split(na.de, boundary("word"))[[1]][2])

na.ab <- tolower(sub(" ", "_", na))

na <- data.frame(name = na, name_description = na.de, name_abbreviate = na.ab)

## synonymies
sy <- html_node(pg.sp, "div.synonymy") %>%
  html_nodes("d") %>%
  html_text()
sy

te <- cbind.data.frame(na, synonymes = sy)

fwrite(te, paste0(na.a, ".txt"), sep = "\t")

da <- rbind(da, te)

da <- rbind(da, data.table(cbind(class = "Amphibia", order = or[i], 
                                 superfamily = sup, family = NA, 
                                 subfamily = NA, genus = sup.ge[j], 
                                 species = sp, link = li.sp)))





