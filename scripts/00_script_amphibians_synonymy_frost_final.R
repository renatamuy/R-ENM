### script amphibians synonymys frost ###

# Mauricio Humberto Vancine - mauricio.vancine@gmail.com

###  amphibians synonymys frost	###

###---------------------------------------------------------------------###

# memory
rm(list = ls())
gc()
memory.limit(size = 1.75e13)

# packages 
if(!require("pacman")) install.packages("pacman")
pacman::p_load(rvest, data.table, stringr, stringi, magrittr, plyr, dplyr)

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
    
    ## synonymies
    
    # names
    sy.na <- html_node(pg.sp, "div.synonymy") %>%
      html_nodes("p") %>%
      html_node("b") %>%
      html_text() %>%
      trimws()
    
    sy.na <- sy.na[str_count(sy.na, "\\S+") > 1]
    sy.na <- na.omit(sy.na)
    
    sy.na.bb <- html_node(pg.sp, "div.synonymy") %>%
      html_nodes(xpath = "p[b]") %>%
      html_nodes("b") %>%
      html_text()
    
    if(length(sy.na.bb) > 0){
      sy.na.bb <- sy.na.bb[str_count(sy.na.bb, "\\S+") == 1]
      sy.na.bb <- paste(sy.na.bb[1], sy.na.bb[2])
      sy.na <- c(sy.na, sy.na.bb)
    }
    
    
    
    # information
    if(length(sy.na) == 0){
      sy.in <- html_node(pg.sp, "div.synonymy") %>%
        html_nodes(xpath = "p") %>%
        html_text()
      
      sy.in <- sy.in[grep(" ", sy.in)]
      
      da.sy <- data.frame(synonymies = last(ta.da), valid_name = last(ta.da), reference = sy.in, 
                          link = paste0("http://research.amnh.org", j))
      
    } else{
      
      if(length(html_node(pg.sp, "div.synonymy") %>% html_nodes(xpath = "p") %>% html_nodes(xpath = "i[b]")) > 0){
        sy.in <- html_node(pg.sp, "div.synonymy") %>%
          html_nodes(xpath = "p") %>%
          html_text()
        
        sy.in <- sy.in[grep(" ", sy.in)]
        
        da.sy <- data.frame(synonymies = sy.na, valid_name = last(ta.da), reference = sy.in, 
                            link = paste0("http://research.amnh.org", j))
        
      } else{
        sy.in <- html_node(pg.sp, "div.synonymy") %>%
          html_nodes(xpath = "p[b]") %>%
          html_text()
        
        sy.in <- sy.in[grep(" ", sy.in)]
        
        if(length(sy.in) == 0){
          sy.in <- html_node(pg.sp, "div.synonymy") %>%
            html_nodes(xpath = "p") %>%
            html_text()
          
          sy.in <- sy.in[grep(" ", sy.in)]
          
          da.sy <- data.frame(synonymies = sy.na, valid_name = last(sy.na), reference = sy.in, 
                              link = paste0("http://research.amnh.org", j))
          
        } else{
          da.sy <- data.frame(synonymies = sy.na, valid_name = last(sy.na), reference = sy.in, 
                              link = paste0("http://research.amnh.org", j))
        }
      }
    }
    
    
    
    # data
    ta <- data.table(matrix(rep(ta.da, each = nrow(da.sy)), nrow(da.sy)))
    colnames(ta) <- ta.na
    
    da <- bind_rows(da, cbind(ta, da.sy))
    
    print(paste(length(unique(da$species)), "of", as.numeric(n.sp), "species"))
    
  }
}


# export
fwrite(da, "synonymes_amphibia_frost.csv", na = "NA")

###---------------------------------------------------------------------###
