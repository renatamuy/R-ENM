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
da <- data.table(class = "", order = "", superfamily = "", family = "", 
                 subfamily = "", genus = "", species = "", synonymies = "", 
                 valid_name = "", valid_full_name = "", reference = "", 
                 english_names = "", distribution = "", comment = "", link = "")

da


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
    
    # species name
    sp.na <- html_node(pg.sp, "title") %>%
      html_text() %>%
      str_split("[|]")
    
    sp.na <- str_trim(sp.na[[1]][1])
    
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
    
    sy.na.bb <- str_subset(sy.na.bb, "[a-zA-Z]")
    
    sy.na.bb.f <- str_count(sy.na.bb, "\\S+")
    
    if(any(duplicated(sy.na.bb[which(str_count(sy.na.bb, "\\S+") == 1)]))){
      sy.na <- sy.na
    }
    
    if(length(str_subset(sy.na.bb[str_count(sy.na.bb, "\\S+") == 1], "[A-Z]")) == 0){
      sy.na <- sy.na
      
    } else {
      
      if(length(sy.na.bb.f[sy.na.bb.f == 1]) > 1){
        sy.na.bb <- sy.na.bb[str_count(sy.na.bb, "\\S+") == 1]
        sy.na.bb <- paste(sy.na.bb[1], sy.na.bb[2])
        sy.na <- c(sy.na, sy.na.bb)
      }
    }

    
    # information
    if(length(sy.na) == 0){
      sy.in <- html_node(pg.sp, "div.synonymy") %>%
        html_nodes(xpath = "p") %>%
        html_text()
      
      sy.in <- sy.in[grep(" ", sy.in)]
      
      da.sy <- data.table(synonymies = last(ta.da), valid_name = last(ta.da), 
                          valid_full_name = sp.na, reference = sy.in)
      
    } else{
      
      if(length(html_node(pg.sp, "div.synonymy") %>% html_nodes(xpath = "p") %>% html_nodes(xpath = "i[b]")) > 0){
        sy.in <- html_node(pg.sp, "div.synonymy") %>%
          html_nodes(xpath = "p") %>%
          html_text()
        
        sy.in <- sy.in[grep(" ", sy.in)]
        
        da.sy <- data.table(synonymies = sy.na, valid_name = last(ta.da), 
                            valid_full_name = sp.na, reference = sy.in, 
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
          
          da.sy <- data.table(synonymies = sy.na, valid_name = last(ta.da), 
                              valid_full_name = sp.na, reference = sy.in)
          
        } else{
          da.sy <- data.table(synonymies = sy.na, valid_name = last(ta.da), 
                              valid_full_name = sp.na, reference = sy.in)
        }
      }
    }
    
    ## english names  
    en <- html_nodes(pg.sp, "#aswContent.column.big.taxon.rank-Species") %>%
      html_nodes(xpath = "p") %>%
      html_text()
    
    en <- en[-c(length(en) - 2, length(en) - 1, length(en))]
    
    en <- paste(en, collapse = "  -")
    
    ## distribution
    di <- html_nodes(pg.sp, "#aswContent.column.big.taxon.rank-Species") %>%
      html_nodes(xpath = "p") %>%
      html_text() %>%
      nth(-3)
    
    ## comment
    co <- html_nodes(pg.sp, "#aswContent.column.big.taxon.rank-Species") %>%
      html_nodes(xpath = "p") %>%
      html_text() %>%
      nth(-2)
    

    ### data
    ta <- data.table(matrix(rep(ta.da, each = nrow(da.sy)), nrow(da.sy)))
    colnames(ta) <- ta.na
    
    da.sy <- data.table(da.sy, english_names = rep(en, nrow(da.sy)), 
                        distribution = rep(di, nrow(da.sy)), 
                        comment = rep(co, nrow(da.sy)), 
                        link = rep(paste0("http://research.amnh.org", j), nrow(da.sy)))
    
    
    da <- bind_rows(da, cbind(ta, da.sy))

    print(paste(length(unique(da$species)), "of", as.numeric(n.sp), "species"))
    
  }
}

# export
fwrite(da[-1, ], "synonymes_amphibia_frost.csv", na = "NA")

###---------------------------------------------------------------------###
