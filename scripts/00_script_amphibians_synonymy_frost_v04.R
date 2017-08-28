
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