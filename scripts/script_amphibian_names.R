library(rvest)
library(data.table)

sp <- NULL

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

strsplit

grep("Gynandropaa yunnanensis", no, value = T)







library(XML)
# url hosting multiple links to data sets
url <- "http://download.bls.gov/pub/time.series/ap/"
# identify the links available
links <- getHTMLLinks(url)











