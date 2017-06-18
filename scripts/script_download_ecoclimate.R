### script download of data bases for enm ###

# Mauricio Humberto Vancine - mauricio.vancine@gmail.com
# 17/06/2017

###-----------------------------------------------------------------------------###
###                               ecoclimate                                    ###
###-----------------------------------------------------------------------------###

# memory
rm(list = ls())
gc()
memory.limit(size = 1.75e13)

# packages
if(!require("pacman")) install.packages("pacman")
pacman::p_load(downloader, rvest, RSelenium, XML)

# check packages
search()


###-----------------------------------------------------------------------------###

# directory
setwd("D:/environmental_data")
dir.create("ecoclimate")
setwd("ecoclimate")
getwd()

# url
en <- "http://ecoclimate.org/downloads/"
en

pg <- read_html(en)
pg

li <- html_nodes(pg, "span") %>%
  html_nodes("a") %>%
  html_attr("href")
li

pg.d <- read_html(li[1])
pg.d

li.d <- html_nodes(pg.d, "ol") %>%
  html_children() %>%
  html_node("a") %>%
  html_attr("href")
li.d

pg.d.a <- read_html(li.d[1])
pg.d.a

li.d.a <- html_nodes(pg.d.a, "a") %>%
  html_attr("class")
li.d.a


require(rvest)
text <- '<a role="menuitem" class="bubble-menu-item" href="#" aria-disabled="false">Download direto</a>'

h <- read_html(text)

h %>% 
  html_nodes(xpath = '//*[@id="a"]') 
%>%
  xml_attr("value")



# download
for(i in li[1]){
  pg.d <- read_html(i)
  li.d <- grep("/sh/", html_attr(html_nodes(pg.d, "a"), "href"), value = T)
  
  li.d[1]
  
  read_html(li.d[1])
  
  <a role="menuitem" class="bubble-menu-item" href="#" aria-disabled="false">Download direto</a>

  for(j in li.d){
    pg.d.a <- read_html(j)
    li.d.a <- html_attr(html_nodes(pg.d.a, "a"), "href")
    
###-----------------------------------------------------------------------------###
  