### script para colocar pontos em coordendas ###

# Maurício Humberto Vancine - mauricio.vancine@gmail.com
# Claudia Kanda - claudiakand@gmail.com 

###----------------------------------------------------------------------------------###
# para poucos
test <- as.character(c(-18987675, -18.96041, -1899584, -19034252, -18989))

for(i in 1:length(test)){
  test[i] <- ifelse(length(strsplit(test[i], "[.]")[[1]]) == 2, 
			   test[i], 
			   paste0(substr(test[i], 1, 3), ".", substr(test[i], 4, 9)))}

test

###----------------------------------------------------------------------------------###

# para muitos
test <- as.character(rep(c(-18987675, -18.96041, -1899584, -19034252, -18989), 1000))

for(i in 1:length(test)){
  test[i] <- ifelse(length(strsplit(test[i], "[.]")[[1]]) == 2, 
			   test[i], 
			   paste0(substr(test[i], 1, 3), ".", substr(test[i], 4, 9)))}

test

###----------------------------------------------------------------------------------###
