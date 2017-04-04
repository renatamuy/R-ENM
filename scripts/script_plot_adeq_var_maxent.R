### script - ###

# mauricio humberto vancine
# 14/12/2016

library(extrafont)

###-----------------------------------------------------------------------------------------###

# diretorio dos plots da saida do maxent
setwd("")

# carregar os dados
bio6 <- read.table("M.tridactyla_bio_6_asc.dat", h = T, sep = ",", dec = ".")
head(bio6)

# grafico
tiff("_bio6.tif", wi = 18, he = 18, un = "cm", res = 300, comp = "lzw", family = "Times New Roman")
par(mar = c(5, 5, 2, 2))
plot(y ~ x, 
	data = bio6,	 					
	type = "l",	
	xlim = c(min(bio6$x) + 10, max(bio6$x) - 10),						
	lwd = 4,							
	ylim = c(0, 1), 	  					
      ylab = "Suitability",  				
	xlab = "Min Temperature of Coldest Month - 30%", 								
	col.axis = "grey30",					
	cex.lab = 1.7,												
	cex.axis = 1.3, 
	las = 1,
	bty = "l")					
dev.off()


