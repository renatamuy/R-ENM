### script raster PCA ### 

###-----------------------------------------------------------------------------------------###

# memory
rm(list = ls())
memory.limit(size = 1.75e13) 

# packages
if(!require(install.load)) install.packages("install.load")
install.load::install_load("raster", "rgdal", "RStoolbox", "data.table", 
                           "tidyverse", "usdm", "viridis", "GGally")

# check packages
search()

###-----------------------------------------------------------------------------------------###

## data

# directory
setwd("E:/github_mauriciovancine/R-ENM/scripts/03_enm/03_01_dismo/03_01_01_multiple_algorithms/wc10")

# variables
en <- stack(dir(patt = ".bil$"))
plot(en[[1]], col = viridis(100))

# data
da <- data.table(rasterToPoints(en))

# pca
da.pca <- prcomp(na.omit(da[, -c(1, 2)]), scale = TRUE)
da.pca

# pca summary
su <- summary(da.pca)

# pcs number
n.pca <- ncol(su$importance[, su$importance[1,] > 1])
n.pca 

fwrite(data.table(su$importance[, su$importance[1,] > 1]), "importance_pca.csv")

# screeplot
tiff("screeplot.tif", wi = 18, he = 18, un = "cm", res = 600, comp = "lzw+p")
screeplot(da.pca, main = "PCs", cex.lab = 1.3)
abline(h = 1, col = "red", lty = 2)
dev.off()

# contribution
fwrite(data.table(var = row.names(abs(round(da.pca$rotation[, 1:n.pca], 2))), 
                  abs(round(da.pca$rotation[, 1:n.pca], 2))), "contribution_pca.csv")

# correlation - vif
co <- vifcor(da[, -c(1, 2)], th = .7)
co

# selection
das <- da[, .SD, .SDcols = c("bio2", "bio8", "bio9", "bio14", "bio15", "bio18", "bio19")]
das

# plot
ggpairs(das[sample(1:nrow(da), 1000), ], 
        lower = list(continuous = wrap(ggally_points, pch = 21, color = "black",
                                       size = 1.2, alpha = .5)),
        diag = list(continuous = wrap(ggally_barDiag, color = "gray50")),
        upper = list(continuous = wrap(ggally_cor, color = "black", size = 5, 
                                       method = "spearman"))) +
  
  theme(text = element_text(colour = "black"),
        axis.text = element_text(size = 8, colour = "black"), 
        strip.text.x = element_text(size = 13),
        strip.text.y = element_text(size = 13), 
        panel.grid.major = element_line(colour = "white"))

ggsave("cor.tiff", wi = 25, he = 18, un = "cm", dpi = 300)

###-----------------------------------------------------------------------------------------###
