# selecting species by names

### script selecionar e exportar ocorrencias com certo numero de frequencia ###

# Maurício Humberto Vancine - mauricio.vancine@gmail.com

###--------------------------------------------------------------------###

# importar dados

# diretorio
setwd("")

# dados
da <- read.table(".txt", h = T, sep = "\t")
da

head(da)
dim(da)

###--------------------------------------------------------------------###

da <- data.frame(sp = rep(c("a", "b", "c"), 20), lat = rnorm(20), long = rnorm(20))
head(da)
da

subset(da, sp %in% c("a", "b", "c"))

sp.c <- c("a", "b", "c")

da[da$sp %in% d, ]


