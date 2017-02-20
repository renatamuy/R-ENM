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

# frequencias
ocorr <- data.frame(table(da$sp))
head(ocorr)
dim(ocorr)
colnames(ocorr) <- c("sp", "f")
head(ocorr)

hist(ocorr$f, col = "gray", xlim = c(0, 100), ylim = c(0, 150))

# ocorrencias acima de um valor
ocorr.10 <- ocorr[ocorr$f >= 10, ]
head(ocorr.10)
dim(ocorr.10)

hist(ocorr.10$f, col = "gray")
boxplot(ocorr.10$f, col = "gray")

###--------------------------------------------------------------------###

# juntar frequencias a ocorrencias
m <- merge(da, ocorr)
head(m)
dim(m)

# selecionar as especies
da.10 <- m[m$f >= 10,]
da.10

# conferir
dim(da.10)
length(unique(da.10$s
              
              
              
              p))

###--------------------------------------------------------------------###

# exportar as tabelas
write.table(da.10, ".xls", sep = "\t", quote = F, row.names = F)

write.table(da.10[, -4], ".txt", sep = "\t", quote = F, row.names = F)

###--------------------------------------------------------------------###


















