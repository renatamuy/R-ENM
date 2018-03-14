rm(list=ls())

library('raster')
library('sdmvspecies')
library('dismo')
library('pbdb')

library('sqldf')
library('maps')
library('tcltk')

rm(list=ls())
setwd('/Users/matheusribeiro/aaa_Matheus_Ribeiro-2014-07-17/Producao Cientifica/em andamento/Thadeu_amazonia/pontos_AM_MA')

env.thrs <- read.table('MAsul.txt', h=T)
colnames(env.thrs) <- c('temp','prec')
id <- which(env.thrs[,'temp'] > 20)
env.thrs <- env.thrs[id,]

coords.thrs <- read.table('coordmasul.txt', h=T)
coords.thrs <- coords.thrs[id,]
						
r=0.5
res <- list((range(env.thrs$temp)[2] - range(env.thrs$temp)[1])/r, (range(env.thrs$prec)[2] - range(env.thrs$prec)[1])/r)
filtered.points <- envSample(coords.thrs, filters= list(env.thrs$temp, env.thrs$prec), res = res, do.plot=F)
							
while(nrow(filtered.points) < 50){
	r<- r+0.5
	res <- list((range(env.thrs$temp)[2] - range(env.thrs$temp)[1])/r, (range(env.thrs$prec)[2] - range(env.thrs$prec)[1])/r)
	filtered.points <- envSample(coords.thrs, filters= list(env.thrs$temp, env.thrs$prec), res = res, do.plot=F)
	
	}#end while(nrow(filtered.points) < 50)
	
filtered.points <- envSample(coords.thrs, filters= list(env.thrs$temp, env.thrs$prec), res = res, do.plot=T)	
						
if(nrow(filtered.points) > 50){
	id.filter <- sample(1:nrow(filtered.points), 50)
	filtered.points <- filtered.points[id.filter,]

	}#end if(nrow(filtered.points) > 50)

write.table(filtered.points, 'filtered_points_AFsul.txt', row.names=F)





