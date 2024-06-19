
##1-MODELAGEM

#Banco de dados
#speciesLink http://www.splink.org.br


#Variáveis climáticas - Worldclim
# http://worldclim.org/version2 na resolução de 2.5 minutes in .tif

library(PresenceAbsence)
library(raster)
library(rgdal)
library(rgeos)
require(rJava)
library(sp)
library(dismo)
library(maptools)
library(usdm)

#importar camadas e definir extensão desejada (Ámerica Latina)

worldclim <- getData('worldclim', var='bio', res=2.5) 
frame<- extent(-82.48333, -33.35,-57.211667,13.43333) #Ámerica Latina
as <- stack(crop(worldclim, frame)) #empilhando todas as camandas de mapas e recortando apenas a visualiza??o escolhida

writeRaster(as[[1]],"bio1.asc" ,fortmat = "asc", overwrite=T)
writeRaster(as[[2]],"bio2.asc" ,fortmat = "asc", overwrite=T)
writeRaster(as[[3]],"bio3.asc" ,fortmat = "asc", overwrite=T)
writeRaster(as[[4]],"bio4.asc" ,fortmat = "asc", overwrite=T)
writeRaster(as[[5]],"bio5.asc" ,fortmat = "asc", overwrite=T)
writeRaster(as[[6]],"bio6.asc" ,fortmat = "asc", overwrite=T)
writeRaster(as[[7]],"bio7.asc" ,fortmat = "asc", overwrite=T)
writeRaster(as[[8]],"bio8.asc" ,fortmat = "asc", overwrite=T)
writeRaster(as[[9]],"bio9.asc" ,fortmat = "asc", overwrite=T)
writeRaster(as[[10]],"bio10.asc" ,fortmat = "asc", overwrite=T)
writeRaster(as[[11]],"bio11.asc" ,fortmat = "asc", overwrite=T)
writeRaster(as[[12]],"bio12.asc" ,fortmat = "asc", overwrite=T)
writeRaster(as[[13]],"bio13.asc" ,fortmat = "asc", overwrite=T)
writeRaster(as[[14]],"bio14.asc" ,fortmat = "asc", overwrite=T)
writeRaster(as[[15]],"bio15.asc" ,fortmat = "asc", overwrite=T)
writeRaster(as[[16]],"bio16.asc" ,fortmat = "asc", overwrite=T)
writeRaster(as[[17]],"bio17.asc" ,fortmat = "asc", overwrite=T)
writeRaster(as[[18]],"bio18.asc" ,fortmat = "asc", overwrite=T)
writeRaster(as[[19]],"bio19.asc" ,fortmat = "asc", overwrite=T)

# Verificando a colinearidade entre preditores por Variance Inflation Factor (VIF) 

v1 <- vifstep(bios)
v1

####As modelagens foram realizadas diretamente do MaxEnt

rm(list=ls())

####2 - PERFIL DE OCUPAÇÃO
library(phyloclim)
library(sp)
library(maptools)
library(raster)
library(ape)
library(dismo)

## The phyloclim function 'pno' integrates the suitability surface over the climate map to generate a histogram of the relative probability of occupying sites of a given value, along a climate axis. These are equivalent to the plots from Evans et al. 2009 in the lecture. However, I found a missing na.rm in the pno function so it wasn't working on this data set. Load a fix:
source('myPno.R')

# runs for one variable at a time, specified as a path to the appropriate asc layer

#bio2
climvar <- paste(getwd(),'/hotlayers/bio2.asc',sep='')
maxent_models <- paste(getwd(),'/outputs/',sep='')
nbins <- 25 # number of histogram bins for climate variable

pnobio2 <- myPno(path_bioclim=climvar,path_model=maxent_models,bin_number=nbins)
plotPNO(pnobio2, legend.pos="topright")
plotPNO(pnobio2, legend.pos= NULL)
pnobio2
write.table(round(abs(pnobio2), 2), "pnobio2.xls", row.names = T, sep = "\t")

#bio3
climvar <- paste(getwd(),'/hotlayers/bio3.asc',sep='')
maxent_models <- paste(getwd(),'/outputs/',sep='')
nbins <- 25 # number of histogram bins for climate variable

pnobio3 <- myPno(path_bioclim=climvar,path_model=maxent_models,bin_number=nbins)
plotPNO(pnobio3, legend.pos= NULL)
write.table(round(abs(pnobio3), 2), "pnobio3.xls", row.names = T, sep = "\t")

#bio8
climvar <- paste(getwd(),'/hotlayers/bio8.asc',sep='')
maxent_models <- paste(getwd(),'/outputs/',sep='')
nbins <- 25 # number of histogram bins for climate variable

pnobio8 <- myPno(path_bioclim=climvar,path_model=maxent_models,bin_number=nbins)
plotPNO(pnobio8,legend.pos= NULL)
write.table(round(abs(pnobio8), 2), "pnobio8.xls", row.names = T, sep = "\t")

#bio9
climvar <- paste(getwd(),'/hotlayers/bio9.asc',sep='')
maxent_models <- paste(getwd(),'/outputs/',sep='')
nbins <- 25 # number of histogram bins for climate variable

pnobio9 <- myPno(path_bioclim=climvar,path_model=maxent_models,bin_number=nbins)
plotPNO(pnobio9, legend.pos= NULL)
write.table(round(abs(pnobio9), 2), "pnobio9.xls", row.names = T, sep = "\t")

#bio13
climvar <- paste(getwd(),'/hotlayers/bio13.asc',sep='')
maxent_models <- paste(getwd(),'/outputs/',sep='')
nbins <- 25 # number of histogram bins for climate variable

pnobio13 <- myPno(path_bioclim=climvar,path_model=maxent_models,bin_number=nbins)
plotPNO(pnobio13,legend.pos= NULL)
write.table(round(abs(pnobio13), 2), "pnobio13.xls", row.names = T, sep = "\t")

#bio14
climvar <- paste(getwd(),'/hotlayers/bio14.asc',sep='')
maxent_models <- paste(getwd(),'/outputs/',sep='')
nbins <- 25 # number of histogram bins for climate variable

pnobio14 <- myPno(path_bioclim=climvar,path_model=maxent_models,bin_number=nbins)
plotPNO(pnobio14,legend.pos= NULL)
write.table(round(abs(pnobio14), 2), "pnobio14.xls", row.names = T, sep = "\t")

#bio15
climvar <- paste(getwd(),'/hotlayers/bio15.asc',sep='')
maxent_models <- paste(getwd(),'/outputs/',sep='')
nbins <- 25 # number of histogram bins for climate variable

pnobio15 <- myPno(path_bioclim=climvar,path_model=maxent_models,bin_number=nbins)
plotPNO(pnobio15,legend.pos= NULL)
write.table(round(abs(pnobio15), 2), "pnobio15.xls", row.names = T, sep = "\t")

#bio18
climvar <- paste(getwd(),'/hotlayers/bio18.asc',sep='')
maxent_models <- paste(getwd(),'/outputs/',sep='')
nbins <- 25 # number of histogram bins for climate variable

pnobio18 <- myPno(path_bioclim=climvar,path_model=maxent_models,bin_number=nbins)
plotPNO(pnobio18,legend.pos= NULL)
write.table(round(abs(pnobio18), 2), "pnobio18.xls", row.names = T, sep = "\t")

#bio19
climvar <- paste(getwd(),'/hotlayers/bio19.asc',sep='')
maxent_models <- paste(getwd(),'/outputs/',sep='')
nbins <- 25 # number of histogram bins for climate variable

pnobio19 <- myPno(path_bioclim=climvar,path_model=maxent_models,bin_number=nbins)
plotPNO(pnobio19,legend.pos= NULL)
write.table(round(abs(pnobio19), 2), "pnobio19.xls", row.names = T, sep = "\t")

### PNO por Clados
#####Clado1
#bio2
climvar <- paste(getwd(),'/hotlayers/bio2.asc',sep='')
maxent_models <- paste(getwd(),'/clado1/',sep='')
nbins <- 25 # number of histogram bins for climate variable
pnobio2 <- myPno(path_bioclim=climvar,path_model=maxent_models,bin_number=nbins)
plotPNO(pnobio2, legend.pos="topright") ##legenda muito grande

#bio3
climvar <- paste(getwd(),'/hotlayers/bio3.asc',sep='')
maxent_models <- paste(getwd(),'/clado1/',sep='')
nbins <- 25 # number of histogram bins for climate variable
pnobio3 <- myPno(path_bioclim=climvar,path_model=maxent_models,bin_number=nbins)
plotPNO(pnobio3, legend.pos= NULL)

#bio8
climvar <- paste(getwd(),'/hotlayers/bio8.asc',sep='')
maxent_models <- paste(getwd(),'/clado1/',sep='')
nbins <- 25 # number of histogram bins for climate variable
pnobio8 <- myPno(path_bioclim=climvar,path_model=maxent_models,bin_number=nbins)
plotPNO(pnobio8, legend.pos= NULL)

#bio9
climvar <- paste(getwd(),'/hotlayers/bio9.asc',sep='')
maxent_models <- paste(getwd(),'/clado1/',sep='')
nbins <- 25 # number of histogram bins for climate variable
pnobio9 <- myPno(path_bioclim=climvar,path_model=maxent_models,bin_number=nbins)
plotPNO(pnobio9, legend.pos= NULL)

#bio13
climvar <- paste(getwd(),'/hotlayers/bio13.asc',sep='')
maxent_models <- paste(getwd(),'/clado1/',sep='')
nbins <- 25 # number of histogram bins for climate variable
pnobio13 <- myPno(path_bioclim=climvar,path_model=maxent_models,bin_number=nbins)
plotPNO(pnobio13,legend.pos= NULL)

#bio14
climvar <- paste(getwd(),'/hotlayers/bio14.asc',sep='')
maxent_models <- paste(getwd(),'/clado1/',sep='')
nbins <- 25 # number of histogram bins for climate variable
pnobio14 <- myPno(path_bioclim=climvar,path_model=maxent_models,bin_number=nbins)
plotPNO(pnobio14,legend.pos= NULL)

#bio15
climvar <- paste(getwd(),'/hotlayers/bio15.asc',sep='')
maxent_models <- paste(getwd(),'/clado1/',sep='')
nbins <- 25 # number of histogram bins for climate variable
pnobio15 <- myPno(path_bioclim=climvar,path_model=maxent_models,bin_number=nbins)
plotPNO(pnobio15,legend.pos= NULL)

#bio18
climvar <- paste(getwd(),'/hotlayers/bio18.asc',sep='')
maxent_models <- paste(getwd(),'/clado1/',sep='')
nbins <- 25 # number of histogram bins for climate variable
pnobio18 <- myPno(path_bioclim=climvar,path_model=maxent_models,bin_number=nbins)
plotPNO(pnobio18,legend.pos= NULL)

#bio19
climvar <- paste(getwd(),'/hotlayers/bio19.asc',sep='')
maxent_models <- paste(getwd(),'/clado1/',sep='')
nbins <- 25 # number of histogram bins for climate variable
pnobio19 <- myPno(path_bioclim=climvar,path_model=maxent_models,bin_number=nbins)
plotPNO(pnobio19,legend.pos= NULL)

#####Clado2
#bio2
climvar <- paste(getwd(),'/hotlayers/bio2.asc',sep='')
maxent_models <- paste(getwd(),'/clado2/',sep='')
nbins <- 25 # number of histogram bins for climate variable
c2pnobio2 <- myPno(path_bioclim=climvar,path_model=maxent_models,bin_number=nbins)
plotPNO(c2pnobio2, legend.pos="topright")

#bio3
climvar <- paste(getwd(),'/hotlayers/bio3.asc',sep='')
maxent_models <- paste(getwd(),'/clado2/',sep='')
nbins <- 25 # number of histogram bins for climate variable
c2pnobio3 <- myPno(path_bioclim=climvar,path_model=maxent_models,bin_number=nbins)
plotPNO(c2pnobio3, legend.pos="topright")

#bio8
climvar <- paste(getwd(),'/hotlayers/bio8.asc',sep='')
maxent_models <- paste(getwd(),'/clado2/',sep='')
nbins <- 25 # number of histogram bins for climate variable
c2pnobio8 <- myPno(path_bioclim=climvar,path_model=maxent_models,bin_number=nbins)
plotPNO(c2pnobio8, legend.pos="topleft")

#bio9
climvar <- paste(getwd(),'/hotlayers/bio9.asc',sep='')
maxent_models <- paste(getwd(),'/clado2/',sep='')
nbins <- 25 # number of histogram bins for climate variable
c2pnobio9 <- myPno(path_bioclim=climvar,path_model=maxent_models,bin_number=nbins)
plotPNO(c2pnobio9, legend.pos="topright")

#bio13
climvar <- paste(getwd(),'/hotlayers/bio13.asc',sep='')
maxent_models <- paste(getwd(),'/clado2/',sep='')
nbins <- 25 # number of histogram bins for climate variable
c2pnobio13 <- myPno(path_bioclim=climvar,path_model=maxent_models,bin_number=nbins)
plotPNO(c2pnobio13,legend.pos="topright")

#bio14
climvar <- paste(getwd(),'/hotlayers/bio14.asc',sep='')
maxent_models <- paste(getwd(),'/clado2/',sep='')
nbins <- 25 # number of histogram bins for climate variable
c2pnobio14 <- myPno(path_bioclim=climvar,path_model=maxent_models,bin_number=nbins)
plotPNO(c2pnobio14,legend.pos="topright")

#bio15
climvar <- paste(getwd(),'/hotlayers/bio15.asc',sep='')
maxent_models <- paste(getwd(),'/clado2/',sep='')
nbins <- 25 # number of histogram bins for climate variable
c2pnobio15 <- myPno(path_bioclim=climvar,path_model=maxent_models,bin_number=nbins)
plotPNO(c2pnobio15,legend.pos="topright")

#bio18
climvar <- paste(getwd(),'/hotlayers/bio18.asc',sep='')
maxent_models <- paste(getwd(),'/clado2/',sep='')
nbins <- 25 # number of histogram bins for climate variable
c2pnobio18 <- myPno(path_bioclim=climvar,path_model=maxent_models,bin_number=nbins)
plotPNO(c2pnobio18,legend.pos="topright")

#bio19
climvar <- paste(getwd(),'/hotlayers/bio19.asc',sep='')
maxent_models <- paste(getwd(),'/clado2/',sep='')
nbins <- 25 # number of histogram bins for climate variable
c2pnobio19 <- myPno(path_bioclim=climvar,path_model=maxent_models,bin_number=nbins)
plotPNO(c2pnobio19, legend.pos="topright")

#####Clado3
#bio2
climvar <- paste(getwd(),'/hotlayers/bio2.asc',sep='')
maxent_models <- paste(getwd(),'/clado3/',sep='')
nbins <- 25 # number of histogram bins for climate variable
c3pnobio2 <- myPno(path_bioclim=climvar,path_model=maxent_models,bin_number=nbins)
plotPNO(c3pnobio2, legend.pos="topright")

#bio3
climvar <- paste(getwd(),'/hotlayers/bio3.asc',sep='')
maxent_models <- paste(getwd(),'/clado3/',sep='')
nbins <- 25 # number of histogram bins for climate variable
c3pnobio3 <- myPno(path_bioclim=climvar,path_model=maxent_models,bin_number=nbins)
plotPNO(c3pnobio3, legend.pos="topright")

#bio8
climvar <- paste(getwd(),'/hotlayers/bio8.asc',sep='')
maxent_models <- paste(getwd(),'/clado3/',sep='')
nbins <- 25 # number of histogram bins for climate variable
c3pnobio8 <- myPno(path_bioclim=climvar,path_model=maxent_models,bin_number=nbins)
plotPNO(c3pnobio8, legend.pos="topleft")

#bio9
climvar <- paste(getwd(),'/hotlayers/bio9.asc',sep='')
maxent_models <- paste(getwd(),'/clado3/',sep='')
nbins <- 25 # number of histogram bins for climate variable
c3pnobio9 <- myPno(path_bioclim=climvar,path_model=maxent_models,bin_number=nbins)
plotPNO(c3pnobio9, legend.pos="topleft")

#bio13
climvar <- paste(getwd(),'/hotlayers/bio13.asc',sep='')
maxent_models <- paste(getwd(),'/clado3/',sep='')
nbins <- 25 # number of histogram bins for climate variable
c3pnobio13 <- myPno(path_bioclim=climvar,path_model=maxent_models,bin_number=nbins)
plotPNO(c3pnobio13,legend.pos="topright")

#bio14
climvar <- paste(getwd(),'/hotlayers/bio14.asc',sep='')
maxent_models <- paste(getwd(),'/clado3/',sep='')
nbins <- 25 # number of histogram bins for climate variable
c3pnobio14 <- myPno(path_bioclim=climvar,path_model=maxent_models,bin_number=nbins)
plotPNO(c3pnobio14,legend.pos="topright")

#bio15
climvar <- paste(getwd(),'/hotlayers/bio15.asc',sep='')
maxent_models <- paste(getwd(),'/clado3/',sep='')
nbins <- 25 # number of histogram bins for climate variable
c3pnobio15 <- myPno(path_bioclim=climvar,path_model=maxent_models,bin_number=nbins)
plotPNO(c3pnobio15,legend.pos="topright")

#bio18
climvar <- paste(getwd(),'/hotlayers/bio18.asc',sep='')
maxent_models <- paste(getwd(),'/clado3/',sep='')
nbins <- 25 # number of histogram bins for climate variable
c3pnobio18 <- myPno(path_bioclim=climvar,path_model=maxent_models,bin_number=nbins)
plotPNO(c3pnobio18,legend.pos="topright")

#bio19
climvar <- paste(getwd(),'/hotlayers/bio19.asc',sep='')
maxent_models <- paste(getwd(),'/clado3/',sep='')
nbins <- 25 # number of histogram bins for climate variable
c3pnobio19 <- myPno(path_bioclim=climvar,path_model=maxent_models,bin_number=nbins)
plotPNO(c3pnobio19, legend.pos="topright")

####Clado4
#Bio2
climvar <- paste(getwd(),'/hotlayers/bio2.asc',sep='')
maxent_models <- paste(getwd(),'/clado4/',sep='')
nbins <- 25 # number of histogram bins for climate variable
c4pnobio2 <- myPno(path_bioclim=climvar,path_model=maxent_models,bin_number=nbins)
plotPNO(c4pnobio2, legend.pos="topright")

#bio3
climvar <- paste(getwd(),'/hotlayers/bio3.asc',sep='')
maxent_models <- paste(getwd(),'/clado4/',sep='')
nbins <- 25 # number of histogram bins for climate variable
c4pnobio3 <- myPno(path_bioclim=climvar,path_model=maxent_models,bin_number=nbins)
plotPNO(c4pnobio3, legend.pos="topright")

#bio8
climvar <- paste(getwd(),'/hotlayers/bio8.asc',sep='')
maxent_models <- paste(getwd(),'/clado4/',sep='')
nbins <- 25 # number of histogram bins for climate variable
c4pnobio8 <- myPno(path_bioclim=climvar,path_model=maxent_models,bin_number=nbins)
plotPNO(c4pnobio8, legend.pos="topleft")

#bio9
climvar <- paste(getwd(),'/hotlayers/bio9.asc',sep='')
maxent_models <- paste(getwd(),'/clado4/',sep='')
nbins <- 25 # number of histogram bins for climate variable
c4pnobio9 <- myPno(path_bioclim=climvar,path_model=maxent_models,bin_number=nbins)
plotPNO(c4pnobio9, legend.pos="topleft")

#bio13
climvar <- paste(getwd(),'/hotlayers/bio13.asc',sep='')
maxent_models <- paste(getwd(),'/clado4/',sep='')
nbins <- 25 # number of histogram bins for climate variable
c4pnobio13 <- myPno(path_bioclim=climvar,path_model=maxent_models,bin_number=nbins)
plotPNO(c4pnobio13,legend.pos="topright")

#bio14
climvar <- paste(getwd(),'/hotlayers/bio14.asc',sep='')
maxent_models <- paste(getwd(),'/clado4/',sep='')
nbins <- 25 # number of histogram bins for climate variable
c4pnobio14 <- myPno(path_bioclim=climvar,path_model=maxent_models,bin_number=nbins)
plotPNO(c4pnobio14,legend.pos="topright")

#bio15
climvar <- paste(getwd(),'/hotlayers/bio15.asc',sep='')
maxent_models <- paste(getwd(),'/clado4/',sep='')
nbins <- 25 # number of histogram bins for climate variable
c4pnobio15 <- myPno(path_bioclim=climvar,path_model=maxent_models,bin_number=nbins)
plotPNO(c4pnobio15,legend.pos="topright")

#bio18
climvar <- paste(getwd(),'/hotlayers/bio18.asc',sep='')
maxent_models <- paste(getwd(),'/clado4/',sep='')
nbins <- 25 # number of histogram bins for climate variable
c4pnobio18 <- myPno(path_bioclim=climvar,path_model=maxent_models,bin_number=nbins)
plotPNO(c4pnobio18,legend.pos="topright")

#bio19
climvar <- paste(getwd(),'/hotlayers/bio19.asc',sep='')
maxent_models <- paste(getwd(),'/clado4/',sep='')
nbins <- 25 # number of histogram bins for climate variable
c4pnobio19 <- myPno(path_bioclim=climvar,path_model=maxent_models,bin_number=nbins)
plotPNO(c4pnobio19, legend.pos="topright")

####Clado5
#Bio2
climvar <- paste(getwd(),'/hotlayers/bio2.asc',sep='')
maxent_models <- paste(getwd(),'/clado5/',sep='')
nbins <- 25 # number of histogram bins for climate variable
c5pnobio2 <- myPno(path_bioclim=climvar,path_model=maxent_models,bin_number=nbins)
plotPNO(c5pnobio2, legend.pos="topright")

#bio3
climvar <- paste(getwd(),'/hotlayers/bio3.asc',sep='')
maxent_models <- paste(getwd(),'/clado5/',sep='')
nbins <- 25 # number of histogram bins for climate variable
c5pnobio3 <- myPno(path_bioclim=climvar,path_model=maxent_models,bin_number=nbins)
plotPNO(c5pnobio3, legend.pos="topright")

#bio8
climvar <- paste(getwd(),'/hotlayers/bio8.asc',sep='')
maxent_models <- paste(getwd(),'/clado5/',sep='')
nbins <- 25 # number of histogram bins for climate variable
c5pnobio8 <- myPno(path_bioclim=climvar,path_model=maxent_models,bin_number=nbins)
plotPNO(c5pnobio8, legend.pos="topleft")

#bio9
climvar <- paste(getwd(),'/hotlayers/bio9.asc',sep='')
maxent_models <- paste(getwd(),'/clado5/',sep='')
nbins <- 25 # number of histogram bins for climate variable
c5pnobio9 <- myPno(path_bioclim=climvar,path_model=maxent_models,bin_number=nbins)
plotPNO(c5pnobio9, legend.pos="topleft")

#bio13
climvar <- paste(getwd(),'/hotlayers/bio13.asc',sep='')
maxent_models <- paste(getwd(),'/clado5/',sep='')
nbins <- 25 # number of histogram bins for climate variable
c5pnobio13 <- myPno(path_bioclim=climvar,path_model=maxent_models,bin_number=nbins)
plotPNO(c5pnobio13,legend.pos="topright")

#bio14
climvar <- paste(getwd(),'/hotlayers/bio14.asc',sep='')
maxent_models <- paste(getwd(),'/clado5/',sep='')
nbins <- 25 # number of histogram bins for climate variable
c5pnobio14 <- myPno(path_bioclim=climvar,path_model=maxent_models,bin_number=nbins)
plotPNO(c5pnobio14,legend.pos="topright")

#bio15
climvar <- paste(getwd(),'/hotlayers/bio15.asc',sep='')
maxent_models <- paste(getwd(),'/clado5/',sep='')
nbins <- 25 # number of histogram bins for climate variable
c5pnobio15 <- myPno(path_bioclim=climvar,path_model=maxent_models,bin_number=nbins)
plotPNO(c5pnobio15,legend.pos="topright")

#bio18
climvar <- paste(getwd(),'/hotlayers/bio18.asc',sep='')
maxent_models <- paste(getwd(),'/clado5/',sep='')
nbins <- 25 # number of histogram bins for climate variable
c5pnobio18 <- myPno(path_bioclim=climvar,path_model=maxent_models,bin_number=nbins)
plotPNO(c5pnobio18,legend.pos="topright")

#bio19
climvar <- paste(getwd(),'/hotlayers/bio19.asc',sep='')
maxent_models <- paste(getwd(),'/clado5/',sep='')
nbins <- 25 # number of histogram bins for climate variable
c5pnobio19 <- myPno(path_bioclim=climvar,path_model=maxent_models,bin_number=nbins)
plotPNO(c5pnobio19, legend.pos="topright")


####Clado6
#Bio2
climvar <- paste(getwd(),'/hotlayers/bio2.asc',sep='')
maxent_models <- paste(getwd(),'/clado6/',sep='')
nbins <- 25 # number of histogram bins for climate variable
c6pnobio2 <- myPno(path_bioclim=climvar,path_model=maxent_models,bin_number=nbins)
plotPNO(c6pnobio2, legend.pos="topright")

#bio3
climvar <- paste(getwd(),'/hotlayers/bio3.asc',sep='')
maxent_models <- paste(getwd(),'/clado6/',sep='')
nbins <- 25 # number of histogram bins for climate variable
c6pnobio3 <- myPno(path_bioclim=climvar,path_model=maxent_models,bin_number=nbins)
plotPNO(c6pnobio3, legend.pos="topright")

#bio8
climvar <- paste(getwd(),'/hotlayers/bio8.asc',sep='')
maxent_models <- paste(getwd(),'/clado6/',sep='')
nbins <- 25 # number of histogram bins for climate variable
c6pnobio8 <- myPno(path_bioclim=climvar,path_model=maxent_models,bin_number=nbins)
plotPNO(c6pnobio8, legend.pos="topleft")

#bio9
climvar <- paste(getwd(),'/hotlayers/bio9.asc',sep='')
maxent_models <- paste(getwd(),'/clado6/',sep='')
nbins <- 25 # number of histogram bins for climate variable
c6pnobio9 <- myPno(path_bioclim=climvar,path_model=maxent_models,bin_number=nbins)
plotPNO(c6pnobio9, legend.pos="topleft")

#bio13
climvar <- paste(getwd(),'/hotlayers/bio13.asc',sep='')
maxent_models <- paste(getwd(),'/clado6/',sep='')
nbins <- 25 # number of histogram bins for climate variable
c6pnobio13 <- myPno(path_bioclim=climvar,path_model=maxent_models,bin_number=nbins)
plotPNO(c6pnobio13,legend.pos="topright")

#bio14
climvar <- paste(getwd(),'/hotlayers/bio14.asc',sep='')
maxent_models <- paste(getwd(),'/clado6/',sep='')
nbins <- 25 # number of histogram bins for climate variable
c6pnobio14 <- myPno(path_bioclim=climvar,path_model=maxent_models,bin_number=nbins)
plotPNO(c6pnobio14,legend.pos="topright")

#bio15
climvar <- paste(getwd(),'/hotlayers/bio15.asc',sep='')
maxent_models <- paste(getwd(),'/clado6/',sep='')
nbins <- 25 # number of histogram bins for climate variable
c6pnobio15 <- myPno(path_bioclim=climvar,path_model=maxent_models,bin_number=nbins)
plotPNO(c6pnobio15,legend.pos="topright")

#bio18
climvar <- paste(getwd(),'/hotlayers/bio18.asc',sep='')
maxent_models <- paste(getwd(),'/clado6/',sep='')
nbins <- 25 # number of histogram bins for climate variable
c6pnobio18 <- myPno(path_bioclim=climvar,path_model=maxent_models,bin_number=nbins)
plotPNO(c6pnobio18,legend.pos="topright")

#bio19
climvar <- paste(getwd(),'/hotlayers/bio19.asc',sep='')
maxent_models <- paste(getwd(),'/clado6/',sep='')
nbins <- 25 # number of histogram bins for climate variable
c6pnobio19 <- myPno(path_bioclim=climvar,path_model=maxent_models,bin_number=nbins)
plotPNO(c6pnobio19, legend.pos="topright")

####Clado7
#Bio2
climvar <- paste(getwd(),'/hotlayers/bio2.asc',sep='')
maxent_models <- paste(getwd(),'/clado7/',sep='')
nbins <- 25 # number of histogram bins for climate variable
c7pnobio2 <- myPno(path_bioclim=climvar,path_model=maxent_models,bin_number=nbins)
plotPNO(c7pnobio2, legend.pos="topright")

#bio3
climvar <- paste(getwd(),'/hotlayers/bio3.asc',sep='')
maxent_models <- paste(getwd(),'/clado7/',sep='')
nbins <- 25 # number of histogram bins for climate variable
c7pnobio3 <- myPno(path_bioclim=climvar,path_model=maxent_models,bin_number=nbins)
plotPNO(c7pnobio3, legend.pos="topright")

#bio8
climvar <- paste(getwd(),'/hotlayers/bio8.asc',sep='')
maxent_models <- paste(getwd(),'/clado7/',sep='')
nbins <- 25 # number of histogram bins for climate variable
c7pnobio8 <- myPno(path_bioclim=climvar,path_model=maxent_models,bin_number=nbins)
plotPNO(c7pnobio8, legend.pos="topleft")

#bio9
climvar <- paste(getwd(),'/hotlayers/bio9.asc',sep='')
maxent_models <- paste(getwd(),'/clado7/',sep='')
nbins <- 25 # number of histogram bins for climate variable
c7pnobio9 <- myPno(path_bioclim=climvar,path_model=maxent_models,bin_number=nbins)
plotPNO(c7pnobio9, legend.pos="topleft")

#bio13
climvar <- paste(getwd(),'/hotlayers/bio13.asc',sep='')
maxent_models <- paste(getwd(),'/clado7/',sep='')
nbins <- 25 # number of histogram bins for climate variable
c7pnobio13 <- myPno(path_bioclim=climvar,path_model=maxent_models,bin_number=nbins)
plotPNO(c7pnobio13,legend.pos="topright")

#bio14
climvar <- paste(getwd(),'/hotlayers/bio14.asc',sep='')
maxent_models <- paste(getwd(),'/clado7/',sep='')
nbins <- 25 # number of histogram bins for climate variable
c7pnobio14 <- myPno(path_bioclim=climvar,path_model=maxent_models,bin_number=nbins)
plotPNO(c7pnobio14,legend.pos="topright")

#bio15
climvar <- paste(getwd(),'/hotlayers/bio15.asc',sep='')
maxent_models <- paste(getwd(),'/clado7/',sep='')
nbins <- 25 # number of histogram bins for climate variable
c7pnobio15 <- myPno(path_bioclim=climvar,path_model=maxent_models,bin_number=nbins)
plotPNO(c7pnobio15,legend.pos="topright")

#bio18
climvar <- paste(getwd(),'/hotlayers/bio18.asc',sep='')
maxent_models <- paste(getwd(),'/clado7/',sep='')
nbins <- 25 # number of histogram bins for climate variable
c7pnobio18 <- myPno(path_bioclim=climvar,path_model=maxent_models,bin_number=nbins)
plotPNO(c7pnobio18,legend.pos="topright")

#bio19
climvar <- paste(getwd(),'/hotlayers/bio19.asc',sep='')
maxent_models <- paste(getwd(),'/clado7/',sep='')
nbins <- 25 # number of histogram bins for climate variable
c7pnobio19 <- myPno(path_bioclim=climvar,path_model=maxent_models,bin_number=nbins)
plotPNO(c7pnobio19, legend.pos="topright")


#######3 - SOBREPOSIÇÃO, EQUIVALENCIA E SIMILADIRADE DE NICHO
#Sobreposição - Por variável climática

# above the diagonal is the D value
# below the diagonal is the I value, another statistic for comparing the niche occupancy profiles. See Warren et al. 2008 for more info

PrecipitationWettestMonth <-niche.overlap(data.frame(pnobio13))
write.table(round(abs(niche.overlap(data.frame(pnobio13))), 2), "overlapbio13.xls", row.names = T, sep = "\t")

PrecipitationDriestMonth<- niche.overlap(data.frame(pnobio14))
write.table(round(abs(niche.overlap(data.frame(pnobio14))), 2), "overlapbio14.xls", row.names = T, sep = "\t")

PrecipitationSeasonality<-niche.overlap(data.frame(pnobio15))
write.table(round(abs(niche.overlap(data.frame(pnobio15))), 2), "overlapbio15.xls", row.names = T, sep = "\t")

PrecipitationWarmestQuarter<- niche.overlap(data.frame(pnobio18))
write.table(round(abs(niche.overlap(data.frame(pnobio18))), 2), "overlapbio18.xls", row.names = T, sep = "\t")

PrecipitationColdestQuarter<-niche.overlap(data.frame(pnobio19))
write.table(round(abs(niche.overlap(data.frame(pnobio19))), 2), "overlapbio19.xls", row.names = T, sep = "\t")

MeanDiurnalRange<-niche.overlap(data.frame(pnobio2))
write.table(round(abs(niche.overlap(data.frame(pnobio2))), 2), "overlapbio2.xls", row.names = T, sep = "\t")

Isothermality<- niche.overlap(data.frame(pnobio3))
write.table(round(abs(niche.overlap(data.frame(pnobio3))), 2), "overlapbio3.xls", row.names = T, sep = "\t")

MeanTemperatureWettestQuarter<-niche.overlap(data.frame(pnobio8))
write.table(round(abs(niche.overlap(data.frame(pnobio8))), 2), "overlapbio8.xls", row.names = T, sep = "\t")

MeanTemperatureDriestQuarter<-niche.overlap(data.frame(pnobio9))
overlap
write.table(round(abs(niche.overlap(data.frame(pnobio9))), 2), "overlapbio9.xls", row.names = T, sep = "\t")

# Sobreposição - Espécies Par a Par
#importar raster das espécies
library(phyloclim)
library(sp)
library(maptools)
library(raster)
library(ape)
library(dismo)
abietina<-raster("abietina.asc")
alata<-raster("alata.asc")
albiflora<-raster("albiflora.asc")
aloifolia<-raster("aloifolia.asc")
auriculata<-raster("auriculata.asc")
barbata<-raster("barbata.asc")
brevifolia<-raster("brevifolia.asc")
breviscapa<-raster("breviscapa.asc")
burlemarxii<-raster("burlemarxii.asc")
caespitosa<-raster("caespitosa.asc")
candida<-raster("candida.asc")
canelinha<-raster("canelinha.asc")
caputardeae<-raster("caputardeae.asc")
caruncularis<-raster("caruncularis.asc")
caudata<-raster("caudata.asc")
ciliata<-raster("ciliata.asc")
compacta<-raster("compacta.asc")
crinita<-raster("crinita.asc")
cryptantha<-raster("cryptantha.asc")
dasypus<-raster("dasypus.asc")
declinans<-raster("declinans.asc")
epidendroides<-raster("epidendroides.asc")
froesii<-raster("froesii.asc")
furcata<-raster("furcata.asc")
geotegens<-raster("geotegens.asc")
gigantea<-raster("gigantea.asc")
giulietiiae<-raster("giulietiiae.asc")
glabra<-raster("glabra.asc")
glauca<-raster("glauca.asc")
goiasensis<-raster("goiasensis.asc")
graminea<-raster("graminea.asc")
graomogolensis<-raster("graomogolensis.asc")
hatschbachii<-raster("hatschbachii.asc")
hemisphaerica<-raster("hemisphaerica.asc")
hirsuta<-raster("hirsuta.asc")
intermedia<-raster("intermedia.asc")
jolyi<-raster("jolyi.asc")
marcescens<-raster("marcescens.asc")
maxillarioides<-raster("maxillarioides.asc")
metzgerae<-raster("metzgerae.asc")
minima<-raster("minima.asc")
nanuzae<-raster("nanuzae.asc")
nivea<-raster("nivea.asc")
obtecta<-raster("obtecta.asc")
ornata<-raster("ornata.asc")
patens<-raster("patens.asc")
peripherica<-raster("peripherica.asc")
plicata<-raster("plicata.asc")
punctulata<-raster("punctulata.asc")
resinosa<-raster("resinosa.asc")
seubertiana <-raster("seubertiana.asc")
spiralis<-raster("spiralis.asc")
stenocarpa<-raster("stenocarpa.asc")
stipitata<-raster("stipitata.asc")
strangii<-raster("strangii.asc")
streptophylla<-raster("streptophylla.asc")
subscabra<-raster("subscabra.asc")
taxifolia<-raster("taxifolia.asc")
tragacantha<-raster("tragacantha.asc")
tubiflora<-raster("tubiflora.asc")
variabilis<-raster("variabilis.asc")
variegata<-raster("variegata.asc")

#listar os rasters
list_specimes <- list(abietina,alata,albiflora,aloifolia,auriculata,barbata,brevifolia,breviscapa,burlemarxii,caespitosa,candida,canelinha,caputardeae,caruncularis,caudata,ciliata,compacta,crinita,cryptantha,dasypus,declinans,epidendroides,froesii,furcata,geotegens,gigantea,giulietiiae,glabra,glauca,goiasensis,graminea,graomogolensis,hatschbachii,hemisphaerica,hirsuta,intermedia,jolyi,marcescens,maxillarioides,metzgerae,minima,nanuzae,nivea,obtecta,ornata,patens,peripherica,plicata,punctulata,resinosa,seubertiana,spiralis,stenocarpa,stipitata,strangii,streptophylla,subscabra,taxifolia,tragacantha,tubiflora,variabilis,variegata)

#index D
i<-1
j <- 0
analise <- 0
for (specie in list_specimes){
  for (specie2 in list_specimes){
    
      analise[i]<-nicheOverlap(specie,specie2, stat='D', mask=F, checkNegatives=F)
      i<-i+1  
    }
}

matriz <- matrix(analise,nrow = 62,ncol = 62)
matriz
write.table(round(abs(niche.overlap(data.frame(matriz))), 8), "D2.xls", row.names = T, sep = "\t")
write.table(round(abs(niche.overlap(data.frame(matriz))), 8), "D2.csv", row.names = T, sep = "\t")
#index I
i<-1
j <- 0
analise <- 0
for (specie in list_specimes){
  for (specie2 in list_specimes){
    
    analise[i]<-nicheOverlap(specie,specie2, stat='I', mask=F, checkNegatives=F)
    i<-i+1  
  }
}

matriz2 <- matrix(analise,nrow = 62,ncol = 62)
write.table(round(abs(niche.overlap(data.frame(matriz2))), 8), "I2.xls", row.names = T, sep = "\t")
write.table(round(abs(niche.overlap(data.frame(matriz2))), 8), "I2.csv", row.names = T, sep = "\t")
 ########ou para pares de espécies

D<-nicheOverlap(albiflora, caruncularis, stat='D', mask=F, checkNegatives=F)
D

I<-nicheOverlap(albiflora, caruncularis, stat='I', mask=F, checkNegatives=F)
I


##### Equivalencia 
spp<-read.table("spp.txt",h=T)


####### 4 - ESTIMATION OF ANCESTRAL CLIMATIC
library(ape)
library(phytools)

###import trees:
besttreePP <- read.nexus("bestVellozia-epithP", force.multi = FALSE)
plot(besttreePP)

multitrees <- read.nexus("Vellozia801trees-epith", force.multi = TRUE)
plotTree(multitrees[[300]])

#read tree
tree<-read.tree('VellPattreefile.txt')
tree
plot(tree)



#utilizar PNOs da ánalise 2 
#plot por variável climática. 

pnobio2P<- as.data.frame(pnobio2)

#Transformar matrix em data frame
class(pnobio13)
pnobio13P <- as.data.frame(pnobio13)
class(pnobio13P)


acbio2 <- anc.clim(target = besttreePP, pno = pnobio2P, n = 10000)
plotAncClim(acbio2, ylab = "Mean Diurnal Range")

acbio3 <- anc.clim(target = besttreePP, pno = pnobio3P, n = 10000)
plotAncClim(acbio3, ylab = "Isothermality")

acbio8 <- anc.clim(target = besttreePP, pno = pnobio8P, n = 10000)
plotAncClim(acbio8, ylab = "Mean Temperature of Wettest Quarter")

acbio9 <- anc.clim(target = besttreePP, pno = pnobio9P, n = 10000)
plotAncClim(acbio9, ylab = "Mean Temperature of Driest Quarter")

acbio13 <- anc.clim(target = besttreePP, pno = pnobio13P, n = 10000)
plotAncClim(acbio13, ylab = "Precipitation of Wettest Month") 

acbio14 <- anc.clim(target = besttreePP, pno = pnobio14P, n = 10000)
plotAncClim(acbio14, ylab = "Precipitation of Driest Month")

acbio15 <- anc.clim(target = besttreePP, pno = pnobio15P, n = 10000)
plotAncClim(acbio15, ylab = "Precipitation Seasonality")

acbio18 <- anc.clim(target = besttreePP, pno = pnobio18P, n = 10000)
plotAncClim(acbio18, ylab = "Precipitation of Warmest Quarter")

acbio19 <- anc.clim(target = besttreePP, pno = pnobio19P, n = 10000)
plotAncClim(acbio19, ylab = "Precipitation of Coldest Quarter")

### 801 trees
treesP <- read.tree("trees801", keep.multi=TRUE) #salvar arquivo anexo no mesmo diretório do R
class(treesP) #tem que ser multiPhylo

acbio2multi<- anc.clim(besttreePP, posterior=treesP, pno = pnobio2P,n=1000)

acbio3multi<- anc.clim(besttreePP, posterior=treesP, pno = pnobio3P,n=1000)

acbio8multi<- anc.clim(besttreePP, posterior=treesP, pno = pnobio8P,n=1000)

acbio9multi<- anc.clim(besttreePP, posterior=treesP, pno = pnobio9P,n=1000)

acbio13multi<- anc.clim(besttreePP, posterior=treesP, pno = pnobio13P,n=1000)

acbio14multi<- anc.clim(besttreePP, posterior=treesP, pno = pnobio14P,n=1000)

acbio15multi<- anc.clim(besttreePP, posterior=treesP, pno = pnobio15P,n=1000)

acbio18multi<- anc.clim(besttreePP, posterior=treesP, pno = pnobio18P,n=1000)

acbio19multi<- anc.clim(besttreePP, posterior=treesP, pno = pnobio19P,n=1000)


acbio2multi<- anc.clim(besttreePP, posterior=treesP, pno = pnobio2P,n=10000)
plotAncClim(acbio2multi, ylab = "Mean Diurnal Range")

acbio3multi<- anc.clim(besttreePP, posterior=treesP, pno = pnobio3P,n=10000)
plotAncClim(acbio3multi, ylab = "Isothermality")

acbio8multi<- anc.clim(besttreePP, posterior=treesP, pno = pnobio8P,n=10000)
plotAncClim(acbio8multi, ylab = "Mean Temperature of Wettest Quarter")

acbio9multi<- anc.clim(besttreePP, posterior=treesP, pno = pnobio9P,n=10000)
plotAncClim(acbio9multi, ylab = "Mean Temperature of Driest Quarter")

acbio13multi<- anc.clim(besttreePP, posterior=treesP, pno = pnobio13P,n=10000)
plotAncClim(acbio13multi, ylab = "Precipitation of Wettest Month") 

acbio14multi<- anc.clim(besttreePP, posterior=treesP, pno = pnobio14P,n=10000)
plotAncClim(acbio14multi, ylab = "Precipitation of Driest Month")

acbio15multi<- anc.clim(besttreePP, posterior=treesP, pno = pnobio15P,n=10000)
plotAncClim(acbio15multi, ylab = "Precipitation Seasonality")

acbio18multi<- anc.clim(besttreePP, posterior=treesP, pno = pnobio18P,n=10000)
plotAncClim(acbio18multi, ylab = "Precipitation of Warmest Quarter")

acbio19multi<- anc.clim(besttreePP, posterior=treesP, pno = pnobio19P,n=10000)
plotAncClim(acbio19multi, ylab = "Precipitation of Coldest Quarter")

