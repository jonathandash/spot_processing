# **************************************************************************
# Attempt to clip all Spot imagery to the extent of the Plots
# J.Dash
# Jonathan.dash@scionresearch.com
# **************************************************************************

library (rgdal)
library(raster)
library(sp)
library(stringr)
library(glcm)
library(reshape2)
library(randomForest)
library(ROCR)

setwd('Q:\\Forest Industry Informatics\\Projects\\TenonProject\\')
output.dir<-'Q:\\Forest Industry Informatics\\Projects\\TenonProject\\RScripts\\outputs\\'
proj.nztm<-"+proj=tmerc +lat_0=0 +lon_0=173 +k=0.9996 +x_0=1600000 +y_0=10000000 +ellps=GRS80 +units=m +no_defs"

#shape.dir<-'D:\\WV\\LUCAS_Plot_Shapes'
shape.dir<-'Q:\\Forest Industry Informatics\\Projects\\TenonProject\\LUCAS_Plot_Shapes_ALL\\'
raster.dir<-'Z:\\SPOTMAPS\\'


# read shapefiles
setwd(shape.dir)
slist=list.files(shape.dir, pattern="shp$", full.names=FALSE)
for(i in slist) { assign(unlist(strsplit(i, "[.]"))[1], shapefile(i)) }

# read rasters
rlist=list=list.files(raster.dir, pattern="tif$", full.names=FALSE)
setwd(raster.dir)
for(i in rlist) { assign(unlist(strsplit(i, "[.]"))[1], stack(i)) }


# #List all objects in the environment with _PF in the name
list.1 <- mget(grep("_PF",ls(),value=TRUE))
list.1[[1]] <- NULL
IDs<-names(list.1) # Get the object names from the list... seems to be easier to work with this
#IDs<-tail(IDs, -1) # remove the name of the output directory from the list

# #List all objects in the environment with SPM in the name
list.2 <- mget(grep("SPM",ls(),value=TRUE))
list.2[[1]] <- NULL
ID2s<-names(list.2) # Get the object names from the list... seems to be easier to work with this
#IDs<-tail(IDs, -1) # remove the name of the output directory from the list


# loop through all the SPOT imagery and crop to bounding box of shapefile
for(i in 1:length(ID2s))
{
for (j in 1:length(IDs))
{
  try(assign(paste('clipped.',IDs[j], sep=""), crop(get(ID2s[i]), get(IDs[j]))))
}
}



# #List all objects in the environment with clipped in the name
list.3 <- mget(grep("clipped",ls(),value=TRUE))
list.3[[1]] <- NULL
ID3s<-names(list.3) # Get the object names from the list... seems to be easier to work with this
length(list.3)
#Make a list to use to mask the shapefile from the cropped rasters
ID4s<-substring(ID3s, 9)



# loop through all the plots and mask them so that they are circular
for(k in 1:length(ID3s))
{
    try(assign(paste('masked.',ID4s[k], sep=""), mask(get(ID3s[k]), get(ID4s[k]))) )
}


#View for checks 
plotRGB(masked.AC164E5_PF120803, r=2, g=3, b=1)
plot(masked.AC164E5_PF120803)

# remove any texture objects in the environment
rm(list=ls(pattern="texture"))
# #List all objects in the environment with masked in the name
list.5 <- mget(grep("masked",ls(),value=TRUE))
list.5[[1]] <- NULL
ID5s<-names(list.5) # Get the o

# loop through all the plots and calculate texture metrics

#outputs<-data.frame(outs=length(8)) 
for(k in 1:length(ID5s))
{
  try(assign(paste('texture1_',ID5s[k], sep=""), glcm(raster(get(ID5s[k]), layer=1), window = c(9, 9), shift=list(c(0,1), c(1,1), c(1,0), c(1,-1)))))
  try(assign(paste('texture2_',ID5s[k], sep=""), glcm(raster(get(ID5s[k]), layer=2), window = c(9, 9), shift=list(c(0,1), c(1,1), c(1,0), c(1,-1)))))
  try(assign(paste('texture3_',ID5s[k], sep=""), glcm(raster(get(ID5s[k]), layer=3), window = c(9, 9), shift=list(c(0,1), c(1,1), c(1,0), c(1,-1)))))
 # try(assign(paste('texture4_',ID5s[k], sep=""), glcm(raster(get(ID5s[k]), layer=4), window=c(10,10), shift=list(c(0,1), c(1,1), c(1,0), c(1,-1)))))
  
  
  #print(s)
  #s$source <- rep(ID5s[k],8)
  #outputs<-try(cbind(outputs,s))
  
  #names<-rbind(names, rep(ID5s[k], 7))
}

#tx_test<-glcm(raster(get(ID5s[1]), layer=1), window = c(3, 3), shift=list(c(0,1), c(1,1), c(1,0), c(1,-1)))
#plot(tx_test)
plot(get(ID5s[k]))

#Blue band textures
list.6 <- mget(grep("texture1",ls(),value=TRUE))
length(list.6)
list.6[[1]] <- NULL
ID6s<-names(list.6) # Get the o

out<-NULL
n<-NULL
for(k in 1:length(ID6s))
{
  s<-cellStats(get(ID6s[k]), mean)
  out<-append(out, s)
  t<-rep(ID6s[k], 8)
  n<-append(n, t)
}

out1.df<-data.frame(out)
out1.df$Source<-n
out1.df$stat<-rep(c("mean", "variance", "homogeneity", "contrast", "dissimilarity", "entropy", "sercond_moment", "correlation"), length(ID6s))
out1.df$band<-rep("red", length(ID6s) * 8)
out1.df$SurveyID<-substring(out1.df$Source, 17, 999)


#Green band textures
list.7 <- mget(grep("texture2",ls(),value=TRUE))
length(list.7)
list.7[[1]] <- NULL
ID7s<-names(list.7) # Get the o

out<-NULL
n<-NULL
for(k in 1:length(ID7s))
{
  s<-cellStats(get(ID7s[k]), mean)
  out<-append(out, s)
  t<-rep(ID7s[k], 8)
  n<-append(n, t)
}

out2.df<-data.frame(out)
out2.df$Source<-n
out2.df$stat<-rep(c("mean", "variance", "homogeneity", "contrast", "dissimilarity", "entropy", "sercond_moment", "correlation"), length(ID7s))
out2.df$band<-rep("green", length(ID7s) * 8)
out2.df$SurveyID<-substring(out2.df$Source, 17, 999)

#Red band textures
list.8 <- mget(grep("texture3",ls(),value=TRUE))
length(list.8)
list.8[[1]] <- NULL
ID8s<-names(list.8) # Get the o

out<-NULL
n<-NULL
for(k in 1:length(ID8s))
{
  s<-cellStats(get(ID8s[k]), mean)
  out<-append(out, s)
  t<-rep(ID8s[k], 8)
  n<-append(n, t)
}

out3.df<-data.frame(out)
out3.df$Source<-n
out3.df$stat<-rep(c("mean", "variance", "homogeneity", "contrast", "dissimilarity", "entropy", "sercond_moment", "correlation"), length(ID8s))
out3.df$band<-rep("blue", length(ID8s) * 8)
out3.df$SurveyID<-substring(out3.df$Source, 17, 999)

#NIR band textures
list.9 <- mget(grep("texture4",ls(),value=TRUE))
length(list.9)
list.9[[1]] <- NULL
ID9s<-names(list.9) # Get the o

out<-NULL
n<-NULL
for(k in 1:length(ID9s))
{
  s<-cellStats(get(ID9s[k]), mean)
  out<-append(out, s)
  t<-rep(ID9s[k], 8)
  n<-append(n, t)
}

out4.df<-data.frame(out)
out4.df$Source<-n
out4.df$stat<-rep(c("mean", "variance", "homogeneity", "contrast", "dissimilarity", "entropy", "sercond_moment", "correlation"), length(ID9s))
out4.df$band<-rep("nir", length(ID9s) * 8)
out4.df$SurveyID<-substring(out4.df$Source, 17, 999)

tx.out<-rbind(out1.df, out2.df, out3.df)
tx.out$variable<-paste(tx.out$stat, '_', tx.out$band, sep="")
tx.out<-tx.out[c("SurveyID", "variable", "out")]
names(tx.out)[names(tx.out) == 'out'] <- 'value'
write.table(tx.out, paste(output.dir, 'tx_out.dat', sep=""))

######################################################################
# Spectral Metrcis
######################################################################

setwd('Q:\\Forest Industry Informatics\\Projects\\TenonProject\\RScripts')
source('VegetationIndices.R')

#Calculate NDVI
out<-NULL
n<-NULL
for(k in 1:length(ID5s))
{
  #try(assign(paste('NDVI',ID5s[k], sep=""), NDVI(NIR= get(ID5s[k])[[4]], RED=get(ID5s[k])[[3]])))
  s<-NDVI(NIR= get(ID5s[k])[[4]], RED=get(ID5s[k])[[3]])
  t<-cellStats(s, mean)
  out<-append(out, t)
  u<-ID5s[k]
  n<-append(n, u)
}
out.ndvi.df<-data.frame(out)
out.ndvi.df$Source<-n
out.ndvi.df$Stat<-rep('ndvi', length(n))
out.ndvi.df$SurveyID<-substring(out.ndvi.df$Source, 8, 999)

#Calculate mNDVI
out<-NULL
n<-NULL
for(k in 1:length(ID5s))
{
  #try(assign(paste('mNDVI',ID5s[k], sep=""), mNDVI(NIR= get(ID5s[k])[[4]], RED=get(ID5s[k])[[3]], BLUE=get(ID5s[k])[[1]])))
  s<-mNDVI(NIR= get(ID5s[k])[[4]], RED=get(ID5s[k])[[3]], BLUE=get(ID5s[k])[[1]])
  t<-cellStats(s, mean)
  out<-append(out, t)
  u<-ID5s[k]
  n<-append(n, u)
}
out.mndvi.df<-data.frame(out)
out.mndvi.df$Source<-n
out.mndvi.df$Stat<-rep('mndvi', length(n))
out.mndvi.df$SurveyID<-substring(out.mndvi.df$Source, 8, 999)

#Calculate mEVI
out<-NULL
n<-NULL
for(k in 1:length(ID5s))
{
  #try(assign(paste('mEVI',ID5s[k], sep=""), mEVI(NIR= get(ID5s[k])[[4]], RED=get(ID5s[k])[[3]], BLUE=get(ID5s[k])[[1]])))
  s<-mEVI(NIR= get(ID5s[k])[[4]], RED=get(ID5s[k])[[3]], BLUE=get(ID5s[k])[[1]])
  t<-cellStats(s, mean)
  out<-append(out, t)
  u<-ID5s[k]
  n<-append(n, u)
}

out.mevi.df<-data.frame(out)
out.mevi.df$Source<-n
out.mevi.df$Stat<-rep('mevi', length(n))
out.mevi.df$SurveyID<-substring(out.mevi.df$Source, 8, 999)

#Calculate SAVI
out<-NULL
n<-NULL
for(k in 1:length(ID5s))
{
  #try(assign(paste('SAVI',ID5s[k], sep=""), SAVI(NIR= get(ID5s[k])[[4]], RED=get(ID5s[k])[[3]])))
  s<-SAVI(NIR= get(ID5s[k])[[4]], RED=get(ID5s[k])[[3]])
  t<-cellStats(s, mean)
  out<-append(out, t)
  u<-ID5s[k]
  n<-append(n, u)
}
out.savi.df<-data.frame(out)
out.savi.df$Source<-n
out.savi.df$Stat<-rep('savi', length(n))
out.savi.df$SurveyID<-substring(out.savi.df$Source, 8, 999)


#Calculate mARVI
out<-NULL
n<-NULL
for(k in 1:length(ID5s))
{
  #try(assign(paste('mARVI',ID5s[k], sep=""), mARVI(NIR= get(ID5s[k])[[4]], RED=get(ID5s[k])[[3]], BLUE=get(ID5s[k])[[1]])))
  s<-mARVI(NIR= get(ID5s[k])[[4]], RED=get(ID5s[k])[[3]], BLUE=get(ID5s[k])[[1]])
  t<-cellStats(s, mean)
  out<-append(out, t)
  u<-ID5s[k]
  n<-append(n, u)
}
out.marvi.df<-data.frame(out)
out.marvi.df$Source<-n
out.marvi.df$Stat<-rep('marvi', length(n))
out.marvi.df$SurveyID<-substring(out.marvi.df$Source, 8, 999)

#Calculate RGRI
out<-NULL
n<-NULL
for(k in 1:length(ID5s))
{
  #try(assign(paste('RGRI',ID5s[k], sep=""), RGRI(GREEN= get(ID5s[k])[[2]], RED=get(ID5s[k])[[3]])))
  s<-RGRI(GREEN= get(ID5s[k])[[2]], RED=get(ID5s[k])[[3]])
  t<-cellStats(s, mean)
  out<-append(out, t)
  u<-ID5s[k]
  n<-append(n, u)
}
out.rgri.df<-data.frame(out)
out.rgri.df$Source<-n
out.rgri.df$Stat<-rep('rgri', length(n))
out.rgri.df$SurveyID<-substring(out.rgri.df$Source, 8, 999)

#Calculate mARI1
out<-NULL
n<-NULL
for(k in 1:length(ID5s))
{
  try(assign(paste('mARI1',ID5s[k], sep=""), mARI1(GREEN= get(ID5s[k])[[2]], RED=get(ID5s[k])[[3]])))
  s<-mARI1(GREEN= get(ID5s[k])[[2]], RED=get(ID5s[k])[[3]])
  t<-cellStats(s, mean)
  out<-append(out, t)
  u<-ID5s[k]
  n<-append(n, u)
}
out.mari1.df<-data.frame(out)
out.mari1.df$Source<-n
out.mari1.df$Stat<-rep('mari1', length(n))
out.mari1.df$SurveyID<-substring(out.mari1.df$Source, 8, 999)

#Calculate mARI2
out<-NULL
n<-NULL
for(k in 1:length(ID5s))
{
  #try(assign(paste('mARI2',ID5s[k], sep=""), mARI2(GREEN= get(ID5s[k])[[2]], RED=get(ID5s[k])[[3]], NIR= get(ID5s[k])[[4]])))
  s<-mARI2(GREEN= get(ID5s[k])[[2]], RED=get(ID5s[k])[[3]], NIR= get(ID5s[k])[[4]])
  t<-cellStats(s, mean)
  out<-append(out, t)
  u<-ID5s[k]
  n<-append(n, u)
}
out.mari2.df<-data.frame(out)
out.mari2.df$Source<-n
out.mari2.df$Stat<-rep('mari2', length(n))
out.mari2.df$SurveyID<-substring(out.mari2.df$Source, 8, 999)

#Calculate TVI
out<-NULL
n<-NULL
for(k in 1:length(ID5s))
{
  #try(assign(paste('TVI',ID5s[k], sep=""), TVI(GREEN= get(ID5s[k])[[2]], RED=get(ID5s[k])[[3]], NIR= get(ID5s[k])[[4]])))
  s<-TVI(GREEN= get(ID5s[k])[[2]], RED=get(ID5s[k])[[3]], NIR= get(ID5s[k])[[4]])
  t<-cellStats(s, mean)
  out<-append(out, t)
  u<-ID5s[k]
  n<-append(n, u)
}
out.tvi.df<-data.frame(out)
out.tvi.df$Source<-n
out.tvi.df$Stat<-rep('tvi', length(n))
out.tvi.df$SurveyID<-substring(out.tvi.df$Source, 8, 999)

#Calculate REEI
out<-NULL
n<-NULL
for(k in 1:length(ID5s))
{
  #try(assign(paste('TVI',ID5s[k], sep=""), TVI(GREEN= get(ID5s[k])[[2]], RED=get(ID5s[k])[[3]], NIR= get(ID5s[k])[[4]])))
  s<-REEI(RED=get(ID5s[k])[[3]], NIR= get(ID5s[k])[[4]])
  t<-cellStats(s, mean)
  out<-append(out, t)
  u<-ID5s[k]
  n<-append(n, u)
}
out.reei.df<-data.frame(out)
out.reei.df$Source<-n
out.reei.df$Stat<-rep('reei', length(n))
out.reei.df$SurveyID<-substring(out.reei.df$Source, 8, 999)


#Calculate scale_red
out<-NULL
n<-NULL
for(k in 1:length(ID5s))
{
  #try(assign(paste('TVI',ID5s[k], sep=""), TVI(GREEN= get(ID5s[k])[[2]], RED=get(ID5s[k])[[3]], NIR= get(ID5s[k])[[4]])))
  s<-scale_red(RED=get(ID5s[k])[[1]], GREEN= get(ID5s[k])[[2]], BLUE=get(ID5s[k])[[3]])
  t<-cellStats(s, mean)
  out<-append(out, t)
  u<-ID5s[k]
  n<-append(n, u)
}
out.scale_red.df<-data.frame(out)
out.scale_red.df$Source<-n
out.scale_red.df$Stat<-rep('scale_red', length(n))
out.scale_red.df$SurveyID<-substring(out.scale_red.df$Source, 8, 999)

#Calculate scale_green
out<-NULL
n<-NULL
for(k in 1:length(ID5s))
{
  #try(assign(paste('TVI',ID5s[k], sep=""), TVI(GREEN= get(ID5s[k])[[2]], RED=get(ID5s[k])[[3]], NIR= get(ID5s[k])[[4]])))
  s<-scale_green(RED=get(ID5s[k])[[1]], GREEN= get(ID5s[k])[[2]], BLUE=get(ID5s[k])[[3]])
  t<-cellStats(s, mean)
  out<-append(out, t)
  u<-ID5s[k]
  n<-append(n, u)
}
out.scale_green.df<-data.frame(out)
out.scale_green.df$Source<-n
out.scale_green.df$Stat<-rep('scale_green', length(n))
out.scale_green.df$SurveyID<-substring(out.scale_green.df$Source, 8, 999)

#Calculate scale_blue
out<-NULL
n<-NULL
for(k in 1:length(ID5s))
{
  #try(assign(paste('TVI',ID5s[k], sep=""), TVI(GREEN= get(ID5s[k])[[2]], RED=get(ID5s[k])[[3]], NIR= get(ID5s[k])[[4]])))
  s<-scale_blue(RED=get(ID5s[k])[[1]], GREEN= get(ID5s[k])[[2]], BLUE=get(ID5s[k])[[3]])
  t<-cellStats(s, mean)
  out<-append(out, t)
  u<-ID5s[k]
  n<-append(n, u)
}
out.scale_blue.df<-data.frame(out)
out.scale_blue.df$Source<-n
out.scale_blue.df$Stat<-rep('scale_blue', length(n))
out.scale_blue.df$SurveyID<-substring(out.scale_blue.df$Source, 8, 999)

#Calculate mean green
out<-NULL
n<-NULL
for(k in 1:length(ID5s))
{
  #try(assign(paste('TVI',ID5s[k], sep=""), TVI(GREEN= get(ID5s[k])[[2]], RED=get(ID5s[k])[[3]], NIR= get(ID5s[k])[[4]])))
  #s<-mean(GREEN= get(ID5s[k])[[2]])
  t<-cellStats(get(ID5s[k])[[2]], mean)
  out<-append(out, t)
  u<-ID5s[k]
  n<-append(n, u)
}
out.mean.green.df<-data.frame(out)
out.mean.green.df$Source<-n
out.mean.green.df$Stat<-rep('mean.green', length(n))
out.mean.green.df$SurveyID<-substring(out.mean.green.df$Source, 8, 999)

#Calculate mean blue
out<-NULL
n<-NULL
for(k in 1:length(ID5s))
{
  #try(assign(paste('TVI',ID5s[k], sep=""), TVI(GREEN= get(ID5s[k])[[2]], RED=get(ID5s[k])[[3]], NIR= get(ID5s[k])[[4]])))
  #s<-mean(GREEN= get(ID5s[k])[[2]])
  t<-cellStats(get(ID5s[k])[[3]], mean)
  out<-append(out, t)
  u<-ID5s[k]
  n<-append(n, u)
}
out.mean.blue.df<-data.frame(out)
out.mean.blue.df$Source<-n
out.mean.blue.df$Stat<-rep('mean.blue', length(n))
out.mean.blue.df$SurveyID<-substring(out.mean.blue.df$Source, 8, 999)


#Calculate mean red
out<-NULL
n<-NULL
for(k in 1:length(ID5s))
{
  #try(assign(paste('TVI',ID5s[k], sep=""), TVI(GREEN= get(ID5s[k])[[2]], RED=get(ID5s[k])[[3]], NIR= get(ID5s[k])[[4]])))
  #s<-mean(GREEN= get(ID5s[k])[[2]])
  t<-cellStats(get(ID5s[k])[[1]], mean)
  out<-append(out, t)
  u<-ID5s[k]
  n<-append(n, u)
}
out.mean.red.df<-data.frame(out)
out.mean.red.df$Source<-n
out.mean.red.df$Stat<-rep('mean.red', length(n))
out.mean.red.df$SurveyID<-substring(out.mean.red.df$Source, 8, 999)



#sp.out<-rbind(out.ndvi.df, out.mndvi.df, out.mevi.df, out.savi.df, out.marvi.df, out.rgri.df, out.mari1.df, out.mari2.df, out.tvi.df, out.reei.df) #brin all the spectal averages into a df
sp.out<-rbind(out.scale_red.df, out.scale_green.df, out.scale_blue.df, out.mean.green.df, out.mean.blue.df, out.mean.red.df) #brin all the spectal averages into a df

sp.out<-sp.out[c("SurveyID", "Stat", "out")]
names(sp.out)[names(sp.out) == 'out'] <- 'value'
names(sp.out)[names(sp.out) == 'Stat'] <- 'variable'
write.table(sp.out, paste(output.dir, 'sp_out.dat', sep=""))



################################################################################
# From here we are starting to look at analytics
# This maybe put into a different script eventually
################################################################################

tx.out<-read.table(paste(output.dir, 'tx_out.dat', sep=""))
setwd('Q:\\Forest Industry Informatics\\Projects\\TenonProject\\RScripts')
source('Tenon_Lucas.R')

lid<-read.csv('Q:\\Forest Industry Informatics\\Projects\\TenonProject\\LUCAS\\LUCAS_clips.csv')

#Combine as a big molten format
mlt_pl_sum<-gather(pl_sum, variable, value,  -SurveyID)
master_melt<-rbind(mlt_pl_sum, tx.out)
master_melt<-rbind(master_melt, sp.out)
#master_melt$SurveyIDtrimd<-trimws(master_melt$SurveyID, which="both")

#write.csv(master_melt, file='D:\\temp\\mast_melt.csv')

#Pivot the "Molten" input into wide fprmat
master.pivot<-dcast(master_melt, SurveyID ~ variable, value.var="value", fun.aggregate = mean)
master.pivot<-merge(master.pivot, lid, by.x='SurveyID', by.y='FileTitle')
plot(TopHeight ~ Elev.P95, master.pivot)
summary(lm(TopHeight ~ Elev.P95, master.pivot))
plot(TopHeight ~ Elev.P50, master.pivot)

#Only include the plots with NO NAs  
model.df<-master.pivot[complete.cases(master.pivot),]
model.df$Age_class<-runif(1,(model.df$Age - 4), (model.df$Age + 4))
model.df$PrunedStatus<-ifelse(model.df$Cohort %in% c(1,2,3), "Pruned", "Unpruned") # calculate a binary value for pruning --review



#First model volumes
myvars <- names(model.df) %in% c("BA", "BA_h", "SurveyID", "sph", "PlotSize", "vol", "TopHeight", "X", "Y", "varb", "PrunedStatus",  "Cohort", "DataFile") 
vol.mod.df<- model.df[!myvars]
vol.mod.df$correlation_blue[is.infinite(vol.mod.df$correlation_blue)] <- 0 
vol.mod.df$correlation_green[is.infinite(vol.mod.df$correlation_green)] <- 0 
vol.mod.df$marvi[is.infinite(vol.mod.df$marvi)] <- 0 
vol.mod.df$mevi[is.infinite(vol.mod.df$mevi)] <- 0 
vol.mod.df$mndvi[is.infinite(vol.mod.df$mndvi)] <- 0 
vol.mod.df$reei[is.infinite(vol.mod.df$reei)] <- 0 
vol.mod.df$correlation_nir[is.infinite(vol.mod.df$correlation_nir)] <- 0 
vol.mod.df$correlation_red[is.infinite(vol.mod.df$correlation_red)] <- 0 


#Fit a RF model for volume including all predictors as a  baseline
set.seed(99)
fit.vol<-randomForest(Age~ Elev.P95, 
                      data = vol.mod.df, importance = TRUE, proximity=TRUE, ntree=2000)

chk<-subset(vol.mod.df, Age<15)
plot(Age ~ scale_green, chk)

varImpPlot(fit.vol, scale=T) #Plot the importance of the predictors
importance(fit.vol)
varUsed(fit.vol, count=FALSE, by.tree=FALSE)
par(mfrow=c(2,2))
plot(Age ~ vol, vol.mod.df) 
plot(Age ~ entropy_blue, vol.mod.df)
plot(Age ~ homogeneity_red, vol.mod.df)
plot(vol ~ mean_blue, vol.mod.df)
dev.off()
plot(fit.vol, log="y")
print(fit.vol)
MDSplot(fit.vol, as.factor(vol.mod.df$Age), k=2)
partialPlot(fit.vol, vol.mod.df, Elev.P95)
max(fit.vol$rsq)

#First model top height
myvarsTH <- names(model.df) %in% c("BA", "BA_h", "SurveyID", "sph", "PlotSize", "vol", "X", "Y", "varb", "PrunedStatus",  "Cohort") 
th.mod.df<- model.df[!myvarsTH]
th.mod.df$correlation_blue[is.infinite(th.mod.df$correlation_blue)] <- 0 
th.mod.df$correlation_green[is.infinite(th.mod.df$correlation_green)] <- 0 
th.mod.df$marvi[is.infinite(th.mod.df$marvi)] <- 0 
th.mod.df$mevi[is.infinite(th.mod.df$mevi)] <- 0 
th.mod.df$mndvi[is.infinite(th.mod.df$mndvi)] <- 0 
th.mod.df$reei[is.infinite(th.mod.df$reei)] <- 0 
th.mod.df$correlation_nir[is.infinite(th.mod.df$correlation_nir)] <- 0 
th.mod.df$correlation_red[is.infinite(th.mod.df$correlation_red)] <- 0 

#Fit a RF model for volume including all predictors as a  baseline
set.seed(99)
fit.th<-randomForest(TopHeight~ ., 
                      data = th.mod.df, importance = TRUE, proximity=TRUE, ntree=2000)
varImpPlot(fit.th, scale=T) #Plot the importance of the predictors
print(fit.th)
max(fit.th$rsq)
MDSplot(fit.th, as.factor(vol.mod.df$vol), k=2)
partialPlot(fit.th, th.mod.df, dissimilarity_red)








boxplot(variance_nir ~ PrunedStatus, vol.mod.df)









partialPlot(fit.vol, vol.mod.df, Age)

getTree(fit.vol)


(fit.vol$rsq)
min(fit.vol$err.rate)
print(fit.vol)



#Try to fit an ROC curve
OOB.votes <- predict (fit.vol,vol.mod.df,type="prob")
OOB.pred <- OOB.votes[,2]

pred.obj <- prediction (OOB.pred,vol.mod.df$PrunedStatus)
RP.perf <- performance(pred.obj, "rec","prec");
plot(RP.perf)

ROC.perf <- performance(pred.obj, "fpr","tpr");
plot (ROC.perf);

plot  (RP.perf@alpha.values[[1]],RP.perf@x.values[[1]]);
lines (RP.perf@alpha.values[[1]],RP.perf@y.values[[1]]);
lines (ROC.perf@alpha.values[[1]],ROC.perf@x.values[[1]]);


fit.vol.p<-cforest(vol ~ ., 
                     data = vol.mod.df)



varImpPlot(fit.vol)






