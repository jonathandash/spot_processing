#3D plot of raster plots

library(scatterplot3d)
library(rLiDAR)
library(rgl)
library(plot3D)
library(ggplot2)
library(gridExtra)

setwd('D:\\WV\\ENVIformat\\Normalised\\')
wv<-readLAS('D:\\WV\\ENVIformat\\NormalisedLAS\\1_3\\nztm\\CU60_PF150824_S1_wv.las')

scatterplot3d(x=wv.df$X, y=wv.df$Y, z=wv.df$Z, pch=16, color = "steelblue", highlight.3d=FALSE)

wv.df<-as.data.frame(wv)
scatter3D(x=wv.df$X, y=wv.df$Y, z=wv.df$Z, pch=19, colkey = FALSE, col="gray", bty = "u", zlim=c(320,380), col.panel ="steelblue",
          col.grid = "darkblue", theta = 60, phi = 35)


ls<-readLAS('D:\\WV\\ENVIformat\\Outputs\\clips\\clipsLidar\\virtual_plot_clips_non_normal\\CU60_PF150824_S1.las')
ls.df<-as.data.frame(ls)
scatterplot3d(x=ls.df$X, y=ls.df$Y, z=ls.df$Z, pch=16, color = "steelblue", highlight.3d=FALSE)
ls1.df<-subset(ls.df, ReturnNumber ==1)

par(mfrow=c(1,2))
scatterplot3d(x=wv.df$X, y=wv.df$Y, z=wv.df$Z, pch=16, zlim=c(320, 380), color = "steelblue", highlight.3d=FALSE, grid=T)
scatterplot3d(x=ls.df$X, y=ls.df$Y, z=ls.df$Z, pch=16, color = "steelblue", highlight.3d=FALSE, grid=T)


par(mfrow=c(1,2))
scatter3D(x=wv.df$X, y=wv.df$Y, z=wv.df$Z, pch=19, colkey = FALSE, col="gray", bty = "u", zlim=c(320,380), col.panel ="steelblue",
          col.grid = "darkblue", theta = 60, phi = 35)
scatter3D(x=ls1.df$X, y=ls1.df$Y, z=ls1.df$Z, pch=19, colkey = FALSE, col="gray", bty = "u", zlim=c(320,380), col.panel ="steelblue",
          col.grid = "darkblue", theta = 60, phi = 35)

wv.df$Source<-"WV"
ls.df$Source<-"ALS"
comb<-rbind(wv.df, ls.df)

setwd('Q:\\Forest Industry Informatics\\Projects\\TenonProject\\Reports\\')
png('PointCloud_SRTM_CU60_PF150824.png', w = 20, h=20, units="cm", res=300)
ggplot (comb, aes(X, Z)) + geom_point(aes(colour=factor(Source))) + theme_light() + labs(color = "Source")
dev.off()


######################################################
# Take mean values for all plots and graph to compare
#######################################################
ABG.lid.dir<-'D:\\WV\\ENVIformat\\Outputs\\clips\\clipsLidar\\virtual_plot_clips_non_normal\\ABG_only\\'
wv.dir<-'D:\\WV\\ENVIformat\\NormalisedLAS\\1_3\\nztm\\'

# read lidar points ABG only
setwd(ABG.lid.dir)
slist=list.files(ABG.lid.dir, pattern="las$", full.names=FALSE)
for(i in slist) { assign(unlist(strsplit(i, "[.]"))[1], as.data.frame(readLAS(i))) }

# read wv points ABG only
setwd(wv.dir)
wvlist=list.files(wv.dir, pattern="las$", full.names=FALSE)
for(i in wvlist) { assign(unlist(strsplit(i, "[.]"))[1], as.data.frame(readLAS(i))) }

# #List all objects in the environment with _PF in the name
list.lid <- mget(grep("_als",ls(),value=TRUE))
list.lid[[1]] <- NULL
IDs<-names(list.lid) # Get the object names from the list... seems to be easier to work with this
#IDs<-tail(IDs, -1) # remove the name of the output directory from the list

# #List all objects in the environment with _PF in the name
list.wv <- mget(grep("_wv",ls(),value=TRUE))
list.wv[[1]] <- NULL
ID2s<-names(list.wv) # Get the object names from the list... seems to be easier to work with this
#IDs<-tail(IDs, -1) # remove the name of the output directory from the list

#View all plots
for(i in 1:length(IDs))
{
  ls.df<-as.data.frame(get(IDs[i]))
  wv.df<-as.data.frame(get(ID2s[i]))
  wv.df$Source<-"WV"
  ls.df$Source<-"ALS"
  comb<-rbind(wv.df, ls.df)
  #png(paste(IDs[i], '.png', sep=""), w = 20, h=20, units="cm", res=300)
  print(ggplot (comb, aes(X, Z)) + geom_point(aes(colour=factor(Source))) + theme_light() + labs(color = "Source"))
  #dev.off()
}
  
  

# Loop through all the lidar files and calculate mean height
avg<-NULL
z<-NULL
h95<-NULL
for(i in 1:length(IDs))
{

  #print(mean(get(IDs[i])$Z))
  f<-subset(get(IDs[i]), ReturnNumber==1)
  #avg<-append(avg, mean(get(IDs[i])$Z))
  avg<-append(avg, mean(f$Z))
  h95<-append(h95, quantile(f$Z, c(.90), names=FALSE))
  z<-append(z, IDs[i])
  #print(IDs[i])
  #ggplot (f, aes(X, Z)) + geom_point(aes(colour=factor(Intensity))) + theme_light() + labs(color = "Return")
  
}

als.out<-data.frame(avg)
als.out$h95<-h95
als.out$Source='ALS'

# Loop through all the wvfiles and calculate mean height
avg<-NULL
z<-NULL
h95<-NULL
for(i in 1:length(ID2s))
{

  #print(mean(get(ID2s[i])$Z))
  avg<-append(avg, mean(get(ID2s[i])$Z))
  h95<-append(h95, quantile(get(ID2s[i])$Z, c(.90), names=FALSE))
  z<-append(z, ID2s[i])
  #print(ID2s[i])
}

wv.out<-data.frame(avg)
wv.out$h95<-h95
wv.out$Source='WV'

#combine 
av.out<-rbind(als.out, wv.out)

# boxplots of the mean z values

plot1<-ggplot(av.out, aes(as.factor(Source), avg)) + geom_boxplot(aes(colour=Source)) +theme_bw () + labs(x="Source", y="mean")
plot2<-ggplot(av.out, aes(as.factor(Source), h95)) + geom_boxplot(aes(colour=Source)) +theme_bw () + labs(x="Source", y="h90")
png('h90_avg.png', w = 20, h=12, units="cm", res=300)
grid.arrange(plot1, plot2, ncol=2)
dev.off()
ggplot(av.out, aes(x=h95, group=Source)) + geom_density(aes(group=Source, colour=Source, fill=Source),  alpha=.3)  + theme_light()


ggplot (av.out, aes(X, Z)) + geom_point(aes(colour=factor(Source))) + theme_light() + labs(color = "Source")

