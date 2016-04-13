# *************************************************************************************************************************
# This analysis is part of the Tenon project
# J.Dash
# Jonathan.dash@scionresearch.com
# **************************************************************************************************************************

# First task is to calculate Volume for the LUCAS plots

#Set libraries
library(RODBC)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(nls2)
library(party)


CpyVol<- function (Ht, BA)
{
  Ht*(BA)*(0.942*(Ht - 1.4)^-1.161+0.317)
}



# Get data
df<-read.csv('Q:\\Forest Industry Informatics\\Projects\\TenonProject\\LUCAS\\LUCAS_TreeData_2.csv') # Read the data based on the query of the Gateway
df$X<-as.numeric(as.character(df$X))
df$Y<-as.numeric(as.character(df$Y))

df$SurveyDate<-as.Date(df$SurveyDate, "%d/%m/%Y")

df_rec<-subset(df, df$SurveyDate > as.Date("2012-01-01")) # Filter on the date

# Format DBH and Height as numbers
df_rec$DBH<-as.numeric(as.character(df_rec$DBH))
df_rec$Height<-as.numeric(as.character(df_rec$Height))
df_rec$Cohort<-as.numeric(as.character(df_rec$Cohort))
df_rec$YearEstablished_Observed<-as.numeric(as.character(df_rec$YearEstablished_Observed))
df_rec$YearEstablished_StandRecords<-as.numeric(as.character(df_rec$YearEstablished_StandRecords))

df_rec<-subset(df_rec, df_rec$SpeciesCode == 'PINRAD') # Radiata only
df_rec<-subset(df_rec, df_rec$PlotNo == 1) # Central plot only
df_rec<-subset(df_rec, df_rec$Status == 'A') # Only include live trees
df_rec$YrEst<-ifelse(is.na(df_rec$YearEstablished_StandRecords) == "TRUE", df_rec$YearEstablished_Observed, df_rec$YearEstablished_StandRecords)
df_rec$PlantDate<-as.Date(paste('01/06/',df_rec$YrEst, sep=""), "%d/%m/%Y") # use a plant date of 1 June in est year

df_rec$Age=round((df_rec$SurveyDate - df_rec$PlantDate) /365,1) # Calculate age in years

# Format DBH and Height as numbers
df_rec$DBH<-as.numeric(as.character(df_rec$DBH))
df_rec$Height<-as.numeric(as.character(df_rec$Height))
df_rec$Cohort<-as.numeric(as.character(df_rec$Cohort))



# Prepare a dataset for the petterson regression
df_pet<-df_rec[,c('SurveyID', 'DBH', 'Height')] # Just useful data 
df_pet<-na.omit(df_pet)
df_pet<-subset(df_pet, DBH<1500)
plot(df_pet$Height~ df_pet$DBH)


#######This loop will fit a non-linear Petterson DBH ~ H regression to by stand and store coefficients in df called results
CmptStdList <- unique(df_pet$SurveyID)
results<-data.frame(coeff1=numeric(), coeff2=numeric(), SurveyID=factor(), n=numeric())
for (i in 1:length(CmptStdList))
#for (i in 1:2)
{  
  t<-subset(df_pet, df_pet$SurveyID == CmptStdList[i]) 
  #if (length(t$stem_height)>5)
  #t<-subset(df2, df2$PopStrat == 'HA5054_1')
  
  try(my.petterson <- nls(Height ~ 1.4 + (b+a/DBH)^2.5,data = t, start = list(a = 4, b = 0.08), trace = T)) # a = 0.5
  try(results<-rbind(results, data.frame(coeff1=summary(my.petterson)$coefficients[1], coeff2=summary(my.petterson)$coefficients[2], SurveyID=CmptStdList[i], n=length(t$Height))))
  #plot(t$DBH, t$ytgI, main=CmptStdList[i]) ###Plots for reference in RStudio viewer
  #abline(my.petterson)
}


######Use the coefficients in results to predict tree heights
dfout<- merge(df_rec,results, by="SurveyID")
dfout$pred_ht<-1.4 + (dfout$coeff2+dfout$coeff1/dfout$DBH)^2.5


# Calculate top height
results2<-data.frame(SurveyID=factor(), TopHeight=numeric())
for (i in 1:length(CmptStdList))
{
     t<-subset(dfout, dfout$SurveyID == CmptStdList[i]) 
     t2<-head(t[order(t$DBH,decreasing = TRUE), ], 2)
     results2<-rbind(results2, data.frame(SurveyID=CmptStdList[i], TopHeight=mean(t2$pred_ht)))
}


# Calcualte basal area and stocking
pl_sum<- dfout %>% group_by(SurveyID) %>% summarise(BA=(sum(0.00007854 * (DBH/10)^2)), 
                                                    BA_h=(sum(0.00007854 * (DBH/10)^2)/mean(PlotSize)),
                                                    sph=length(DBH)/mean(PlotSize),
                                                    PlotSize = mean(PlotSize),
                                                    Cohort = median(Cohort),
                                                    Age = median(Age), 
                                                    X = median (X),
                                                    Y = median (Y))

pl_sum<-merge(pl_sum, results2, by="SurveyID") #Merge in the top height
pl_sum$vol<-round(CpyVol(Ht=pl_sum$TopHeight, BA = pl_sum$BA_h),2) #Calculate volume
pl_sum$Age<-as.numeric(pl_sum$Age)

#Clean the dataset
pl_sum<-subset(pl_sum, TopHeight <100)
pl_sum<-subset(pl_sum, SurveyID != 'DT61_PF150812')
pl_sum<-subset(pl_sum, SurveyID != 'DF58_PF150922')
pl_sum<-subset(pl_sum, SurveyID != 'AW119_PF100725')
pl_sum<-subset(pl_sum, SurveyID != 'BZ102_PF100603')


png('LUCAS_PLOT.png', w = 17.5, h=17.5, units="cm", res=300)
par(mfrow=c(2,2))
plot(Age ~ TopHeight, pl_sum)
lines(0:50, predict(age.nlm , list(TopHeight = (0:50))), lty=1, col='red')
plot(Age ~ vol, pl_sum, xlab="Volume")
plot(TopHeight ~ BA_h, pl_sum, xlab="Basal Area")
plot(Age ~ Cohort, pl_sum, xlab="Pruned Lifts")
dev.off()

Cohort(pl_sum$Age, na.rm=T)

#th.nlm<-nls(TopHeight = (0.25 + (c - 0.25)(1 - exp(-a*Age)))*1/b, start=list(a = 40,b =1, c=8), data=pl_sum)
age.nlm<-nls(Age ~ -log(1-((TopHeight-0.25)/(c-0.25))^b)/a, start = list(a = 0.05695 , b = 0.6406, c = 49), nls.control(minFactor=0.0000000000000001), data=pl_sum)
summary(age.nlm)

#Plots for report
plot(Age ~ TopHeight, pl_sum)
lines(0:50, predict(age.nlm , list(TopHeight = (0:50))), lty=1, col='red')

plot(TopHeight ~ Age, pl_sum, ylab="Top Height (m)", xlab="Age (y)")
lines(predict(age.nlm , list(TopHeight = (0:50))), 0:50, lty=1, col='red')

mlt_pl_sum<-gather(pl_sum, variable, value,  -SurveyID)

# Make a shapefile for GIS checks
survey_shp<-df[c("SurveyID", "X", "Y")]
survey_shp_out<- survey_shp  %>% group_by(SurveyID) %>% summarise(X=median(X), Y=median(Y))
write.csv(survey_shp_out, file="LUCAS_Shape_all.csv")















