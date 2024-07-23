# Fitness Modeling of Phaseolus 
# by Alison, Spring 2024


###There is a command 
#### 1. Setup ####
# Libraries
library(rgdal)
library(naniar) # to homogenize missing value codes
library(ape)
library(tidyverse) # general data wrangling
library(supportR)
librarian::shelf(packReq)
library(ggplot2)
install.packages("usethis")
library(usethis)
library(ggcorrplot)
library(ggbiplot)
library(glmmTMB) # fitting generalised linear mixed models
library(ggpubr)
library(tidymodels)
library(tidyverse)
library(dplyr)
library(readxl)
library(raster)
library(sf)
library(terra)
library(tidyr)
library(dplyr)
library(car)
library(remotes)
library(Matrix)
library(lme4)
library(conflicted)
library(GGally)
require(devtools)
require(ggplot2)
library(pscl)
library(ggcorrplot)
library(factoextra)
library(vegan)
library(datacleanr)
library(boot)
library(factoextra)
library(naniar) # to homogenize missing value codes
library(tidyverse) # general data wrangling
library(supportR)
library(geosphere)
library(sp)
library(ape)
devtools::install_version("Matrix", version = "1.6-4", repos = "http://cran.us.r-project.org")

getwd()

# Working directory
setwd("C:/Users/asnyder/Desktop/P. filiformis/PhaseolusRepo/Data")

# Functions
'%notin%' <- Negate('%in%')

# Data
dates<-read_excel("Pfilifom_data.xlsx",sheet="DatesEct")
AllSA<-read_excel("Pfilifom_data.xlsx",sheet="AllLeafSA")
water<-read_excel("Pfilifom_data.xlsx",sheet="AverageWater")
morphdata<-read_excel("Pfilifom_data.xlsx",sheet="NondestructiveMeasurments")
anothercodesys<-read_excel("Pfilifom_data.xlsx",sheet="anothercode")
AllSAmerge <- merge(AllSA,anothercodesys, by="Idnum",all.x=TRUE)
morphMerge<-merge(morphdata,anothercodesys, by="Idnum",all.x=TRUE)
codesys<-read_excel("Pfilifom_data.xlsx",sheet="CodeSystem")
CO2<-read_excel("Pfilifom_data.xlsx",sheet="LeafDataDestruct")
CO2merge<-merge(CO2,anothercodesys, by="Idnum",all.x=TRUE)
seeds<-read_excel("Pfilifom_data.xlsx",sheet="SeedData")
seedsmerge<-merge(seeds,anothercodesys, by="Idnum")
WUE<-read_excel("Pfilifom_data.xlsx",sheet="WUE")
PlantDestruct<-read_excel("Pfilifom_data.xlsx",sheet="Destruct")
SLA<-read_excel("Pfilifom_data.xlsx",sheet="LeafDataDestruct")
SLAmerge<-merge(SLA,anothercodesys, by="Idnum",all.x=TRUE)
Ion<-read_excel("Pfilifom_data.xlsx",sheet="Ion")
Ionmerge<-merge(Ion,anothercodesys, by="Idnum",all.x=TRUE)
WUE<-read_excel("Pfilifom_data.xlsx",sheet="WUE")


###Bioclim and elevation Data

##terrian
sal<-raster("sq5.tif")
gps<-read.csv("GPSPhaseolus.csv")
elev<- raster("wc2.1_2.5m_elev.tif")
##precipitation data
preci1<-raster("wc2.1_2.5m_prec_01.tif")
is.na(preci1)
preci2<-raster("wc2.1_2.5m_prec_02.tif")
preci3<-raster("wc2.1_2.5m_prec_03.tif")
preci4<-raster("wc2.1_2.5m_prec_04.tif")
preci5<-raster("wc2.1_2.5m_prec_05.tif")
preci6<-raster("wc2.1_2.5m_prec_06.tif")
preci7<-raster("wc2.1_2.5m_prec_07.tif")
preci8<-raster("wc2.1_2.5m_prec_08.tif")
preci9<-raster("wc2.1_2.5m_prec_09.tif")
preci10<-raster("wc2.1_2.5m_prec_10.tif")
preci11<-raster("wc2.1_2.5m_prec_11.tif")
preci12<-raster("wc2.1_2.5m_prec_12.tif")
##temperature data
temp1<- raster("wc2.1_2.5m_tavg_01.tif")
temp2<- raster("wc2.1_2.5m_tavg_02.tif")
temp3<- raster("wc2.1_2.5m_tavg_03.tif")
temp4<- raster("wc2.1_2.5m_tavg_04.tif")
temp5<- raster("wc2.1_2.5m_tavg_05.tif")
temp6<- raster("wc2.1_2.5m_tavg_06.tif")
temp7<- raster("wc2.1_2.5m_tavg_07.tif")
temp8<- raster("wc2.1_2.5m_tavg_08.tif")
temp9<- raster("wc2.1_2.5m_tavg_09.tif")
temp10<- raster("wc2.1_2.5m_tavg_10.tif")
temp11<- raster("wc2.1_2.5m_tavg_11.tif")
temp12<- raster("wc2.1_2.5m_tavg_12.tif")
###bioclimdata
annualtemp<-raster("wc2.1_2.5m_bio_1.tif")
meandiurnalrange<-raster("wc2.1_2.5m_bio_2.tif")
isotherm<-raster("wc2.1_2.5m_bio_3.tif")
tempseason<-raster("wc2.1_2.5m_bio_4.tif")
maxtempwarm<-raster("wc2.1_2.5m_bio_5.tif")
mintempcold<-raster("wc2.1_2.5m_bio_6.tif")
tempannualrange<-raster("wc2.1_2.5m_bio_7.tif")
meantempwet<-raster("wc2.1_2.5m_bio_8.tif")
meantempdry<-raster("wc2.1_2.5m_bio_9.tif")
meantempwarm<-raster("wc2.1_2.5m_bio_10.tif")
meantempcold<-raster("wc2.1_2.5m_bio_11.tif")
annualpreci<-raster("wc2.1_2.5m_bio_12.tif")
precipwettest<-raster("wc2.1_2.5m_bio_13.tif")
precipdriest<-raster("wc2.1_2.5m_bio_14.tif")
seasonalprecip <-raster("wc2.1_2.5m_bio_15.tif")
precipwetquarter<-raster("wc2.1_2.5m_bio_16.tif")
precipdriestquarter<-raster("wc2.1_2.5m_bio_17.tif")
precipwarmestquarter<-raster("wc2.1_2.5m_bio_18.tif")
precipcoldestquarter<-raster("wc2.1_2.5m_bio_19.tif")

###2. Cleaning up data####

#-- all of those that did not GERMINATE or were eaten by RATS will get NA values. All that died will get 0's 
summedSeeds<-0
summedSeeds <- seeds %>%
  group_by(ID,Rep) %>%
  summarise(TotalSeeds = sum(NumSeeds, na.rm = TRUE), Pods = sum(numpods, na.rm = TRUE), AverageSeedsPerPod = mean(SeedperPod,na.rm=TRUE), AverageSeedWeight = mean(Weightseed,na.rm=TRUE), TotalSeedWeight=mean(SeedsWeight, na.rm=TRUE))
summedSeeds
seedsmerge<-0
seedsmerge<-merge(summedSeeds,PlantDestruct, by="ID", all=TRUE)

seedsmerge <- seedsmerge %>%
  dplyr::mutate(Rep.x = coalesce(Rep.x, Rep.y)) %>%  # Fill missing values in Rep.x with Rep.y
  dplyr::select(-Rep.y) %>%                         # Omit the column Rep.y
  dplyr::rename(Rep = Rep.x)   

seedsmerge$TotalPods<-seedsmerge$Pods+seedsmerge$NumemptyPods+seedsmerge$Numgreenpods
seedsmerge<- seedsmerge%>%
  dplyr::select(ID,TotalSeeds,Rep,TotalPods,AverageSeedsPerPod,AverageSeedWeight,EC,Moisture,Numleaves,NumFlowers,NumemptyPods,Numgreenpods,TotalAboveDryMass,AboveWater,WF)

seedsmerge<-merge(seedsmerge,dates,by="ID",all=TRUE)
seedsmerge <- seedsmerge %>%
  dplyr::mutate(Rep.x = coalesce(Rep.x, Rep.y)) %>%  # Fill missing values in Rep.x with Rep.y
  dplyr::select(-Rep.y) %>%                         # Omit the column Rep.y
  dplyr::rename(Rep = Rep.x) 

seedsmerge <- seedsmerge %>%
  rename(Idnum = IDnum)
seedsmerge<- seedsmerge%>%
  dplyr::select(ID,Idnum,Species, Rep,Treatment, ReasonofDeath, Germ, Accession, TotalSeeds,Alive,AverageSeedWeight, TotalPods,AverageSeedsPerPod,EC,Moisture,Numleaves,NumFlowers,NumemptyPods,Numgreenpods,TotalAboveDryMass,AboveWater,WF) 
seedsmerge<- seedsmerge %>% dplyr::filter(!is.na(ID))


###eaten by rats or didnt germinate will be left NA. Only pods with NA will be changed to 0, rest will be NA
seedsmerge<- seedsmerge%>%
  dplyr:: filter (Idnum != 18 & 28)
seedsmerge <- seedsmerge %>%
  mutate(
    TotalSeeds = case_when(
      Germ == 0 | ReasonofDeath == "RATS" ~ NA,
      Germ == 1 & Alive == 0 & (TotalPods == 0| is.na(TotalPods)) ~ NA,
      Germ==1 &Alive==1 & (is.na(TotalPods)| TotalPods>0) & is.na(TotalSeeds)~ NA,
      TRUE ~ TotalSeeds
    ),
    TotalPods = case_when(
      Germ == 0 | ReasonofDeath == "RATS" ~ NA,
      Germ == 1 & Alive == 0 & (TotalPods == 0| is.na(TotalPods))|(Germ == 0 & Species=="P. vulgaris") ~ 0,
      Germ==1 &Alive==1 & (is.na(TotalPods)| TotalPods>0) & is.na(TotalPods)~ 0,
      TRUE ~ TotalPods
    ),
    AverageSeedWeight = case_when(
      Germ == 0 | ReasonofDeath == "RATS" ~ NA,
      Germ == 1 & Alive == 0 & (TotalPods == 0| is.na(TotalPods)) ~ NA,
      AverageSeedWeight == 0 ~ NA,
      TRUE ~ AverageSeedWeight
    ),
    AverageSeedsPerPod = case_when(
      Germ == 0 | ReasonofDeath == "RATS" ~ NA,
      Germ == 1 & Alive == 0 & (TotalPods == 0| is.na(TotalPods)) ~ NA,
      AverageSeedsPerPod == 0 ~ NA,
      TRUE ~ AverageSeedsPerPod
    ),
   Numleaves = case_when(
          Germ == 0 | ReasonofDeath == "RATS" ~ NA,
          Germ == 1 & Alive == 0 & (TotalPods == 0| is.na(TotalPods)) ~ NA,
          Numleaves == 0~ NA,
          TRUE ~ Numleaves
    ),
    TotalAboveDryMass = case_when(
      Germ == 0 | ReasonofDeath == "RATS" ~ NA,
      Germ == 1 & Alive == 0 & (TotalPods == 0| is.na(TotalPods)) ~0,
      TotalAboveDryMass == 0~ 0,
      TRUE ~ TotalAboveDryMass
    ),
   AboveWater = case_when(
     Germ == 0 | ReasonofDeath == "RATS" ~ NA,
     Germ == 1 & Alive == 0 & (TotalPods == 0| is.na(TotalPods)) ~0,
     AboveWater == 0~ 0,
     TRUE ~ AboveWater
   ),
   WF = case_when(
     Germ == 0 | ReasonofDeath == "RATS" ~ NA,
     Germ == 1 & Alive == 0 & (TotalPods == 0| is.na(TotalPods)) ~0,
     WF == 0~ 0,
     TRUE ~ WF
   )
  )

seedsmerge<- seedsmerge%>%
  dplyr:: filter (Idnum != 18 & 28) ##none of 18 or 28 germinated
write.csv(seedsmerge,"seedsmerge.csv")

fitness<- seedsmerge%>%
  dplyr::select(ID,Idnum,Rep,Treatment, Germ, Accession,Alive,AverageSeedWeight,AverageSeedsPerPod, TotalPods,TotalAboveDryMass,Species,WF,AboveWater)

fitnesfili<-fitness%>%
  dplyr::filter(Species=="P. filiformis")

fitnessvulgaris<-fitness%>%
  dplyr::filter(Species=="P. vulgaris")

###This is for predicting rep 3--- no longer need to do this
anothercodesysSpecies<-anothercodesys%>%
  dplyr::select(Species,Idnum)


fitness<-merge(fitness,anothercodesys,by="Idnum")
fitness<-fitness %>%
  dplyr::select(-Accession.y)%>%
  dplyr::rename(Accession = Accession.x)

fili<-fitness%>%
  dplyr::filter(Species.x=="P. filiformis")

#### 3. Predict NA's ####

##Average Seed Weight
fitnesmodelSeedWeight<-lmer(log(AverageSeedWeight)~Accession+Treatment+(1|Rep),data=fili ,na.action = "na.exclude")
PredicSeedWeight<-exp(predict (fitnesmodelSeedWeight, newdata = fili))
fili <-fili %>%
  mutate(AverageSeedWeight = ifelse(is.na(AverageSeedWeight), PredicSeedWeight, AverageSeedWeight))

###glmer with rep being randomized-- keep NA's and zeros
# Predict values--TotalPods

fitnesmodelPod<-glmer(TotalPods~Accession+Treatment+(1|Rep),data=fili, family=poisson(),na.action = "na.exclude")
PredicPod<-predict(fitnesmodelPod, type = "response",newdata=fili)
sum(is.na(fili$TotalPods), na.rm = TRUE)

fili <-fili %>%
  mutate(TotalPods = ifelse(is.na(TotalPods), PredicPod, TotalPods))

### Now changing Number of SeedPods with predicted values
fitnesmodelSeedPod<-lmer(log(AverageSeedsPerPod)~Accession+Treatment+ (1|Rep),data=fili ,na.action = "na.exclude")

fili <-fili %>%
  mutate(naPredicSeedPod = exp(predict (fitnesmodelSeedPod, newdata = fili))) %>%
  mutate(AverageSeedsPerPod = ifelse(is.na(AverageSeedsPerPod), naPredicSeedPod, AverageSeedsPerPod))

### Above Ground Biomass-- a lot of missing data for rep 3 so won't do right now 
fitnesmodelBiomass<-lmer(log(TotalAboveDryMass+.00001)~Accession+Treatment+ (1|Rep),data=fili ,na.action = "na.exclude")

fili <-fili %>%
  mutate(naTotalAboveDryMass = exp(predict (fitnesmodelBiomass, newdata = fili))) %>%
  mutate(TotalAboveDryMass = ifelse(is.na(TotalAboveDryMass), naTotalAboveDryMass, TotalAboveDryMass))


##abovewater
fitnesmodelAboveWater<-lmer(log(AboveWater+.00001)~Accession+Treatment+ (1|Rep),data=fili ,na.action = "na.exclude")
PredicAboveWater<-predict(fitnesmodelAboveWater, type = "response",newdata=fili)
fili <-fili %>%
  mutate(AboveWater = ifelse(is.na(AboveWater), PredicAboveWater, AboveWater))
##WF
fitnesmodelWF<-lmer(log(WF+.0001)~Accession+Treatment+ (1|Rep),data=fili ,na.action = "na.exclude")
PredicWF<-predict(fitnesmodelWF, type = "response",newdata=fili)
fili <-fili %>%
  mutate(WF = ifelse(is.na(WF), PredicWF, WF))

sum(is.na(fili$WF))

write.csv(fili,"filifitness.csv")

fitness <- fitness %>%
  rename(Species= Species.x)%>%
  dplyr::select( -Species.y)



#4. vulgaris + filiformis ####-- so for now, there were only 2 that didn't germinate, so I am just going to take the average of the 2 reps to fill it 


fitnessvulgaris<-fitness%>%
  dplyr::filter(Species=="P. vulgaris")

###average seed weight 

fitnessvulgaris<- fitnessvulgaris %>%
  group_by(Idnum, Treatment) %>%
  dplyr::mutate(
    mean_weight = mean(AverageSeedWeight, na.rm = TRUE),
    mean_weight = ifelse(is.na(mean_weight), 0, mean_weight),
    AverageSeedWeight = ifelse(is.na(AverageSeedWeight), mean_weight, AverageSeedWeight)
  ) %>%
  dplyr::select(-mean_weight) %>%
  ungroup()

###pods

fitnessvulgaris<- fitnessvulgaris %>%
  group_by(Idnum, Treatment) %>%
  dplyr::mutate(
    mean_pods = mean(TotalPods, na.rm = TRUE),
    mean_pods = ifelse(is.na(mean_pods), 0, mean_pods),
    TotalPods = ifelse(is.na(TotalPods), mean_pods, TotalPods)
  ) %>%
  dplyr::select(-mean_pods) %>%
  ungroup()

### Now changing Number of SeedPods with predicted values
fitnessvulgaris<- fitnessvulgaris %>%
  group_by(Idnum, Treatment) %>%
  dplyr::mutate(
    mean_seed = mean(AverageSeedsPerPod, na.rm = TRUE),
    mean_seed  = ifelse(is.na(mean_seed ), 0, mean_seed ),
    AverageSeedsPerPod = ifelse(is.na(AverageSeedsPerPod), mean_seed , AverageSeedsPerPod)
  ) %>%
  dplyr::select(-mean_seed ) %>%
  ungroup()
### Above Ground Biomass-- a lot of missing data for rep 3 so won't do right now 

fitnessvulgaris<- fitnessvulgaris %>%
  group_by(Idnum, Treatment) %>%
  dplyr::mutate(
    mean_biomass = mean(TotalAboveDryMass, na.rm = TRUE),
    mean_biomass = ifelse(is.na(mean_biomass), 0, mean_biomass),
    TotalAboveDryMass = ifelse(is.na(TotalAboveDryMass), mean_biomass, TotalAboveDryMass)
  ) %>%
  dplyr::select(-mean_biomass) %>%
  ungroup()
fitnesss<-0
fitness<-bind_rows(fitnessvulgaris,fili)
fitness <- fitness %>%
  mutate(Species = ifelse(is.na(Species), Species.x, Species))%>%
  dplyr::select(-Species.x, -Species.y)
fitness<-merge(fitness, anothercodesys[,1:3], by="Idnum")
fitness<-unique(fitness)
fitness <- fitness %>%
  rename(Species= Species.x)%>%
  dplyr::select( -Species.y)
write.csv(fitness,"allfitness.csv")


###4. Terrain and Bioclim####
bioclimsd<-0

###Stacking similar plots
precip_stack<- stack(preci1,preci2,preci3,preci4,preci5,preci6,preci7,preci8,preci9,preci10,preci11,preci12)
precip<-raster::stack(precip_stack)
temp_stack<- stack(temp1,temp2,temp3,temp4,temp5,temp6,temp7,temp8,temp9,temp10,temp11,temp12)
temp<-raster::stack(temp_stack)
masterstack<-c(annualtemp,meandiurnalrange,isotherm,tempseason,maxtempwarm,mintempcold,tempannualrange,meantempwet,meantempdry,meantempwarm,meantempcold,annualpreci,precipwettest,precipdriest,seasonalprecip,precipwetquarter,precipdriestquarter,precipwarmestquarter,precipcoldestquarter,temp,precip)

###Getting all accession GPS Coordinates###
gps <- gps %>%
  mutate(Idnum = sprintf("%02d", Idnum))
bioclimsd<- bioclimsd %>% rename(Accession = accession)
gps <- gps %>%
  dplyr::filter(Species=="P. filiformis")
gps_coor<- data.frame(gps$Accession, gps$Idnum, Latitude = gps$Lat, Longitide=gps$Long)
gps_coords<-SpatialPoints(data.frame(x=gps$Long,y=gps$Lat), proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs "))


## making a matrix of all of the precipitation data for every month at each accession coordinate
bioclimsd<-data.frame(gps$Idnum)
bioclimsd$species<-gps$Species#bioclimsd which I will add all variables too
precip_at_coords<-terra::extract(precip_stack,gps_coords)
np_rows<-nrow(precip_at_coords)
np_cols<-ncol(precip_at_coords)
np_rows
combine_precip <- matrix(nrow=35,ncol=2)
colnames(combine_precip) <- c("Idnum", "AnnualPrecipitation")
combine_precip[,1]<-gps$Idnum
combine_precip

x<-0
for (i in 1:35){
  for(z in 1:12){
    x=x+precip_at_coords[i,z]
  }
  combine_precip[i,2]<-x
  x<-0
}
combine_precip
bioclimsd$AnnualPrecip<-combine_precip

### making a matrix of all the average temperature data for every month at each accession coordinate
temp_at_coords<-terra::extract(temp_stack,gps_coords)
temp_at_coords
nt_rows<-nrow(temp_at_coords)
nt_cols<-ncol(temp_at_coords)

avg_temp <- matrix(nrow=35,ncol=1)
y<-0
for (i in 1:35){
  for(z in 1:12){
    y=y+temp_at_coords[i,z]
  }
  y<-y/12
  avg_temp[i,1]<-y
  y<-0
}
print(avg_temp)
bioclimsd$Average_temp<-avg_temp
bioclimsd<-bioclimsd %>%

### Soils 
gps$Idnum
salsd<-data.frame(gps$Idnum)
salinity<-matrix(nrow=35,ncol=2)
sal_at_coords<-terra::extract(sal,gps_coords)
salsd$Salinity_class<-sal_at_coords
salsd <- salsd %>% rename(Idnum = gps.Idnum)
salsd<-merge(salsd,anothercodesys, by="Idnum")


##bioclim

bioclimsd$Lat<-gps$Lat
bioclimsd$Long<-gps$Long



Mean_diurnal_range<- terra::extract(meandiurnalrange,gps_coords)
bioclimsd$Mean_Diurnal_Range<-Mean_diurnal_range

isothermality<- terra::extract(isotherm,gps_coords)
bioclimsd$Isothermality<-isothermality

temperature_seasonality<- terra::extract(tempseason,gps_coords)
bioclimsd$Temperature_seasonality<-temperature_seasonality

max_temp_warmest_month<- terra::extract(maxtempwarm,gps_coords)
bioclimsd$Max_Temp_Warmest_Month<-max_temp_warmest_month

min_temp_coolest_month<- terra::extract(mintempcold,gps_coords)
bioclimsd$Min_Temp_Coolest_Month<-min_temp_coolest_month

temp_annual_range<- terra::extract(tempannualrange,gps_coords)
bioclimsd$Temp_Annual_Range<-temp_annual_range

mean_temp_wettest_quarter<- terra::extract(meantempwet,gps_coords)
bioclimsd$Mean_Temp_Wettest_Quarter<-mean_temp_wettest_quarter

Mean_temp_driest_quarter<- terra::extract(meantempdry,gps_coords)
bioclimsd$Mean_Temp_Driest_Quarter<-Mean_temp_driest_quarter

mean_temp_warmest_quarter<- terra::extract(meantempwarm,gps_coords)
bioclimsd$Mean_Temp_Warmest_Quarter<-mean_temp_warmest_quarter

mean_temp_coolest_quarter<- terra::extract(meantempcold,gps_coords)
bioclimsd$Mean_Temp_Coolest_Quarter<-mean_temp_coolest_quarter

annual_precip<- terra::extract(annualpreci,gps_coords)
bioclimsd$Annual_Precip<-annual_precip

precip_wettest_month<- terra::extract(precipwettest,gps_coords)
bioclimsd$Precip_Wettest_Month<-precip_wettest_month

precip_driest_month<- terra::extract(precipdriest,gps_coords)
bioclimsd$Precip_Driest_Month<-precip_driest_month

precip_seasonality<- terra::extract(seasonalprecip,gps_coords)
bioclimsd$Precip_Seasonality<-precip_seasonality

precip_wettest_quarter<- terra::extract(precipwetquarter,gps_coords)
bioclimsd$Precip_Wettest_Quarter<-precip_wettest_quarter

precip_driest_quarter<- terra::extract(precipdriestquarter,gps_coords)
bioclimsd$Precip_Driest_Quarter<-precip_driest_quarter

precip_warmest_quarter<- terra::extract(precipwarmestquarter,gps_coords)
bioclimsd$Precip_Warmest_Quarter<-precip_warmest_quarter

precip_coldest_quarter<- terra::extract(precipcoldestquarter,gps_coords)
bioclimsd$Precip_Coolest_Quarter<-precip_coldest_quarter

##Terain Data
elevation<- terra::extract(elev,gps_coords)
elevation<-as.numeric(elevation)
bioclimsd$elevation<-elevation

slop<-terra::terrain(elev,"slope")
slopee<-terra::extract(slop,gps_coords)
slopee<- as.numeric(slopee)
bioclimsd$slope<-slopee

aspectt<-terra::terrain(elev,"aspect")
asspectt<-terra::extract(aspectt,gps_coords)
asspectt<-as.numeric(asspectt)
bioclimsd$aspect<-asspectt

roughnes<-terra::terrain(elev,"roughness")
roghness<-terra::extract(roughnes,gps_coords)
roghness<-as.numeric(roghness)
bioclimsd$roughness<-roghness

TPpI<-terra::terrain(elev,"TPI")
tpii<-terra::extract(TPpI,gps_coords)
tpii<-as.numeric(tpii)
bioclimsd$TPI<-tpii

TRIi<- terra::terrain(elev,"TRI")
TRiIi<-terra::extract(TRIi,gps_coords)
TRiIi<-as.numeric(TRiIi)
bioclimsd$TRI<-TRiIi

flow_dirr<-terra::terrain(elev,"flowdir")
flow_dirre<-terra::extract(flow_dirr,gps_coords)
flow_dirre<-as.numeric(flow_dirre)
bioclimsd$flow_direction<-flow_dirre


##only filiformis 

names(bioclimsd)
bioclimsd<-bioclimsd%>%
  dplyr::filter(species=="P. filiformis")
bioclimsd<-bioclimsd%>%
  dplyr::rename(Idnum="gps.Idnum")
bioclimsd<-merge(bioclimsd,salsd, by="Idnum")
bioclimsd <- bioclimsd %>%
  mutate(Idnum = sprintf("%02d", Idnum))
write.csv(bioclimsd, "bioclimsd.csv")

#with spatial points for Morgans stuff 

sp_bioclim_df <- SpatialPointsDataFrame(gps_coords, data = bioclimsd)
writeOGR(sp_bioclim_df, dsn = "sp_bioclim_df.shp", layer = "sp_bioclim_df", driver = "ESRI Shapefile")