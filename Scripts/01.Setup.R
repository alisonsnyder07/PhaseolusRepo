# Fitness Modeling of Phaseolus 
# by Alison, Spring 2024



#### 1. Setup ####
# Libraries
library(naniar) # to homogenize missing value codes
library(tidyverse) # general data wrangling
library(supportR)
librarian::shelf(packReq)
library(ggplot2)
install.packages("usethis")
library(usethis)
library(ggcorrplot)
library(ggbiplot)
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
      Germ == 1 & Alive == 0 & (TotalPods == 0| is.na(TotalPods)) ~ NA,
      TotalAboveDryMass == 0~ NA,
      TRUE ~ TotalAboveDryMass
    )
  )

fitness<- seedsmerge%>%
  dplyr::select(ID,Idnum,Rep,Treatment, Germ, Accession,Alive,AverageSeedWeight,AverageSeedsPerPod, TotalPods,TotalAboveDryMass,Species)

fitness<- fitness %>%
  dplyr:: filter (Idnum != 18 & 28) ##none of 18 or 28 germinated

fitnesfili<-fitness%>%
  dplyr::filter(Species=="P. filiformis")

fitnessvulgaris<-fitness%>%
  dplyr::filter(Species=="P. vulgaris")

###This is for predicting rep 3--- no longer need to do this
anothercodesysSpecies<-anothercodesys%>%
  dplyr::select(Species,Accession)

fitness<-merge(fitness,anothercodesysSpecies,by="Accession")

#rep3 = fitness %>%
  #dplyr::select(-Accession)


#fitness <- fitness %>%
  #dplyr::filter(Rep !=3 )


#FAKE Data for 3rd rep
#rep3 = fitness %>%
  #dplyr::filter(Rep ==2)
#names(rep3)
#rep3$AverageSeedWeight_fake <-rep3$AverageSeedWeight + rnorm(nrow(rep3), 0, sd (fitness$AverageSeedWeight, na.rm = TRUE))
#rep3$AverageSeedPod_fake <-rep3$AverageSeedsPerPod + rnorm(nrow(rep3), 1, sd (fitness$AverageSeedsPerPod, na.rm = TRUE))
#rep3$AveragTotalPodPod_fake <-round(rep3$TotalPods +rnorm(nrow(rep3), 1, sd (fitness$TotalPods, na.rm = TRUE)))

#rep3 <-rep3 %>%
  #mutate(AverageSeedPod_fake  = ifelse(AverageSeedPod_fake <0, 0.0001, AverageSeedPod_fake ))

#rep3 <-rep3 %>%
  #mutate(AveragTotalPodPod_fake = ifelse(AveragTotalPodPod_fake<0, 0, AveragTotalPodPod_fake))

#rep3 <-rep3 %>%
  #mutate(AverageSeedWeight_fake = ifelse(AverageSeedWeight_fake<0, 0.01, AverageSeedWeight_fake))
#rep3$AveragTotalPodPod_fake


#rep3$AverageSeedWeight<-rep3$AverageSeedWeight_fake 
#rep3$AverageSeedsPerPod<-rep3$AverageSeedPod_fake 
#rep3$TotalPods<-rep3$AveragTotalPodPod_fake 

#rep3<- rep3%>%
  #dplyr::select(-AveragTotalPodPod_fake ,-AverageSeedPod_fake ,-AverageSeedWeight_fake)

#rep3$Rep<-3

#fitness<-rbind(fitness,rep3)

###The formula we will by using is seeds/pod * #pods * weight/pod

fili<-fitness%>%
  dplyr::filter(Species.x=="P. filiformis")

#### 3. Predict NA's ####

##Average Seed Weight
fitnesmodelSeedWeight<-lmer(log(AverageSeedWeight)~Accession+Treatment+(1|Rep),data=fili ,na.action = "na.exclude")
PredicSeedWeight<-exp(predict (fitnesmodelSeedWeight, newdata = fili))
fili <-fili %>%
  mutate(AverageSeedWeight = ifelse(is.na(AverageSeedWeight), PredicSeedWeight, AverageSeedWeight))



###glmer with rep being randomized-- keep NA's and zeros

fitnesmodelPod<-glmer(TotalPods~Accession+Treatment+(1|Rep),data=fili, family=poisson(),na.action = "na.exclude")
PredicPod<-predict(fitnesmodelPod, type = "response",newdata=fili)
sum(is.na(fili$TotalPods), na.rm = TRUE)

fili <-fili %>%
  mutate(TotalPods = ifelse(is.na(TotalPods), PredicPod, TotalPods))
Anova(fitnesmodelPod) ##number of pods correlated with Accession and Treatment 

# Predict values--TotalPods


### Now changing Number of SeedPods with predicted values
fitnesmodelSeedPod<-lmer(log(AverageSeedsPerPod)~Accession+Treatment+ (1|Rep),data=fili ,na.action = "na.exclude")

fili <-fili %>%
  mutate(naPredicSeedPod = exp(predict (fitnesmodelSeedPod, newdata = fili))) %>%
  mutate(AverageSeedsPerPod = ifelse(is.na(AverageSeedsPerPod), naPredicSeedPod, AverageSeedsPerPod))

### Above Ground Biomass-- a lot of missing data for rep 3 so won't do right now 
fitnesmodelBiomass<-lmer(log(TotalAboveDryMass)~Accession+Treatment+ (1|Rep),data=fili ,na.action = "na.exclude")

fili <-fili %>%
  mutate(naTotalAboveDryMass = exp(predict (fitnesmodelBiomass, newdata = fili))) %>%
  mutate(TotalAboveDryMass = ifelse(is.na(TotalAboveDryMass), naTotalAboveDryMass, TotalAboveDryMass))

tibble::view(fili)

write.csv(fili,"01fitness.csv")
fitness <- fitness %>%
  rename(Species= Species.x)%>%
  dplyr::select( -Species.y)
write.csv(fitness,"allfitness.csv")
tibble::view(fili)



#4. vulgaris + filiformis ####-- so for now, there were only 2 that didn't germinate, so I am just going to take the average of the 2 reps to fill it 

tibble::view(fitnessvulgaris)

fitnessvulgaris<-fitness%>%
  dplyr::filter(Species=="P. vulgaris")


fitnessvulgaris<- fitnessvulgaris %>% mutate_all(~ ifelse(is.nan(.), NA, .))
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

###pods--- 0 inflated

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
fitnesmodelBiomass<-lmer(log(TotalAboveDryMass)~Accession+Treatment+ (1|Rep),data=fili ,na.action = "na.exclude")

fitnessvulgaris<- fitnessvulgaris %>%
  group_by(Idnum, Treatment) %>%
  dplyr::mutate(
    mean_biomass = mean(TotalAboveDryMass, na.rm = TRUE),
    mean_biomass = ifelse(is.na(mean_biomass), 0, mean_biomass),
    TotalAboveDryMass = ifelse(is.na(TotalAboveDryMass), mean_biomass, TotalAboveDryMass)
  ) %>%
  dplyr::select(-mean_biomass) %>%
  ungroup()

fitness<-bind_rows(fitnessvulgaris,fili)
fitness <- fitness %>%
  mutate(Species = ifelse(is.na(Species), Species.x, Species))%>%
  dplyr::select(-Species.x, -Species.y)
fitness<-merge(fitness, anothercodesys[,3:4], by="Accession")
write.csv(fitness,"allfitness.csv")


#5. Looking at PCA as a fitness Score ####
## Need to redo with new data--- didn't save :(


##regular anova

seedsmergeG <- seedsmerge%>%
  dplyr::filter(Germ == 1)
anovapods<-lm(TotalPods~0+Rep+Accession,data=fili)
pred_df = expand.grid(Rep = unique (seedsmergeG$Rep),
                      Accession = unique (seedsmergeG$Accession))
est_Pods <- predict(anovapods, pred_df)
pred_df$est_Pods <-est_Pods

pred_df
lmpred<-lm(est_Pods~Rep,data=pred_df)
anova(lmpred)## significantly correlated with accession and with rep 

##variable for PCA 2---seeaccession##variable for PCA 2---seedsperpod

anovaAverageSeedsPerPod<-lm(AverageSeedsPerPod~ 0 + Rep + Accession,data=fili)
predseedsperpod_df = expand.grid(Rep = unique (seedsmergeG$Rep),
                      Accession = unique (seedsmergeG$Accession))
est_AverageSeedsPerPod <- predict(anovaAverageSeedsPerPod,predseedsperpod_df)
predseedsperpod_df$est_AverageSeedsPerPod <-est_AverageSeedsPerPod
predseedsperpod_df



##variable for PCA 3---MeanSeedWeight

anovaAverageSeedWeight<-lm(AverageSeedWeight~0 + Rep+Accession,data=seedsmergeG)
AverageSeedWeight_df = expand.grid(Rep = unique (seedsmergeG$Rep),
                                 Accession = unique (seedsmergeG$Accession))
est_AverageSeedWeight <- predict(anovaAverageSeedWeight, AverageSeedWeight_df)
AverageSeedWeight_df$est_AverageSeedWeight <-est_AverageSeedWeight
AverageSeedWeight_df2<-AverageSeedWeight_df%>% filter(Rep==2)
AverageSeedWeight_df2<-merge(anothercodesys,AverageSeedWeight_df2, by="Accession")
AverageSeedWeight_df2
AverageSeedWeight_df

##numberLeaves

anovaLeaves<-lm(Numleaves~0 + Rep+Accession,data=seedsmergeG)
AverageNumLeaves_df = expand.grid(Rep = unique (seedsmergeG$Rep),
                                   Accession = unique (seedsmergeG$Accession))
est_NumLeaves_df <- predict(anovaLeaves, AverageNumLEaves_df)
AverageNumLeaves_df$est_NumLeaves_df <-est_NumLeaves_df
AverageNumLeaves_df2<-AverageNumLeaves_df%>% filter(Rep==2)

AverageNumLeaves_df2

###variable for PCA4-- aboveground dry biomass-- only have Rep1- all that are dead = 0, all that did not germinate = NA?
names(seedsmerge)
seedsmerge1<-seedsmergeG%>% filter(Rep==1)
biomass1<- seedsmerge1 %>%
  select(Accession, TotalAboveDryMass)
biomass1$ID <- paste(biomass1$Accession, sep = "_")
biomass1
seedsmerge1

PCA_df<-0
PCA_df<-merge(pred_df2,predseedsperpod_df2,by="Accession",all.y=FALSE,all.x=FALSE)
PCA_df<-merge(PCA_df,AverageSeedWeight_df2, by="Accession")
PCA_df<-merge(PCA_df,AverageNumLeaves_df2,by="Accession")
PCA_df <- unique(PCA_df)
names(PCA_df)
PCA_df<- PCA_df%>%
  select(est_Pods, est_AverageSeedsPerPod, est_AverageSeedWeight,est_NumLeaves_df,Accession)
PCA_df<-merge(PCA_df,anothercodesys,by="Accession")
PCA_df

PCA_df<- PCA_df%>%filter(Species=="P. filiformis")
PCA_df
PCA_df<- PCA_df%>%
  select(est_Pods, est_AverageSeedsPerPod,est_AverageSeedWeight,est_NumLeaves_df)

corr_matrix <- cor(PCA_df)
ggcorrplot(corr_matrix)

data.pca <- princomp(corr_matrix)
summary(data.pca)


data.pca$loadings[, 1:2]
fviz_eig(data.pca, addlabels = TRUE)
fviz_pca_var(data.pca, col.var = "black")

###5. Terrain and Bioclim####
bioclimsd<-0

###Stacking similar plots
precip_stack<- stack(preci1,preci2,preci3,preci4,preci5,preci6,preci7,preci8,preci9,preci10,preci11,preci12)
precip<-raster::stack(precip_stack)
temp_stack<- stack(temp1,temp2,temp3,temp4,temp5,temp6,temp7,temp8,temp9,temp10,temp11,temp12)
temp<-raster::stack(temp_stack)
masterstack<-c(annualtemp,meandiurnalrange,isotherm,tempseason,maxtempwarm,mintempcold,tempannualrange,meantempwet,meantempdry,meantempwarm,meantempcold,annualpreci,precipwettest,precipdriest,seasonalprecip,precipwetquarter,precipdriestquarter,precipwarmestquarter,precipcoldestquarter,temp,precip)

###Getting all accession GPS Coordinates###
gps
sspecies<-gps$Species
accession<-gps$Accession
bioclimsd<- bioclimsd %>% rename(Accession = accession)

lat<-gps$Lat
long<-gps$Long
lat
long
gps_coor<- data.frame(Name=accession, Latitude = lat, Longitide=long)
gps_coords<-SpatialPoints(data.frame(x=long,y=lat), proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs "))
class(gps_coords)

## making a matrix of all of the precipitation data for every month at each accession coordinate
bioclimsd<-data.frame(accession)
bioclimsd$species<-sspecies#bioclimsd which I will add all variables too
precip_at_coords<-terra::extract(precip_stack,gps_coords)
class(precip_at_coords)
np_rows<-nrow(precip_at_coords)
np_cols<-ncol(precip_at_coords)
np_rows
combine_precip <- matrix(nrow=45,ncol=2)
colnames(combine_precip) <- c("Accession", "AnnualPrecipitation")
combine_precip[,1]<-accession
combine_precip
class(precip_at_coords)


x<-0
for (i in 1:45){
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

avg_temp <- matrix(nrow=45,ncol=1)
y<-0
for (i in 1:45){
  for(z in 1:12){
    y=y+temp_at_coords[i,z]
  }
  y<-y/12
  avg_temp[i,1]<-y
  y<-0
}
print(avg_temp)
bioclimsd$Average_temp<-avg_temp
bioclimsd

### Soils 
salsd<-data.frame(accession)
salinity<-matrix(nrow=54,ncol=2)
sal_at_coords<-terra::extract(sal,gps_coords)
salsd$Salinity_class<-sal_at_coords
salsd <- salsd %>% rename(Accession = accession)
salsd<-merge(salsd,anothercodesys, by="Accession")
salsd<-salsd %>%
  dplyr::filter(Species== "P. filiformis", Idnum!=39)
salsd <- salsd %>%
  distinct()


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
bioclimsd <- bioclimsd %>% dplyr::rename(Accession = accession) 
bioclimsd<-merge(bioclimsd,salsd, by="Accession")
write.csv(bioclimsd, "bioclimsd.csv")




###6.Ggally####
bioclimsd
names(bioclimsd)


bioclimsd <- bioclimsd %>% rename(Accession = accession)
##bioclimsd<-merge(bioclimsd,controlvsalt_yieldR, by= "Accession")
bioclimfili<-bioclimsd %>% 
  dplyr::filter(species== "P. filiformis")
         
bioclimfili$AnnualPrecip<-  bioclimfili$AnnualPrecip[,2]
bioclimfili$AnnualPrecip<-as.numeric(bioclimfili$AnnualPrecip)
bioclimfili$Average_temp <-bioclimfili$Average_temp[,1]


names(bioclimfili)
view(sd)
ggpairs(sd[sample.int(nrow(sd), ncol(sd)), ])

names(bioclimfili)
bioclimfili <- bioclimfili %>% dplyr::select(-species)
str(bioclimfili)


ncol(bioclimfili)

cor(bioclimfili[,c(2:30)],method="spearman", use = "complete.obs")


p_value <- lapply(bioclimfili, function(x) corrplot::cor.mtest(bioclimfili[, 4:12])[["p"]])
correlation <- lapply(bioclimfili, function(x) cor(x[, 4:12], method = "spearman", use = 'complete.obs'))

###7.PCA for bioclim and terrain####

ncol(bioclimfili)

rownames(corr_matrix)
corr_matrix <- cor(bioclimfili[,c(2:21)])
ggcorrplot(corr_matrix)


data.pca <- princomp(corr_matrix)
summary(data.pca)

data.pca$loadings[, 1:2]
fviz_eig(data.pca, addlabels = TRUE)

fviz_pca_var(data.pca, col.var = "black",title=" PCA of Bioclimactic Variables correlated to Accession's Origin")

###trying vegan package here
names(bioclimfili)
bioclimSmallerPCA<-bioclimfili %>%
  dplyer::select()


nrow(bioclimfili)
pca.out1<-prcomp(bioclimfili[,c(2:21)],scale=TRUE)
biplot(pca.out1)

fviz_pca_biplot(pca.out1, repel = TRUE, select.var = list(contrib = 15))### PCA of 20 bioclimactic variables to pick the top 5 most influential

bioclimfili<-data.frame(bioclimfili)

bioclimSmallerPCA<-subset(bioclimfili,
  select=c(Mean_Temp_Coolest_Quarter,Temp_Annual_Range,Temperature_seasonality,Precip_Seasonality,AnnualPrecip,Precip_Driest_Quarter,Precip_Wettest_Quarter))### now dataframe of top 7 contributers of PCA, PCA1 and PCA2 explain 90% of the variation!!

pca.out2<-prcomp(bioclimSmallerPCA,scale=TRUE)
fviz_pca_biplot(pca.out2)


diffST<-SDsaltTolerance%>%
  dplyr::select(Accession,diffST)
salttolerancePCA<-salttolerancePCA%>%
  dplyr::select(Accession,STYield)

bioclim$Accession<-bioclimfili$Accession
scores<-as.data.frame(predict(pca.out2, newdata=bioclimSmallerPCA))
summary(scores)
bioclim <- bioclimSmallerPCA %>%
  mutate(PCA1 = scores$PC1,
         PCA2=scores$PC2)
salsd <- salsd %>% rename(Accession = accession)
elevationsd <- elevationsd %>% rename(Accession = accession)
bioclim<-merge(salsd,bioclim, by="Accession", all.x=FALSE)
bioclim<-merge(elevationsd,bioclim,by="Accession",all.x=FALSE)
bioclim<-merge(salttolerancePCA,bioclim,by="Accession",all.x=TRUE)
bioclim<-merge(saltyieldTolerance,bioclim,by="Accession",all.x=FALSE)
names(bioclim)


###model of prediciting values for salt tolerance via SALT YIELD

names(bioclim)
modelfit1 <- lm (log(STYield) ~ PCA1 + PCA2 + elevation+ Salinity_class,data=bioclim)##slightly significant
  modelfit1.1<- lm (log(STYield) ~ PCA1,data=bioclim)##nope
  modelfit1.1<- lm (log(STYield) ~ PCA1,data=bioclim)
  
#Signifcant Salt Treatment to Environemnt #######################################
modelfit2 <- lm (log(saltyieldTolerance) ~PCA1 + PCA2 + elevation+ Salinity_class,data=bioclim)##significant
  modelfit2.1<-lm(log(saltyieldTolerance) ~PCA1,data=bioclim) ### super significant!!-- PCA1 and saltyieldtolerance
  modelfit2.2<-lm(log(saltyieldTolerance) ~PCA2,data=bioclim)##slightly significant with PCA2
  modelfit2.3<-lm(log(saltyieldTolerance) ~elevation,data=bioclim)###not signficant with elevation
  modelfit2.4<-lm(log(saltyieldTolerance) ~Salinity_class,data=bioclim)## slightly signifcant with salty model-- .07112
-------------------------------------
modelfit3 <- lm (log(diffST) ~ PCA1 + PCA2 + elevation+ Salinity_class,data=bioclim) ###not significant with anything--?

anova(modelfit2.4)


Loadings<-as.data.frame(pca.out$rotation)
Loadings
str(pca.out)
biplot(pca.out)
rda.out <- vegan::rda(bioclimfili[,c(2:21)], scale = TRUE)
rda_scores <- scores(rda.out)
biplot(rda.out, 
       display = "sites")
biplot(rda.out, display = "sites")

ordihull(rda.out,
         group = AnnualPrecip$order,
         col = 1:7,
         lty = 1:7,
         lwd = c(3,6))

y<-rda(bioclimfili[,c(2:21)]) 
summary(y)

str(data.pca)
predict(data.)


###now trying psych???
library(psych)
pc<-prcomp(bioclimfili[,c(2:21)],centr=TRUE,scale.=TRUE)
attributes(pc)
pc$scale ##for normalization
pc$center
print(pc)##loadings and standard deviaitons
summary(pc)

g <- ggbiplot(pc,
              obs.scale = 1,
              var.scale = 1,
              groups = bioclimfili$Accession,
              ellipse = TRUE,
              circle = TRUE,
              ellipse.prob = 0.68)

g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal',
               legend.position = 'top')



  
#example
p_ <- GGally::print_if_interactive
df_x <- rnorm(100)
df_y <- df_x + rnorm(100, 0, 0.1)
df <- data.frame(x = df_x, y = df_y, c = sqrt(df_x^2 + df_y^2))
pm <- ggpairs(
  df,
  columnLabels = c("alpha[foo]", "alpha[bar]", "sqrt(alpha[foo]^2 + alpha[bar]^2)")
)
p_(pm)

pm <- ggpairs(
  tst,
  columnLabels = c("alpha[foo]", "alpha[bar]", "sqrt(alpha[foo]^2 + alpha[bar]^2)")
)
p_(pm)




###Now I want to try Pearsons correlation

ggcorrplot(cor(PCA_df))
cor.test(PCA_df$est_Pods, PCA_df$est_AverageSeedsPerPod, PCA_df$TotalAboveDryMass,
    PCA_df$est_AverageSeedWeight, method = "spearman")


##samething including Treatment SEPERATELY
#pods
seedsmerge<-seedsmerge%>%filter(Germ==1)
anovapods<-lm(TotalPods~0 +Treatment+ Rep+Accession,data=seedsmerge)
pred_dfT = expand.grid(Rep = unique (seedsmerge$Rep),
                      Accession = unique (seedsmerge$Accession),
                      Treatment= unique (seedsmerge$Treatment))
est_Pods <- predict(anovapods, pred_dfT)


pred_dfT$est_Pods <-est_Pods
pred_df2T<-pred_dfT%>% filter(Rep==2)
pred_df2T
pred_df2T$ID <- paste(pred_df2T$Accession, pred_df2T$Treatment, sep = "_")
pred_df2T<- pred_df2T%>%
  select(ID,est_Pods )

#seeds
anovaAverageSeedsPerPod<-lm(AverageSeedsPerPod~0 +Treatment + Rep+Accession,data=seedsmerge)
predseedsperpod_dfT = expand.grid(Rep = unique (seedsmerge$Rep),
                                 Accession = unique (seedsmerge$Accession),
                                 Treatment= unique (seedsmerge$Treatment))
est_AverageSeedsPerPod <- predict(anovaAverageSeedsPerPod,predseedsperpod_dfT)

predseedsperpod_dfT$est_AverageSeedsPerPod <-est_AverageSeedsPerPod
predseedsperpod_df2T<-predseedsperpod_dfT%>% filter(Rep==2)
predseedsperpod_df2T$ID <- paste(predseedsperpod_df2T$Accession, predseedsperpod_df2T$Treatment, sep = "_")
predseedsperpod_df2T<- predseedsperpod_df2T%>%
  select(ID,est_AverageSeedsPerPod,Accession )
predseedsperpod_df2T
##seedsweight
anovaAverageSeedWeight<-lm(AverageSeedWeight~0 +Treatment + Rep+Accession,data=seedsmerge)
AverageSeedWeight_dfT = expand.grid(Rep = unique (seedsmerge$Rep),
                                   Accession = unique (seedsmerge$Accession),
                                   Treatment= unique (seedsmerge$Treatment))
est_AverageSeedWeight <- predict(anovaAverageSeedWeight, AverageSeedWeight_dfT)
AverageSeedWeight_dfT$est_AverageSeedWeight <-est_AverageSeedWeight
AverageSeedWeight_df2T<-AverageSeedWeight_dfT%>% filter(Rep==2)
AverageSeedWeight_df2T$ID <- paste(AverageSeedWeight_df2T$Accession, AverageSeedWeight_df2T$Treatment, sep = "_")
AverageSeedWeight_df2T<- AverageSeedWeight_df2T%>%
  select(ID,est_AverageSeedWeight)
AverageSeedWeight_df2T
##biomass
seedsmerge1<-seedsmergeG%>% filter(Rep==1)
biomass1T<- seedsmerge1 %>%
  select(Accession, TotalAboveDryMass,Treatment)
biomass1T$ID <- paste(biomass1$Accession,biomass1$Treatment, sep = "_")
biomass1T

PCAT_df<-0
PCAT_df<-merge(pred_df2T,predseedsperpod_df2T,by="ID",all.y=FALSE,all.x=FALSE)
PCAT_df<-merge(PCAT_df,AverageSeedWeight_df2T, by="ID")


####PCAT_df<-merge(PCAT_df,biomass1T, by="ID")--not until I have all the measurments 
PCAT_df <- unique(PCAT_df)
PCAT_df
PCAT_df<-merge(anothercodesys,PCAT_df,by="Accession")
PCAT_df<- PCAT_df%>%filter(Species=="P. filiformis")
PCAT_df<- PCAT_df%>%
  select(est_Pods, est_AverageSeedsPerPod, est_AverageSeedWeight)
corr_matrix <- cor(PCAT_df)
PCAT_df
ggcorrplot(corr_matrix)
data.pca <- princomp(corr_matrix)
summary(data.pca)

data.pca$loadings[, 1:2]
fviz_eig(data.pca, addlabels = TRUE)
fviz_pca_var(data.pca, col.var = "black")




###metan package
library(metan)

#pods
pred_df = expand.grid(Rep = unique (seedsmergeG$Rep),
                      Accession = unique (seedsmergeG$Accession),
                      Treatment = unique (seedsmergeG$Treatment))
est_Pods <- predict(anovapods, pred_df,)


pred_df$est_Pods <-est_Pods
names(pred_df)
pred_df<-merge(anothercodesys,pred_df,by="Accession")
pred_df$ID <- paste(pred_df$Accession, pred_df$Treatment, pred_df$Rep, sep = "_")
pred_df<- pred_df%>%
  select(est_Pods,ID,Accession,Rep)

pred_df

#seedsperpod

anovaAverageSeedsPerPod<-lm(AverageSeedsPerPod~0 +Treatment + Rep+Accession,data=seedsmergeG)
predseedsperpod_dfT = expand.grid(Rep = unique (seedsmergeG$Rep),
                                  Accession = unique (seedsmergeG$Accession),
                                  Treatment= unique (seedsmergeG$Treatment))
est_AverageSeedsPerPod <- predict(anovaAverageSeedsPerPod,predseedsperpod_dfT)

predseedsperpod_dfT$est_AverageSeedsPerPod <-est_AverageSeedsPerPod
predseedsperpod_dfT
predseedsperpod_dfT$ID <- paste(predseedsperpod_dfT$Accession, predseedsperpod_dfT$Treatment, predseedsperpod_dfT$Rep, sep = "_")

predseedsperpod_dfT<- predseedsperpod_dfT%>%
  select(est_AverageSeedsPerPod,ID)
predseedsperpod_dfT
##seed weight
anovaAverageSeedWeight<-lm(AverageSeedWeight~0 +Treatment + Rep+Accession,data=seedsmergeG)
AverageSeedWeight_dfT = expand.grid(Rep = unique (seedsmergeG$Rep),
                                    Accession = unique (seedsmergeG$Accession),
                                    Treatment= unique (seedsmergeG$Treatment))
est_AverageSeedWeight <- predict(anovaAverageSeedWeight, AverageSeedWeight_dfT)

AverageSeedWeight_dfT$est_AverageSeedWeight <-est_AverageSeedWeight
AverageSeedWeight_dfT$ID <- paste(AverageSeedWeight_dfT$Accession, AverageSeedWeight_dfT$Treatment,AverageSeedWeight_dfT$Rep, sep = "_")
AverageSeedWeight_dfT<- AverageSeedWeight_dfT%>%
  select(ID,est_AverageSeedWeight)
AverageSeedWeight_dfT


##biomass
#biomass1<- seedsmerge1 %>%
  select(Accession, TotalAboveDryMass,Treatment,Rep)
#biomass1$ID <- paste(biomass1$Accession, biomass1$Treatment,biomass1$Rep, sep = "_")
#biomass1
#biomass1<- biomass1%>%
  #select(TotalAboveDryMass,ID)


pred_df$ID <- paste(pred_df$Accession, pred_df$Rep, sep = "_")
predseedsperpod_df$ID <- paste(predseedsperpod_df$Accession, predseedsperpod_df$Rep, sep = "_")
predseedsperpod_df<- predseedsperpod_df%>%
  select(est_AverageSeedsPerPod,ID)
AverageSeedWeight_df$ID <- paste(AverageSeedWeight_df$Accession, AverageSeedWeight_df$Rep, sep = "_")
AverageSeedWeight_df<- AverageSeedWeight_df%>%
  select(est_AverageSeedWeight ,ID)
pred_df


metanatt<-0
metanatt<-merge(pred_df,predseedsperpod_dfT,by="ID")
metanatt<-merge(metanatt,AverageSeedWeight_dfT,by="ID")
names(metanatt)




metanatt
ge_plot(metanatt,Rep,Accession,est_AverageSeedsPerPod,type=1,value=TRUE, xlab= "Average Seeds Per Pod", ylab="Accession")
help(ge_plot)
help(plot_factbars())
plot_factbars(metanatt ,Accession,est_AverageSeedsPerPod,resp=Rep)
help(plot_factbars)

PCA_df
corr_plot(PCA_df)


help(plot_scores)
names(metanatt)
model<-waas(metanatt,env=(est_AverageSeedWeight),gen=Accession,rep=Rep)
plot_scores(model)
