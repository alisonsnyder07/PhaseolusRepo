setwd("C:/Users/asnyder/Desktop/P. filiformis/MeasurmentData/Data")

fili<-read.csv("01fitness.csv")
tibble::view(fili)



##2. Fitness Score--Ratio####

##adding a fitness measurement of pods*seeds/pod*mass/seed
fitnessscore1 <- fili
fitnessscore1$TotalYieldMass <- fitnessscore1$AverageSeedWeight * fitnessscore1$AverageSeedsPerPod * fitnessscore1$TotalPods

fitnessscore1<- fitnessscore1 %>%
  dplyr::select(Idnum,ID ,Rep, Treatment, Germ, Accession, Alive, AverageSeedWeight,AverageSeedsPerPod,TotalPods, TotalYieldMass)

tibble::view(fitnessscore1)


##Average Fitness Long

fitnessyield_averageacrossreps <- fitnessscore1 %>%
  group_by(Accession, Treatment) %>%
  summarise(AverageYieldMass = mean(TotalYieldMass),
            StandardDeviation = sd(TotalYieldMass),
            SE=StandardDeviation/sqrt(n()))
tibble::view(fitnessyield_averageacrossreps)


ggplot(fitnessyield_averageacrossreps,aes( Treatment, AverageYieldMass, fill=Treatment))+
  geom_bar(stat = "summary", fun = "mean") +
  facet_wrap(~Accession)+
  geom_errorbar(data=fitnessyield_averageacrossreps,
                aes(x = Treatment, ymin = AverageYieldMass-SE, ymax = AverageYieldMass + SE),
                width = 0) +
  scale_fill_manual(values = c("S" = "red", "C" = "blue"))+ # Add error bars
  labs(title = "Difference In Total Mass Yield (g yield) by Phaseolus filiformis Accession and Treatmenst",
       x = "Category",
       y = "Value") +
  theme_minimal()


####Seperated By Rep Yield-- SHORT
controlvsalt_yield <-0

YieldC<- fitnessscore1 %>%
  dplyr::filter(Treatment == "C")

YieldS <- fitnessscore1 %>%
  dplyr::filter(Treatment == "S")


controlvsalt_yield <- data.frame(
  Accession =YieldC$Accession) 
controlvsalt_yield$c_TotalYieldMass<-YieldC$TotalYieldMass
controlvsalt_yield$Rep<-YieldC$Rep
controlvsalt_yield$S_TotalYieldMass<-YieldS$TotalYieldMass

controlvsalt_yield


#### Linear Regression Significance ####


###this model is predicting a "score of fitness" for ONLY salt treatment... This is an alternative to predicting the NA values
mod1S <-lmer(log(TotalYieldMass+0.001) ~ 0+ Accession + (1|Rep), data = fitnessscore1 %>%
              dplyr::filter(Treatment == 'S' & Germ ==1))

Anova(mod1S)### There is a significant difference in yield of salt treated accession by accession but not Treatment


mod1C <-lmer(log(TotalYieldMass+0.001) ~ 0+ Accession + (1|Rep), data = fitnessscore1 %>%
               dplyr::filter(Treatment == 'C' & Germ ==1))
Anova(mod1C)### There is a significant difference in yield of salt treated accession by accession and also Treatment? but it is only control so I am sort of confused on that 


###this is looking at total yield not seperating by treatment 
mod2<-lmer(log(TotalYieldMass+0.001)~Accession + (1|Rep) + Treatment,data=fitnessscore1)
Anova(mod2)


# mod2 <-lmer(TotalYieldMass ~ 0+ Accession + (1|Rep), data = fitnessscore1 %>%
#               dplyr::filter(Treatment == 'S' & Germ ==1))
# 
# check_model(mod2)


tst<-fitnessscore1 %>%
  dplyr::select(Accession) %>%
  distinct()

saltyieldTolerance<-data.frame(unique(YieldC$Accession))
saltyieldTolerance <- saltyieldTolerance %>% rename(Accession = unique.YieldC.Accession.)


##so this is predicting the total seed produced by only the salt treatment across accessions combining Rep?? i dont know I am kind of confused on this one
saltyieldTolerance$saltyieldTolerance<-exp(predict(mod1S, newdata = fitnessscore1 %>%
                                                     dplyr::select(Accession) %>%
                                                     distinct(), re.form = NA))-0.001

ggplot (fitnessscore1, aes (x=Accession, y=TotalYieldMass, group =interaction(Accession,Treatment), fill = Treatment))+
  geom_boxplot()


##this is in long format to compare the salt and the control versus the last was for comparing across accessions
controlvsalt_yieldR <- controlvsalt_yield %>%
  group_by(Accession) %>%
  summarise(AverageYieldMass_C = mean(c_TotalYieldMass),
            StandardDeviation_C = sd(c_TotalYieldMass),
            SEC=StandardDeviation_C/sqrt(n()),
            AverageYieldMass_S = mean(S_TotalYieldMass),
            StandardDeviation_S = sd(S_TotalYieldMass),
            SES=StandardDeviation_C/sqrt(n()))


ggplot(controlvsalt_yieldR, aes(x=AverageYieldMass_C, y=AverageYieldMass_S, color=Accession))+ 
  geom_errorbar(data=controlvsalt_yieldR, aes(xmin=(AverageYieldMass_C -SEC), 
                                              xmax=(AverageYieldMass_C+SEC),###I don't knnow why error bars aren't showing up
                                              ymin=(AverageYieldMass_S-SES), 
                                              ymax=(AverageYieldMass_S-SES)))+
  geom_point()+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red")+
  ylim(0,6)+
  xlim(0,8)




controlvsalt_yieldR$ST<-controlvsalt_yieldR$AverageYieldMass_S/controlvsalt_yieldR$AverageYieldMass_C


fitnessanova<-lm(AverageYieldMass~Accession+Treatment, data=fitnessyield_averageacrossreps)
anova(fitnessanova)##highly correlated across accessions but not treatment?? i dont understand that 

tibble::view(controlvsalt_yieldR)
###Salt Tolerance: ST in fitnessyield_averageacrossreps####

controlvsalt_yieldR %>% arrange(desc(ST))

###putting in decreasing order of ST

ST_df<-data.frame(controlvsalt_yieldR$Accession)
ST_df$ST<-controlvsalt_yieldR$ST
ST_df %>% arrange(desc(ST))



##2. Checking significance ####
seeds<-read_excel("Pfilifom_data.xlsx",sheet="SeedData")
seedsmerge<-merge(seeds,anothercodesys, by="Idnum")

seedsmergeG <- seedsmerge%>%
  filter(Germ == 1)
view(seedsmergeG)
anovapods<-lm(TotalPods~0+Rep+Accession,data=seedsmergeG)
pred_df = expand.grid(Rep = unique (seedsmergeG$Rep),
                      Accession = unique (seedsmergeG$Accession))
est_Pods <- predict(anovapods, pred_df)
pred_df$est_Pods <-est_Pods

pred_df
pred_df2<-pred_df%>% filter(Rep==2)
pred_df2

pred_df2

##variable for PCA 2---seedsperpod
names(seedsMerge)
anova(anovaAverageSeedsPerPod)
anovaAverageSeedsPerPod<-lm(AverageSeedsPerPod~ 0 + Rep + Accession,data=seedsmergeG)
predseedsperpod_df = expand.grid(Rep = unique (seedsmergeG$Rep),
                                 Accession = unique (seedsmergeG$Accession))
est_AverageSeedsPerPod <- predict(anovaAverageSeedsPerPod,predseedsperpod_df)
predseedsperpod_df$est_AverageSeedsPerPod <-est_AverageSeedsPerPod
predseedsperpod_df2<-predseedsperpod_df%>% filter(Rep==2)
predseedsperpod_df2



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


