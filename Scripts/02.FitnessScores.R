setwd("C:/Users/asnyder/Desktop/P. filiformis/MeasurmentData/Data")

fili<-read.csv("01fitness.csv")
seedsmerge<-read.csv("seedsmerge.csv")

tibble::view(fili)

##Fitness Score--Ratio####

##adding a fitness measurement of pods*seeds/pod*mass/seed
fitnessscore1 <- fili
fitnessscore1$TotalYieldMass <- fitnessscore1$AverageSeedWeight * fitnessscore1$AverageSeedsPerPod * fitnessscore1$TotalPods

fitnessscore1<- fitnessscore1 %>%
  dplyr::select(Idnum,ID ,Rep, Treatment, Germ, Accession, Alive, AverageSeedWeight,AverageSeedsPerPod,TotalPods, TotalYieldMass)



##Average Fitness Long

fitnessyield_averageacrossreps <- fitnessscore1 %>%
  group_by(Accession, Treatment) %>%
  summarise(AverageYieldMass = mean(TotalYieldMass),
            StandardDeviation = sd(TotalYieldMass),
            SE=StandardDeviation/sqrt(n()))

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
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red")




controlvsalt_yieldR$ST<-controlvsalt_yieldR$AverageYieldMass_S/controlvsalt_yieldR$AverageYieldMass_C


fitnessanova<-lm(AverageYieldMass~Accession+Treatment, data=fitnessyield_averageacrossreps)
anova(fitnessanova)##highly correlated across accessions but not treatment?? i dont understand that 

tibble::view(controlvsalt_yieldR)
###Salt Tolerance: ST in fitnessyield_averageacrossreps####

controlvsalt_yieldR %>% arrange(desc(ST))###putting in decreasing order of ST

ST_df<-data.frame(controlvsalt_yieldR$Accession)
ST_df$ST<-controlvsalt_yieldR$ST
names(controlvsalt_yieldR)
