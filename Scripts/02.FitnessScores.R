setwd("C:/Users/asnyder/Desktop/P. filiformis/PhaseolusRepo/Data")

fili<-read.csv("01fitness.csv")
seedsmerge<-read.csv("seedsmerge.csv")

tibble::view(fili)

##Fitness Score--Ratio####

##adding a fitness measurement of pods*seeds/pod*mass/seed
fitnessscore1 <- fili
fitnessscore1$TotalYieldMass <- fitnessscore1$AverageSeedWeight * fitnessscore1$TotalPods *fitnessscore1$AverageSeedsPerPod


ggplot(fitnessscore1, aes(x=log(AverageSeedsPerPod)))+
  geom_histogram()




fitnessscore1<- fitnessscore1 %>%
  dplyr::select(Idnum,ID ,Rep, Treatment, Germ, Accession, Alive,TotalAboveDryMass, AverageSeedWeight,AverageSeedsPerPod,TotalPods, TotalYieldMass)



##Average Fitness Long

fitnessyield_averageacrossreps <- fitnessscore1 %>%
  group_by(Accession, Treatment) %>%
  summarise(AverageYieldMass = mean(TotalYieldMass),##yield mass
            StandardDeviationY = sd(TotalYieldMass),
            SEY= StandardDeviationY/sqrt(n()),
            AveragePods = mean(TotalPods),##Pods
            StandardDeviationP = sd(TotalPods),
            SEP=StandardDeviationP/sqrt(n()),
            AverageBiomass = mean(TotalAboveDryMass),##Biomass
            StandardDeviationB = sd(TotalAboveDryMass),
            SEB=StandardDeviationB/sqrt(n()))
            
            

tibble::view(fitnessyield_averageacrossreps)
ggplot(fitnessyield_averageacrossreps,aes( Treatment, AverageYieldMass, fill=Treatment))+
  geom_bar(stat = "summary", fun = "mean") +
  facet_wrap(~Accession)+
  geom_errorbar(data=fitnessyield_averageacrossreps,
                aes(x = Treatment, ymin = AverageYieldMass-SEY, ymax = AverageYieldMass + SEY),
                width = 0) +
  scale_fill_manual(values = c("S" = "red", "C" = "blue"))+ # Add error bars
  labs(title = "Difference In Total Mass Yield (g yield) by Phaseolus filiformis Accession and Treatmenst",
       x = "Category",
       y = "Value") +
  theme_minimal()+
  ylim(0,15)


fitnessyield_ <- fitnessscore1 %>%
  group_by(Accession, Treatment,Rep) %>%
  summarise(AverageYieldMass = mean(TotalYieldMass),
            StandardDeviation = sd(TotalYieldMass),
            SE=StandardDeviation/sqrt(n()))

ggplot(fitnessyield_,aes( Treatment, AverageYieldMass, fill=Treatment))+
  geom_boxplot() +
  facet_wrap(~Accession)+
  scale_fill_manual(values = c("S" = "red", "C" = "blue"))+ # Add error bars
  labs(title = "Difference In Total Mass Yield (g yield) by Phaseolus filiformis Accession and Treatmenst",
       x = "Category",
       y = "Value") +
  theme_minimal()+
  ylim(0,15)



####Seperated By Rep Yield-- SHORT
controlvsalt_yield <-0

YieldC<- fitnessscore1 %>%
  dplyr::filter(Treatment == "C")
YieldS <- fitnessscore1 %>%
  dplyr::filter(Treatment == "S")


controlvsalt_yield <- data.frame(
  Accession =YieldC$Accession) 
controlvsalt_yield$Rep<-YieldC$Rep
controlvsalt_yield$c_TotalYieldMass<-YieldC$TotalYieldMass
controlvsalt_yield$S_TotalYieldMass<-YieldS$TotalYieldMass
controlvsalt_yield$c_Pods<-YieldC$TotalPods
controlvsalt_yield$s_Pods<-YieldS$TotalPods
controlvsalt_yield$c_Biomass<-YieldC$TotalAboveDryMass
controlvsalt_yield$s_Biomass<-YieldS$TotalAboveDryMass
names(controlvsalt_yield)
#### Linear Regression Significance ####


###this model is predicting a "score of fitness" for ONLY salt treatment... This is an alternative to predicting the NA values
mod1S <-lmer(log(TotalYieldMass+0.001) ~ 0+ Accession + (1|Rep), data = fitnessscore1 %>%
              dplyr::filter(Treatment == 'S' & Germ ==1))
Anova(mod1S)##VERY significatn 

##looking at only total pods
mod1SPods <-lmer((TotalPods+0.001) ~ 0+ Accession + (1|Rep), data = fitnessscore1 %>%
               dplyr::filter(Treatment == 'S' & Germ ==1))
Anova(mod1SPods)## SIGNIFICANT with and without log 

##looking at biomass
mod1SBiomass <-lmer(log(TotalAboveDryMass+0.001) ~ 0+ Accession + (1|Rep), data = fitnessscore1 %>%
               dplyr::filter(Treatment == 'S' & Germ ==1))
Anova(mod1SBiomass) ### VERY SIGNIFICANT 


saltyieldTolerance<-data.frame(unique(YieldC$Accession))
saltyieldTolerance <- saltyieldTolerance %>% rename(Accession = unique.YieldC.Accession.)
saltyieldTolerance$saltyieldToleranceYield<-exp(predict(mod1S, newdata = fitnessscore1 %>%
                                                     dplyr::select(Accession) %>%
                                                     distinct(), re.form = NA))-0.001
saltyieldTolerance$saltyieldTolerancePods<-exp(predict(mod1SPods, newdata = fitnessscore1 %>%
                                                          dplyr::select(Accession) %>%
                                                          distinct(), re.form = NA))-0.001
saltyieldTolerance$saltyieldToleranceBiomass<-exp(predict(mod1SBiomass, newdata = fitnessscore1 %>%
                                                          dplyr::select(Accession) %>%
                                                          distinct(), re.form = NA))-0.001
write.csv(saltyieldTolerance,"saltyieldTolerance.csv")


mod1C <-lmer(log(TotalYieldMass+0.001) ~ 0+ Accession + (1|Rep), data = fitnessscore1 %>%
               dplyr::filter(Treatment == 'C' & Germ ==1))
Anova(mod1C) 
ControlyieldTolerance<-data.frame(unique(YieldC$Accession))
ControlyieldTolerance <- ControlyieldTolerance %>% rename(Accession = unique.YieldC.Accession.)
ControlyieldTolerance$controlyieldTolerance<-(exp(predict(mod1C, newdata = fitnessscore1 %>%
                                                     dplyr::select(Accession) %>%
                                                     distinct(), re.form = NA))-0.001) ###error in X %*% fixef(object) : non-conformable arguments


###this is looking at total yield not seperating by treatment 
mod2<-lmer(log(TotalYieldMass+0.001)~Accession + (1|Rep) + Treatment,data=fitnessscore1)
Anova(mod2)





saltyieldTolerance<-data.frame(unique(YieldC$Accession))
saltyieldTolerance <- saltyieldTolerance %>% rename(Accession = unique.YieldC.Accession.)


ggplot (fitnessscore1, aes (x=Accession, y=TotalYieldMass, group =interaction(Accession,Treatment), fill = Treatment))+
  geom_boxplot()


##this is in long format to compare the salt and the control versus the last was for comparing across accessions
controlvsalt_yieldR <- controlvsalt_yield %>%
  group_by(Accession) %>%
  summarise(AverageYieldMass_C = mean(c_TotalYieldMass), ##yield
            StandardDeviation_C = sd(c_TotalYieldMass),
            SECY=StandardDeviation_C/sqrt(n()),
            AverageYieldMass_S = mean(S_TotalYieldMass),
            StandardDeviation_S = sd(S_TotalYieldMass),
            SESY=StandardDeviation_C/sqrt(n()),
            AveragePods_C = mean(c_Pods), ##pods
            StandardDeviation_C = sd(c_Pods),
            SECP=StandardDeviation_C/sqrt(n()),
            AveragePods_S = mean(s_Pods),
            StandardDeviation_S = sd(s_Pods),
            SESP=StandardDeviation_C/sqrt(n()),
            AverageBiomass_C = mean(c_Biomass), ##biomass
            StandardDeviation_C = sd(c_Biomass),
            SECB=StandardDeviation_C/sqrt(n()),
            AverageBiomass_S = mean(s_Biomass),
            StandardDeviation_S = sd(s_Biomass),
            SESB=StandardDeviation_C/sqrt(n()))
            
          

ggplot(controlvsalt_yieldR, aes(x=AverageYieldMass_C, y=AverageYieldMass_S, color=Accession))+ 
  geom_errorbar(data=controlvsalt_yieldR, aes(xmin=(AverageYieldMass_C -SECY), 
                                              xmax=(AverageYieldMass_C+SECY),###I don't knnow why error bars aren't showing up
                                              ymin=(AverageYieldMass_S-SESY), 
                                              ymax=(AverageYieldMass_S-SESY)))+
  ylim(0,25)+
  geom_point()+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red")



tibble::view(controlvsalt_yieldR)
controlvsalt_yieldR$STYield<- ifelse(controlvsalt_yieldR$AverageYieldMass_C == 0, 0, controlvsalt_yieldR$AverageYieldMass_S/controlvsalt_yieldR$AverageYieldMass_C)
controlvsalt_yieldR$STPods<-ifelse(controlvsalt_yieldR$AveragePods_C == 0, 0, controlvsalt_yieldR$AveragePods_S/controlvsalt_yieldR$AveragePods_C)
controlvsalt_yieldR$STBiomass<-ifelse(controlvsalt_yieldR$AverageBiomass_C== 0, 0, controlvsalt_yieldR$AverageBiomass_S/controlvsalt_yieldR$AverageBiomass_C)

fitnessanova1<-lm(log(AverageYieldMass)~Accession+Treatment, data=fitnessyield_averageacrossreps)
anova(fitnessanova1) ###relationship between treatment???

fitnessanova2<-lm(AveragePods~Accession+Treatment, data=fitnessyield_averageacrossreps)
anova(fitnessanova2) ###highly correlated across TREATMENT and ACCESSIONS

fitnessanova3<-lm(AverageBiomass~Accession+Treatment, data=fitnessyield_averageacrossreps)
anova(fitnessanova3) ###highly correlated across TREATMENT AND ACCESSION

tibble::view(controlvsalt_yieldR)


###Salt Tolerance: ST in fitnessyield_averageacrossreps####
controlvsalt_yieldR %>% arrange(desc(STYield))###putting in decreasing order of ST
names(controlvsalt_yieldR)

ST_df<-data.frame(controlvsalt_yieldR$Accession)
ST_df$STYield<-controlvsalt_yieldR$STYield
ST_df$STPods<-controlvsalt_yieldR$STPods
ST_df$STBiomass<-controlvsalt_yieldR$STBiomass
names(ST_df)[names(ST_df) == "controlvsalt_yieldR.Accession"] <- "Accession"
names(ST_df)

ST_df$Accession <- factor(ST_df$Accession, levels = ST_df$Accession[order(ST_df$STYield, decreasing = FALSE)])

ggplot(ST_df, aes(y=Accession, x=STYield, fill=Accession))+ 
  geom_bar(stat = "identity")+
  geom_vline(xintercept = 1, color = "red", size = .5)

write.csv(ST_df,"YieldST.csv")

