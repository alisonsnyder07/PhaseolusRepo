setwd("C:/Users/asnyder/Desktop/P.filiformis/PhaseolusRepo/Data")

fili<-read.csv("filifitness.csv")
seedsmerge<-read.csv("seedsmerge.csv")
allfitness<-read.csv("allfitness.csv")

##Fitness Score--Ratio####

##adding a fitness measurement of pods*seeds/pod*mass/seed
fitnessscore1 <- fili  
fitnessscore1$TotalYieldMass <- fitnessscore1$AverageSeedWeight * fitnessscore1$TotalPod *fitnessscore1$AverageSeedsPerPod
fitnessscore1$TotalSeeds<-fitnessscore1$TotalPods *fitnessscore1$AverageSeedsPerPod

fitnessscore1<- fitnessscore1 %>%
  dplyr::select(Idnum,ID ,Rep, Treatment, Germ, Accession, Alive,TotalAboveDryMass, AverageSeedWeight,AverageSeedsPerPod,TotalPods, TotalSeeds, TotalYieldMass)
write.csv(fitnessscore1,"RYieldLong.csv")

##Average Fitness Long

fitnessyield_averageacrossreps <- fitnessscore1 %>%
  group_by(Accession, Treatment) %>%
  dplyr::summarise(AverageYieldMass = mean(TotalYieldMass),##yield mass
            StandardDeviationY = sd(TotalYieldMass),
            SEY= StandardDeviationY/sqrt(n()),
            AveragePods = mean(TotalPods),##Pods
            StandardDeviationP = sd(TotalPods),
            SEP=StandardDeviationP/sqrt(n()),
            AverageBiomass = mean(TotalAboveDryMass),##Biomass
            StandardDeviationB = sd(TotalAboveDryMass),
            SEB=StandardDeviationB/sqrt(n()))
            
#good graph ####
ggplot(fitnessyield_averageacrossreps,aes(Treatment, AverageYieldMass, fill=Treatment))+
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
  ylim(0,4)

fitnessscore1$Treatment_Rep <- interaction(fitnessscore1$Treatment, fitnessscore1$Rep, sep = " Rep ")

### difference in yield by accession treatment and Rep ####
ggplot(fitnessscore1 ,aes( x=Treatment_Rep,y=TotalYieldMass, fill=Treatment))+
  geom_bar(stat = "summary", fun = "mean") +
  facet_wrap(~Accession)+
  scale_fill_manual(values = c("S" = "red", "C" = "blue"))+ # Add error bars
  labs(title = "Difference In Total Mass Yield (g yield) by Phaseolus filiformis Accession and Treatment",
       x = "Category",
       y = "Value") +
  theme_minimal()+
  ylim(0,5) 

###difference by Rep####
ggplot(fitnessscore1 ,aes( x=Treatment_Rep,y=TotalYieldMass, fill=Treatment))+
  geom_bar(stat = "summary", fun = "mean") +
  scale_fill_manual(values = c("S" = "red", "C" = "blue"))+ # Add error bars
  labs(title = "Difference In Total Mass Yield (g yield) by Phaseolus filiformis Rep and Treatment",
       x = "Category",
       y = "Value") +
  theme_minimal()+
  ylim(0,5) 



fitnessyield_ <- fitnessscore1 %>%
  group_by(Accession, Treatment) %>%
  summarise(AverageYieldMass = mean(TotalYieldMass),
            StandardDeviationY = sd(TotalYieldMass),
            SEY=StandardDeviationY/sqrt(n()),
            AverageBiomass = mean(TotalAboveDryMass),
            StandardDeviationB = sd(TotalAboveDryMass),
            SEB=StandardDeviationB/sqrt(n()))

write.csv(fitnessyield_,"AYieldLong.csv")

####Seperated by Rep-- short to long: Yield Mass 

names(fitnessscore1)

YieldC<- fitnessscore1 %>%
  dplyr::filter(Treatment == "C")
YieldS <- fitnessscore1 %>%
  dplyr::filter(Treatment == "S")

controlvsalt_yield <- data.frame(
  Accession =YieldC$Accession) 
controlvsalt_yield$Rep<-YieldC$Rep
controlvsalt_yield$c_TotalYieldMass<-YieldC$TotalYieldMass
controlvsalt_yield$s_TotalYieldMass<-YieldS$TotalYieldMass
controlvsalt_yield$STYield<-ifelse(controlvsalt_yield$c_TotalYieldMass==0 & controlvsalt_yield$s_TotalYieldMass==0,0, YieldS$TotalYieldMass/YieldC$TotalYieldMass)
controlvsalt_yield$STYield <- replace(controlvsalt_yield$STYield, !is.finite(controlvsalt_yield$STYield), 1)

controlvsalt_yield $c_Totalseeds <-YieldC$TotalSeeds
controlvsalt_yield $s_Totalseeds <-YieldS$TotalSeeds
controlvsalt_yield$STseeds<-ifelse(controlvsalt_yield$c_Totalseeds==0 & controlvsalt_yield$s_Totalseeds==0,0, YieldS$Totalseeds/YieldC$TotalSeeds)
controlvsalt_yield$STseeds <- replace(controlvsalt_yield$STseeds, !is.finite(controlvsalt_yield$STseeds), 1)

controlvsalt_yield$c_Pods<-YieldC$TotalPods
controlvsalt_yield$s_Pods<-YieldS$TotalPods
controlvsalt_yield$STPods<-ifelse(controlvsalt_yield$c_Pods==0 & controlvsalt_yield$s_Pods==0,0, YieldS$TotalPods/YieldC$TotalPods)
controlvsalt_yield$STPods <- replace(controlvsalt_yield$STPods, !is.finite(controlvsalt_yield$STPods), 1)

controlvsalt_yield$c_Biomass<-YieldC$TotalAboveDryMass
controlvsalt_yield$s_Biomass<-YieldS$TotalAboveDryMass
controlvsalt_yield$STBiomass<-ifelse(controlvsalt_yield$c_Biomass ==0 & controlvsalt_yield$s_Biomass==0,0, YieldS$TotalAboveDryMass/YieldC$TotalAboveDryMass)
controlvsalt_yield$STBiomass <- replace(controlvsalt_yield$STBiomass, !is.finite(controlvsalt_yield$STBiomass), 1)

controlvsalt_yield<- controlvsalt_yield%>%
  distinct()
write.csv(controlvsalt_yield,"STrepseperate.csv")


## not seperated by rep 
ggplot(controlvsalt_yield, aes(x=c_TotalYieldMass, y= s_TotalYieldMass,color= Accession))+
  geom_point()+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red")

saltTolerancecorrelation<-lmer(STYield~Accession+ (1|Rep), data=controlvsalt_yield)
Anova(saltTolerancecorrelation)

write.csv(controlvsalt_yield, "STbyRep.csv")

ggplot(controlvsalt_yield, aes(y=s_TotalYieldMass, x=Rep,fill= Accession))+
  facet_wrap(~Accession)+
  geom_bar(stat = "identity")+
  labs(title = "Total Seed Yield salt treated Accession")

ggplot(controlvsalt_yield, aes(y=s_Biomass, x=Rep,fill= Accession))+
  facet_wrap(~Accession)+
  geom_bar(stat = "identity")+
  theme_minimal()+
  labs(title = "Dry Biomass salt treated Accession")

ggplot(controlvsalt_yield, aes(y=s_TotalYieldMass, x=s_Biomass, color= Accession,size=Accession))+
  geom_point()+
  labs(title = "Total Seed Yield by Biomass of salt treated Accession",
       x = "Salt Treated Dry Biomass (g) " ,
       y = "Salt Treated Total Seed Yield (g) ") +
  theme_minimal()

salttolerant<-controlvsalt_yield%>%
  group_by(Accession)%>%
  dplyr::filter(STYield>.9 |STBiomass>.9)%>%
  ungroup()

ggplot(salttolerant, aes(y=s_TotalYieldMass, x=s_Biomass, color= Accession,shape=Accession))+
  geom_point()+
  labs(title = "Total Seed Yield by Biomass of salt treated Accession of top 40% salt tolerant filiformis",
       x = "Salt Treated Dry Biomass (g) " ,
       y = "Salt Treated Total Seed Yield (g) ") +
  theme_minimal()


anything<-lm(s_Biomass~Accession*s_TotalYieldMass,data=controlvsalt_yield)
Anova(anything)

#Linear Regression Significance ####


###this model is predicting a "score of fitness" for ONLY salt treatment... This is an alternative to predicting the NA values


mod4 <- glmmTMB(TotalYieldMass+.000001 ~ Accession+ (1|Rep), 
            family = Gamma(link = "log"),
                     data = fitnessscore1)%>%
  dplyr::filter(Treatment == 'S' & Germ ==1))


mod1S <-lmer(log(TotalYieldMass+0.00001) ~ Accession + (1|Rep), data = fitnessscore1 %>%
              dplyr::filter(Treatment == 'S' & Germ ==1))

Anova(mod1S) 

##looking at only total pods
mod1SPods <-lmer(log(TotalPods+0.00001) ~ Accession + (1|Rep), data = fitnessscore1 %>%
               dplyr::filter(Treatment == 'S' & Germ ==1))
Anova(mod1SPods)


##total seeds
mod1Sseeds <-lmer((TotalSeeds+0.00001) ~ Accession + (1|Rep), data = fitnessscore1 %>%
                   dplyr::filter(Treatment == 'S' & Germ ==1))
Anova(mod1Sseeds)


##looking at biomass
mod1SBiomass <-lmer(log(TotalAboveDryMass+0.000001) ~  Accession + (1|Rep), data = fitnessscore1 %>%
               dplyr::filter(Treatment == 'S' & Germ ==1))
Anova(mod1SBiomass) 
mod1Biomass <-lmer(log(TotalAboveDryMass+0.000001) ~  Accession + (1|Rep), data = fitnessscore1)
Anova(mod1Biomass)

saltyieldTolerance<-data.frame(unique(YieldC$Accession)) ### this dataframe is predicting the amount of salt yield produced-- so the higher the number the more the plant produced under salty conditions 

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



###this is looking at total yield not seperating by treatment 

ggplot(fitnessscore1, aes(x = log(TotalYieldMass))) +
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black") +
  ggtitle("Histogram of Total Yield Mass Produced by P. filiformis") +
  xlab("log(TotalYieldMass (g))") +
  ylab("Frequency") +
  theme_minimal()


mod2<-lm(log(TotalAboveDryMass+.00001)~ Accession * Treatment,data=fitnessscore1)
Anova(mod2)

mod3<-lm(log(TotalYieldMass+.00001)~ Accession * Treatment,data=fitnessscore1)
Anova(mod3)

##trying poisson
mod4 <- glmmTM(TotalYieldMass+.000001 ~ Accession * Treatment+ (1|Rep), 
            family = Gamma(link = "log"),
                     data = fitnessscore1)
Anova(mod4)


modseedsvyield<-lm(TotalYieldMass~Accession *Treatment+Rep ,data=fitnessscore1)
Anova(modseedsvyield) ##difference is significantly explained by Accession (p=.003166)






saltyieldTolerance<-data.frame(unique(YieldC$Accession))
saltyieldTolerance <- saltyieldTolerance %>% rename(Accession = unique.YieldC.Accession.)



##this is in long format to compare the salt and the control versus the last was for comparing across accessions
controlvsalt_yieldR <- controlvsalt_yield %>%
  group_by(Accession) %>%
  summarise(AverageYieldMass_C = mean(c_TotalYieldMass), ##yield
            StandardDeviation_C = sd(c_TotalYieldMass),
            SECY=StandardDeviation_C/sqrt(n()),
            AverageYieldMass_S = mean(s_TotalYieldMass),
            StandardDeviation_S = sd(s_TotalYieldMass),
            SESY=StandardDeviation_C/sqrt(n()),
            AveragePods_C = mean(c_Pods), ##pods
            StandardDeviation_C = sd(c_Pods),
            SECP=StandardDeviation_C/sqrt(n()),
            AveragePods_S = mean(s_Pods),
            StandardDeviation_S = sd(s_Pods),
            SESP=StandardDeviation_C/sqrt(n()),
            AverageSeeds_C = mean(c_Totalseeds), ##seeds
            StandardDeviation_C = sd(c_Totalseeds),
            SECS=StandardDeviation_C/sqrt(n()),
            AverageSeeds_S = mean(s_Totalseeds),
            StandardDeviation_S = sd(s_Totalseeds),
            SESS=StandardDeviation_C/sqrt(n()),
            AverageBiomass_C = mean(c_Biomass), ##biomass
            StandardDeviation_C = sd(c_Biomass),
            SECB=StandardDeviation_C/sqrt(n()),
            AverageBiomass_S = mean(s_Biomass),
            StandardDeviation_S = sd(s_Biomass),
            SESB=StandardDeviation_C/sqrt(n())) ##yield

write.csv(controlvsalt_yieldR,"controlvsalt_yieldR.csv")
###total yield-- going in poster 
ggplot(controlvsalt_yieldR, aes(x=AverageYieldMass_C, y=AverageYieldMass_S, color=Accession))+ 
  geom_errorbar(data=controlvsalt_yieldR, aes(xmin=(AverageYieldMass_C -SECY), 
                                              xmax=(AverageYieldMass_C+SECY),###I don't knnow why error bars aren't showing up
                                              ymin=(AverageYieldMass_S-SESY), 
                                              ymax=(AverageYieldMass_S-SESY)))+
  ylim(0,4)+
  geom_point()+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red")+
  labs(title= "Salt Tolerance of Phaseolus germplasm")









modseedsvyield<-lm(log(TotalYieldMass+.001)~Accession *Treatment+Rep ,data=fitnessscore1)
Anova(modseedsvyield) ##difference is significantly explained by Accession (p=.003166)



##total seeds
ggplot(controlvsalt_yieldR, aes(x=AverageYieldMass_C, y=AverageYieldMass_S, color=Accession))+ 
  geom_point()+
  geom_errorbar(data=controlvsalt_yieldR, aes(xmin=(AverageYieldMass_C -SECY), 
                                              xmax=(AverageYieldMass_C+SECY),###I don't knnow why error bars aren't showing up
                                              ymin=(AverageYieldMass_S-SESY), 
                                              ymax=(AverageYieldMass_S-SESY)))+
  ylim(0,2.5)+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red")

### which one
modseedsvyield<-lmer(TotalYieldMass~ (1|Rep)+ TotalSeeds  ,data=fitnessscore1)
Anova(modseedsvyield) #Accession p-value = .003166, treatment p-value= 2.39e-06

###conclusion: which ever...

controlvsalt_yieldR$STYield<- ifelse(controlvsalt_yieldR$AverageYieldMass_C == 0 , 0, controlvsalt_yieldR$AverageYieldMass_S/controlvsalt_yieldR$AverageYieldMass_C)
controlvsalt_yieldR$STPods<-ifelse(controlvsalt_yieldR$AveragePods_C == 0, 0, controlvsalt_yieldR$AveragePods_S/controlvsalt_yieldR$AveragePods_C)
controlvsalt_yieldR$STBiomass<-ifelse(controlvsalt_yieldR$AverageBiomass_C== 0, 0, controlvsalt_yieldR$AverageBiomass_S/controlvsalt_yieldR$AverageBiomass_C)

fitnessanova1<-lm(AverageYieldMass~Accession+Treatment, data=fitnessyield_averageacrossreps)
anova(fitnessanova1) ###relationship between treatment and accession


fitnessanova2<-lm(AveragePods~Accession+Treatment, data=fitnessyield_averageacrossreps)
anova(fitnessanova2) ###highly correlated across TREATMENT NOT ACCESSIONS

fitnessanova3<-lm(AverageBiomass~Accession+Treatment, data=fitnessyield_averageacrossreps)
anova(fitnessanova3) ###highly correlated across TREATMENT AND ACCESSION


###Salt Tolerance: ST in fitnessyield_averageacrossreps####
controlvsalt_yieldR %>% arrange(desc(STYield))###putting in decreasing order of ST

ST_df<-data.frame(controlvsalt_yieldR$Accession)
ST_df$STYield<-controlvsalt_yieldR$STYield
ST_df$STPods<-controlvsalt_yieldR$STPods
ST_df$STBiomass<-controlvsalt_yieldR$STBiomass
names(ST_df)[names(ST_df) == "controlvsalt_yieldR.Accession"] <- "Accession"
names(ST_df)

ST_df$Accession <- factor(ST_df$Accession, levels = ST_df$Accession[order(ST_df$STYield, decreasing = FALSE)])
##important graph
ggplot(ST_df, aes(y=Accession, x=STYield, fill=Accession))+ 
  geom_bar(stat = "identity")+
  geom_vline(xintercept = 1, color = "red", size = .5)+
  labs(title= "Yield Salt tolerance (g) of Phaseolus filiformis")+
  theme_minimal()+
  theme(legend.position = "none")+
  theme(panel.grid.major = element_blank(),
        strip.background = element_blank(),  
        strip.text = element_text(family="Nunito",size = 14),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        text = element_text(family = "Nunito"),
        axis.text = element_text(family = "Nunito", size = 12),
        axis.title.x = element_text(family = "Nunito", size = 16), 
        axis.title.y = element_text(family = "Nunito", size = 16), 
        legend.text = element_text(family = "Nunito", size = 12), 
        plot.title = element_text(family = "Nunito", size = 16),
        legend.title = element_text(family = "Nunito", size = 14))

write.csv(ST_df,"YieldST.csv")

###salt tolerance graph seperating reps 
controlvsalt_yield


### P. vulgaris ALSO ####
fitnessscoreVulg <- allfitness
fitnessscoreVulg$TotalYieldMass <- fitnessscoreVulg$AverageSeedWeight * fitnessscoreVulg$TotalPods *fitnessscoreVulg$AverageSeedsPerPod
fitnessscoreVulg$TotalSeeds<-fitnessscoreVulg$TotalPods *fitnessscoreVulg$AverageSeedsPerPod

fitnessscoreVulg <- fitnessscoreVulg  %>%
  dplyr::select(Idnum,ID ,Rep, Treatment, Germ, Accession, Alive,TotalAboveDryMass, AverageSeedWeight,AverageSeedsPerPod,TotalPods,TotalAboveDryMass, TotalYieldMass,Species)


YieldC<- fitnessscoreVulg %>%
  dplyr::filter(Treatment == "C")
YieldS <- fitnessscoreVulg %>%
  dplyr::filter(Treatment == "S")


controlvsalt_yieldV <- data.frame(
  Accession =YieldC$Accession) 
controlvsalt_yieldV $Rep<-YieldC$Rep
controlvsalt_yieldV$Species<-YieldC$Species
controlvsalt_yieldV$c_TotalYieldMass<-YieldC$TotalYieldMass
controlvsalt_yieldV$s_TotalYieldMass<-YieldS$TotalYieldMass
controlvsalt_yieldV$STYield<-ifelse(controlvsalt_yieldV$c_TotalYieldMass==0 & controlvsalt_yieldV$s_TotalYieldMass==0,0, YieldS$TotalYieldMass/YieldC$TotalYieldMass)
controlvsalt_yieldV$STYield <- replace(controlvsalt_yieldV$STYield, !is.finite(controlvsalt_yieldV$STYield), 1)

controlvsalt_yieldV$c_Pods<-YieldC$TotalPods
controlvsalt_yieldV$s_Pods<-YieldS$TotalPods
controlvsalt_yieldV$STPods<-YieldS$TotalPods/YieldC$TotalPods
controlvsalt_yieldV$STPods <- replace(controlvsalt_yieldV$STPods, !is.finite(controlvsalt_yieldV$STPods), 1)

controlvsalt_yieldV$c_Biomass<-YieldC$TotalAboveDryMass
controlvsalt_yieldV$s_Biomass<-YieldS$TotalAboveDryMass
controlvsalt_yieldV$STBiomass<-ifelse(controlvsalt_yieldV$c_Biomass==0 & controlvsalt_yieldV$s_Biomass==0,0, YieldS$TotalAboveDryMass/YieldC$TotalAboveDryMass)
controlvsalt_yieldV$STBiomass <- replace(controlvsalt_yieldV$STBiomass, !is.finite(controlvsalt_yieldV$STBiomass), 1)


controlvsalt_yieldVulg <- controlvsalt_yieldV %>%
  group_by(Accession) %>%
  summarise(AverageYieldMass_C = mean(c_TotalYieldMass), ##yield
            StandardDeviation_C = sd(c_TotalYieldMass),
            SECY=StandardDeviation_C/sqrt(n()),
            AverageYieldMass_S = mean(s_TotalYieldMass),
            StandardDeviation_S = sd(s_TotalYieldMass),
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
            SESB=StandardDeviation_C/sqrt(n())) ##yield


ST_dfvulg<-data.frame(controlvsalt_yieldV$Accession)
ST_dfvulg$STYield<-controlvsalt_yieldV$STYield
ST_dfvulg$STPods<-controlvsalt_yieldV$STPods
ST_dfvulg$STBiomass<-controlvsalt_yieldV$STBiomass
names(ST_dfvulg)[names(ST_dfvulg) == "controlvsalt_yieldV.Accession"] <- "Accession"
controlvsalt_yieldV<- merge(anothercodesys, controlvsalt_yieldV)
write.csv(controlvsalt_yieldV,"fitnessall.csv")

