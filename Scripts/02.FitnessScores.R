setwd("C:/Users/asnyder/Desktop/P. filiformis/PhaseolusRepo/Data")

fili<-read.csv("filifitness.csv")
seedsmerge<-read.csv("seedsmerge.csv")
allfitness<-read.csv("allfitness.csv")

##Fitness Score--Ratio####

##adding a fitness measurement of pods*seeds/pod*mass/seed
fitnessscore1 <- fili
fitnessscore1$TotalYieldMass <- fitnessscore1$AverageSeedWeight * fitnessscore1$TotalPods *fitnessscore1$AverageSeedsPerPod
fitnessscore1$TotalSeeds<-fitnessscore1$TotalPods *fitnessscore1$AverageSeedsPerPod

fitnessscore1<- fitnessscore1 %>%
  dplyr::select(Idnum,ID ,Rep, Treatment, Germ, Accession, Alive,TotalAboveDryMass, AverageSeedWeight,AverageSeedsPerPod,TotalPods, TotalSeeds, TotalYieldMass)

write.csv(fitnessscore1, "filifitnessforposter.csv")


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
  ylim(0,4)

fitnessscore1$Treatment_Rep <- interaction(fitnessscore1$Treatment, fitnessscore1$Rep, sep = " Rep ")

### difference in yield by accession treatment and Rep ####
ggplot(fitnessscore1 ,aes( x=Treatment_Rep,y=TotalYieldMass, fill=Treatment))+
  geom_bar(stat = "summary", fun = "mean") +
  facet_wrap(~Accession)+
  scale_fill_manual(values = c("S" = "red", "C" = "blue"))+
  theme_minimal()+
  labs(title = "Difference In Total Mass Yield (g yield) by Phaseolus filiformis Accession and Treatment, showing every replicate plant ",
       x = "Replicated Accession",
       y = "") +
 theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  
  ylim(0,5) 

###difference by Re - 
ggplot(fitnessyield_averageacrossreps ,aes( x=Treatment_Rep,y=TotalYieldMass, fill=Treatment))+
  geom_bar(stat = "summary", fun = "mean") +
  scale_fill_manual(values = c("S" = "red", "C" = "blue"))+ # Add error bars
  labs(title = "Difference In Total Mass Yield (g yield) by "~italic("Phaseolus filiformis")~"Rep and Treatment",
       x = "Category",
       y = "Value") +
  theme_minimal()+
  labs(title= "Difference in Salt Tolerance of"  ~ italic("Phaseolus")~ "across Species", y="Salt Tolerance Index", x="Species", legend = "Species", fill= "Species")+
  theme(panel.grid.major = element_blank(),
        strip.background = element_blank(),  
        strip.text = element_text(family="Nunito",size = 14),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        text = element_text(family = "Nunito"),
        axis.text = element_text(family = "Nunito", size = 14), 
        legend.text = element_text(family = "Nunito", size = 16), 
        plot.title = element_text(family = "Nunito", size = 16),
        legend.title = element_text(family = "Nunito", size = 16))

  ylim(0,5) 



fitnessyield_ <- fitnessscore1 %>%
  group_by(Accession, Treatment) %>%
  summarise(AverageYieldMass = mean(TotalYieldMass),
            StandardDeviation = sd(TotalYieldMass),
            SE=StandardDeviation/sqrt(n()))
write.csv(fitnessyield_,"forPCAyieldmass.csv")


####Seperated by Rep-- short to long: Yield Mass 


YieldC<- fitnessscore1 %>%
  dplyr::filter(Treatment == "C")
YieldS <- fitnessscore1 %>%
  dplyr::filter(Treatment == "S")

controlvsalt_yield <- data.frame(
  Accession =YieldC$Accession) 
controlvsalt_yield$Rep<-YieldC$Rep
controlvsalt_yield$c_TotalYieldMass<-YieldC$TotalYieldMass
controlvsalt_yield$s_TotalYieldMass<-YieldS$TotalYieldMass
controlvsalt_yield$STYield<-YieldS$TotalYieldMass/YieldC$TotalYieldMass
  controlvsalt_yield$STYield <- replace(controlvsalt_yield$STYield, !is.finite(controlvsalt_yield$STYield), 1)
controlvsalt_yield$STYield2 <- replace(controlvsalt_yield$STYield2, !is.finite(controlvsalt_yield$STYield2), 1)

controlvsalt_yield $c_Totalseeds <-YieldC$TotalSeeds
controlvsalt_yield $s_Totalseeds <-YieldS$TotalSeeds
controlvsalt_yield$STseeds<-YieldS$TotalSeeds/YieldC$TotalSeeds
controlvsalt_yield$STseeds <- replace(controlvsalt_yield$STseeds, !is.finite(controlvsalt_yield$STseeds), 1)

controlvsalt_yield$c_Pods<-YieldC$TotalPods
controlvsalt_yield$s_Pods<-YieldS$TotalPods
controlvsalt_yield$STPods<-YieldS$TotalPods/YieldC$TotalPods
controlvsalt_yield$STPods <- replace(controlvsalt_yield$STPods, !is.finite(controlvsalt_yield$STPods), 1)

controlvsalt_yield$c_Biomass<-YieldC$TotalAboveDryMass
controlvsalt_yield$s_Biomass<-YieldS$TotalAboveDryMass
controlvsalt_yield$STBiomass<-YieldS$TotalBiomass/YieldC$TotalBiomass
controlvsalt_yield$STBiomass <- replace(controlvsalt_yield$STBiomass, !is.finite(controlvsalt_yield$STBiomass), 1)

controlvsalt_yield<- controlvsalt_yield%>%
  distinct()
write.csv(controlvsalt_yield,"STrepseperate.csv")

totalyieldlm<-lmer(c_TotalYieldMass~Accession+(1|Rep),data=controlvsalt_yield)
Anova(totalyieldlm)##0.0803
totalyieldlm<-lmer(s_TotalYieldMass~Accession+(1|Rep),data=controlvsalt_yield)
Anova(totalyieldlm)##0.259

## not seperated by rep 
ggplot(controlvsalt_yield, aes(x=c_TotalYieldMass, y= s_TotalYieldMass,color= Accession))+
  geom_point()+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red")

saltTolerancecorrelation<-lmer(log(STYield+.001)~Accession+(1|Rep), data=controlvsalt_yield)
anova(saltTolerancecorrelation) ## not even correlated by accession

###difference by rep-- salt tolerance 
ggplot(controlvsalt_yield,aes(x=Rep, y=STYield))+
  geom_bar(stat = "summary",fill="purple")+
  facet_wrap(~Accession)+
  labs(title = "Salt Tolerance by Accession across replicates of "~italic("Phaseolus filiformis"),
       x = "Category",
       y = "Value")+
  theme_minimal()+
  theme(
        strip.text = element_text(family="Nunito",size = 14),
        text = element_text(family = "Nunito"),
        axis.text = element_text(family = "Nunito", size = 10), 
        legend.text = element_text(family = "Nunito", size = 16), 
        plot.title = element_text(family = "Nunito", size = 16),
        legend.title = element_text(family = "Nunito", size = 16))


write.csv(controlvsalt_yield, "STbyRep.csv")

#Linear Regression Significance ####


###this model is predicting a "score of fitness" for ONLY salt treatment... This is an alternative to predicting the NA values
mod1S <-lmer(log(TotalYieldMass+0.00001) ~ 0+ Accession + (1|Rep), data = fitnessscore1 %>%
              dplyr::filter(Treatment == 'S' & Germ ==1))

Anova(mod1S) ##p-value = .09162

##looking at only total pods--- kind of interesting??
mod1SPods <-lmer((TotalPods+0.00001) ~ 0+ Accession + (1|Rep), data = fitnessscore1 %>%
               dplyr::filter(Treatment == 'S' & Germ ==1))
Anova(mod1SPods)## .04!!


##total seeds
mod1Sseeds <-lmer((TotalSeeds+0.00001) ~ 0+ Accession + (1|Rep) , data = fitnessscore1 %>%
                   dplyr::filter(Treatment == 'S' & Germ ==1))
Anova(mod1Sseeds) #p-value= .03804


##looking at biomass
mod1SBiomass <-lmer(log(TotalAboveDryMass+0.000001) ~ 0+ Accession + (1|Rep), data = fitnessscore1 %>%
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



###this is looking at total yield not seperating by treatment 
mod2<-lmer(log(TotalYieldMass+0.001)~Accession + (1|Rep) + Treatment,data=fitnessscore1)
Anova(mod2)## no significant relationship between yield and accession-- SAD

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
            SESS=StandardDeviation_C/sqrt(n()))
            #AverageBiomass_C = mean(c_Biomass), ##biomass
            #StandardDeviation_C = sd(c_Biomass),
            #SECB=StandardDeviation_C/sqrt(n()),
            #AverageBiomass_S = mean(s_Biomass),
            #StandardDeviation_S = sd(s_Biomass),
            #SESB=StandardDeviation_C/sqrt(n())) ##yield

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



##total seeds
ggplot(controlvsalt_yieldR, aes(x=AverageSeeds_C, y=AverageSeeds_S, color=Accession))+ 
  geom_errorbar(data=controlvsalt_yieldR, aes(xmin=(AverageYieldMass_C -SECY), 
                                              xmax=(AverageYieldMass_C+SECY),###I don't knnow why error bars aren't showing up
                                              ymin=(AverageYieldMass_S-SESY), 
                                              ymax=(AverageYieldMass_S-SESY)))+
  geom_point()+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red")


### which one
modseedsvyield<-lm(TotalYieldMass~Accession+ (1|Rep) +Treatment  ,data=fitnessscore1)
anova(modseedsvyield) #Accession p-value = .003166, treatment p-value= 2.39e-06



###conclusion: which ever...

controlvsalt_yieldR$STYield<- ifelse(controlvsalt_yieldR$AverageYieldMass_C == 0, 0, controlvsalt_yieldR$AverageYieldMass_S/controlvsalt_yieldR$AverageYieldMass_C)
controlvsalt_yieldR$STPods<-ifelse(controlvsalt_yieldR$AveragePods_C == 0, 0, controlvsalt_yieldR$AveragePods_S/controlvsalt_yieldR$AveragePods_C)
controlvsalt_yieldR$STBiomass<-ifelse(controlvsalt_yieldR$AverageBiomass_C== 0, 0, controlvsalt_yieldR$AverageBiomass_S/controlvsalt_yieldR$AverageBiomass_C)

fitnessanova1<-lm(log(AverageYieldMass)~Accession+Treatment, data=fitnessyield_averageacrossreps)
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
  labs(title= "Yield Salt tolerance (g) of Phaseolus filiformis")

write.csv(ST_df,"YieldST.csv")


### P. vulgaris ALSO ####

fitnessscoreVulg <- allfitness
fitnessscoreVulg$TotalYieldMass <- fitnessscoreVulg$AverageSeedWeight * fitnessscoreVulg$TotalPods *fitnessscoreVulg$AverageSeedsPerPod
fitnessscoreVulg$TotalSeeds<-fitnessscoreVulg$TotalPods *fitnessscoreVulg$AverageSeedsPerPod

fitnessscoreVulg <- fitnessscoreVulg  %>%
  dplyr::select(Idnum,ID ,Rep, Treatment, Germ, Accession, Alive,TotalAboveDryMass, AverageSeedWeight,AverageSeedsPerPod,TotalPods, TotalYieldMass,Species,WvC)


YieldC<- fitnessscoreVulg %>%
  dplyr::filter(Treatment == "C")
YieldS <- fitnessscoreVulg %>%
  dplyr::filter(Treatment == "S")


controlvsalt_yieldV <- data.frame(
  Accession =YieldC$Accession) 
controlvsalt_yieldV $Rep<-YieldC$Rep
controlvsalt_yieldV$Species<-YieldC$Species
controlvsalt_yieldV$c_TotalYieldMass<-YieldC$TotalYieldMass
controlvsalt_yieldV$S_TotalYieldMass<-YieldS$TotalYieldMass
controlvsalt_yieldV$STYield<-YieldS$TotalYieldMass/YieldC$TotalYieldMass
controlvsalt_yieldV$STYield <- replace(controlvsalt_yieldV$STYield, !is.finite(controlvsalt_yieldV$STYield), 1)

controlvsalt_yieldV$c_Pods<-YieldC$TotalPods
controlvsalt_yieldV$s_Pods<-YieldS$TotalPods
controlvsalt_yieldV$STPods<-YieldS$TotalPods/YieldC$TotalPods
controlvsalt_yieldV$STPods <- replace(controlvsalt_yieldV$STPods, !is.finite(controlvsalt_yieldV$STPods), 1)

controlvsalt_yieldV$c_Biomass<-YieldC$TotalAboveDryMass
controlvsalt_yieldV$s_Biomass<-YieldS$TotalAboveDryMass
controlvsalt_yieldV$STBiomass<-YieldS$TotalBiomass/YieldC$TotalBiomass
controlvsalt_yieldV$STBiomass <- replace(controlvsalt_yield$STBiomass, !is.finite(controlvsalt_yield$STBiomass), 1)


####significance of accession to STYield -- this is vulgaris :()
totalyieldlm<-lmer(log(STYield+.001)~Accession+(1|Rep),data=controlvsalt_yieldV)
Anova(totalyieldlm)

controlvsalt_yieldVulg <- controlvsalt_yieldV %>%
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
            SESB=StandardDeviation_C/sqrt(n())) ##yield


ST_dfvulg<-data.frame(controlvsalt_yieldV$Accession)
ST_dfvulg$STYield<-controlvsalt_yieldV$STYield
ST_dfvulg$STPods<-controlvsalt_yieldV$STPods
ST_dfvulg$STBiomass<-controlvsalt_yieldV$STBiomass
names(ST_dfvulg)[names(ST_dfvulg) == "controlvsalt_yieldV.Accession"] <- "Accession"
controlvsalt_yieldV<- merge(anothercodesys, controlvsalt_yieldV)
write.csv(controlvsalt_yieldV,"fitnessall.csv")
tibble::view(controlvsalt_yieldV)
