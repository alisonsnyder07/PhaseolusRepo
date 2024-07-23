setwd("C:/Users/asnyder/Desktop/P. filiformis/PhaseolusRepo/Data")

YieldST<-read.csv("YieldST.csv")
AverageYieldLong<-read.csv("AYieldLong.csv")
SeperateYieldLong<-read.csv("RYieldLong.csv")
STandYieldSeperate<-read.csv("fitnessall.csv")
STandYieldSeperate<- STandYieldSeperate%>%
  mutate(Idnum = sprintf("%02d",Idnum))
SpecificLeafArea<-read_excel("Pfilifom_data.xlsx",sheet="LeafDataDestruct")
seedsmerge<-read.csv("seedsmerge.csv")
morphdata<-read_excel("Pfilifom_data.xlsx",sheet="NondestructiveMeasurments")
morphMerge<-merge(morphdata,anothercodesys, by="Idnum",all.x=TRUE)
anothercodesys<-read_excel("Pfilifom_data.xlsx",sheet="anothercode")
dates<-read_excel("Pfilifom_data.xlsx",sheet="DatesEct")
ion<-read_excel("Pfilifom_data.xlsx",sheet="Ion")
filidestruction<-read.csv("filifitness.csv")



#1. Specific Leaf Area (SLA)####
names(SpecificLeafArea )
SpecificLeafArea<-merge(SpecificLeafArea,anothercodesys,by="Idnum")
SLA <- SpecificLeafArea %>%
  group_by(Idnum,Rep,Treatment,Accession,Species,WvC) %>%
  summarise(SLA = mean(SLA), ##SLA
            StandardDeviation_SLA = sd(SLA),
            SE_SLA=StandardDeviation_SLA/sqrt(n()),
            WaterSLA = mean(WaterSLA), ##Water SLA
            StandardDeviation_WSLA = sd(WaterSLA),
            SE_WSLA=StandardDeviation_WSLA/sqrt(n()),
            LeafWater = mean(Water), ##Water
            StandardDeviation_Water = sd(Water),
            SE_Water=StandardDeviation_Water /sqrt(n()))
SLA$SpeciesWvC <- interaction(SLA$Species, SLA$WvC, sep = "_")

names(SLA)
ggplot(SLA,aes(x=Treatment,y=SLA, fill=Treatment))+
  facet_wrap(~SpeciesWvC)+
  geom_bar(stat = "summary", fun = "mean")

SLATreatment<-lmer(SLA~(1|Rep)+Species*Treatment,data=SLA)
Anova(SLATreatment)

tibble::view(SLA)

SLA_select<-SLA %>%
  dplyr::select(Idnum,Rep,Treatment,LeafWater,WaterSLA,SLA,Accession)

SLA_wide <- pivot_wider(SLA_select, names_from = Treatment, values_from =c(LeafWater,WaterSLA,SLA),values_fill=NA)
SLA_wide$STSLA<-SLA_wide$SLA_S/SLA_wide$SLA_C

SLAfitness<-0

SLAfitness<-merge(SLA_wide,STandYieldSeperate,  by = c("Idnum", "Rep","Species","Accession"))
eq<-SLA_C~(offset(log(STYield)+ (1|Rep)))

SLASTrelationship<-lmer(eq, data=SLAfitness, ziformula=~0, family=poisson)
summary(SLASTrelationship)
                           
                           

gglot(SLAfitness,aes(y=STYield))+
  geom_histogram()


Anova(SLASTrelationship) ## salt SLA is significantly correlated with the salt tolerance measured by yield--- smaller the SLA_S, the more salt tolerant, i.e. MAYBE, the

fixed_effects <- fixef(SLASTrelationship)
# Get the random effects
random_effects <- ranef(SLASTrelationship)$Rep
# Create a dataframe for the predictions
df_predictions <- SLAfitness%>%
  group_by(Rep) %>%
  mutate(predicted_y = fixed_effects["(Intercept)"] + fixed_effects["x"] * x + random_effects[Rep, "(Intercept)"])


ggplot(SLAfitness, aes(x=log(STYield+.001),y=SLA_S,color=factor(Rep)))+
  geom_point()+
  geom_line(data = df_predictions, aes(x = x, y = predicted_y, group = Rep), color = "blue") +
  theme_minimal() +
  stat_regline_equation(label.x = -2, label.y =1, aes(label = ..eq.label..)) +
  stat_cor(label.x = -2, label.y = .9, aes(label = ..rr.label..)) +
  stat_cor(label.x = -2, label.y = .8, aes(label = ..p.value..)) +
  theme_minimal()

SLASTrelationship<-lmer(log(STBiomass+.001)~(1|Rep)+SLA_S+SLA_C,data=SLAfitness)
Anova(SLASTrelationship)


SLAfitness1<-SLAfitness%>%
  dplyr::filter(STYield > 0)

ggplot(SLAfitness,aes(x=STYield,y=SLA_S))+
  geom_point()+
  geom_smooth(method="lm")

SLASTrelationship1<-lmer(log(STYield+.001)~(1|Rep)+SLA_S,data=SLAfitness)
Anova(SLASTrelationship1)

summary(SLASTrelationship1)

SLASTrelationship2<-lm(log(STBiomass+.001)~SLA_S,data=SLAfitness)
anova(SLASTrelationship2)


#2. Above Water ####

filidestruct<-filidestruction %>%
  dplyr::filter(Alive==1)%>%
  dplyr::select(Idnum,Rep,Treatment,AboveWater,WF,Alive)
seedsmerge_wide<-0

seedsmerge_wide <- pivot_wider(filidestruct, names_from = Treatment, values_from =c(,AboveWater,WF),values_fill=NA)
seedsmerge_wide<-merge(seedsmerge_wide,STandYieldSeperate,  by = c("Idnum", "Rep"))%>%
  dplyr::filter(Alive==1)

WateRrelationship<-lmer(log(STBiomass+.001)~(1|Rep)+AboveWater_C+AboveWater_S,data=seedsmerge_wide) 
WateRrelationship1<-lmer(log(STBiomass+.001)~(1|Rep)+AboveWater_C,data=seedsmerge_wide)
WateRrelationship2<-lmer(log(STYield+.001)~(1|Rep)+AboveWater_S,data=seedsmerge_wide)
Anova(WateRrelationship2)

ggplot(seedsmerge_wide, aes(y= AboveWater_S,x=STYield))+
         geom_point()+
  geom_smooth(method="lm")
  

WFrelationship<-lmer(log(STBiomass+.001)~(1|Rep)+WF_C+WF_S,data=seedsmerge_wide) 
WFrelationship1<-lmer(log(STBiomass+.001)~(1|Rep)+WF_C,data=seedsmerge_wide)## not significant 
WFrelationship2<-lmer(log(STBiomass+.001)~(1|Rep)+WF_S,data=seedsmerge_wide)###confused on this one
Anova(WateRrelationship2)


##The amount of water stored in a plant is correlated with the plants resistence to change with salt treatment. The more water the salt treated plant stored--- the more salt tolerant 

ggplot(seedsmerge_wide, aes(y= AboveWater_S,x=AboveWater_C))+
  geom_point()+
  geom_smooth(method="lm")
##Salt plants stored more water than control plants 




#IONS####

ion
Ion <- ion%>%
  group_by(Idnum,Rep,Treatment) %>%
  summarize(Cond = mean(Cond), 
            Na = mean(Na),
            K = mean(K),
            Kna = mean(Kna),.groups = 'drop',
            )

ionwide<-pivot_wider(Ion, names_from = Treatment, values_from =c(Cond,Na,K,Kna),values_fill=NA)
ionsmerge_wide<-merge(ionwide,STandYieldSeperate,  by = c("Idnum", "Rep"))

ionmerge<-merge(ion,STandYieldSeperate,by=c("Idnum", "Rep"))

ggplot(ionmerge,aes(x=Treatment,y=Cond,fill=Treatment))+
  facet_wrap(~Species)+
  geom_bar(stat = "identity")

ionimportance1<-lmer(log(STYield+.001)~(1|Rep)+Kna_S+Kna_C,data=ionsmerge_wide)
ionimportance2<-lmer(log(STYield+.001)~(1|Rep)+Na_S+Na_C,data=ionsmerge_wide)
ionimportance3<-lmer(log(STYield+.001)~(1|Rep)+Cond_S+Cond_C,data=ionsmerge_wide)
ionimportance4<-lmer(log(STYield+.001)~(1|Rep)+K_S+K_C,data=ionsmerge_wide)
Anova(ionimportance4)


ggplot(ionsmerge_wide,)
