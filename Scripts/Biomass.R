library(ggplot2)
library(ggpubr)
library(tidymodels)
library(tidyverse)
library(dplyr)
library(glmnet)
library(readxl)
library(agricolae)
setwd("C:/Users/asnyder/Desktop/P. filiformis/MeasurmentData")
AllSA<-read_excel("Pfilifom_data.xlsx",sheet="AllLeafSA")
water<-read_excel("Pfilifom_data.xlsx",sheet="AverageWater")
morphdata<-read_excel("Pfilifom_data.xlsx",sheet="NondestructiveMeasurments")
anothercodesys<-read_excel("Pfilifom_data.xlsx",sheet="anothercode")
AllSAmerge <- merge(AllSA,anothercodesys, by="Idnum",all.x=TRUE)
morphMerge<-merge(morphdata,anothercodesys, by="Idnum",all.x=TRUE)
write.csv(AllSAmerge,"AllSAmerge.csv")
write.csv(morphMerge,"Morphmerge.csv")
codesys<-read_excel("Pfilifom_data.xlsx",sheet="CodeSystem")
CO2<-read_excel("Pfilifom_data.xlsx",sheet="LeafDataDestruct")
CO2merge<-merge(CO2,anothercodesys, by="Idnum",all.x=TRUE)
seeds<-read_excel("Pfilifom_data.xlsx",sheet="SeedData")
WUE<-read_excel("Pfilifom_data.xlsx",sheet="WUE")
PlantDestruct<-read_excel("Pfilifom_data.xlsx",sheet="Destruct")
PlantDestructMerge<-merge(PlantDestruct,anothercodesys,by="Idnum",all.x=TRUE)
SLA<-read_excel("Pfilifom_data.xlsx",sheet="LeafDataDestruct")
SLAmerge<-merge(SLA,anothercodesys, by="Idnum",all.x=TRUE)
Ion<-read_excel("Pfilifom_data.xlsx",sheet="Ion")
Ionmerge<-merge(Ion,anothercodesys, by="Idnum",all.x=TRUE)
dates<-read_excel("Pfilifom_data.xlsx",sheet="DatesEct")

PlantDestruct
PlantDestructMerge<-merge(PlantDestruct,anothercodesys, by="Idnum")
names(PlantDestructMerge)



###finding the degree of differences 
PfiliDestruct <- PlantDestructMerge[PlantDestructMerge$Species == "P. filiformis", ]

anovadistruct<-lm(Rep~Accession*TotalAboveDryMass,data=PlantDestructMerge)

anova(anovadistruct)

PfiliDestruct
PfiliDestruct<- filter(PfiliDestruct, Rep == 1)

names(PlantDestructMerge)  

ggplot(PlantDestructMerge, aes(x = TotalAboveDryMass, y = AboveWater, color = Treatment))+
  geom_point() + 
  geom_smooth(method="lm",se=FALSE)+
  labs(title = "Above ground dry biomass P. filiformis",
       x = "Total Above Dry Mass",
       y = "Above Water") +
  theme_minimal()

names(PfiliDestruct)

ggplot(PfiliDestruct, aes( y= Numleaves, x= Treatment, fill = Treatment)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Idnum)+# Add points to the scatter plot
  scale_fill_manual(values = c("S" = "red", "C" = "blue")) + 
  labs(title = "Number of Leaves on day 95 by ID and Treatment  of P. filiformis",
       y = "Above Water") +
  theme_minimal()

ggplot(PlantDestructMerge, aes( y= Numleaves, x= Treatment, fill = Treatment)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Species)+# Add points to the scatter plot
  scale_fill_manual(values = c("S" = "red", "C" = "blue")) + 
  labs(title = "Number of Leaves on day 95 by ID and Treatment  of P. filiformis",
       y = "Above Water") +
  theme_minimal()

ggplot(PlantDestructMerge, aes( y= AboveWater, x= Treatment, fill = Treatment)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Accession*Species,labeller = function (labels) {
    labels <- lapply(labels, as.character)
    list(do.call(paste, c(labels, list(sep = "\n")))) })+
  scale_fill_manual(values = c("S" = "red", "C" = "blue")) + 
  labs(title = "Above Ground Water Mass by Accession,Species,and Treatment",
       y = "Above Water") +
  theme_minimal()

names(PfiliDestruct)

ggplot(PlantDestructMerge, aes( y= TotalAboveDryMass, x= Treatment, fill = Treatment)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Accession*Species,labeller = function (labels) {
    labels <- lapply(labels, as.character)
    list(do.call(paste, c(labels, list(sep = "\n")))) })+
  scale_fill_manual(values = c("S" = "red", "C" = "blue")) + 
  labs(title = "Dry Above Ground Biomass by Accession and Treatment",
       y = "Dry Biomass Above Ground (g)",x=NULL) +
  theme(panel.grid.major = element_blank(),
        strip.text = element_text(family="Nunito",size = 8),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        text = element_text(family = "Nunito"),
        axis.text = element_text(family = "Nunito", size = 14), 
        legend.text = element_text(family = "Nunito", size = 10), 
        plot.title = element_text(family = "Nunito", size = 16),
        axis.title.x = element_text(family = "Nunito",size = 12),
        axis.title.y = element_text(family = "Nunito",size = 12),
        legend.title = element_text(family = "Nunito", size = 14))


ggplot(PlantDestructMerge, aes( y= TotalAboveDryMass, x= Treatment, fill = Treatment)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Species*WvC)+
  scale_fill_manual(values = c("S" = "red", "C" = "blue")) + 
  labs(title = "Dry Above Ground Biomass by Species and Treatment",
       y = "Dry Biomass Above Ground (g)") +
  theme(panel.grid.major = element_blank(),
        strip.text = element_text(family="Nunito",size = 12),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(),
        text = element_text(family = "Nunito"),
        axis.text = element_text(family = "Nunito", size = 14), 
        legend.text = element_text(family = "Nunito", size = 10), 
        plot.title = element_text(family = "Nunito", size = 18),
        axis.title.x = element_text(family = "Nunito",size = 12),
        axis.title.y = element_text(family = "Nunito",size = 12),
        legend.title = element_text(family = "Nunito", size = 14))