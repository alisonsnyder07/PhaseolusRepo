#1. Set Up ####
setwd("C:/Users/asnyder/Desktop/P. filiformis/PhaseolusRepo/Data")

bioclimsd<-read.csv("bioclimsd.csv")
bioclimsd <- bioclimsd %>%
  mutate(Idnum = sprintf("%02d", Idnum))
seedsmerge<-read.csv("seedsmerge.csv")
YieldST<-read.csv("YieldST.csv")
code<-read.csv("code.csv")
saltyieldTolerance<-read.csv("saltyieldTolerance.csv")
STbyrep<-read.csv("STrepseperate.csv")
GPSformap<-merge(gps,YieldST, by = "Accession")
STbyRep<-read.csv("fitnessall.csv")
STbyRep<-STbyRep %>%
  mutate(Idnum = sprintf("%02d", Idnum))
spatialpoints <- readOGR(dsn = ".", layer = "sp_bioclim_df")
gpsformap<-read.csv("gpsformap.csv")


bioclimfili<-bioclimsd %>% 
  dplyr::select(-AnnualPrecip.Idnum,-X)
YieldST<-merge(anothercodesys,YieldST,by="Accession")
bioclimfili<-merge(bioclimfili[,c(1:33)],YieldST, by= "Idnum")

bioclimfili<-bioclimfili %>%
  dplyr::select(-X.2,-X.1,-species)


names(bioclimfili)

###2. Vegan package ####
corr_matrix1<-cor(bioclimfili[,c(6:23)]) ###not including terrain
pca.out1<-prcomp(corr_matrix1)
biplot(pca.out1)
fviz_eig(pca.out1, addlabels = TRUE) ##95%



fviz_pca_biplot(pca.out1, repel = TRUE, select.var = list(contrib = 10))### PCA of 20 bioclimactic variables to pick the top 10 most influential



bioclimfili<-data.frame(bioclimfili)

bioclimSmallerPCA<-subset(bioclimfili,
                          select=c(Accession,Mean_Temp_Coolest_Quarter,Temp_Annual_Range,Temperature_seasonality,Precip_Seasonality,Annual_Precip,Precip_Driest_Quarter,Average_temp,Max_Temp_Warmest_Month,Min_Temp_Coolest_Month,Isothermality, Precip_Driest_Quarter,Lat,Salinity_class,elevation, STYield,STPods, STBiomass))
corr_matrix2<-cor(bioclimSmallerPCA[,c(2:13)])
pca.out2<-prcomp(corr_matrix2)
fviz_pca_biplot(pca.out2)
fviz_eig(pca.out2, addlabels = TRUE)###most of variation is in first PCA--- I want to use the bioclim variables for the PCA 

##get the loadings of the first PC's
loadings <-pca.out1$rotation
print(loadings)

##PC1 is split on temp seasonality and how dry its gets
##PC2 is mostly precipitation (51%), Driest Quarter(29%), and Precip Seasonality(17%)

salttolerancePCA<-YieldST%>%
  dplyr::select(Accession,STYield,STPods,STBiomass)

bioclimSmallerPCA$Accession<-bioclimfili$Accession
scores<-as.data.frame(predict(pca.out1, newdata=bioclimfili))
summary(scores)

length(scores$PC1)

bioclim <- bioclimfili %>%
  mutate(PCA1 = scores$PC1,
         PCA2=scores$PC2)

gpsformap<-merge(gpsformap,bioclim, by = "Idnum")
write.csv(gpsformap,"gpsformap.csv")

### bioclim now has the 11 bioclim variables, the 2 PC that describe 98% of the variation across the populations, salt index value and elevation###



###looking for significance when seperatedbyRep

bioclimWT<-bioclim%>%
  dplyr::select(-X, - STYield, -STBiomass, - STPods)
STbyRep1<-merge(bioclimWT,STbyRep, by = "Idnum",all.x=FALSE)
STbyRepPfil<-STbyRep1%>%
  dplyr::filter(STYield>0)
names(STbyRepPfil)
totalyieldlm1<-lmer(log(STYield+.00001)~(1|Rep)+Average_temp,data=STbyRepPfil)
totalyieldlm2 <- glmmTMB(STYield ~ Average_temp+ (1|Rep), 
                family = poisson, 
                data = STbyRepPfil)
Anova(totalyieldlm1) 
summary(totalyieldlm1)

ggplot(STbyRepPfil, aes(x=log(STYield+.00001), y = Average_temp))+
  geom_point()+
  geom_smooth(method="lm")+
  labs(title = " Relationship Between Salt Tolerance and Average Temperature of " ~ italic("Phaseolus filiformis")~ " Accessions across it's range ",
       y = "Average Annual Temperature",
       x = "log(SaltTolerancebyYield)")+
  theme(
    panel.background = element_blank(), # Remove panel background
    panel.grid.major = element_line(color = "grey80"), # Customize major gridlines
    panel.grid.minor = element_line(color = "grey90"), # Customize minor gridlines
    panel.border = element_blank(), # Remove panel border
    strip.background = element_blank(),
    strip.text = element_text(family="Nunito",size = 14),
    text = element_text(family = "Nunito"),
    axis.text = element_text(family = "Nunito", size = 12),
    axis.title.x = element_text(family = "Nunito", size = 16), 
    axis.title.y = element_text(family = "Nunito", size = 16), 
    legend.text = element_text(family = "Nunito", size = 12), 
    plot.title = element_text(family = "Nunito", size = 16),
    legend.title = element_text(family = "Nunito", size = 14))+
  theme_minimal()



STbyRepMOST<-STbyRepPfil %>%
  dplyr::filter(Accession %in% gpsformap$Accession)

MostSaltTolerantlm<-lmer(log(STYield+.00001)~(1|Rep)+Lat,data=STbyRepMOST)
Anova(MostSaltTolerantlm) 
summary(totalyieldlm1)




totalyieldlm2<-lmer(log(STBiomass+.00001)~(1|Rep)+Accession.x+PCA2,data=STbyRepPfil)
Anova(totalyieldlm2)
summary(totalyieldlm2)

ggplot(STbyRepPfil,aes(x=log(STBiomass,y=Annual_Precip))+
  geom_point()
#morans 
pointsdistR<-as.matrix(dist(cbind(STbyRepPfil$Long,STbyRepPfil$Lat)))
pointsdist.dists.invR <- 1/(1+pointsdistR)
diag(pointsdist.dists.invR) <- 0

pointsdist.binR <- (pointsdistR > 0 & pointsdistR <= .75)
Moran.I(log(STbyRepPfil$STBiomass+.0001), pointsdist.dists.invR)

Moran.I(log(STbyRepPfil$STYield+.0001), pointsdist.dists.invR)

###how to the two salt variables compare to eachother
saltytraits<-lm(STYield~(saltyieldToleranceYield) , data=bioclim)
anova(saltytraits)### p-value: .0019--makes sense, ST yeild is very correlated with saltyieldTolerance 

##3.model of prediciting values for salt tolerance and bioclim relationship ####

##totalYield

modelfit1 <- lm(log(STYield) ~ PCA1 + PCA2 + elevation+Lat+Average_temp,data=bioclim)
modelfit1.1<- lm (log(STYield)~ PCA1,data=bioclim) #.906
modelfit1.12<-lm (log(STYield) ~ Annual_Precip,data=bioclim) #.3726
modelfit1.2<- lm (log(STYield) ~ PCA2,data=bioclim) #.4162
modelfit1.3<- lm (log(STYield) ~ Salinity_class,data=bioclim) #.707
modelfit1.4<- lm (log(STYield) ~ Lat,data=bioclim) #.808
modelfit1.5<- lm (log(STYield) ~ Average_temp,data=bioclim) #.5
Anova(modelfit1)

###GRAPHS
ggplot(bioclim, aes(x= PCA1, y = STYield))+
  geom_point()+
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  stat_regline_equation(label.x = 200, label.y = 2, aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), 
                        formula = y ~ x, size = 4, color = "black")+
  ylim(0,2.5)+
  labs(title= "Linear model relationship between Salt Tolerance (g seed) and 1st Principle Component of " ~italic("P.filiformis"), x= "1st Principle Component of bioclimactic and geographic variables", y= "Salt Tolerance")+
  theme(
    panel.background = element_blank(), # Remove panel background
    panel.grid.major = element_line(color = "grey80"), # Customize major gridlines
    panel.grid.minor = element_line(color = "grey90"), # Customize minor gridlines
    panel.border = element_blank(), # Remove panel border
    strip.background = element_blank(),
    strip.text = element_text(family="Nunito",size = 10),
    text = element_text(family = "Nunito"),
    axis.text = element_text(family = "Nunito", size = 10), 
    legend.text = element_text(family = "Nunito", size = 10), 
    plot.title = element_text(family = "Nunito", size = 10),
    legend.title = element_text(family = "Nunito", size = 10))


ggplot(bioclim, aes(x= PCA2, y = STYield))+
  geom_point()+
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  stat_regline_equation(label.x = 500, label.y = 1.5, aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), 
                        formula = y ~ x, size = 4, color = "black")+
  ylim(0,2.5)+
  labs(title= "Linear model relationship between Salt Tolerance (g seed) and 2nd Principle Component of " ~italic("P.filiformis"), x= "2nd Principle Component of Bioclimactic and geographic variables", y= "Salt Tolerance")+
  theme(
    panel.background = element_blank(), # Remove panel background
    panel.grid.major = element_line(color = "grey80"), # Customize major gridlines
    panel.grid.minor = element_line(color = "grey90"), # Customize minor gridlines
    panel.border = element_blank(), # Remove panel border
    strip.background = element_blank(),
    strip.text = element_text(family="Nunito",size = 10),
    text = element_text(family = "Nunito"),
    axis.text = element_text(family = "Nunito", size = 10), 
    legend.text = element_text(family = "Nunito", size = 10), 
    plot.title = element_text(family = "Nunito", size = 10),
    legend.title = element_text(family = "Nunito", size = 10))



##totalPods
names(bioclim)
modelfit2 <- lm (log(STPods) ~ PCA1 + PCA2 + elevation,data=bioclim)
modelfit2.1<- lm (log(STPods) ~ PCA1,data=bioclim) 
modelfit2.2<- lm (log(STPods) ~ PCA2,data=bioclim) 
modelfit2.3<- lm (log(STPods) ~ Salinity_class,data=bioclim) 
modelfit2.4<- lm (log(STPods) ~ Lat,data=bioclim) 
modelfit2.5<- lm (log(STPods) ~ elevation,data=bioclim) 
Anova(modelfit2)

ggplot(bioclim, aes( x= STYield, y = Lat))+
  geom_point()

ggplot(bioclim, aes(x = log(STYield))) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") 


##totalBiomass
names(bioclim)

biomassYield<-lm(STBiomass~STYield, data=bioclim)
anova(biomassYield)
modelfit3 <- lm(STBiomass ~ PCA1 + PCA2 + elevation,data=bioclim)
modelfit3.1<- lm(log(STBiomass) ~ PCA1,data=bioclim) 
modelfit3.2<- lm(log(STBiomass)~PCA2,data=bioclim)
modelfit3.3<- lm (log(STBiomass) ~ Salinity_class,data=bioclim) 
modelfit3.4<- lm (log(STBiomass) ~ Lat,data=bioclim)
modelfit3.5<- lm (log(STBiomass) ~ elevation,data=bioclim)
modelfit3.6<- lm (log(STBiomass) ~ Annual_Precip,data=bioclim)
modelfit3.7<- lm (log(STBiomass) ~ Average_temp,data=bioclim)
modelfit3.8<- lm (log(STBiomass) ~ Isothermality,data=bioclim)
##no
Anova(modelfit3)

ggplot(bioclim, aes(x= STBiomass, y=PCA2 ))+
  geom_point() ## positive relationship-- higher PCA2, higher STBiomass

ggplot(bioclim, aes(x=STBiomass, y= Salinity_class))+
  geom_bar(stat = "identity")

##4. Morans ####

spatialpoints

pointsdist<-as.matrix(dist(cbind(spatialpoints$Long,spatialpoints$Lat)))
pointsdist.dists.inv <- 1/(pointsdist+1)
diag(pointsdist.dists.inv) <- 0

Moran.I(log(bioclim$STYield), pointsdist.dists.inv)

Moran.I(log(bioclim$STBiomass), pointsdist.dists.inv)

#5.Signifcant Salt Treatment to Environemnt-- not taking into account control #######################################
modelfit4 <- lm (log(saltyieldToleranceYield+.001) ~PCA1 + PCA2 + elevation+ Salinity_class,data=bioclim)
modelfit4.1<-lm(log(saltyieldToleranceYield+.001) ~PCA1, data=bioclim) 
modelfit4.2<-lm(log(saltyieldToleranceYield+.001) ~PCA2,data=bioclim)
modelfit4.3<-lm(log(saltyieldToleranceYield+.001) ~elevation,data=bioclim)
modelfit4.4<-lm(log(saltyieldToleranceYield+.001) ~Salinity_class,data=bioclim) 
modelfit4.5<-lm(saltyieldToleranceYield ~ Lat,data=bioclim) 
Anova(modelfit4) ### none are statistically significant?


write.csv(forposterST, "Results.csv")





####I want to look at significance when seperating reps 
bioclim2 <- bioclimfili %>%
  mutate(PCA1 = scores$PC1,
         PCA2=scores$PC2) %>%
  dplyr::select(-STYield,-STPods)
bioclim2 <- bioclim2 %>%
  distinct()
bioclim2 <- left_join(STbyrep, bioclim2, by = "Accession",relationship =
                        "many-to-many")

bioclim2rep2<-bioclim2 %>%
  dplyr::filter(Rep==2)

bioclim2$Rep <- factor(bioclim2$Rep)
bioclim2$Accession <- factor(bioclim2$Accession)
bioclim2$PCA2 <- factor(bioclim2$Treatment)

modelfit1.1<- lmer(s_Pods~ Lat+ (1|Accession)+ (1|Rep), data=bioclim2)

names(bioclim2)

modelfit1.12<-lm(log(STYield.x+.0001) ~ Annual_Precip,data=bioclim2)
modelfit1.2<- lm(log(STYield.x+.0001) ~ PCA2,data=bioclim2) ###no 
modelfit1.3<- lm((STYield.x+.0001) ~ Salinity_class,data=bioclim2)
modelfit1.4<- lm(log(STYield.x+.0001) ~ Lat,data=bioclim2) ## p-values: 0.02483
Anova(modelfit1.1)

ggplot(bioclim2rep2, aes(x= PCA2, STYield))+
  geom_point()

write.csv(bioclim2, "bioclimSTacrossallreps.csv")

#5. Results of Fitness and Bioclim####
## There is NO significant relationship between salt tolerance and geographic location :((





