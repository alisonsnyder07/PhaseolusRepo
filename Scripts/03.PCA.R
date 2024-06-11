#1. Set Up ####
setwd("C:/Users/asnyder/Desktop/P. filiformis/PhaseolusRepo/Data")

bioclimsd<-read.csv("bioclimsd.csv")
seedsmerge<-read.csv("seedsmerge.csv")
YieldST<-read.csv("YieldST.csv")
code<-read.csv("code.csv")
saltyieldTolerance<-read.csv("saltyieldTolerance.csv")
STbyrep<-read.csv("STbyRep.csv")
GPSformap<-merge(gps,YieldST, by = "Accession")

bioclimfili<-bioclimsd %>% 
  dplyr::filter(species== "P. filiformis") %>%
  dplyr::select(-AnnualPrecip.AnnualPrecipitation,-AnnualPrecip.Accession,-species)
bioclimfili<-merge(bioclimfili,YieldST, by= "Accession")

names(bioclimfili)

###2. Vegan package ####
corr_matrix1<-cor(bioclimfili[,c(3:24)])
pca.out1<-prcomp(corr_matrix1)
biplot(pca.out1)
fviz_eig(pca.out1, addlabels = TRUE) ##95%
fviz_pca_biplot(pca.out1, repel = TRUE, select.var = list(contrib = 10))### PCA of 20 bioclimactic variables to pick the top 10 most influential



bioclimfili<-data.frame(bioclimfili)

bioclimSmallerPCA<-subset(bioclimfili,
                          select=c(Accession,Mean_Temp_Coolest_Quarter,Temp_Annual_Range,Temperature_seasonality,Precip_Seasonality,Annual_Precip,Precip_Driest_Quarter,Average_temp,Max_Temp_Warmest_Month,Min_Temp_Coolest_Month,Isothermality, Precip_Driest_Quarter,Lat,Salinity_class,elevation, STYield,STPods))# STBiomass))### now dataframe of top 7 contributers of PCA, PCA1 and PCA2 explain 90% of the variation!!
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
  dplyr::select(Accession,STYield,STPods)

bioclimSmallerPCA$Accession<-bioclimfili$Accession
scores<-as.data.frame(predict(pca.out1, newdata=bioclimfili))
scores
summary(scores)

bioclim <- bioclimfili %>%
  mutate(PCA1 = scores$PC1,
         PCA2=scores$PC2)
bioclim<-merge(saltyieldTolerance,bioclim,by="Accession",all.x=FALSE)
names(bioclim)### bioclim now has the 11 bioclim variables, the 2 PC that describe 98% of the variation across the populations, salt index value and elevation###


###how to the two salt variables compare to eachother
tibble::view(bioclim)
saltytraits<-lm(STYield~(saltyieldToleranceYield) , data=bioclim)
anova(saltytraits)### p-value: .0034--makes sense, ST yeild is very correlated with saltyieldTolerance 



STparam<-lm(log(STYield)~log(STPods) , data=bioclim)
Anova(STparam)##extremely correlated, good 

##3.model of prediciting values for salt tolerance and bioclim relationship ####

##totalYield
tibble::view(bioclim)
modelfit1 <- lm (log(STYield) ~ PCA1 + PCA2 + elevation+ Salinity_class,data=bioclim)
modelfit1.1<- lm (log(STYield)~ PCA1,data=bioclim) 
modelfit1.12<-lm (log(STYield) ~ Annual_Precip,data=bioclim)
modelfit1.2<- lm (log(STYield) ~ PCA2,data=bioclim) 
modelfit1.3<- lm ((STYield) ~ Salinity_class,data=bioclim)
modelfit1.4<- lm (log(STYield) ~ Lat,data=bioclim) 
anova(modelfit1)


ggplot(bioclim, aes(x=log(STYield), y = Lat))+
  geom_point()+
  geom_smooth(method="lm", se=FALSE)+
  stat_regline_equation(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~")),
                        label.x = -1, label.y = 0)

##totalPods
names(bioclim)
modelfit2 <- lm (log(STPods) ~ PCA1 + PCA2 + elevation+ Salinity_class,data=bioclim)
modelfit2.1<- lm (log(STPods) ~ PCA1,data=bioclim) ### YES p-value: .002282
modelfit2.2<- lm (log(STPods) ~ PCA2,data=bioclim) ##no
modelfit2.3<- lm (log(STPods) ~ Salinity_class,data=bioclim) ### YES p-value: .03141
modelfit2.4<- lm (log(STPods) ~ Lat,data=bioclim) ### YES, p-value: .02687
modelfit2.5<- lm (log(STPods) ~ elevation,data=bioclim) ##no
Anova(modelfit2)

ggplot(bioclim, aes( x= STYield, y = Lat))+
  geom_point()

ggplot(bioclim, aes(x = log(STYield))) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") 


##totalBiomass
names(bioclim)

biomassYield<-lm(STBiomass~STYield, data=bioclim)
Anova(biomassYield)
modelfit3 <- lm (log(STBiomass) ~ PCA1 + PCA2 + elevation+ Salinity_class,data=bioclim)
modelfit3.1<- lm (log(STBiomass) ~ PCA1,data=bioclim) ##no
modelfit3.2<- lm (log(STBiomass) ~ PCA2,data=bioclim) ##Yes: 0.01913 
modelfit3.3<- lm (log(STBiomass) ~ Salinity_class,data=bioclim) ##no
modelfit3.4<- lm (log(STBiomass) ~ Lat,data=bioclim) ##no
modelfit3.5<- lm (log(STBiomass) ~ elevation,data=bioclim) ##no
Anova(modelfit3.5)

ggplot(bioclim, aes(x= STBiomass, y=PCA2 ))+
  geom_point() ## positive relationship-- higher PCA2, higher STBiomass

ggplot(bioclim, aes(x=STBiomass, y= Salinity_class))+
  geom_bar(stat = "identity")



#4.Signifcant Salt Treatment to Environemnt-- not taking into account control #######################################
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
         PCA2=scores$PC2)
bioclim2 <- bioclim2 %>%
  distinct()
bioclim2 <- left_join(STbyrep, bioclim2, by = "Accession",relationship =
                        "many-to-many")
tibble::view(bioclim2)
names(bioclim2)

modelfit1.1<- lm(log(STYield.x+.0001)~ PCA1),data=bioclim2) ### YES! p-value= .001474
modelfit1.12<-lm(log(STYield.x+.0001) ~ Annual_Precip,data=bioclim2)
modelfit1.2<- lm(log(STYield.x+.0001) ~ PCA2,data=bioclim2) ###no 
modelfit1.3<- lm((STYield.x+.0001) ~ Salinity_class,data=bioclim2)
modelfit1.4<- lm(log(STYield.x+.0001) ~ Lat,data=bioclim2) ## p-values: 0.02483
anova(modelfit1.4)


#5. Results of Fitness and Bioclim####
## There is NO significant relationship between salt tolerance and geographic location :((





