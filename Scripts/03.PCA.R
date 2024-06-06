setwd("C:/Users/asnyder/Desktop/P. filiformis/PhaseolusRepo/Data")

bioclimsd<-read.csv("bioclimsd.csv")
seedsmerge<-read.csv("seedsmerge.csv")
bioclimsd <- bioclimsd %>% dplyr::rename(Accession = accession) %>%  dplyr::rename(Idnum=X)
YieldST<-read.csv("YieldST.csv")
code<-read.csv("code.csv")
saltyieldTolerance<-read.csv("saltyieldTolerance.csv")


names(bioclimf)
bioclimfili<-bioclimsd %>% 
  dplyr::filter(species== "P. filiformis") %>%
  dplyr::select(-AnnualPrecip.AnnualPrecipitation,-AnnualPrecip.Accession,-species)
bioclimfili<-merge(bioclimfili,YieldST, by= "Accession")


##ggcorplot idk 
str(bioclimfili)
corr_matrix <- cor(bioclimfili[,c(3:22)]) ###Loading numbers 
rownames(corr_matrix)
ggcorrplot(corr_matrix) ###correlation plot


data.pca <- princomp(corr_matrix)
summary(data.pca)

data.pca$loadings[, 1:2] ##96% of variation is explained in the 2 principle components
fviz_eig(data.pca, addlabels = TRUE)
fviz_pca_var(data.pca, col.var = "black",title=" PCA of Bioclimactic Variables correlated to Accession's Origin")

###2. Vegan package ####
names(bioclimfili)
ncol(bioclimfili)
corr_matrix1<-cor(bioclimfili[,c(3:23)])
pca.out1<-prcomp(corr_matrix1)
biplot(pca.out1)
fviz_eig(pca.out1, addlabels = TRUE) ##95%
fviz_pca_biplot(pca.out1, repel = TRUE, select.var = list(contrib = 10))### PCA of 20 bioclimactic variables to pick the top 10 most influential
bioclimfili<-data.frame(bioclimfili)
bioclimSmallerPCA<-subset(bioclimfili,
                          select=c(Accession,Mean_Temp_Coolest_Quarter,Temp_Annual_Range,Temperature_seasonality,Precip_Seasonality,Annual_Precip,Precip_Driest_Quarter,Average_temp,Max_Temp_Warmest_Month,Min_Temp_Coolest_Month,Precip_Driest_Quarter,Lat))### now dataframe of top 7 contributers of PCA, PCA1 and PCA2 explain 90% of the variation!!
corr_matrix2<-cor(bioclimSmallerPCA[,c(2:12)])
pca.out2<-prcomp(corr_matrix2)
fviz_pca_biplot(pca.out1)
fviz_eig(pca.out1, addlabels = TRUE)###98% variation explained in 2 PC's... BINGO using 11 variables

##get the loadings of the first PC's
loadings <-pca.out2$rotation
print(loadings)

##PC1 is split on temp seasonality and how dry its gets
##PC2 is mostly precipitation (51%), Driest Quarter(29%), and Precip Seasonality(17%)

salttolerancePCA<-YieldST%>%
  dplyr::select(Accession,STYield,STPods,STBiomass)

bioclimSmallerPCA$Accession<-bioclimfili$Accession
scores<-as.data.frame(predict(pca.out2, newdata=bioclimSmallerPCA))
scores
summary(scores)
bioclim <- bioclimSmallerPCA %>%
  mutate(PCA1 = scores$PC1,
         PCA2=scores$PC2)
salsd <- salsd %>% rename(Accession = accession)
elevationsd <- elevationsd %>% rename(Accession = accession)
bioclim<-merge(salsd,bioclim, by="Accession", all.x=FALSE)
bioclim<-merge(elevationsd,bioclim,by="Accession",all.x=FALSE)
bioclim<-merge(YieldST,bioclim,by="Accession",all.x=TRUE)
bioclim<-merge(saltyieldTolerance,bioclim,by="Accession",all.x=FALSE)
names(bioclim)### bioclim now has the 11 bioclim variables, the 2 PC that describe 98% of the variation across the populations, salt index value and elevation###


###how to the two salt variables compare to eachother
tibble::view(bioclim)
saltytraits<-lm(log(STYield)~(saltyieldToleranceYield) , data=bioclim)
anova(saltytraits)### p-value: .02596--makes sense



STparam<-lm(log(STYield)~(STPods) , data=bioclim)
Anova(STparam)##extremely correlated

##3.model of prediciting values for salt tolerance and bioclim relationship ####
tibble::view(bioclim)
##totalYield
names(bioclim)
modelfit1 <- lm (log(STYield) ~ PCA1 + PCA2 + elevation+ Salinity_class,data=bioclim)
modelfit1.1<- lm (log(STYield) ~ PCA1,data=bioclim) ### YES! p-value= .001474
modelfit1.12<-lm (log(STYield) ~ Annual_Precip,data=bioclim)
modelfit1.2<- lm (log(STYield) ~ PCA2,data=bioclim) ###no 
modelfit1.3<- lm ((STYield) ~ Salinity_class,data=bioclim) ## p-value=.6245--- without the average seeds/pod YES, p-values: .00151 
modelfit1.4<- lm (log(STYield) ~ Lat,data=bioclim) ## p-values: 0.02483
anova(modelfit1.12)


ggplot(bioclim, aes(x=STYield, y = PCA1))+
  geom_point()

## outliers.... how do I fix those...

dcr_app(bioclim)

##totalPods
names(bioclim)
modelfit2 <- lm (log(STPods) ~ PCA1 + PCA2 + elevation+ Salinity_class,data=bioclim)
modelfit2.1<- lm (log(STPods) ~ PCA1,data=bioclim) ### YES p-value: .002282
modelfit2.2<- lm (log(STPods) ~ PCA2,data=bioclim) ##no
modelfit2.3<- lm (log(STPods) ~ Salinity_class,data=bioclim) ### YES p-value: .03141
modelfit2.4<- lm (log(STPods) ~ Lat,data=bioclim) ### YES, p-value: .02687
modelfit2.5<- lm (log(STPods) ~ elevation,data=bioclim) ##no
Anova(modelfit2.4)

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



#Signifcant Salt Treatment to Environemnt-- not taking into account control #######################################
modelfit4 <- lm (log(saltyieldToleranceYield) ~PCA1 + PCA2 + elevation+ Salinity_class,data=bioclim)
modelfit4.1<-lm(log(saltyieldToleranceYield) ~PCA1, data=bioclim) ## YES: p-value: .0291
modelfit4.2<-lm(log(saltyieldToleranceYield) ~PCA2,data=bioclim) ###no 
modelfit4.3<-lm(log(saltyieldToleranceYield) ~elevation,data=bioclim) ##no
modelfit4.4<-lm(log(saltyieldToleranceYield) ~Salinity_class,data=bioclim) ###no-- so the total production of salty fruit is not correlated with the salt class--- BUT salt tolerance when looking at yield IS correlated with salt class??
modelfit4.5<-lm(log(saltyieldToleranceYield) ~Lat,data=bioclim) ###yes: p-value: .047
Anova(modelfit4.5) ### none are statistically significant?

forposterST<-merge(bioclim,bioclimsd, by="Accession")
write.csv(forposterST, "Results.csv")



#Results of Fitness and Bioclim####
# it looks as if for Total Yield mass and Total Pods (which are extremely correlated with eachother), that PCA1, latitude, and salinity class are significantly correlated with the log (TotalPods/TotalYield). Total Biomass on the other hand is only correlated with PCA2 and 



--------------------------------------------------------------- ###dont know
###dont remember whats going on here
Loadings<-as.data.frame(pca.out2$rotation)
Loadings
biplot(pca.out2)
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

---------------------------------------------------------
str(pca.out2)
predict(pca.out2)


###now trying psych???--- also dont know what I am doing here??
library(psych)
pc<-prcomp(bioclimfili[,c(2:21)],centr=TRUE,scale.=TRUE)
attributes(pc)
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

----------------------------------------------------------- ###dont know


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




