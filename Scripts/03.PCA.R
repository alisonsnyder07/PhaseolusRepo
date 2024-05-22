setwd("C:/Users/asnyder/Desktop/P. filiformis/PhaseolusRepo/Data")

bioclimsd<-read.csv("bioclimsd.csv")
seedsmerge<-read.csv("seedsmerge.csv")
bioclimsd <- bioclimsd %>% dplyr::rename(Accession = accession) %>%  dplyr::rename(Idnum=X)
YieldST<-read.csv("YieldST.csv")
code<-read.csv("code.csv")
saltyieldTolerance<-read.csv("saltyieldTolerance.csv")

bioclimfili<-bioclimsd %>% 
  dplyr::filter(species== "P. filiformis") %>%
  dplyr::select(-X.1,-AnnualPrecipitation.Accession,-AnnualPrecipitation.AnnualPrecipitation,-species)

###1. trying ggpairs ####
ggpairs(bioclimfili[sample.int(nrow(bioclimfili), ncol(bioclimfili), )])

names(bioclimfili)
bioclimfili <- bioclimfili %>% dplyr::select(-species)
str(bioclimfili)
ncol(bioclimfili)

cor(bioclimfili[,c(4:21)],method="spearman", use = "complete.obs")


p_value <- lapply(bioclimfili, function(x) corrplot::cor.mtest(bioclimfili[, 4:10])[["p"]])
correlation <- lapply(bioclimfili, function(x) cor(x[, 4:10], method = "spearman", use = 'complete.obs')) ###Incorrect number of correlations i dont know why-- i dont know what this is doing


str(bioclimfili)
corr_matrix <- cor(bioclimfili[,c(3:21)]) ###Loading numbers 
rownames(corr_matrix)
ggcorrplot(corr_matrix) ###correlation plot


data.pca <- princomp(corr_matrix)
summary(data.pca)

data.pca$loadings[, 1:2] ##96% of variation is explained in the 2 principle components
fviz_eig(data.pca, addlabels = TRUE)
fviz_pca_var(data.pca, col.var = "black",title=" PCA of Bioclimactic Variables correlated to Accession's Origin")

###2. Vegan package ####

nrow(bioclimfili)
corr_matrix1<-cor(bioclimfili[,c(3:21)])
pca.out1<-prcomp(corr_matrix1)
biplot(pca.out1)
fviz_eig(pca.out1, addlabels = TRUE) ##95%
fviz_pca_biplot(pca.out1, repel = TRUE, select.var = list(contrib = 10))### PCA of 20 bioclimactic variables to pick the top 10 most influential

bioclimfili<-data.frame(bioclimfili)
bioclimSmallerPCA<-subset(bioclimfili,
                          select=c(Accession,Mean_Temp_Coolest_Quarter,Temp_Annual_Range,Temperature_seasonality,Precip_Seasonality,Annual_Precip,Precip_Driest_Quarter,Precip_Wettest_Quarter,Average_temp,Max_Temp_Warmest_Month,Min_Temp_Coolest_Month,Precip_Driest_Quarter))### now dataframe of top 7 contributers of PCA, PCA1 and PCA2 explain 90% of the variation!!
corr_matrix2<-cor(bioclimSmallerPCA[,c(2:12)])
pca.out2<-prcomp(corr_matrix2)
fviz_pca_biplot(pca.out2)
fviz_eig(pca.out2, addlabels = TRUE)###98% variation explained in 2 PC's... BINGO using 11 variables

salttolerancePCA<-YieldST%>%
  dplyr::select(Accession,ST)

bioclim$Accession<-bioclimfili$Accession
scores<-as.data.frame(predict(pca.out2, newdata=bioclimSmallerPCA))
summary(scores)
bioclim <- bioclimSmallerPCA %>%
  mutate(PCA1 = scores$PC1,
         PCA2=scores$PC2)
salsd <- salsd %>% rename(Accession = accession)
elevationsd <- elevationsd %>% rename(Accession = accession)
bioclim<-merge(salsd,bioclim, by="Accession", all.x=FALSE)
bioclim<-merge(elevationsd,bioclim,by="Accession",all.x=FALSE)
bioclim<-merge(salttolerancePCA,bioclim,by="Accession",all.x=TRUE)
bioclim<-merge(saltyieldTolerance,bioclim,by="Accession",all.x=FALSE)
names(bioclim)### bioclim now has the 11 bioclim variables, the 2 PC that describe 98% of the variation across the populations, salt index value and elevation###


###how to the two salt variables compare to eachother
saltytraits<-lm(log(ST)~log(saltyieldTolerance) , data=bioclim)
Anova(saltytraits)### they are not correlated??


##3.model of prediciting values for salt tolerance and bioclim relationship ####
tibble::view(bioclim)
names(bioclim)
modelfit1 <- lm (log(ST) ~ PCA1 + PCA2 + elevation+ Salinity_class,data=bioclim)##slightly significant with PCA1 and salinity_class
modelfit1.1<- lm (log(ST) ~ PCA1,data=bioclim)##nope
modelfit1.2<- lm (log(ST) ~ PCA2,data=bioclim) 
modelfit1.3<- lm (log(ST) ~ Salinity_class,data=bioclim) ##nope
Anova(modelfit1.3)

#Signifcant Salt Treatment to Environemnt-- not taking into account control #######################################
modelfit2 <- lm (log(saltyieldTolerance) ~PCA1 + PCA2 + elevation+ Salinity_class,data=bioclim)
modelfit2.1<-lm(log(saltyieldTolerance) ~PCA1,data=bioclim) ##p-value=.0633
modelfit2.2<-lm(log(saltyieldTolerance) ~PCA2,data=bioclim)##.3853
modelfit2.3<-lm(log(saltyieldTolerance) ~elevation,data=bioclim)### .749
modelfit2.4<-lm(log(saltyieldTolerance) ~Salinity_class,data=bioclim)##.2821
anova(modelfit2)


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




