bioclimsd<-read.csv("bioclimsd.csv")
seedsmerge<-read.csv("seedsmerge.csv")
bioclimsd <- bioclimsd %>% dplyr::rename(Accession = accession) %>%  dplyr::rename(Idnum=X)
bioclimfili<-bioclimsd %>% 
  dplyr::filter(species== "P. filiformis")

ggpairs(bioclimfili[sample.int(nrow(bioclimfili), ncol(bioclimfili), ]))

names(bioclimfili)
bioclimfili <- bioclimfili %>% dplyr::select(-species)
str(bioclimfili)


ncol(bioclimfili)

cor(bioclimfili[,c(2:30)],method="spearman", use = "complete.obs")


p_value <- lapply(bioclimfili, function(x) corrplot::cor.mtest(bioclimfili[, 4:12])[["p"]])
correlation <- lapply(bioclimfili, function(x) cor(x[, 4:12], method = "spearman", use = 'complete.obs'))

###7.PCA for bioclim and terrain####

ncol(bioclimfili)

rownames(corr_matrix)
corr_matrix <- cor(bioclimfili[,c(2:21)])
ggcorrplot(corr_matrix)


data.pca <- princomp(corr_matrix)
summary(data.pca)

data.pca$loadings[, 1:2]
fviz_eig(data.pca, addlabels = TRUE)

fviz_pca_var(data.pca, col.var = "black",title=" PCA of Bioclimactic Variables correlated to Accession's Origin")

###trying vegan package here
names(bioclimfili)
bioclimSmallerPCA<-bioclimfili %>%
  dplyer::select()


nrow(bioclimfili)
pca.out1<-prcomp(bioclimfili[,c(2:21)],scale=TRUE)
biplot(pca.out)

fviz_pca_biplot(pca.out1, repel = TRUE, select.var = list(contrib = 15))### PCA of 20 bioclimactic variables to pick the top 5 most influential

bioclimfili<-data.frame(bioclimfili)

bioclimSmallerPCA<-subset(bioclimfili,
                          select=c(Mean_Temp_Coolest_Quarter,Temp_Annual_Range,Temperature_seasonality,Precip_Seasonality,AnnualPrecip,Precip_Driest_Quarter,Precip_Wettest_Quarter))### now dataframe of top 7 contributers of PCA, PCA1 and PCA2 explain 90% of the variation!!

pca.out2<-prcomp(bioclimSmallerPCA,scale=TRUE)
fviz_pca_biplot(pca.out2)


diffST<-SDsaltTolerance%>%
  dplyr::select(Accession,diffST)
salttolerancePCA<-salttolerancePCA%>%
  dplyr::select(Accession,STYield)

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
bioclim<-merge(diffST,bioclim,by="Accession",all.x=FALSE)
bioclim<-merge(saltyieldTolerance,bioclim,by="Accession",all.x=FALSE)

names(bioclim)


###model of prediciting values for salt tolerance via SALT YIELD

names(bioclim)
modelfit1 <- lm (log(STYield) ~ PCA1 + PCA2 + elevation+ Salinity_class,data=bioclim)##slightly significant
modelfit1.1<- lm (log(STYield) ~ PCA1,data=bioclim)##nope
modelfit1.1<- lm (log(STYield) ~ PCA1,data=bioclim)

#Signifcant Salt Treatment to Environemnt #######################################
modelfit2 <- lm (log(saltyieldTolerance) ~PCA1 + PCA2 + elevation+ Salinity_class,data=bioclim)##significant
modelfit2.1<-lm(log(saltyieldTolerance) ~PCA1,data=bioclim) ### super significant!!-- PCA1 and saltyieldtolerance
modelfit2.2<-lm(log(saltyieldTolerance) ~PCA2,data=bioclim)##slightly significant with PCA2
modelfit2.3<-lm(log(saltyieldTolerance) ~elevation,data=bioclim)###not signficant with elevation
modelfit2.4<-lm(log(saltyieldTolerance) ~Salinity_class,data=bioclim)## slightly signifcant with salty model-- .07112
-------------------------------------
  modelfit3 <- lm (log(diffST) ~ PCA1 + PCA2 + elevation+ Salinity_class,data=bioclim) ###not significant with anything--?

anova(modelfit2.4)


Loadings<-as.data.frame(pca.out$rotation)
Loadings
str(pca.out)
biplot(pca.out)
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
summary(y)

str(data.pca)
predict(data.)


###now trying psych???
library(psych)
pc<-prcomp(bioclimfili[,c(2:21)],centr=TRUE,scale.=TRUE)
attributes(pc)
pc$scale ##for normalization
pc$center
print(pc)##loadings and standard deviaitons
summary(pc)

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




