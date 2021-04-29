

#---------------------------------------------------------------------------------------#

# SCRIPT README      


## Article: Zarb? et al., 2021. Mapping and characterizing social-ecological land systems of South America. Ecoogy and Society.
## R codes for running the SELS clustering analysis

## All input data has already been pre-processed. Here we perform the clustering and produce performance metrics

## Code was writen for R version:
#R version 4.0.2 (2020-06-22) -- "Taking Off Again"
#Copyright (C) 2020 The R Foundation for Statistical Computing
#Platform: x86_64-w64-mingw32/x64 (64-bit)

#---------------------------------------------------------------------------------------#




# required packages
library(tidyverse)
library(corrplot)
library(cluster)
library(fpc)
library(factoextra)  # for visualizing cluster results
library(clValid)
library(gbm)


path="D:/Google Drive/2015 - LUCC TELECONECCIONES/Proyecto LATAM/Compartidos_varios/LZ_MPR/SELS/Manuscript for submittion/Ec&Soc/Resubmition/SELS_data_and_Rcodes"

#-------------------#
# IMPORT THE DATA #####
#-------------------#

#variable's statistics per hexagon
vars_standardized <- read_csv(paste0(path,"/SELS_input_data_40km_v3.csv"))
str(vars_standardized)

# set column types
#-----------#
vars_standardized$UNQ<-as.character(vars_standardized$UNQ)
vars_standardized$`Urbanization type` <- base::ordered(vars_standardized$`Urbanization type`, levels= c("rural", "small city", "medium city", "big city", "metropolis"))
vars_standardized$`Anthropization century` <- base::ordered(vars_standardized$`Anthropization century`, levels= c( "y1700","y1800", "y1900", "y2000","wild"))






#---------------------------------------------------#
# ACTIVATE IF TEST RUN 
#vars_standardized<-vars_standardized[8000:8100,]
#---------------------------------------------------#






#------------------------#
# CLUSTER MODEL (V6.3)  #####
#-----------------------#

# 1. CHECK MODEL V6 VARIABLES 

# CORPLOT 
ccor_P <- vars_standardized%>% 
  select(-UNQ)%>%
  mutate_if(is.factor,as.numeric)%>%
  cor(method="pearson", use="complete.obs")

ccor_S <- vars_standardized%>% 
  select(-UNQ)%>%
  mutate_if(is.factor,as.numeric)%>%
  cor(method="spearman", use="complete.obs")

#corrplot(ccor_S, method = "number", main="spearman correlations", tl.col="black", type = "upper")
#corrplot(ccor_P, method = "number", main="pearson correlations", tl.col="black", type = "upper")






# 2. CLUSTERING 

#define the weights: all = 1, except WBI variables that are weighted down to 0.25
W<-c(rep(1,20), rep(0.25,4), rep(1,2))
cbind(colnames(vars_standardized)[-1],W) #check weigths

#calculate distance matrix
GOWERdist<-daisy(vars_standardized[-1], metric="gower", weights=W) #gower distances for mixed data

#run the clustering analysis
DianaClust <- diana(GOWERdist, diss=TRUE, stop.at.k =16 )
DianaClust$dc



# all cutting options
clus_models<-vars_standardized 
clus_models$K5<-cutree(DianaClust, k = 5)
clus_models$K6<-cutree(DianaClust, k = 6)
clus_models$K7<-cutree(DianaClust, k = 7)
clus_models$K8<-cutree(DianaClust, k = 8)
clus_models$K9<-cutree(DianaClust, k = 9)
clus_models$K10<-cutree(DianaClust, k = 10)
clus_models$K11<-cutree(DianaClust, k = 11)
clus_models$K12<-cutree(DianaClust, k = 12)
clus_models$K13<-cutree(DianaClust, k = 13)
clus_models$K14<-cutree(DianaClust, k = 14)
clus_models$K15<-cutree(DianaClust, k = 15)
clus_models$K16<-cutree(DianaClust, k = 16)



# save the clustering output
write.table(clus_models, paste("clus_models_V6_3.csv",sep="/"), sep=",", row.names=F)







# 3. CLUSTERS EVALUATION

num_data<-vars_standardized[-1] %>%   mutate_if(is.factor,as.numeric)
num_data$type_urbanization <-num_data$type_urbanization /4
num_data$anthrop_century<-num_data$anthrop_century/5
num_data <-as.matrix(num_data)

summary(num_data)

# stability metrics
clmethods <- c("hierarchical","kmeans","pam","diana", "som")
stability <- clValid(obj=num_data , nClust = 5:16,
                  clMethods = clmethods, validation = "stability", metric="manhattan")

ValidMetrics<- as.data.frame.table(stability@measures)%>% 
  rename(Metric=Var1, K=Var2, ClustMethod=Var3, Value=Freq)

#Internal validation metrics
internal <- clValid(obj=num_data, nClust = 5:16,
                     clMethods = clmethods, validation = "internal", metric="manhattan")
internal2<- as.data.frame.table(internal@measures)%>% 
  rename(Metric=Var1, K=Var2, ClustMethod=Var3, Value=Freq)

ValidMetrics<-rbind(ValidMetrics,internal2)
write.csv(ValidMetrics, "validation measures.csv" )


#plot
# ggplot(ValidMetrics, aes(x=K, y=Value, group=ClustMethod,col=ClustMethod))+
#   geom_line()+facet_wrap("Metric", scales="free")



#------------------#

#cluster stats
clus_eval<- data.frame()
col_IDs <- which(grepl("K",colnames(clus_models)))

for (i in col_IDs){ 
  Kname<-colnames(clus_models[i])
  stats <- cluster.stats(d = GOWERdist, clustering=pull(clus_models,i), 
                         silhouette=TRUE, wgap=TRUE)
  
  print(summary(stats))
  
  stats2<-data.frame(Kname, 
                     #internal validation (characteristics of the clusters)
                      avg.silwidth=stats$avg.silwidth, 
                     dunn=stats$dunn,
                     av.between=stats$average.between, 
                     av.within=stats$average.within)
  
  clus_eval <- rbind(clus_eval, stats2)
}



clus_eval$Kname <- factor(clus_eval$Kname, levels= paste0("K",5:16))
write_csv(clus_eval, "optimal number of clusters.csv")



