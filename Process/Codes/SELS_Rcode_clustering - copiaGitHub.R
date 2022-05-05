

#---------------------------------------------------------------------------------------#

# SCRIPT README      


## Article: Zarbá et al., 2022. Mapping and characterizing social-ecological land systems of South America. Ecoogy and Society.

## R codes for running the SELS clustering analysis - VERSION CLUSTER MODEL V6.3
## All input data has already been pre-processed and organized in a table format. This code runs the clustering analysis only (including performance metrics) because it is demanding on computing power. The R codes for the post-clustering statistics and boosted regression tree analysis are uploaded in github platform as well https://github.com/luciazarba/SELS-SA.

#---------------------------------------------------------------------------------------#



# required packages
 library(tidyverse)
 library(corrplot)
 library(cluster)
 library(fpc)
 library(clValid)


 


 
 
# IMPORT THE DATA 

#variable's statistics per hexagon
input_vars <- read_csv("SELS_input_data_40km_v3.csv")

# set column types
input_vars$UNQ<-as.character(input_vars$UNQ)
input_vars$`Urbanization type` <- base::ordered(input_vars$`Urbanization type`, levels= c("rural", "small city", "medium city", "big city", "metropolis"))
input_vars$`Anthropization century` <- base::ordered(input_vars$`Anthropization century`, levels= c( "y1700","y1800", "y1900", "y2000","wild"))
str(input_vars)


# CHECK CORRELATIONS 
ccor_S <- input_vars%>% 
  select(-UNQ)%>%
  mutate_if(is.factor,as.numeric)%>%
  cor(method="spearman", use="complete.obs")

corrplot(ccor_S, method = "number", main="spearman correlations", tl.col="black", type = "upper")




# COMPARING CLUSTERING METHODS

# DISCLAIMER: this section uses a different distance algorithm due technical constrains. Beware of this if comparing results from different sections of the script. For further informatio refer to Appendix 1 of the publication.

# data transformation: ordinal variables to double (proportions of 1)
num_data<-input_vars[-1] %>%   mutate_if(is.factor,as.numeric)
num_data$`Urbanization type` <-num_data$`Urbanization type`/length(unique(num_data$`Urbanization type`))
num_data$`Anthropization century`<-num_data$`Anthropization century`/length(unique(num_data$`Anthropization century`))
num_data <-as.matrix(num_data)

summary(num_data) #check

# stability metrics (WITH MANHATTAN DISTANCE ALGORITHM)
clmethods <- c("agglomerative","kmeans","pam","diana","som")
stability <- clValid(obj=num_data , nClust = 5:16,
                     clMethods = clmethods, validation = "stability", metric="manhattan", maxitems=nrow(num_data))

ValidMetrics<- as.data.frame.table(stability@measures)%>% 
  rename(Metric=Var1, K=Var2, ClustMethod=Var3, Value=Freq)

# internal validation metrics (WITH MANHATTAN DISTANCE ALGORITHM)
internal <- clValid(obj=num_data, nClust = 5:16,
                    clMethods = clmethods, validation = "internal", metric="manhattan", maxitems=nrow(num_data))
internal2<- as.data.frame.table(internal@measures)%>% 
  rename(Metric=Var1, K=Var2, ClustMethod=Var3, Value=Freq)

ValidMetrics<-rbind(ValidMetrics,internal2)




# GOWER DISTANCE MATRIX  

#define the weights: the four WBI variables are weighted down to 0.25, all other variables remain weighting 1.
W<-c(rep(1,20), rep(0.25,4), rep(1,2))
cbind(colnames(input_vars)[-1],W) #check correspondence variables-weights

#calculate distance matrix
GOWERdist<-daisy(input_vars[-1], metric="gower", weights=W) #gower algorithm for mixed data


# DIANA CLUSTERING 

#run diana function - divisive hierarchical clustering
DianaClust <- diana(GOWERdist, diss=TRUE, stop.at.k =16 )

# generate classifications cutting the tree at different heights
clus_models<-input_vars #copy the input variables 
clus_models$K5<-cutree(DianaClust, k = 5) #k indicates number of clusters
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



# DIANA CLUSTERING PERFORMANCE METRICS

# Computes a number of distance based statistics, which can be used for cluster validation and deciding the optimal number of clusters

# compute cluster evaluation statistics
clus_eval<- data.frame()
col_IDs <- which(grepl("K",colnames(clus_models))) #detect classification columns

for (i in col_IDs){ 
  Kname<-colnames(clus_models[i])
  stats <- cluster.stats(d = GOWERdist, clustering=pull(clus_models,i), 
                         silhouette=TRUE, wgap=TRUE)
  capture.output(stats, file = paste0("validation stats ",Kname, ".txt"))
  
  stats2<-data.frame(Kname, 
                     avg.silwidth=stats$avg.silwidth, #average silhouette width
                     dunn=stats$dunn, #minimum separation / maximum diameter
                     av.within=stats$average.within) #average distance within clusters
  
  clus_eval <- rbind(clus_eval, stats2)
}


clus_eval$Kname <- factor(clus_eval$Kname, levels= paste0("K",5:16))







