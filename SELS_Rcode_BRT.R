
#Article: Zarbá et al., 2020. Mapping and characterizing social-ecological land systems of South America. Ecoogy and Society.
#R codes for running the boosted regression trees


# required packages
library(tidyverse)
library(sf)
library(gbm)
library(dismo)



#-------------------#
# IMPORT THE DATA #
#-------------------#

# HEX 40KM 
#-----------#

#variable's stattistics per hexagon
SELS_MAP <- st_read(dsn=getwd(), layer="SELS_variable_statistics_hex40km")
var_names <- c("UNQ",	"pct_flat",	"precipitation",	"temperature",	"plants_div",	"protected_areas",	"forest_cover",	"shrub_cover",	"grass_cover",	"crops_cover",	"plantation_cover",	"cover_div",	"centrality",	"TT_cities",	"TT_ports",	"crops_div",	"cattle_density",	"mining_density",	"irrigation",	"pop_density",	"type_urbanization",	"WBI_gov_ef",	"WBI_pol_stab",	"WBI_rule_law",	"WBI_reg_qual",	"languages_div",	"anthrop_century",
               "SELS_code", "SER_code", "avg_diss_SELS", "SELS_name", "geometry")

#set colnames
colnames(SELS_MAP)<-var_names

# set column types
SELS_MAP$UNQ<-as.character(SELS_MAP$UNQ)
SELS_MAP$type_urbanization<- base::ordered(SELS_MAP$type_urbanization, levels= c("rural", "small city", "medium city", "big city", "metropolis"))
SELS_MAP$anthrop_century<- base::ordered(SELS_MAP$anthrop_century, levels= c( "y1700","y1800", "y1900", "y2000","wild"))

SELS_table<- data.frame(SELS_MAP) 

#---------------------------------#
# VARIABLES RELATIVE IMPORTANCE ####
#---------------------------------#
# 
# #variables' names by category
# list_physical<-c("pct_flat", "precipitation", "temperature")
# list_biological<-c("plants_div", "protected_areas")
# list_landcover<-c("forest_cover", "shrub_cover" ,"grass_cover", "crops_cover" ,"plantation_cover" , "cover_div")
# list_economic<-c("centrality" , "TT_cities", "TT_ports" , "crops_div", "cattle_density" , "mining_density", "irrigation" )
# list_demographic<-c("pop_density", "type_urbanization")
# list_political<-c("WBI_gov_ef", "WBI_pol_stab", "WBI_rule_law", "WBI_reg_qual")
# list_cultural<-c("languages_div", "anthrop_century")
# 
# #check
# variables<-c(list_physical, list_biological, list_landcover, list_economic, list_demographic, list_political, list_cultural)
# '%ni%' <- Negate('%in%')
# colnames(SELS_table)[colnames(SELS_table)%ni%variables]



#---------------------------#
# BRT FOR Level1 SER
#---------------------------#

#fit the model Level1 SER
model_BRT<- SELS_table %>% dplyr::select(pct_flat:anthrop_century, SER_code)

BRT_SER <- gbm(SER_code~. , data=model_BRT, distribution = "multinomial",
                   n.trees = 500, shrinkage=0.005,
                   bag.fraction = 0.75,
                   n.cores =3)

ST_SER<- summary(BRT_SER)

#fit the model Level2 SELS
model_BRT<- SELS_table %>% dplyr::select(pct_flat:anthrop_century, SELS_code)

BRT_SELS <- gbm(SELS_code~. , data=model_BRT, distribution = "multinomial", 
                   n.trees = 5000, shrinkage=0.005,
                   bag.fraction = 0.75, 
                   cv.folds = 3) #13 minutos :)

ST_SELS<- summary(BRT_SELS)



#ROBUSTNESS CHECK
# get MSE and compute RMSE
sqrt(min(BRT_SELS$cv.error))

# plot loss function as a result of n trees added to the ensemble
gbm.perf(BRT_SELS, method = "cv")
gbm.perf(BRT_SELS,  method = "OOB")




#---------------------------#
### BRT MASKING CLUSTERS 
#---------------------------#

#create binary response variables for each cluster
model_BRT<- SELS_table %>% dplyr::select(pct_flat:anthrop_century, SELS_code)
for (i in 1:13){
  SELSi=levels(model_BRT$SELS_code)[i]
  model_BRT$MM<-0
  model_BRT$MM[ model_BRT$SELS_code==SELSi]<-1
  
  clus_BRT<- gbm.step(data=model_BRT, gbm.x = var_nums, gbm.y = "MM",
                      family = "bernoulli", 
                      learning.rate = 0.005,   bag.fraction = 0.5) #21:47
  
  colnames(model_BRT)[27+i]<-  SELSi
  
  BRT_results[[i]] <-clus_BRT
  
  
  sum_clus_BRT<- summary(clus_BRT) %>%
    mutate(var_category = as.factor(case_when(var %in% list_physical ~ "Physical", 
                                              var %in% list_biological  ~ "Biological",
                                              var %in% list_landscape  ~ "Landscape",
                                              var %in% list_demographic  ~ "Demographic",
                                              var %in% list_economic  ~ "Economic",
                                              var %in% list_political  ~ "Political",
                                              var %in% list_cultural ~ "Cultural")))
  
  print(SELSi)
  print(Sys.time())
}

#adjust settings manually
colnames(model_BRT[1:26])
var_nums<-c(1:26)
colnames(model_BRT[27])
clus_nums<-27

BRT_results<-list() #create empty object for saving results


# run the BRT
for (i in 1:13){
  SELSnamei<-SELSnames[i]
  SELSi<- colnames(model_BRT[clus_nums+i])
  clus_BRT<- gbm.step(data=model_BRT, gbm.x = var_nums, gbm.y = SELSi,
                      family = "bernoulli", 
                      learning.rate = 0.005,   bag.fraction = 0.5) #21:47
  
  BRT_results[[i]] <-clus_BRT
  
  
  sum_clus_BRT<- summary(clus_BRT) %>%
    mutate(var_category = as.factor(case_when(var %in% list_physical ~ "Physical", 
                                              var %in% list_biological  ~ "Biological",
                                              var %in% list_landscape  ~ "Landscape",
                                              var %in% list_demographic  ~ "Demographic",
                                              var %in% list_economic  ~ "Economic",
                                              var %in% list_political  ~ "Political",
                                              var %in% list_cultural ~ "Cultural")))
  
  print(SELSi)
  print(Sys.time())
}


