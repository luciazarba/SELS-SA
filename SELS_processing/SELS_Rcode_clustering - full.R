

#---------------------------------------------------------------------------------------#

# SCRIPT FOR THE ARTICLE: Zarbá, L., M. Piquer-Rodríguez, S. Boillat, C. Levers, I. Gasparri, T. Aide, N. L. Álvarez-Berríos, L. O. Anderson, E. Araoz, E. Arima, M. Batistella, M. Calderón-Loor, C. Echeverría, M. Gonzalez-Roglich, E. G. Jobbágy, S.-L. Mathez-Stiefel, C. Ramirez-Reyes, A. Pacheco, M. Vallejos, K. R. Young, and R. Grau. 2022. Mapping and characterizing social-ecological land systems of South America. Ecology and Society 27(2):27. https://doi.org/10.5751/ES-13066-270227

## R codes for running the SELS clustering analysis - VERSION CLUSTER MODEL V6.3
# Please note that not all input variables are available in the input_data folder since some of them where not public. Also note this code is not completely automated, it requires creating folders and other queries, those lines are marked with the text "ACTION REQUIRED!!!"

#Download input data for this script from https://drive.google.com/drive/folders/1Iq4gtgESZw94RcVZUcXWIJIbBRdyhfGb?usp=sharing
#---------------------------------------------------------------------------------------#






#---------------------#
# GENERAL SETTINGS  
#---------------------#



# REQUIRED PACKAGES #

library(tidyverse)
library(rgdal)
library(sp)
library(raster)
library(rgeos)
library(sf)
library(GISTools)
library(tmap)




# USEFUL TOOLS #

#useful maps
SA_mask<-st_read( dsn="input_data", layer="SA_mask")
SA_mask_WGS84<-st_transform(SA_mask,4326)
country_limits <- st_read(dsn="input_data", layer="SA_adm0_saaeca")
templateRaster <- raster("input_data/template_raster.tif")

#Reference projection South american albers equal area conic
proy.saaeac<-crs(SA_mask)
#Reference projection geographic coordinates
proy.WGS84<-crs(SA_mask_WGS84)

#continental extent
extent_SA_saaeac<- extent(c(-2370711, 2743105, -2605683, 4879880))
extent_SA_WGS84<- extent(c(-81.32674,-34.79012,-55.97022,12.46331))




# CREATION OF 40 Km HEXAGONAL GRIDS #

coords <- extent_SA_saaeac+c(-100000,100000,-100000,100000) #expando 100km para cada lado
p = Polygon( data.frame(x=c(coords[1],coords[1],coords[2],coords[2],coords[1]), #creo el polígono con esos límites
                        y=c(coords[4],coords[3],coords[3],coords[4],coords[4])))
sps = SpatialPolygons(list(Polygons(list(p),1)))
plot(sps); plot(SA_mask, col="lightgreen", border=NA, add=T)

HexPts <-spsample(sps, type="hexagonal", cellsize=40000) #creo los centroides
HexPols <- HexPoints2SpatialPolygons(HexPts) #convierto los centroides en polígonos hexagonales

df<-as.data.frame(1:length(HexPols))# creo una tabla inicial
HexPolsdf <-SpatialPolygonsDataFrame(HexPols, data = df, match.ID =FALSE) 

df<-as.data.frame(1:length(HexPolsdf)) #creo un data frame
HexPols_df <-SpatialPolygonsDataFrame(HexPolsdf, data = df, match.ID =FALSE)
HexPols_sf<-st_as_sf(HexPols_df) # convierto la grilla a clase simple feature
st_crs(HexPols_sf)<-102033 #le asigno sistema de coordenadas
compareCRS(HexPols_sf, SA_mask)
SA_mask<-st_as_sf(SA_mask) #lo convierto a clase simple feature
clip_grid<-HexPols_sf %>% st_intersection(SA_mask) #hago el clip del borde continental
grilla_hex <- sf::as_Spatial(clip_grid)  #lo vuelvo a convertir en sp para guardarlo
grilla_hex$H_ID<-as.character(1:nrow(grilla_hex))
grilla_hex <- grilla_hex[,3]
plot(grilla_hex[,"H_ID"] ,axes=TRUE, border="gray")
writeOGR(grilla_hex, dsn="input_data", layer="grilla_hex_40km", driver="ESRI Shapefile", overwrite_layer = TRUE) 

# #create raster version of the hexagon grid
hex_ras<-rasterize(grilla_hex, templateRaster)
names(hex_ras)<-"hex_id"
writeRaster(hex_ras, "input_data/grilla_hex_40km.tif")


#--------------------------------------------#


# Load hexagon grid
grilla_hex <- st_read(dsn="input_data", layer="grilla_hex_40km")

# Create sample subset grid
cel<-grilla_hex %>% filter(H_ID==5290)
buf <- st_buffer(cel, dist = 100000)
sample_hex <- st_intersection(grilla_hex, buf)%>%dplyr::select(-H_ID.1)
plot(sample_hex[,"H_ID"] ,axes=TRUE)


# ACTION REQUIRED!!! chose to work with a sample or the full dataset
#shp_template <-sample_hex # run the test area only (subset)
shp_template<-grilla_hex # run on the full area









#====================================================================#
# FIRST SECTION - SPATIAL DATA PREPROCESING                  #######
#====================================================================#



#---------------------#
# SECTION SETTINGS  
#---------------------#


# ACTION REQUIRED!!! create and set your own temporal maps folder
preprocessed <- "input_data/preprocessed_maps"
preprocessedCrops <-"input_data/preprocessed_maps/crops"



# FUNCTIONS #

#function to preprocess raster objects
convertir_a_SA_saaeac<- function(mapa_input){
  if (compareCRS(mapa_input, proy.saaeac)) { #está en saaeac?
    rec_rep<-crop(mapa_input, extent_SA_saaeac)  
  } else{
    if (compareCRS(mapa_input, proy.WGS84)) {  
      rec<-crop(mapa_input, extent_SA_WGS84)  
      rec_rep <-projectRaster(rec, crs=proy.saaeac) #reproyecta
    } else{
      rec_rep<-"projección desconocida"}}  #no está en ninguna de esas dos proyecciones?
  
  return(rec_rep) #devuelve el resultado
}



#function to clip sppoints
gClip <- function(shp, bb){
  if(class(bb) == "matrix") b_poly <- as(extent(as.vector(t(bb))), "SpatialPolygons")
  else b_poly <- as(extent(bb), "SpatialPolygons")
  gIntersection(shp, b_poly, byid = T)
}



#raster spatial check function
raster_caract<-function(mapa){
  return(c(crs(mapa),
           dim(mapa),
           res(mapa),
           extent(mapa) ))
}



#---------------------#
# DATA PREPROCESING  
#---------------------#



# PHYSICAL DATA


# 1. Flat relief
#GME_K3classes (montañas karagulle 2017)
LANDFORMS<-raster("input_data/k3classes.tif")
raster_caract(LANDFORMS)
#adapt to saaeac
compareCRS(mapa_input, proy.WGS84)  
rec<-crop(mapa_input, extent_SA_WGS84)  
rec_rep <-projectRaster(rec, crs=proy.saaeac)
mask<-mask(rec_rep, SA_mask)
writeRaster(mask, paste0(preprocessed,"/GME_k3classes.tif", overwrite=TRUE))


# 2. Temperature
mapa_input<-raster("input_data/Normal_1981-2010_MAT.tif" ) 
raster_caract(mapa_input)
#adapt to saaeac
compareCRS(mapa_input, proy.WGS84)  
mask<-mask(mapa_input,SA_mask)
writeRaster(mask, paste0(preprocessed,"/climateSA_Normal_1981-2010_MAT.tif", overwrite=TRUE))


# 3. Precipitation
mapa_input<-raster("input_data/Normal_1981-2010_MAP.tif",) 
raster_caract(mapa_input)
#adapt to saaeac
compareCRS(mapa_input, proy.WGS84)  
mask<-mask(mapa_input,SA_mask)
writeRaster(mask, paste0(preprocessed,"/climateSA_Normal_1981-2010_MAP.tif",  overwrite=TRUE))



# BIOLOGICAL

# 4. Plants diversity
PLANT_SP<-st_read(dsn="input_data", layer="kreftjetzdata")
st_crs(PLANT_SP)
PLANT_SP<-st_transform(PLANT_SP, 102033) #reproject
clip_PLANT_SP<-PLANT_SP %>% st_intersection(SA_continente) # clip continental borders
clip_PLANT_SP2 <- sf::as_Spatial(clip_PLANT_SP)  #lo vuelvo a convertir en sp para guardarlo
writeOGR(clip_PLANT_SP2, dsn= preprocessed, layer= "PLANT_SP_Kreft&Jetz2007PNAS", driver="ESRI Shapefile")


# 5. Protected areas
#Manually preprocessed due geometry errors. Based on the layer "WDPA_May2019-shapefile-polygons" 
mapa_input<-st_read(dsn="input_data", layer="Protected_areas_WDPA_May2019") 
plot(mapa_input)
writeOGR(APROT2, dsn= preprocessed, layer= "Protected_areas_WDPA_May2019", driver="ESRI Shapefile",overwrite_layer=TRUE)




# LAND COVER
# These layers do not need pre processing. Anyways, they are not available in the input_data folder.
# 6. Forest cover
# 7. Shrublands cover
# 8. Grasslands cover
# 9. Crop cover
# 10. Plantations cover
# 11. Cover diversity



# ECONOMIC


# 12. Centrality
mapa_input<-raster("input_data/earth_at_night.tif")
raster_caract(mapa_input)
#adapt to saaeac
compareCRS(mapa_input, proy.WGS84) #está en WGS84?
rec<-crop(mapa_input, extent_SA_WGS84) #recorta
rec_rep <-projectRaster(rec, crs=proy.saaeac)
mask<-mask(rec_rep, SA_mask)
writeRaster(mask, paste(preprocessed, "Night_Time_Lights.tif", sep='/'),overwrite=TRUE)


# 13. Cattle density
mapa_input<-raster( "input_data/SA_Cattle1km_AD_2010_v2_1.tif")
raster_caract(mapa_input)
#adapt to saaeac
compareCRS(mapa_input, proy.WGS84)  
rec<-crop(mapa_input, extent_SA_WGS84)  
rec_rep <-projectRaster(rec, crs=proy.saaeac)
mask<-mask(rec_rep, SA_mask)
writeRaster(mask, paste0(preprocessed,"/SA_Cattle1km_AD_2010_v2_1.tif"),overwrite=TRUE )


# 14. Mine sites density
mapa_input<-st_read(dsn="input_data", layer="mrds-2018-07-03-16-28-37") 
colnames(mapa_input)
rep<-st_transform(mapa_input, 102033) #reproject
rep2 <- sf::as_Spatial(rep)  #lo vuelvo a convertir en sp para guardarlo
writeOGR(rep2, dsn= preprocessed, layer= "MINING_MRDS", driver="ESRI Shapefile")



# 15. Crop diversity
dir.crops<-"input_data/HarvestedAreaYield175Crops_Geotiff"
lista<-list.files(dir.crops)
lista<-lista [lista !="desktop.ini"]
for (i in 1:length(lista)){
  print(Sys.time()) # to keep track of time
  folder<-paste(dir.crops, lista[i], sep="/") #identifica la carpeta
  file<- list.files(paste(dir.crops, lista[i], sep="/"),pattern = glob2rx("*_HarvestedAreaHectares.tif")) #identifica el archivo

  mapa_input<-raster(paste(folder,file, sep='/' )) #carga el mapa
  rec<-crop(mapa_input, extent_SA_WGS84)  
  rec_rep <-projectRaster(rec, crs=proy.saaeac) #reproyecta
  writeRaster(rec_rep, paste(preprocessedCrops, "crops_SA_saaeac", file, sep='/'),overwrite=TRUE) #guarda
}


# 16. Irrigation
IRRIGATION<-st_read(dsn="input_data", layer="gmia_v5_aei_pct_cellarea") 
colnames(IRRIGATION)
IRRIGATION<-st_transform(IRRIGATION, 102033) %>% #reproject
  st_intersection(SA_mask)
IRRIGATION2 <- sf::as_Spatial(IRRIGATION)  #lo vuelvo a convertir en sp para guardarlo
writeOGR(IRRIGATION2, dsn= preprocessed, layer= IRRIGATION_gmia_v5_aei_pct_cellarea, driver="ESRI Shapefile",overwrite_layer=TRUE)


# 17. cities travel time
mapa_input<-raster("input_data/acc_50K/w001001.adf") 
raster_caract(mapa_input)
#adapt to saaeac
compareCRS(mapa_input, proy.WGS84)  
rec<-crop(mapa_input, extent_SA_WGS84)  
rec_rep <-projectRaster(rec, crs=proy.saaeac)
mask<-mask(rec_rep, SA_mask)
writeRaster(mask, paste0(preprocessed,"/ACCES.tif", overwrite=TRUE))


# 18. ports travel time
mapa_input<-raster("input_data/TravelTime2Ports_Zarba.tif") 
raster_caract(mapa_input)
writeRaster(mapa_input, paste0(preprocessed,"/PORTS.tif", overwrite=TRUE))




# DEMOGRAPHIC

# 19. Population density
mapa_input<-raster("input_data/saaeac_pop12near_rec_near_clip.tif")
raster_caract(mapa_input)
mask<-mask(mapa_input, SA_mask)
writeRaster(mask,  paste(preprocessed, "Env Population 2012.tif", sep='/'), overwrite=TRUE)


# 20. Urbanization type
CITIES<-st_read(dsn="input_data", layer="pop_places_50k") 
rec_rep<-st_transform(CITIES, 102033)%>%
  st_intersection(SA_mask)
st_write(rec_rep, dsn=preprocessed, layer = "CITIES", driver = "ESRI Shapefile", overwrite=TRUE)




# POLITICAL
# These datasets do not need pre-processing
# 21. WBI gov. effectiveness
# 22. WBI political stability
# 23. WBI rule of law
# 24. WBI regulatory quality



# CULTURAL

# 25. languages density 
LANGUAJES<-st_read(dsn="input_data", layer="lenguasSudamerica") #DATASET NOT PUBLIC
colnames(LANGUAJES)
LANGUAJES<-st_transform(LANGUAJES, 102033) %>% #reproject
  st_intersection(SA_mask)
LANGUAJES$lengua_mat[is.na(LANGUAJES$lengua_mat)]<-2
LANGUAJES2 <- sf::as_Spatial(LANGUAJES)  #lo vuelvo a convertir en sp para guardarlo
writeOGR(LANGUAJES2, dsn= preprocessed, layer= "LANGUAJES_MuturZikin", driver="ESRI Shapefile",overwrite_layer=TRUE)


# 26. Anthropization century
#Anthropogenic biome (v2) classes for year 1700
mapa_input<-raster("input_data/anthro2_a1700.tif") 
raster_caract(mapa_input)
rec<-crop(mapa_input, extent_SA_WGS84)  
rec_rep <-projectRaster(rec, crs=proy.saaeac, method= 'ngb' )
mask<-mask(rec_rep, SA_mask)
writeRaster(mask, paste(preprocessed, "anthro2_a1700.tif", sep='/'), overwrite=TRUE)
#Anthropogenic biome (v2) classes for year 1800
mapa_input<-raster("input_data/anthro2_a1800.tif" ) 
rec<-crop(mapa_input, extent_SA_WGS84)  
rec_rep <-projectRaster(rec, crs=proy.saaeac, method= 'ngb' )
mask<-mask(rec_rep, SA_mask)
writeRaster(mask, paste(preprocessed, "anthro2_a1800.tif", sep='/'), overwrite=TRUE)
#Anthropogenic biome (v2) classes for year 1900
mapa_input<-raster("input_data/anthro2_a1900.tif" ) 
rec<-crop(mapa_input, extent_SA_WGS84)  
rec_rep <-projectRaster(rec, crs=proy.saaeac, method= 'ngb' )
mask<-mask(rec_rep, SA_mask)
writeRaster(mask, paste(preprocessed, "anthro2_a1900.tif", sep='/'), overwrite=TRUE)
#Anthropogenic biome (v2) classes for year 2000
mapa_input<-raster("input_data/anthro2_a2000.tif" ) 
rec<-crop(mapa_input, extent_SA_WGS84)  
rec_rep <-projectRaster(rec, crs=proy.saaeac, method= 'ngb' )
mask<-mask(rec_rep, SA_mask)
writeRaster(mask, paste(preprocessed, "anthro2_a2000.tif", sep='/'), overwrite=TRUE)















#====================================================================#
# SECOND SECTION - HEX STATS CALCULATION                      #######
#====================================================================#



#---------------------#
# SECTION SETTINGS  
#---------------------#


# ACTION REQUIRED!!! create and set your own folder for temporal extract files
folder_extract<-"input_data/hex_extract_40km_folder" 
folder_extract_crops <- "input_data/hex_extract_40km_folder/crops"






# FUNCTIONS #

# Create the getmode function.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}





#--------------------------------#
# CALCULATE HEXAGON STATISTICS #
#--------------------------------#

list.files("input_data")
list.files(folder_extract)





# PHYSICAL DATA


# 1. Flat relief
RASTER<-raster(paste(preprocessed,"GME_K3classes.tif", sep='/')) 
names(RASTER)<-"geo"
RASTERbin<-RASTER
RASTERbin[RASTERbin %in% c(26,27,31,32)]<-1

hex_stat <- raster::extract(RASTERbin, shp_template, fun=mean , na.rm=TRUE,  df=TRUE) #prop of mountain
hex_stat <- hex_stat -1 #inverse
write.table(hex_stat, paste(folder_extract, "/hex_prop_flat_geomorphology.csv", sep="/"), sep=",", row.names = FALSE)



# 2. Temperature
RASTER<-raster(paste(preprocessed, "climateSA_Normal_1981-2010_MAT.tif", sep='/')) 
names(RASTER)<-"temp"

start_time <- Sys.time() #set timer
hex_stat <- raster::extract(RASTER, shp_template, fun=median , na.rm=TRUE, df=TRUE)
write.table(hex_stat, paste(folder_extract, "/hex_median_temperature.csv", sep="/"), sep=",", row.names = FALSE)
end_time <- Sys.time();end_time - start_time #finish timer



# 3. Precipitation
RASTER<-raster(paste(preprocessed, "climateSA_Normal_1981-2010_MAP.tif", sep='/')) 
names(RASTER)<-"prec"

start_time <- Sys.time() #set timer
hex_stat <- raster::extract(RASTER, shp_template, fun=median , na.rm=TRUE, df=TRUE)
write.table(hex_stat, paste(folder_extract, "/hex_median_precipitation.csv", sep="/"), sep=",", row.names = FALSE)
end_time <- Sys.time();end_time - start_time #finish timer




# BIOLOGICAL


# 4. Plants diversity
SHAPE <- st_read(dsn = preprocessed, layer= "PLANT_SP_Kreft&Jetz2007PNAS")
out<-st_join( shp_template , SHAPE%>% dplyr::select(Comb ,Krig, CoKrig ),largest = TRUE)
hex_stat <-out%>% as.data.frame()%>% dplyr::select(H_ID, Comb ,Krig, CoKrig )
write.table(hex_stat, paste(folder_extract, "/hex_plant_richness_Kreft.csv", sep="/"), sep=",", row.names = FALSE)


# 5. Protected areas
shape<-st_read(dsn =preprocessed, layer="Protected_areas_WDPA_May2019")
shape$coef<-1
val<-st_is_valid(shape) #it has overlapping polygons that inflates the sum
table(val)
shape5<-shape[val,]
shape7<-st_union(shape5)

int <- as_tibble(st_intersection( shp_template,shape7))
int$areaPA <- st_area(int$geometry)
tb_APbyHEX <- int %>%
  group_by(H_ID) %>%
  summarise(areaPA = sum(areaPA))
shp_out <-left_join(shp_template, tb_APbyHEX, by = 'H_ID')
shp_out$areaPA[is.na(shp_out$areaPA)]<-0
shp_out<- shp_out%>%mutate(areaHEX=st_area(geometry),
                           propPA=areaPA/areaHEX)

#save
PAS_v7 <- sf::as_Spatial(shp_out)  
writeOGR(PAS_v7, dsn =preprocessed, 
         layer="Protected_areas_WDPA_v7", driver="ESRI Shapefile", overwrite_layer =  TRUE)

write_csv(t, paste(folder_extract, "/hex_area_PA.csv", sep="/"))





# LAND COVER

# 6. Forest cover
# 7. Shrublands cover
# 8. Grasslands cover
# 9. Crop cover
# 10. Plantations cover

# jordan graesser V2 (hard, no mix category) - NOT AVAILABLE IN THE INPUT DATA FOLDER
cob_2001<-raster("input_data/2001_hard.tif") 
cob_2002<-raster("input_data/2002_hard.tif") 
cob_2003<-raster("input_data/2003_hard.tif") 
cob_2004<-raster("input_data/2004_hard.tif") 
cob_2005<-raster("input_data/2005_hard.tif")  
cob_2006<-raster("input_data/2006_hard.tif")  
cob_2007<-raster("input_data/2007_hard.tif") 
cob_2008<-raster("input_data/2008_hard.tif") 
cob_2009<-raster("input_data/2009_hard.tif")  
cob_2010<-raster("input_data/2010_hard.tif") 
cob_2011<-raster("input_data/2011_hard.tif") 
cob_2012<-raster("input_data/2012_hard.tif") 
cob_2013<-raster("input_data/2013_hard.tif") 
cob_2014<-raster("input_data/2014_hard.tif") 

#Check the projection
proj4string(hex_ras)
proj4string(cob_2001)
#resample
hex_ras<-resample(hex_ras,cob_2001 ) 

#contingency tables
ct_2001<-crosstab (hex_ras, cob_2001 , progress="text") 
ct_2002<-crosstab (hex_ras, cob_2002 , progress="text") 
ct_2003<-crosstab (hex_ras, cob_2003 , progress="text") 
ct_2004<-crosstab (hex_ras, cob_2004 , progress="text") 
ct_2005<-crosstab (hex_ras, cob_2005 , progress="text") 
ct_2006<-crosstab (hex_ras, cob_2006 , progress="text") 
ct_2007<-crosstab (hex_ras, cob_2007 , progress="text") 
ct_2008<-crosstab (hex_ras, cob_2008 , progress="text") 
ct_2009<-crosstab (hex_ras, cob_2009 , progress="text") 
ct_2010<-crosstab (hex_ras, cob_2010 , progress="text") 
ct_2011<-crosstab (hex_ras, cob_2011 , progress="text") 
ct_2012<-crosstab (hex_ras, cob_2012 , progress="text") 
ct_2013<-crosstab (hex_ras, cob_2013 , progress="text") 
ct_2014<-crosstab (hex_ras, cob_2014 , progress="text") 

#reshape for all the other images
ct_2001<-ct_2001 %>% 
  as.data.frame()  %>% 
  reshape(idvar = c("hex_id"), timevar = "X2001_hard", direction = "wide")
ct_2002<-ct_2002 %>% 
  as.data.frame()  %>% 
  reshape(idvar = c("hex_id"), timevar = "X2002_hard", direction = "wide")
ct_2003<-ct_2003 %>% 
  as.data.frame()  %>% 
  reshape(idvar = c("hex_id"), timevar = "X2003_hard", direction = "wide")
ct_2004<-ct_2004 %>% 
  as.data.frame()  %>% 
  reshape(idvar = c("hex_id"), timevar = "X2004_hard", direction = "wide")
ct_2005<-ct_2005 %>% 
  as.data.frame()  %>% 
  reshape(idvar = c("hex_id"), timevar = "X2005_hard", direction = "wide")
ct_2006<-ct_2006 %>% 
  as.data.frame()  %>% 
  reshape(idvar = c("hex_id"), timevar = "X2006_hard", direction = "wide")
ct_2007<-ct_2007 %>% 
  as.data.frame()  %>% 
  reshape(idvar = c("hex_id"), timevar = "X2007_hard", direction = "wide")
ct_2008<-ct_2008 %>% 
  as.data.frame()  %>% 
  reshape(idvar = c("hex_id"), timevar = "X2008_hard", direction = "wide")
ct_2009<-ct_2009 %>% 
  as.data.frame()  %>% 
  reshape(idvar = c("hex_id"), timevar = "X2009_hard", direction = "wide")
ct_2010<-ct_2010 %>% 
  as.data.frame()  %>% 
  reshape(idvar = c("hex_id"), timevar = "X2010_hard", direction = "wide")
ct_2011<-ct_2011 %>% 
  as.data.frame()  %>% 
  reshape(idvar = c("hex_id"), timevar = "X2011_hard", direction = "wide")
ct_2012<-ct_2012 %>% 
  as.data.frame()  %>% 
  reshape(idvar = c("hex_id"), timevar = "X2012_hard", direction = "wide")
ct_2013<-ct_2013 %>% 
  as.data.frame()  %>% 
  reshape(idvar = c("hex_id"), timevar = "X2013_hard", direction = "wide")
ct_2014<-ct_2014 %>% 
  as.data.frame()  %>%
  reshape(idvar = c("hex_id"), timevar = "X2014_hard", direction = "wide") 

#combine all tables in one
hex_cob<-bind_rows (cbind(YEAR=rep(2001, nrow(ct_2001)),ct_2001),
                    cbind(YEAR=rep(2002, nrow(ct_2002)),ct_2002),
                    cbind(YEAR=rep(2003, nrow(ct_2003)),ct_2003),
                    cbind(YEAR=rep(2004, nrow(ct_2004)),ct_2004),
                    cbind(YEAR=rep(2005, nrow(ct_2005)),ct_2005),
                    cbind(YEAR=rep(2006, nrow(ct_2006)),ct_2006),
                    cbind(YEAR=rep(2007, nrow(ct_2007)),ct_2007),
                    cbind(YEAR=rep(2008, nrow(ct_2008)),ct_2008),
                    cbind(YEAR=rep(2009, nrow(ct_2009)),ct_2009),
                    cbind(YEAR=rep(2010, nrow(ct_2010)),ct_2010),
                    cbind(YEAR=rep(2011, nrow(ct_2011)),ct_2011),
                    cbind(YEAR=rep(2012, nrow(ct_2012)),ct_2012),
                    cbind(YEAR=rep(2013, nrow(ct_2013)),ct_2013),
                    cbind(YEAR=rep(2014, nrow(ct_2014)),ct_2014))  

#frequency table (number of pixels)
hex_cob_freq<- hex_cob %>%
  mutate (UNQ_num=as.integer(as.character(hex_id)),
          AREA_Npix=rowSums(hex_cob[,c(3:12)]),
          AREA_milHa=AREA_Npix*62500/(10000*1000)) %>% #pixel modis=62500m2, hay 10000m2 por hectarea y multiplico por mil para expresar en miles de ha
  dplyr::rename (
    UNQ_fac=  hex_id ,
    Null= Freq.0 ,
    Urban= Freq.1 ,
    Cropland= Freq.2 ,
    Shrubland= Freq.3 ,
    Trees= Freq.4 ,
    Grassland= Freq.5 ,
    Barren= Freq.6 ,
    Plantation.fruits= Freq.7 ,
    Water= Freq.8 ,
    Plantation.wood= Freq.9  )

#calculate proportions (proportion of area)
hex_cob_prop<- hex_cob_freq #make a copy
colnames(hex_cob_freq[,3:12]) #identify the landcover columns
hex_cob_prop[,3:12]<-hex_cob_freq[,3:12]/hex_cob_freq$AREA_Npix #divide them by total pixels in the hexagon
rowSums(hex_cob_prop[1:100,3:12]) #check

write.table(hex_cob_prop, paste(folder_extract,"hex_cob_prop_long.csv", sep="/"), sep=",", row.names = FALSE)



# 11. Cover diversity

y2010<-hex_cob_prop%>%filter(YEAR==2010)%>%as.matrix%>%as_tibble
# the diversity function works with count data as input. we need to convert our table
classes<- y2010 %>% dplyr::select(Urban:Plantation.wood) #select the classes columns
classes<-classes*y2010$AREA_Npix #convert proportions into number of pixels
H_cob<- diversity(classes, index = "shannon", MARGIN = 1)
D1_cob<- diversity(classes, index = "simpson", MARGIN = 1)
D2_cob<- diversity(classes, index="invsimpson", MARGIN =1 )
E_cob<-D2_cob/9 #Simpson's evenness E=D2/S (Simpson's dominance divided by the number of classes)
div_cob<- cbind(UNQ_num=y2010$UNQ_num, H_cob, D1_cob, D2_cob, E_cob)
write.table(div_cob, paste(folder_extract, "hex_diversity_landcover_classes.csv", sep="/"), sep=",", row.names = FALSE)







# ECONOMIC


# 12. Centrality
RASTER<-raster(paste(preprocessed, "Night_Time_Lights.tif", sep='/')) 
names(RASTER)<-"lights"

#mean
start_time <- Sys.time() #set timer
hex_stat <- raster::extract(RASTER, shp_template, fun=mean , na.rm=TRUE, df=TRUE)
write.table(hex_stat, paste(folder_extract, "/hex_mean_nighttimelights.csv", sep="/"), sep=",", row.names = FALSE)
Sys.time() - start_time #finish timer

#sum
start_time <- Sys.time() #set timer
hex_stat <- raster::extract(RASTER, shp_template, fun=sum , na.rm=TRUE, df=TRUE)
write.table(hex_stat, paste(folder_extract, "/hex_sum_nighttimelights.csv", sep="/"), sep=",", row.names = FALSE)
Sys.time() - start_time #finish timer

#country mean
start_time <- Sys.time() #set timer
hex_stat <- raster::extract(RASTER, country_limits, fun=mean , na.rm=TRUE, df=TRUE)
hex_stat$ID_0 <- country_limits$ID_0
write.table(hex_stat, paste(folder_extract, "/country_mean_nighttimelights.csv", sep="/"), sep=",", row.names = FALSE)
end_time <- Sys.time();end_time - start_time #finish timer

#country sum
start_time <- Sys.time() #set timer
hex_stat <- raster::extract(RASTER, country_limits, fun=sum , na.rm=TRUE, df=TRUE)
hex_stat$ID_0 <- country_limits$ID_0
write.table(hex_stat, paste(folder_extract, "/country_sum_nighttimelights.csv", sep="/"), sep=",", row.names = FALSE)
end_time <- Sys.time();end_time - start_time #finish timer



# 13. Cattle density
RASTER<-raster(paste(preprocessed, "SA_Cattle1km_AD_2010_v2_1.tif", sep='/')) 
names(RASTER)<-"cattle"

start_time <- Sys.time() #set timer
hex_stat <- raster::extract(RASTER, shp_template, fun=sum , na.rm=TRUE, df=TRUE)
write.table(hex_stat, paste(folder_extract, "/hex_sum_cattle_density.csv", sep="/"), sep=",", row.names = FALSE)
end_time <- Sys.time();end_time - start_time #finish timer



# 14. Mine sites density
shape<-readOGR(dsn = preprocessed, layer="MINING_MRDS")
pts<-subset(shape, select="com_type")
hex_stat<-poly.counts(pts, as(shp_template, "Spatial"))
hex_stat<-data.frame(min_count=hex_stat, ID=1:nrow(shp_template))
shp_template$min_count<-hex_stat$min_count
write.table(hex_stat, paste(folder_extract, "hex_mining_MRDS_sum_prod_size.csv", sep="/"), sep=",", row.names = FALSE)



# 15. Crop diversity
lista<-list.files(preprocessedCrops)
lista<-lista[!lista %in% "desktop.ini"]

#calculate the area by hexagon for each crop and save it in csv files
for (i in 31:length(lista)){
  print(Sys.time())
  file<-lista[i]
  RASTER<-raster(paste(preprocessedCrops,file, sep="/"))
  hex_stat <- raster::extract(RASTER, shp_template, fun=sum , na.rm=TRUE, df=TRUE)
  write.table(hex_stat, paste0(folder_extract_crops,"/hex_sum_",  names(RASTER), ".csv"), sep=",", row.names = FALSE)
}
print(Sys.time())


#list of all crop csv files
lista<-list.files(folder_extract_crops)
lista<-lista[!lista %in% "desktop.ini"]

# compile a unified data frame containing all crops
cob_crop<-data.frame(ID=1:nrow(shp_template))
for (i in 1:length(lista)){
  file<-lista[i]
  t<-read_csv(paste(folder_extract_crops,file, sep="/"))
  cob_crop[,i+1] <- t[,2]
}

#calculate the diversity
crop_divH<- diversity(cob_crop[,-1], index = "shannon", MARGIN = 1)
crop_divD1<- diversity(cob_crop[,-1], index = "simpson", MARGIN = 1)
crop_divD2<-diversity(cob_crop[,-1], index = "invsimpson", MARGIN = 1)
crop_divE<-D2/(ncol(cob_crop)-1)
div_crop<- cbind(ID=1:nrow(hex_stat), crop_divH, crop_divD1, crop_divD2, crop_divE)

write.table(div_crop, paste(folder_extract, "hex_diversity_crops.csv", sep="/"), sep=",", row.names = FALSE)




# 16. Irrigation
shape<-st_read(dsn =preprocessed, layer="IRRIGATION_gmia_v5_aei_pct_cellarea")
str(shape)
plot(shape[,2])
shape<-dplyr::select(shape, PCT_AEI) #dejo la columna que me interesa

start_time <- Sys.time() #set timer
interp<-st_interpolate_aw(shape, shp_template, extensive=FALSE)
hex_stat<- shp_template %>% st_join(interp)
hex_stat$PCT_AEI[is.na(hex_stat$PCT_AEI)] <- 0


irrig<-numeric()
for (i in as.numeric(shp_template$H_ID)) {
  x<-hex_stat %>% dplyr::filter (H_ID==i)%>% dplyr::select(PCT_AEI)%>% st_drop_geometry()
  irrig[i]<-  sum(x)
  }
end_time <- Sys.time();end_time - start_time #finish timer

hex_stat2<-data.frame(H_ID=shp_template$H_ID, irrig)
write.table(hex_stat2, paste(folder_extract, "hex_irrigation_AEI.csv", sep="/"), sep=",", row.names = FALSE)



# 17. cities travel time
RASTER<-raster(paste(preprocessed, "Acces_50k_SA_saaeac.tif", sep='/')) 
names(RASTER)<-"dist_cities"
start_time <- Sys.time() #set timer
hex_stat <- raster::extract(RASTER, shp_template, fun=mean , na.rm=TRUE, df=TRUE)
write.table(hex_stat, paste(folder_extract, "/hex_mean_traveltime_cities.csv", sep="/"), sep=",", row.names = FALSE)
end_time <- Sys.time();end_time - start_time #finish timer


# 18. ports travel time
RASTER<-raster(paste(preprocessed, "PORTS.tif", sep='/')) 
names(RASTER)<-"dist_ports"
RASTER[RASTER==1e6]<-NA
plot(RASTER)

start_time <- Sys.time() #set timer
hex_stat <- raster::extract(RASTER, shp_template, fun=mean , na.rm=TRUE, df=TRUE)
write.table(hex_stat, paste(folder_extract, "hex_mean_traveltime_ports.csv", sep="/"), sep=",", row.names = FALSE)
end_time <- Sys.time();end_time - start_time #finish timer




# DEMOGRAPHIC

# 19. Population density
RASTER<-raster(paste0(preprocessed, "/saaeac_pop12near_rec_near_clip.tif") )
names(RASTER)<-"pop12"

start_time <- Sys.time() #set timer
hex_stat <- raster::extract(RASTER, shp_template, fun=mean , na.rm=TRUE, df=TRUE)
write.table(hex_stat, paste(folder_extract, "/hex_mean_environmental_population_2012.csv", sep="/"), sep=",", row.names = FALSE)
end_time <- Sys.time();end_time - start_time #finish timer



# 20. Urbanization type
shape<-readOGR(dsn = preprocessed, layer="CITIES")
spplot(shape["POP"], zcol="POP")
pts<-subset(shape, select="POP")
#within buffer 100km
buf <- buffer(pts, width=100000, dissolve=FALSE)
plot(buf)
hex_stat<-sp::over(as(shp_template, "Spatial"), buf, fn = max)
hex_stat$ID<-1:nrow(hex_stat)
hex_stat$POP[is.na(hex_stat$POP)]<-0
shp_template$POP_buf<-hex_stat$POP
tm_shape(shp_template)+
  tm_fill(col= "POP_buf", style="pretty", n=20)
write.table(hex_stat, paste(folder_extract, "hex_pop_buf100km_max_city.csv", sep="/"), sep=",", row.names = FALSE)




# POLITICAL
# These datasets do not need pre-processing
# 21. WBI gov. effectiveness
# 22. WBI political stability
# 23. WBI rule of law
# 24. WBI regulatory quality

shp_template2<-st_join(shp_template, country_limits, largest=TRUE)
df<-data.frame(shp_template2)%>%
  dplyr::select(H_ID, ID_0, ISO, NAME_FAO)
write.table(df, paste(folder_extract, "hex_country_id_largest.csv", sep="/"), sep=",", row.names = FALSE)


# CULTURAL

# 25. languages density 
shape<-st_read(dsn =preprocessed, layer="LANGUAJES_MuturZikin")
shape2<-shape%>% dplyr::filter(lengua_mat==2) %>%  dplyr::select( unq) #dejo la columna que me interesa
plot(shape2[,1])
summary(shape2$unq)

#within buffer 100km
buf = st_buffer(shape2, dist = 100000)
plot(buf)

start_time <- Sys.time() #set timer
joined<-st_join(shp_template,shape2, left = TRUE)
nrow(joined)#each intersection is recorded in a row. hexagons that intersects with more than one polygon are repeated
nas<-joined$H_ID[ is.na(joined$unq)] # hexagons that doesn't intercept with any polygon also have a row, as the ones that intersects 1 polygon
reps<-  table(as.numeric(joined$H_ID))+1
reps[as.numeric(nas)]<- 1 #country official languaje
table(reps)
end_time <- Sys.time();end_time - start_time #finish timer 
hex_stat<-data.frame(ID=1:nrow(shp_template),reps)
hex_stat$ID<-as.character(hex_stat$ID)
MAP <- left_join(grilla_hex, hex_stat, by=c("H_ID"="ID"))

t <- tm_shape(MAP)+
  tm_fill(col="Freq", breaks = c(0,1,2,3,4,5,6,7,8,9,10, 15),palette="viridis", title="Richness")+
  tm_layout(frame=FALSE, legend.position = c("right","bottom"),legend.text.size=0.3)+
  tm_shape(countries)+ tm_borders()

write.table(hex_stat, paste(folder_extract, "hex_laguage_div_zikin.csv", sep="/"), sep=",", row.names = FALSE)




# 26. Anthropization century

# anthromes 1700
RASTER<-raster(paste(preprocessed, "anthro2_a1700.tif", sep='/')) 
names(RASTER)<-"ant1700"
RASTER_1700<-RASTER
RASTER_1700[RASTER_1700%in%c(0,  #water 
                             34, #Remote croplands
                             43, #Remote rangelands
                             53, #Remote woodlands
                             61, #Wild woodlands
                             62 #Wild treeless and barren lands
)]<-5000 #wild

RASTER_1700[RASTER_1700!=5000]<-1700 #anthropic
table(RASTER_1700[])
plot(RASTER_1700)
start_time <- Sys.time() #set timer
hex_stat <- raster::extract(RASTER, shp_template, fun=mean , na.rm=TRUE, df=TRUE)
write.table(hex_stat, paste(folder_extract, "/hex_mean_anthromesV2_1700.csv", sep="/"), sep=",", row.names = FALSE)
end_time <- Sys.time();end_time - start_time #finish timer


# anthromes 1800
RASTER<-raster(paste(preprocessed, "anthro2_a1800.tif", sep='/')) 
names(RASTER)<-"ant1800"
RASTER_1800<-RASTER
RASTER_1800[RASTER_1800%in%c(0, 34, 43, 53, 61,62)]<-5000 #wild
RASTER_1800[RASTER_1800!=5000]<-1800 #anthropic
plot(RASTER_1800)
table(RASTER_1800[])
start_time <- Sys.time() #set timer
hex_stat <- raster::extract(RASTER, shp_template, fun=mean , na.rm=TRUE, df=TRUE)
write.table(hex_stat, paste(folder_extract, "/hex_mean_anthromesV2_1800.csv", sep="/"), sep=",", row.names = FALSE)
end_time <- Sys.time();end_time - start_time #finish timer

# anthromes 1900
RASTER<-raster(paste(preprocessed, "anthro2_a1900.tif", sep='/')) 
names(RASTER)<-"ant1900"
RASTER_1900<-RASTER
RASTER_1900[RASTER_1900%in%c(0, 34, 43, 53, 61,62)]<-5000 #wild
RASTER_1900[RASTER_1900!=5000]<-1900 #anthropic
table(RASTER_1900[])
plot(RASTER_1900)
start_time <- Sys.time() #set timer
hex_stat <- raster::extract(RASTER, shp_template, fun=mean , na.rm=TRUE, df=TRUE)
write.table(hex_stat, paste(folder_extract, "/hex_mean_anthromesV2_1900.csv", sep="/"), sep=",", row.names = FALSE)
end_time <- Sys.time();end_time - start_time #finish timer

# anthromes 2000
RASTER<-raster(paste(preprocessed, "anthro2_a2000.tif", sep='/')) 
names(RASTER)<-"ant2000"
RASTER_2000<-RASTER
RASTER_2000[RASTER_2000%in%c(0, 34, 43, 53, 61,62)]<-5000 #wild
RASTER_2000[RASTER_2000!=5000]<-2000 #anthropic
plot(RASTER_2000)
table(RASTER_2000[])
start_time <- Sys.time() #set timer
hex_stat <- raster::extract(RASTER, shp_template, fun=mean , na.rm=TRUE, df=TRUE)
write.table(hex_stat, paste(folder_extract, "/hex_mean_anthromesV2_2000.csv", sep="/"), sep=",", row.names = FALSE)
end_time <- Sys.time();end_time - start_time #finish timer

# compute the human occupation length
earliest<-min(RASTER_1700, RASTER_1800, RASTER_1900, RASTER_2000)
tm_shape(earliest)+tm_raster(style="cat")
hex_ras2<-raster::resample(earliest,hex_ras, method="ngb" ) # resample
# extract values
start_time <- Sys.time() #set timer
anthromes<-crosstab (hex_ras, hex_ras2 , progress="text") #compute contingency table
end_time <- Sys.time(); end_time - start_time #finish timer
anthromes2<- anthromes %>%  as.data.frame()  
anthromes2<- anthromes2 %>% 
  reshape(idvar = c("hex_id"), timevar = "layer", direction = "wide")  %>%rename(Freq.Wild=Freq.5000)
anthromes2$npix<-rowSums(anthromes2[,2:6])

anthromes2$occup<- "wild"
anthromes2$occup[anthromes2$Freq.2000/anthromes2$npix >0.3]<- "y2000"
anthromes2$occup[anthromes2$Freq.1900/anthromes2$npix >0.3]<- "y1900"
anthromes2$occup[anthromes2$Freq.1800/anthromes2$npix >0.3]<- "y1800"
anthromes2$occup[anthromes2$Freq.1700/anthromes2$npix >0.3]<- "y1700"
table(anthromes2$occup)

write.table(anthromes2, paste(folder_extract, "/hex_30pct_anthromesV2.csv", sep="/"), sep=",", row.names = FALSE)















#====================================================================#
# THIRD SECTION - COMPILING TABLE OF HEXAGON STATS            #######
#====================================================================#



#create an empty HEXAGON data frame
HEX_DATA<-as.data.frame(shp_template)
HEX_DATA$UNQ<-as.character(HEX_DATA$H_ID)
HEX_DATA$H_area_km <- st_area(shp_template)/1e6


# add political divisions data
t<-read_csv(paste(folder_extract, "hex_country_id_largest.csv", sep="/"))
t$H_ID<-as.character(t$H_ID)
HEX_DATA<- left_join(HEX_DATA, dplyr::select(t, H_ID, ISO, NAME_FAO), by=c("UNQ"="H_ID"))

#create an empty COUNTRY data frame
COUNTRY_DATA<-country_limits%>%
  as.data.frame()%>%
  dplyr::select(ID_0, ISO, NAME_FAO)%>% 
  distinct()





# PHYSICAL DATA


# 1. Flat relief
t<-read_csv(paste(folder_extract, "hex_prop_flat_geomorphology.csv", sep="/"))
HEX_DATA$geo_flat<- t$geo
HEX_DATA$geo_flat[is.na(HEX_DATA$geo_flat)]<-0

# 2. Temperature
t<-read_csv(paste(folder_extract, "hex_median_temperature.csv", sep="/")) 
HEX_DATA$temp<- t$temp


# 3. Precipitation
t<-read_csv(paste(folder_extract, "hex_median_precipitation.csv", sep="/")) 
HEX_DATA$prec<- t$prec





# BIOLOGICAL


# 4. Plants diversity
t<-read_csv(paste(folder_extract, "hex_plant_richness_Kreft.csv", sep="/")) 
HEX_DATA$plant_spp<- t$CoKrig
HEX_DATA$plant_spp[is.na(HEX_DATA$plant_spp)]<-0

# 5. Protected areas
t<-read_csv(paste(folder_extract, "hex_area_PA.csv", sep="/")) 
HEX_DATA$PA<- t$prcntPA


# LAND COVER

# all 5 land cover classes:
# 6. Forest cover
# 7. Shrublands cover
# 8. Grasslands cover
# 9. Crop cover
# 10. Plantations cover
t<-read_csv(paste(folder_extract, "hex_prop_landcover_classes.csv", sep="/")) 
t$ID<-as.character(t$ID)
HEX_DATA<- left_join(HEX_DATA,t, by=c("UNQ"="ID"))

# 11. Cover diversity
t<-read.csv(paste(folder_extract, "hex_diversity_landcover_classes.csv", sep="/")) 
t$UNQ_num<-as.character(t$UNQ_num)
HEX_DATA<- left_join(HEX_DATA,t, by=c("UNQ"="UNQ_num")) 




# ECONOMIC


# 12. Centrality: composed calculation

# night time lights - hexagon sum
t<-read_csv(paste(folder_extract, "hex_sum_nighttimelights.csv", sep="/"))
t$ID<-as.character(t$ID)
HEX_DATA<- left_join(HEX_DATA,t, by=c("UNQ"="ID"))%>% rename (lights_sum=lights)

# country night time lights
t<-HEX_DATA %>% 
  group_by(NAME_FAO) %>% 
  summarise(NTL_country_sum = sum(lights_sum),
            country_n_hex= n(),
            NTL_country_mean =NTL_country_sum/n())
HEX_DATA<- left_join(HEX_DATA,t)
COUNTRY_DATA<- COUNTRY_DATA %>% left_join(t)

# country GDP
t<-read_csv("input_data/WB_GDP_world.csv")
t<-t %>%  rename(Country_Code=`Country Code`,
                 Country_Name= `Country Name`,
                 Indicator_Name= `Indicator Name`,
                 Indicator_Code= `Indicator Code`)
t2<-t[t$Country_Code  %in% as.character(COUNTRY_DATA$ISO),]%>%
  dplyr::select(Country_Code, `2008`:`2012`)
t2$meanGDP=rowMeans(t2[,-1])  
t2<-dplyr::select(t2,Country_Code, meanGDP)

COUNTRY_DATA<- COUNTRY_DATA %>% left_join(t2, by=c("ISO"="Country_Code"))
COUNTRY_DATA$meanGDP[COUNTRY_DATA$ISO=="GUF"] <-17336 
HEX_DATA<- left_join(HEX_DATA,COUNTRY_DATA)

# economic centrality
HEX_DATA<- HEX_DATA %>% mutate(
  rel_NTL=lights_sum/NTL_country_sum,
  centrality=meanGDP*rel_NTL)



# 13. Cattle density
t<-read_csv(paste(folder_extract, "hex_sum_cattle_density.csv", sep="/")) 
t$ID<-as.character(t$ID)
HEX_DATA<- left_join(HEX_DATA,t, by=c("UNQ"="ID")) 


# 14. Mine sites density
t<-read_csv(paste(folder_extract, "hex_mining_MRDS_sum_prod_size.csv", sep="/"))
t$ID<-as.character(t$ID)
HEX_DATA<- left_join(HEX_DATA,t, by=c("UNQ"="ID"))


# 15. Crop diversity
t<-read.csv(paste(folder_extract, "hex_diversity_crops.csv", sep="/")) 
t$ID<-as.character(t$ID)
HEX_DATA<- left_join(HEX_DATA,t, by=c("UNQ"="ID")) 


# 16. Irrigation
t<-read_csv(paste(folder_extract, "hex_irrigation_AEI.csv", sep="/"))
t$ID<-as.character(t$ID)
HEX_DATA<- left_join(HEX_DATA,t, by=c("UNQ"="ID"))
HEX_DATA$irrig[is.na(HEX_DATA$irrig)]<-0




# 17. cities travel time
t<-read_csv(paste(folder_extract, "hex_mean_traveltime_cities.csv", sep="/"))
t$ID<-as.character(t$ID)
HEX_DATA<- left_join(HEX_DATA,t, by=c("UNQ"="ID"))
HEX_DATA$dist_cities[is.na(HEX_DATA$dist_cities)]<-0
HEX_DATA<-HEX_DATA %>% rename ( TT_cities=dist_cities)



# 18. ports travel time
t<-read_csv(paste(folder_extract, "hex_mean_traveltime_ports.csv", sep="/"))
t$ID<-as.character(t$ID)
HEX_DATA<- left_join(HEX_DATA,t, by=c("UNQ"="ID"))
HEX_DATA$dist_ports[is.na(HEX_DATA$dist_ports)]<-0
HEX_DATA<-HEX_DATA %>% rename ( TT_ports=dist_ports)



# DEMOGRAPHIC

# 19. Population density
t<-read_csv(paste(folder_extract, "hex_mean_environmental_population_2012.csv", sep="/"))
t$ID<-as.character(t$ID)
HEX_DATA<- left_join(HEX_DATA,t, by=c("UNQ"="ID")) %>% rename (pop_dens=pop12)


# 20. Urbanization type
t<-read_csv(paste(folder_extract, "hex_pop_buf100km_max_city.csv", sep="/"))  
t$ID<-as.character(1:nrow(t))
plot(density(t$POP, na.rm=T))
summary(t$POP)
t$cat<-as.character(cut(t$POP, breaks=c(0, 1e5, 1e6, 1e7, max(t$POP,na.rm=T)), labels=c("small city", "medium city", "big city", "metropolis" ) ))
t$cat[is.na(t$cat)]<-"rural"
table(t$cat) #predefined city sizes
HEX_DATA<- left_join(HEX_DATA,dplyr::select(t, ID, cat) , by=c("UNQ"="ID"))%>% rename (type_city=cat)




# POLITICAL
# All political variables: 
# 21. WBI gov. effectiveness
# 22. WBI political stability
# 23. WBI rule of law
# 24. WBI regulatory quality

t<-read_csv("input_data/WGIData.csv")
t<-t %>%  rename(Indicator_Name=`Indicator Name`,
                 Indicator_Code=`Indicator Code`,
                 Country_Code=`Country Code`,
                 Country_Name=`Country Name`,
                 Y2010=`2010`)%>% 
  dplyr::filter( grepl("Estimate", Indicator_Name))

t2<-t[t$Country_Code  %in% as.character(COUNTRY_DATA$ISO),]%>%
  dplyr::select(Country_Name, Country_Code, Indicator_Code, Y2010)%>% 
  spread( key = Indicator_Code, value = Y2010)%>%
  rename(cont_cor =CC.EST,
         gov_ef   =GE.EST,
         pol_stab =PV.EST,
         rule_law =RL.EST,
         reg_qual =RQ.EST,
         voice_acc=VA.EST)

COUNTRY_DATA<- COUNTRY_DATA %>% left_join(t2, by=c("ISO"="Country_Code")) 
HEX_DATA<- HEX_DATA %>% left_join(t2, by=c("ISO"="Country_Code"))




# CULTURAL

# 25. languages density 
t<-read_csv(paste(folder_extract, "hex_laguage_div_zikin.csv", sep="/"))
t$ID<-as.character(t$ID)
HEX_DATA<- left_join(HEX_DATA, dplyr::select (t, ID ,Freq) , by=c("UNQ"="ID")) %>% rename (N_languajes=Freq)


# 26. Anthropization century
t<-read.csv(paste(folder_extract, "hex_30pct_anthromesV2.csv", sep="/")) 
table(t$occup)
t$hex_id<-as.character(t$hex_id)
HEX_DATA<- left_join(HEX_DATA, dplyr::select(t,-npix) , by=c("UNQ"="hex_id"))%>% rename (Anthrop=occup)
HEX_DATA$Anthrop<-as.character(HEX_DATA$Anthrop)
HEX_DATA$Anthrop[is.na(HEX_DATA$Anthrop)]<-"wild"
table(HEX_DATA$Anthrop) 





#---------------------#
#  CHECK
#---------------------#

#all variables together

str(HEX_DATA)
summary(HEX_DATA)
str(COUNTRY_DATA)
summary(COUNTRY_DATA)



# CHANGE NAMES
colnames(HEX_DATA)

HEX_DATA<- HEX_DATA %>% rename(
  cover_div_H=  H_cob.x,
  cover_div_D1= D1_cob.x,
  cover_div_D2= D2_cob.x,
  cover_div_E=  E_cob.x,
  
  crop_div_H=  H_cob.y,
  crop_div_D1= D1_cob.y,
  crop_div_D2= D2_cob.y,
  crop_div_E=  E_cob.y,
  
  setteled_1700 = Freq.1700,
  setteled_1800 = Freq.1800,
  setteled_1900 = Freq.1900,
  setteled_2000 = Freq.2000,
  setteled_never = Freq.Wild,
  anthrop_century = Anthrop,
  
  type_urbanization = type_city,
  pct_flat = geo_flat
)%>% dplyr::select(-geometry, -H_ID)


# SAVE

write.table(HEX_DATA, "input_data/hex_statistics_40km_v3.csv", row.names = FALSE)
write.table(COUNTRY_DATA, "input_data/country_statistics.csv", row.names = FALSE)





#------------------------#
# STANDARDIZE VARIABLES
#-----------------------#

#subset variables
var_names <- c("pct_flat","temp","prec",   #physical
          "plant_spp", "PA",        #biological
          "Trees","Shrubland","Grassland","Cropland","Plantations", "cover_div_H",    #land cover     
          "centrality","cattle","min_count","crop_div_H","irrig","TT_cities","TT_ports", #economic
          "pop_dens","type_urbanization",    #demographic
          "gov_ef","pol_stab","rule_law","reg_qual", #political
          "N_languajes","anthrop_century" #cultural
          ) 
input_vars <- data.frame(UNQ=HEX_DATA$UNQ)
input_vars <-cbind(input_vars, HEX_DATA[,colnames(HEX_DATA) %in% var_names])


#remove NAs
standardized_vars <-input_vars[complete.cases(input_vars), ]



# function to implement min max standardization
minMax <- function(x) { (x - min(x)) / (max(x) - min(x))}

#apply the function
for (i in 2:ncol(standardized_vars)){
  if (is.numeric(standardized_vars[,i])) {
    standardized_vars[,i] <- minMax(standardized_vars[,i])}
}

summary(standardized_vars)


#save
write.csv(standardized_vars, "input_data/standardized_vars_40km_v3.csv", row.names = FALSE)












#====================================================================#
# FOURTH SECTION - CLUSTERING ANALYSIS                      #######
#====================================================================#


# required packages
 library(tidyverse)
 library(corrplot)
 library(cluster)
 library(fpc)
 library(clValid)


 
 
# IMPORT THE DATA 

#variable's statistics per hexagon
standardized_vars <- read_csv("input_data/standardized_vars_40km_v3.csv")
str(standardized_vars)

# set column types
standardized_vars$UNQ<-as.character(standardized_vars$UNQ)
standardized_vars$type_urbanization <- base::ordered(standardized_vars$type_urbanization, levels= c("rural", "small city", "medium city", "big city", "metropolis"))
standardized_vars$anthrop_century <- base::ordered(standardized_vars$anthrop_century, levels= c( "y1700","y1800", "y1900", "y2000","wild"))
str(standardized_vars)


#visual check
for (var in var_names){
  gg_standardized_vars <- dplyr::select(standardized_vars, var)
  colnames(gg_standardized_vars) <- "XVAR"
  print(ggplot(gg_standardized_vars,aes(x=XVAR))+
    geom_density()+
    xlab(var))
}

# CHECK CORRELATIONS 
ccor_S <- standardized_vars%>% 
  dplyr::select(-UNQ)%>%
  mutate_if(is.factor,as.numeric)%>%
  cor(method="spearman", use="complete.obs")

corrplot(ccor_S, method = "number", main="spearman correlations", tl.col="black", type = "upper")




# COMPARING ALTERNATIVE CLUSTERING METHODS

# DISCLAIMER: for this section we had to use a different distance algorithm than the rest of the study due technical constrains.  Please beware of this if comparing results from different sections of the script. For further information refer to Appendix 1 of the publication.

# data transformation: ordinal variables to double (proportions of 1)
num_data<-standardized_vars[-1] %>%   mutate_if(is.factor,as.numeric)
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
cbind(colnames(standardized_vars)[-1],W) #check correspondence variables-weights

#calculate distance matrix
GOWERdist<-daisy(standardized_vars[-1], metric="gower", weights=W) #gower algorithm for mixed data


# DIANA CLUSTERING 

#run diana function - divisive hierarchical clustering
DianaClust <- diana(GOWERdist, diss=TRUE, stop.at.k =16 )

# generate classifications cutting the tree at different heights
clus_models<-standardized_vars #copy the input variables 
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




######


