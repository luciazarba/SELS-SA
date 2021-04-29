

library(tidyverse)
library(rgdal)
library(raster)
library(devtools)
library(maptools)



# SET DIRECTIONS

#PC-PICT (IER)
tablas <- "C:/Users/Lucía/Google Drive/2015 - LUCC TELECONECCIONES/ANLAISIS LATAM/tablas y archivos de datos"
dir.templates <-"D:/LUCIA/BIBLIOTECA/### DATOS ###/GISES PICT LATAM/TEMPLATES - poligonos vacios"
dir.inputdata <- "D:/LUCIA/BIBLIOTECA/### DATOS ###/GISES PICT LATAM/DATOS INPUT/"
dir.SA_saaeac<-"D:/LUCIA/BIBLIOTECA/### DATOS ###/GISES PICT LATAM/AJUSTADOS Y PROYECTADOS (SA_saaeac)"
dir.mapas.jordan <- "D:/LUCIA/BIBLIOTECA/### DATOS ###/GISES PICT LATAM/DATOS INPUT/coberturas - MODIS_land_cover_v2.0 (jordan diciembre 2017)"

#PC-CASA
tablas <- "C:/Users/usuario/Google Drive/2015 - LUCC TELECONECCIONES/ANLAISIS LATAM/tablas y archivos de datos"
dir.templates <-"C:/Users/usuario/Lucia/ARCHIVOS BIOLOGOS/### DATOS ###/GISES PICT LATAM/TEMPLATES - poligonos vacios"
dir.inputdata <- "C:/Users/usuario/Lucia/ARCHIVOS BIOLOGOS/### DATOS ###/GISES PICT LATAM/DATOS INPUT/"
dir.SA_saaeac<-"C:/Users/usuario/Lucia/ARCHIVOS BIOLOGOS/### DATOS ###/GISES PICT LATAM/AJUSTADOS Y PROYECTADOS (SA_saaeac)"
dir.mapas.jordan <- "C:/Users/usuario/Lucia/ARCHIVOS BIOLOGOS/### DATOS ###/GISES PICT LATAM/DATOS INPUT/coberturas - MODIS_land_cover_v2.0 (jordan diciembre 2017)"



# TEMPLATE SHAPEFILES

# SHAPE LÍMITE CONTINENTAL en sudam en latlong
sudam_latlon <- readOGR(dsn=dir.templates, layer="sudam_LIMITE CONTINENTAL (en WGS84)")
# SHAPE LÍMITES ADMINISTRATIVOS
#sa_muni <- readOGR(dsn=dir.templates, layer="SA_adm2_saaeca")
# ECORREGIONES RASTERIZADAS
eco.ras<-raster(paste(dir.SA_saaeac, "sa_eco_rasterizado_2.tif", sep='/')) 

# proyección South American Alberts Equal Area Conic
proy.saaeac<-crs(eco.ras)
# ventana sudamérica en latlon
ventana_sa_latlon<-extent(sudam_latlon)

# SHAPE LÍMITE CONTINENTAL en saaeac
sudam_saaeac <-spTransform(sudam_latlon, proy.saaeac ) %>%
  SpatialLines2PolySet() %>% 
  PolySet2SpatialPolygons()  #convierto en SpatialPolygon
crs(sudam_saaeac)<-crs(eco.ras) #le doy sistema de coordenadas






# PUERTOS
# abro el shape con los datos
puertos<-readOGR(dsn=paste(dir.SA_saaeac,"conectividad - ne_10m_ports-roads",sep="/") ,layer="sudam_todos_los_puertos")
# los observo
sp::spplot(puertos, zcol="CAT")
# creo una copia en latlong
#puertos_latlong<-spTransform(puertos, crs(sudam_latlon))
#puertos categoría 1 (más relevantes)
#puertos1<-subset(puertos_latlong, CAT==1) 
puertos1<-subset(puertos, CAT==1) 
puertos2<-subset(puertos, CAT==2) 




# PENDIENTES
DEM<-raster(paste(dir.inputdata, "DEM_sdat_10003_1_20170721_165424762.tif",sep='/') ) 
DEM<-crop(DEM, ventana_sa_latlon)
res(DEM) 
nrow(DEM);ncol(DEM)
DEM<-raster::aggregate(DEM, fun=mean, fact=5)#multiplico x 5 para llegar a la resolución 2.5 arc min
#DEM<-raster::aggregate(DEM, fun=mean, fact=8,expand=TRUE)#multiplico x 9 para ver si logra correr
range(DEM[]) #los valores están en metros, las distancias también deberían.
DEM_saaeac<-projectRaster(DEM,crs=proy.saaeac) #esta proyección está en metros
plot(DEM_saaeac)
nrow(DEM_saaeac);ncol(DEM_saaeac)

# observo en el mapa
plot(DEM_saaeac); plot (puertos,col=puertos@data$CAT, pch=1,  add=T)



# RUTAS 
#abro el raster de velocidades (en km/hr)
vel_ras_km<-raster(paste(dir.SA_saaeac, "velocidades.tif",sep='/') ) 
table(vel_ras_km[])
# cambio la velocidad a metros / segundo
vel_ras <-vel_ras_km #hago una copia
table(vel_ras[])
vel_ras[vel_ras_km ==20] <- 1 #en lugar de ser velocidades en sí mismas, son factores que multiplican las velocidades dadas por la pendiente
vel_ras[vel_ras_km ==80] <- 4 
vel_ras[vel_ras_km ==120] <- 6 

#hago un resample del del raster de velocidades de rutas en su resolución Albers para que coincidan los pixeles con el DEM
vel_ras_res <- projectRaster(vel_ras,DEM_saaeac,method = 'bilinear')
# observo en el mapa
nrow(vel_ras_res);ncol(vel_ras_res)
plot(vel_ras_res); plot (puertos,col=puertos@data$CAT, pch=1,  add=T)



# RUTAS OSM
vel_ras_km_osm<-raster(paste(dir.SA_saaeac, "conectividad - ne_10m_ports-roads/vel_ras_osm.tif",sep='/') ) 
plot(vel_ras_km_osm)
vel_ras_km_osm[vel_ras_km_osm<20]<-20 #había un sendero marcado del template que usé, cuyos valores eran aprox 10

vel_ras <-vel_ras_km_osm #hago una copia
vel_ras[] <-  vel_ras_km_osm[]/20  #en lugar de ser velocidades en sí mismas, son factores que multiplican las velocidades dadas por la pendiente
table(vel_ras[])
nrow(vel_ras);ncol(vel_ras)

#hago un resample del del raster de velocidades de rutas en su resolución Albers para que coincidan los pixeles con el DEM
vel_ras_res <- projectRaster(vel_ras,DEM_saaeac,method = 'bilinear')
# observo en el mapa
nrow(vel_ras_res);ncol(vel_ras_res)
plot(vel_ras_res); plot (puertos,col=puertos@data$CAT, pch=1,  add=T)




#RECORTES PARA PRUEBA
p_n4<-subset(puertos1, n_fila %in% c(17,4,52,8)) #subset de 4 puntos de prueba
p17<-subset(puertos1, n_fila ==17)        #Buenos Aires
p124<-subset(puertos1, n_fila ==124)      #Rosario

#ext_latlong<- extent(vel_ras);  ext[1]<- -62;  ext[2]<- -58;  ext[3]<- -36;  ext[4]<- -32
ext_saaeac<- extent(vel_ras_res);  ext_saaeac[1]<- -170000;  ext_saaeac[2]<- 330000;  ext_saaeac[3]<- -400000;  ext_saaeac[4]<- -20000
vel_ras_rec<- crop(vel_ras_res, ext_saaeac)
DEM_rec<- crop(DEM_saaeac, ext_saaeac)
nrow(DEM_rec);ncol(DEM_rec)

#ploteo para ver que coincidan
plot(vel_ras_rec); plot(p17, add=T); plot(p124, add=T)
plot(DEM_rec); plot(p17, add=T); plot(p124, add=T)




#--------------#
 # DISTANCIAS #
#--------------#

library(gdistance)
#https://cran.r-project.org/web/packages/gdistance/vignettes/gdistance1.pdf



# MATRIZ DE TRANSICIÓN

#PRIMERO EN EL RECORTE

#calculo las velocidades según la pendiente según Tobler’s Hiking Function
altDiff <- function(x){x[2] - x[1]}
hd <- transition(DEM_rec, altDiff, 8, symm=FALSE)
slope <- geoCorrection(hd) # pendiente con valores asimétricos para subida y bajada
adj <- adjacent(DEM_rec, cells=1:ncell(DEM_rec), pairs=TRUE, directions=8) #artilugio para descartar los valores negativos

speed <- slope # hago una copia
speed[adj] <- 6 * exp(-3.5 * abs(slope[adj] + 0.05)) #calculo la velocidad de caminata según la pendiente
plot(raster(speed))
# cuando pendiente=0 la velocidad es: 5 m/s (18 km/h)
6 * exp(-3.5 * abs(0 + 0.05))
# cuando pendiente=0.2 la velocidad es: 2.5 m/s  (9km/hr)
6 * exp(-3.5 * abs(0.2 + 0.05))

#ajusto de peatón a vehículo 
#habíamos quedado en que las velocidades eran 20, 80 y 120 km/hr o 5.56, 22.2 y 33.3 m/s
#podemos cambiar el máximo a 7 y cuando haya calles le multiplicamos por 4 y en autopistas por 6
speed_slope <- slope # hago una copia
speed_slope[adj] <- 7 * exp(-3.5 * abs(slope[adj] + 0.05)) #calculo la velocidad de caminata según la pendiente
plot(raster(speed_slope))

#ahora las velocidades según las rutas
spped_rutas <- transition(vel_ras_rec, transitionFunction=mean, directions=8) #create TransitionLayer
plot(raster(spped_rutas))

# finalmente las multiplico
speed_slope_rutas_rec<-speed_slope*spped_rutas
plot(raster(speed_slope_rutas_rec))
range(speed_slope_rutas_rec@transitionMatrix)

# y calculo la conductancia
CONDUCTANCIA_rec <- geoCorrection(speed_slope_rutas_rec) #matriz de conductancia final
plot(raster(speed_slope_rutas_rec)); plot(p17, add=T); plot(p124, add=T)
plot(raster(CONDUCTANCIA_rec)); plot(p17, add=T); plot(p124, add=T)
plot(speed_slope_rutas_rec@transitionMatrix[1:1000,1:100],1/CONDUCTANCIA_rec@transitionMatrix[1:1000,1:100],
     ylab="tiempo(s)", xlab="velocidad (m/s)", main="relación conductancia-velocidad")
res(raster(CONDUCTANCIA_rec))
#los valores de conductancia geocorregida representan la inversa del tiempo, es decir que son 1/segundos
#los pixeles tienen aprox 4x5 km






# AHORA EL RASTER ENTERO

#calculo las velocidades según la pendiente según Tobler’s Hiking Function
altDiff <- function(x){x[2] - x[1]}
hd <- transition(DEM_saaeac, altDiff, 8, symm=FALSE)
slope <- geoCorrection(hd) # pendiente con valores asimétricos para subida y bajada
adj <- adjacent(DEM_saaeac, cells=1:ncell(DEM_saaeac), pairs=TRUE, directions=8) #artilugio para descartar los valores negativos
speed_slope <- slope # hago una copia
speed_slope[adj] <- 7 * exp(-3.5 * abs(slope[adj] + 0.05)) #calculo la velocidad de caminata según la pendiente
plot(raster(speed_slope))

#ahora las velocidades según las rutas
speed_rutas <- transition(vel_ras_res, transitionFunction=mean, directions=8) #create TransitionLayer
plot(raster(speed_rutas))

# finalmente las multiplico
speed_slope_rutas<-speed_slope*speed_rutas
plot(raster(speed_slope_rutas))
range(speed_slope_rutas@transitionMatrix)

# y calculo la conductancia
CONDUCTANCIA <- geoCorrection(speed_slope_rutas) #matriz de conductancia final
plot(raster(CONDUCTANCIA)); plot(p17,add=T)
TIEMPO_TRAVESIA<-1/raster(CONDUCTANCIA)/60  #hago un raster que muestra el tiempo de travesía en minutos
plot(TIEMPO_TRAVESIA)
#writeRaster(TIEMPO_TRAVESIA,paste(dir.SA_saaeac, "sa_TIEMPO_TRAVESIA_OSM(min).tif", sep='/'), overwrite=TRUE)





# RECORTE Y UN PUERTO - sin loop
#------------------------------------#

#puerto de buenos aires
TRmat=CONDUCTANCIA_rec                      #defino la matriz de transición
puerto<-p17                           #elijo un pouerto
grilla <- raster(TRmat); grilla[] <- 1  #fabrico una grilla vacía
pts <- rasterToPoints(grilla)[,-3]    #guardo en una matriz de 2 columnas las coordenadas de cada pixel 

#calculo las distancias
nrow(pts) #9516 demora 5 seg
res=costDistance(TRmat, fromCoords = pts, toCoords = puerto) 
grilla[]<-matrix(res, byrow = T)
plot(grilla); plot(puerto, add=T)

#puerto de rosario
TRmat=CONDUCTANCIA_rec                        #defino la matriz de transición
puerto<-p124                           #elijo un pouerto
grilla <- raster(TRmat); grilla[] <- 1  #fabrico una grilla vacía
pts <- rasterToPoints(grilla)[,-3]    #guardo en una matriz de 2 columnas las coordenadas de cada pixel 

#calculo las distancias
res=costDistance(TRmat, fromCoords = pts, toCoords = puerto) #demora 8 segundos!
grilla[]<-matrix(res, byrow = T)
plot(grilla); plot(puerto, add=T)



# RASTER COMPLETO Y UN PUERTO - sin loop :(  NO COREEEEEEE
#------------------------------------#
#no logra correr a ninguna de las resoluciones probadas

#puerto de buenos aires
TRmat=CONDUCTANCIA                         #defino la matriz de transición
puerto<-p124                           #elijo un pouerto
grilla <- raster(TRmat); grilla[] <- 1  
pts <- rasterToPoints(grilla)[,-3]    
nrow(pts) #antes era 2236946, ahora 881118

#calculo las distancias
res=costDistance(TRmat, fromCoords = pts, toCoords = puerto) # 21:12
res
grilla[]<-matrix(res, byrow = T)
plot(grilla); plot(puerto, add=T)




# COSTDISTANCE ENTRE 4 PUERTOS
#------------------------------#

#pruebo las funciones
plot(raster(CONDUCTANCIA)) ; plot(p_n4[3,], col="red", add=T, pch=21)
p_n4@coords
costDistance(CONDUCTANCIA, p_n4)

#entre tuc y el puerto de bs as
p_tuc<-c(-491575,590890)
costDistance(CONDUCTANCIA, fromCoords = p_tuc, toCoords = p17)
61321/3600 # paso de segundos a horas y obtengo unas 17hs. suena un poco mucho, pero razonable.
#google maps dice 14hs a una velocidad promedio de 89km/hr
SP<-shortestPath(CONDUCTANCIA, p_tuc, p17, output="SpatialLines")
plot(raster(CONDUCTANCIA_rec), xlab="x coordinate (m)", ylab="y coordinate (m)")
lines(SP, col="red", lwd=2)
#no coincide a la perfección pero bue

#entre tuc y el puerto de antofagasta en chile
costDistance(CONDUCTANCIA, fromCoords = p_tuc, toCoords = p_n4[3,])
56048.5/3600 #15.5hs, google maps dice 13.5hs, a vel promedio de 83.25 km/hr
SP<-shortestPath(CONDUCTANCIA, p_tuc, p_n4[3,], output="SpatialLines")
plot(raster(CONDUCTANCIA), xlab="x coordinate (m)", ylab="y coordinate (m)")
lines(SP, col="red", lwd=2)




# RECORTE Y ZOOM A LOS PUERTOS CON LAPPLY
#------------------------------#
ext_p<- extent(CONDUCTANCIA)
radio_buffer<-500000 #defino el radio del recorte: 500 km

#creo una función que hace el recorte y calcula las distnaicas. devuelve el raster en minutos
recorte_distancias_p<- function(puerto){
  #hago el recorte
  coor<- if (class(puerto) %in% c("numeric","matrix") ){puerto} else{puerto@coords}
  ext_p[1]<- coor[1]-radio_buffer;  ext_p[2]<- coor[1]+radio_buffer;  ext_p[3]<- coor[2]-radio_buffer;  ext_p[4]<-coor[2]+radio_buffer
  #TRmat<- crop(CONDUCTANCIA, ext_p) #no funciona, vamo a tener que hacer esto más largo
  vel_ras_rec<- crop(vel_ras_res, ext_p)
  DEM_rec<- crop(DEM_saaeac, ext_p)
  altDiff <- function(x){x[2] - x[1]}
  hd <- transition(DEM_rec, altDiff, 8, symm=FALSE)
  slope <- geoCorrection(hd) # pendiente con valores asimétricos para subida y bajada
  adj <- adjacent(DEM_rec, cells=1:ncell(DEM_rec), pairs=TRUE, directions=8) #artilugio para descartar los valores negativos
  speed_slope <- slope # hago una copia
  speed_slope[adj] <- 7 * exp(-3.5 * abs(slope[adj] + 0.05)) #calculo la velocidad de caminata según la pendiente
  #ahora las velocidades según las rutas
  speed_rutas <- transition(vel_ras_rec, transitionFunction=mean, directions=8) #create TransitionLayer
  # finalmente las multiplico
  speed_slope_rutas_rec<-speed_slope*speed_rutas
  # y calculo la conductancia
  TRmat <- geoCorrection(speed_slope_rutas_rec) #matriz de conductancia final
  
  #calculo las distancias
  grilla <- raster(TRmat); grilla[] <- 1  #fabrico una grilla vacía
  pts <- rasterToPoints(grilla)[,-3]    #guardo en una matriz de 2 columnas las coordenadas de cada pixel 
  res=costDistance(TRmat, fromCoords = pts, toCoords = puerto) #demora como 3 o 4 minutos
  grilla[]<-matrix(res, byrow = T)
  dist_min<-grilla/60
  
  return(dist_min)
}

#pruebo la función
puerto<-p17 #elijo un pouerto
dist_17<-recorte_distancias_p(puerto, radio_buffer)
#prueba 2
dist_np10_2<-recorte_distancias_p(puertos1[10,], 500000)
#prueba 3
dist_np10_3<-recorte_distancias_p(puertos1[10,])
#prueba 4
dist_np10_4<-recorte_distancias_p(l_ports[[10]]) #se clava
#prueba 5
dist_np11_5<-recorte_distancias_p(l_ports2[[11]])

# lapply funciona con listas o vectores, no matrices ni spatial objects. asì que tengo que convertir
l_ports<-split(pts, 1:nrow(pts)) 
l_ports2<-split(puertos1, 1:nrow(puertos1)) 
l_ports3<-l_ports2[1:3]

# no entiendo por què no anda... no tiene sentido
l_res<-lapply(l_ports3, recorte_distancias_p) #error al toque
l_res<-lapply(l_ports2, recorte_distancias_p) #error al toque
l_res<-lapply(l_ports, recorte_distancias_p)  #error a las 6hs!!!! grrrr :/




#------------------------------------------------------------------------------#

# RECORTE Y ZOOM A LOS PUERTOS MANUAL - ES EL QUE VA!
#------------------------------------------------------------------------------#


DIST_PORT_1<-raster(CONDUCTANCIA) #creo un raster
DIST_PORT_1[]<-1000000 #le asigno un valor alto
DIST_PORT_2<-DIST_PORT_1 #creo un raser para e los puertos cat 2
NAS<-DIST_PORT_1;  #genero otro raster de valores altos
ext_p<- extent(CONDUCTANCIA) #creo un objeto extent
radio_buffer<-700000 #defino el radio del recorte: 700 km

#creo una función que hace el recorte y calcula las distnaicas. devuelve el raster en minutos
recorte_distancias_p<- function(puerto){
  #hago el recorte
  coor<- if (class(puerto) %in% c("numeric","matrix") ){puerto} else{puerto@coords}
  ext_p[1]<- coor[1]-radio_buffer;  ext_p[2]<- coor[1]+radio_buffer;  ext_p[3]<- coor[2]-radio_buffer;  ext_p[4]<-coor[2]+radio_buffer
  #TRmat<- crop(CONDUCTANCIA, ext_p) #no funciona, vamo a tener que hacer esto más largo
  vel_ras_rec<- crop(vel_ras_res, ext_p)
  DEM_rec<- crop(DEM_saaeac, ext_p)
  altDiff <- function(x){x[2] - x[1]}
  hd <- transition(DEM_rec, altDiff, 8, symm=FALSE)
  slope <- geoCorrection(hd) # pendiente con valores asimétricos para subida y bajada
  adj <- adjacent(DEM_rec, cells=1:ncell(DEM_rec), pairs=TRUE, directions=8) #artilugio para descartar los valores negativos
  speed_slope <- slope # hago una copia
  speed_slope[adj] <- 7 * exp(-3.5 * abs(slope[adj] + 0.05)) #calculo la velocidad de caminata según la pendiente
  #ahora las velocidades según las rutas
  speed_rutas <- transition(vel_ras_rec, transitionFunction=mean, directions=8) #create TransitionLayer
  # finalmente las multiplico
  speed_slope_rutas_rec<-speed_slope*speed_rutas
  # y calculo la conductancia
  TRmat <- geoCorrection(speed_slope_rutas_rec) #matriz de conductancia final
  
  #calculo las distancias
  grilla <- raster(TRmat); grilla[] <- 1  #fabrico una grilla vacía
  pts <- rasterToPoints(grilla)[,-3]    #guardo en una matriz de 2 columnas las coordenadas de cada pixel 
  res=costDistance(TRmat, fromCoords = pts, toCoords = puerto) #demora como 3 o 4 minutos
  grilla[]<-matrix(res, byrow = T)
  dist_min<-grilla/60
  
  return(dist_min)
}

#aplico la función a los puertos CAT 1
# registro el tiempo
ptm <- proc.time()# Start the clock!
r<-recorte_distancias_p(puertos1[1,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS) #le doy el extent continental
DIST_PORT_1<-overlay(DIST_PORT_1,NASr, fun=min) #elijo los valores mínimos
proc.time() - ptm # Stop the clock

r<-recorte_distancias_p(puertos1[2,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_1<-overlay(DIST_PORT_1,NASr, fun=min)

r<-recorte_distancias_p(puertos1[3,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_1<-overlay(DIST_PORT_1,NASr, fun=min)

r<-recorte_distancias_p(puertos1[4,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_1<-overlay(DIST_PORT_1,NASr, fun=min)

r<-recorte_distancias_p(puertos1[5,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_1<-overlay(DIST_PORT_1,NASr, fun=min)

r<-recorte_distancias_p(puertos1[6,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_1<-overlay(DIST_PORT_1,NASr, fun=min)

r<-recorte_distancias_p(puertos1[7,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_1<-overlay(DIST_PORT_1,NASr, fun=min)

r<-recorte_distancias_p(puertos1[8,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_1<-overlay(DIST_PORT_1,NASr, fun=min)

r<-recorte_distancias_p(puertos1[9,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_1<-overlay(DIST_PORT_1,NASr, fun=min)

r<-recorte_distancias_p(puertos1[10,]) #puerto 10
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_1<-overlay(DIST_PORT_1,NASr, fun=min)

r<-recorte_distancias_p(puertos1[11,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_1<-overlay(DIST_PORT_1,NASr, fun=min)

r<-recorte_distancias_p(puertos1[12,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_1<-overlay(DIST_PORT_1,NASr, fun=min)

r<-recorte_distancias_p(puertos1[13,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_1<-overlay(DIST_PORT_1,NASr, fun=min)

r<-recorte_distancias_p(puertos1[14,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_1<-overlay(DIST_PORT_1,NASr, fun=min)

r<-recorte_distancias_p(puertos1[15,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_1<-overlay(DIST_PORT_1,NASr, fun=min)

r<-recorte_distancias_p(puertos1[16,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_1<-overlay(DIST_PORT_1,NASr, fun=min)

r<-recorte_distancias_p(puertos1[17,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_1<-overlay(DIST_PORT_1,NASr, fun=min)

r<-recorte_distancias_p(puertos1[18,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_1<-overlay(DIST_PORT_1,NASr, fun=min)

r<-recorte_distancias_p(puertos1[19,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_1<-overlay(DIST_PORT_1,NASr, fun=min)

r<-recorte_distancias_p(puertos1[20,])  #puerto 20
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_1<-overlay(DIST_PORT_1,NASr, fun=min)

r<-recorte_distancias_p(puertos1[21,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_1<-overlay(DIST_PORT_1,NASr, fun=min)

r<-recorte_distancias_p(puertos1[22,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_1<-overlay(DIST_PORT_1,NASr, fun=min)

r<-recorte_distancias_p(puertos1[23,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_1<-overlay(DIST_PORT_1,NASr, fun=min)

r<-recorte_distancias_p(puertos1[24,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_1<-overlay(DIST_PORT_1,NASr, fun=min)

r<-recorte_distancias_p(puertos1[25,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_1<-overlay(DIST_PORT_1,NASr, fun=min)

r<-recorte_distancias_p(puertos1[26,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_1<-overlay(DIST_PORT_1,NASr, fun=min)

r<-recorte_distancias_p(puertos1[27,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_1<-overlay(DIST_PORT_1,NASr, fun=min)

r<-recorte_distancias_p(puertos1[28,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_1<-overlay(DIST_PORT_1,NASr, fun=min)

r<-recorte_distancias_p(puertos1[29,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_1<-overlay(DIST_PORT_1,NASr, fun=min)

r<-recorte_distancias_p(puertos1[30,]) #puerto 30
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_1<-overlay(DIST_PORT_1,NASr, fun=min)

r<-recorte_distancias_p(puertos1[31,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_1<-overlay(DIST_PORT_1,NASr, fun=min)

r<-recorte_distancias_p(puertos1[32,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_1<-overlay(DIST_PORT_1,NASr, fun=min)

r<-recorte_distancias_p(puertos1[33,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_1<-overlay(DIST_PORT_1,NASr, fun=min)

r<-recorte_distancias_p(puertos1[34,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_1<-overlay(DIST_PORT_1,NASr, fun=min)

r<-recorte_distancias_p(puertos1[35,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_1<-overlay(DIST_PORT_1,NASr, fun=min)

r<-recorte_distancias_p(puertos1[36,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_1<-overlay(DIST_PORT_1,NASr, fun=min)

r<-recorte_distancias_p(puertos1[37,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_1<-overlay(DIST_PORT_1,NASr, fun=min)

r<-recorte_distancias_p(puertos1[38,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_1<-overlay(DIST_PORT_1,NASr, fun=min)

r<-recorte_distancias_p(puertos1[39,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_1<-overlay(DIST_PORT_1,NASr, fun=min)

r<-recorte_distancias_p(puertos1[40,]) #puerto 40
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_1<-overlay(DIST_PORT_1,NASr, fun=min)

r<-recorte_distancias_p(puertos1[41,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_1<-overlay(DIST_PORT_1,NASr, fun=min)

r<-recorte_distancias_p(puertos1[42,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_1<-overlay(DIST_PORT_1,NASr, fun=min)

r<-recorte_distancias_p(puertos1[43,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_1<-overlay(DIST_PORT_1,NASr, fun=min)

r<-recorte_distancias_p(puertos1[44,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_1<-overlay(DIST_PORT_1,NASr, fun=min)

r<-recorte_distancias_p(puertos1[45,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_1<-overlay(DIST_PORT_1,NASr, fun=min)

r<-recorte_distancias_p(puertos1[46,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_1<-overlay(DIST_PORT_1,NASr, fun=min)

#writeRaster(DIST_PORT_1,paste(dir.SA_saaeac, "sa_MIN_DISTANCIA_A_PUERTO_OSM_CAT1(min)_3.tif", sep='/'))
max(DIST_PORT_1[DIST_PORT_1<1000000])

plot(DIST_PORT_1)





#aplico la función a los puertos CAT 2
nrow(puertos2)
r<-recorte_distancias_p(puertos2[1,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS) #le doy el extent continental
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min) #elijo los valores mínimos

r<-recorte_distancias_p(puertos2[2,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[3,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[4,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[5,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[6,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[7,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[8,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[9,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[10,]) #puerto 10
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[11,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[12,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[13,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[14,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[15,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[16,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[17,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[18,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[19,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[20,])  #puerto 20
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[21,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[22,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[23,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[24,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[25,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[26,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[27,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[28,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[29,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[30,]) #puerto 30
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[31,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[32,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[33,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[34,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[35,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[36,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[37,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[38,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[39,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[40,]) #puerto 40
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[41,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[42,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[43,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[44,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[45,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[46,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[47,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[48,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[49,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[50,]) #puerto 50
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[51,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[52,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[53,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[54,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[55,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[56,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[57,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[58,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[59,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[60,]) #puerto 60
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[61,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[62,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[63,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[64,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[65,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[66,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[67,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[68,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[69,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[70,]) #puerto 70
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[71,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[72,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[73,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[74,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[75,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[76,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[77,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[78,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[79,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[80,]) #puerto 80
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[81,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[82,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[83,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[84,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[85,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[86,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[87,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[88,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[89,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[90,]) #puerto 90
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[91,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[92,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[93,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[94,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[95,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[96,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[97,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[98,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[99,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[100,]) #puerto 100
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[101,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[102,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)

r<-recorte_distancias_p(puertos2[103,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
DIST_PORT_2<-overlay(DIST_PORT_2,NASr, fun=min)


#writeRaster(DIST_PORT_2,paste(dir.SA_saaeac, "sa_MIN_DISTANCIA_A_PUERTOS_OSM_CAT2(min).tif", sep='/'))
max(DIST_PORT_2[DIST_PORT_2<1000000])





# SITIOS VACÍOS A LOS PUERTOS MANUAL - TODO DENTRO DEL RECORTE
#------------------------------#
#####
#hago copias por las dudas
P1<- raster(paste(dir.SA_saaeac, "sa_MIN_DISTANCIA_A_PUERTO_OSM_CAT1(min)_3.tif", sep='/'))
DIST_PORT_1<-P1

radio_buffer<-700000 #defino el radio del recorte: 700 km
NAS<-P1;  #genero otra copia del raster, para asignar valores altos
NAS[]<-1000000
#creo una función que calcula las distancias entre un set de puntos y un puerto, y devuelve el raster de distancias en minutos
dist_grilla_puerto<- function(TRmat, grilla, puerto){
  res=costDistance(TRmat, fromCoords = pts, toCoords = puerto) #demora como 3 o 4 minutos
  grilla[]<-matrix(res, byrow = T)
  dist_min<-grilla/60
  return(dist_min)}
#Puntos focales extra
p_focales<-read.csv(paste(tablas,"tablas extra - SELS/puntos extra puertos 1.csv",sep="/"))


#RECORTE 1
#hago el recorte del punto 1 y extraigo los pts de cada celda
coor<-c(1022877,2129297)  #coordenadas del punto 1
ext_p<- extent(vel_ras_res)
ext_p[1]<- coor[1]-radio_buffer;  ext_p[2]<- coor[1]+radio_buffer;  ext_p[3]<- coor[2]-radio_buffer;  ext_p[4]<-coor[2]+radio_buffer
ras_recorte<- crop(vel_ras_res, ext_p)
grilla <- ras_recorte; grilla[] <- 1  #fabrico una grilla vacía
pts <- rasterToPoints(grilla)[,-3]    #guardo en una matriz de 2 columnas las coordenadas de cada pixel 
plot(P1);plot(ras_recorte,add=T)

#hago el recorte del TRmat
vel_ras_rec<- crop(vel_ras_res, ext_p)
DEM_rec<- crop(DEM_saaeac, ext_p)
altDiff <- function(x){x[2] - x[1]}
hd <- transition(DEM_rec, altDiff, 8, symm=FALSE)
slope <- geoCorrection(hd) # pendiente con valores asimétricos para subida y bajada
adj <- adjacent(DEM_rec, cells=1:ncell(DEM_rec), pairs=TRUE, directions=8) #artilugio para descartar los valores negativos
speed_slope <- slope # hago una copia
speed_slope[adj] <- 7 * exp(-3.5 * abs(slope[adj] + 0.05)) #calculo la velocidad de caminata según la pendiente
#ahora las velocidades según las rutas
speed_rutas <- transition(vel_ras_rec, transitionFunction=mean, directions=8) #create TransitionLayer
# finalmente las multiplico
speed_slope_rutas_rec<-speed_slope*speed_rutas
# y calculo la conductancia
TRmat <- geoCorrection(speed_slope_rutas_rec) #matriz de conductancia final


p_focal1<-subset(p_focales, p==11)
p_focal2<-subset(p_focales, p==12)
p_focal3<-subset(p_focales, p==13)
plot(raster(TRmat))
points(p_focal1$x,p_focal1$y, col="red")
points(p_focal2$x,p_focal2$y, col="blue")
points(p_focal3$x,p_focal3$y, col="darkgreen")

# punto focal 1 - 22 min
ptm <- proc.time()# Start the clock!
r<-dist_grilla_puerto(TRmat, grilla, puerto=as.matrix(p_focal1[,2:3]))
r2<-r+p_focal1$min_dist
NAS[]<-1000000; NASr<-raster::merge(r2,NAS)
P1<-overlay(P1,NASr, fun=min)
proc.time() - ptm # Stop the clock
plot(P1)
#defino el punto focal 2
ptm <- proc.time()# Start the clock!
r<-dist_grilla_puerto(TRmat, grilla, puerto=as.matrix(p_focal2[,2:3] ))
r2<-r+p_focal2$min_dist
NAS[]<-1000000; NASr<-raster::merge(r2,NAS)
P1<-overlay(P1,NASr, fun=min)
proc.time() - ptm # Stop the clock
plot(P1)
#defino el punto focal 3
ptm <- proc.time()# Start the clock!
r<-dist_grilla_puerto(TRmat, grilla, puerto=as.matrix(p_focal3[,2:3] ))
r2<-r+p_focal3$min_dist
NAS[]<-1000000; NASr<-raster::merge(r2,NAS)
P1<-overlay(P1,NASr, fun=min)
proc.time() - ptm # Stop the clock
plot(P1)



#RECORTE 2

#hago el recorte del punto 1 y extraigo los pts de cada celda
coor<-c(71026,2129297)  #coordenadas del punto 1
ext_p<- extent(vel_ras_res)
ext_p[1]<- coor[1]-radio_buffer;  ext_p[2]<- coor[1]+radio_buffer;  ext_p[3]<- coor[2]-radio_buffer;  ext_p[4]<-coor[2]+radio_buffer
ras_recorte<- crop(vel_ras_res, ext_p)
grilla <- ras_recorte; grilla[] <- 1  #fabrico una grilla vacía
pts <- rasterToPoints(grilla)[,-3]    #guardo en una matriz de 2 columnas las coordenadas de cada pixel 
plot(P1);plot(ras_recorte,add=T)

#hago el recorte del TRmat
vel_ras_rec<- crop(vel_ras_res, ext_p)
DEM_rec<- crop(DEM_saaeac, ext_p)
altDiff <- function(x){x[2] - x[1]}
hd <- transition(DEM_rec, altDiff, 8, symm=FALSE)
slope <- geoCorrection(hd) # pendiente con valores asimétricos para subida y bajada
adj <- adjacent(DEM_rec, cells=1:ncell(DEM_rec), pairs=TRUE, directions=8) #artilugio para descartar los valores negativos
speed_slope <- slope # hago una copia
speed_slope[adj] <- 7 * exp(-3.5 * abs(slope[adj] + 0.05)) #calculo la velocidad de caminata según la pendiente
#ahora las velocidades según las rutas
speed_rutas <- transition(vel_ras_rec, transitionFunction=mean, directions=8) #create TransitionLayer
# finalmente las multiplico
speed_slope_rutas_rec<-speed_slope*speed_rutas
# y calculo la conductancia
TRmat <- geoCorrection(speed_slope_rutas_rec) #matriz de conductancia final

p_focal1<-subset(p_focales, p==21)
p_focal2<-subset(p_focales, p==22)
p_focal3<-subset(p_focales, p==23)
plot(raster(TRmat))
points(p_focal1$x,p_focal1$y, col="red")
points(p_focal2$x,p_focal2$y, col="blue")
points(p_focal3$x,p_focal3$y, col="darkgreen")

# punto focal 1 - 22 min
ptm <- proc.time()# Start the clock!
r<-dist_grilla_puerto(TRmat, grilla, puerto=as.matrix(p_focal1[,2:3] ))
r2<-r+p_focal1$min_dist
NAS[]<-1000000; NASr<-raster::merge(r2,NAS)
P1<-overlay(P1,NASr, fun=min)
proc.time() - ptm # Stop the clock
plot(P1)
#defino el punto focal 2
ptm <- proc.time()# Start the clock!
r<-dist_grilla_puerto(TRmat, grilla, puerto=as.matrix(p_focal2[,2:3] ))
r2<-r+p_focal2$min_dist
NAS[]<-1000000; NASr<-raster::merge(r2,NAS)
P1<-overlay(P1,NASr, fun=min)
proc.time() - ptm # Stop the clock
plot(P1)
#defino el punto focal 3
ptm <- proc.time()# Start the clock!
r<-dist_grilla_puerto(TRmat, grilla, puerto=as.matrix(p_focal3[,2:3] ))
r2<-r+p_focal3$min_dist
NAS[]<-1000000; NASr<-raster::merge(r2,NAS)
P1<-overlay(P1,NASr, fun=min)
proc.time() - ptm # Stop the clock
plot(P1)


#RECORTE 3

#hago el recorte del punto 1 y extraigo los pts de cada celda
coor<-c(-652441,2888886)  #coordenadas del punto 1
ext_p<- extent(vel_ras_res)
ext_p[1]<- coor[1]-radio_buffer;  ext_p[2]<- coor[1]+radio_buffer;  ext_p[3]<- coor[2]-radio_buffer;  ext_p[4]<-coor[2]+radio_buffer
ras_recorte<- crop(vel_ras_res, ext_p)
grilla <- ras_recorte; grilla[] <- 1  #fabrico una grilla vacía
pts <- rasterToPoints(grilla)[,-3]    #guardo en una matriz de 2 columnas las coordenadas de cada pixel 
plot(P1);plot(grilla,add=T)
#hago el recorte del TRmat
vel_ras_rec<- crop(vel_ras_res, ext_p)
DEM_rec<- crop(DEM_saaeac, ext_p)
altDiff <- function(x){x[2] - x[1]}
hd <- transition(DEM_rec, altDiff, 8, symm=FALSE)
slope <- geoCorrection(hd) # pendiente con valores asimétricos para subida y bajada
adj <- adjacent(DEM_rec, cells=1:ncell(DEM_rec), pairs=TRUE, directions=8) #artilugio para descartar los valores negativos
speed_slope <- slope # hago una copia
speed_slope[adj] <- 7 * exp(-3.5 * abs(slope[adj] + 0.05)) #calculo la velocidad de caminata según la pendiente
#ahora las velocidades según las rutas
speed_rutas <- transition(vel_ras_rec, transitionFunction=mean, directions=8) #create TransitionLayer
# finalmente las multiplico
speed_slope_rutas_rec<-speed_slope*speed_rutas
# y calculo la conductancia
TRmat <- geoCorrection(speed_slope_rutas_rec) #matriz de conductancia final
plot(P1);plot(raster(TRmat),add=T)

p_focal1<-subset(p_focales, p==31)
p_focal2<-subset(p_focales, p==32)
p_focal3<-subset(p_focales, p==33) #tiene un error
plot(raster(TRmat))
points(p_focal1$x,p_focal1$y, col="red")
points(p_focal2$x,p_focal2$y, col="blue")
points(p_focal3$x,p_focal3$y, col="darkgreen")

# punto focal 1 - 22 min
ptm <- proc.time()# Start the clock!
r<-dist_grilla_puerto(TRmat, grilla, puerto=as.matrix(p_focal1[,2:3] ))
r2<-r+p_focal1$min_dist
NAS[]<-1000000; NASr<-raster::merge(r2,NAS)
P1<-overlay(P1,NASr, fun=min)
proc.time() - ptm # Stop the clock
plot(P1)
#defino el punto focal 2
ptm <- proc.time()# Start the clock!
r<-dist_grilla_puerto(TRmat, grilla, puerto=as.matrix(p_focal2[,2:3] ))
r2<-r+p_focal2$min_dist
NAS[]<-1000000; NASr<-raster::merge(r2,NAS)
P1<-overlay(P1,NASr, fun=min)
proc.time() - ptm # Stop the clock
plot(P1)
#defino el punto focal 3
ptm <- proc.time()# Start the clock!
r<-dist_grilla_puerto(TRmat, grilla, puerto=as.matrix(p_focal3[,2:3]))
r2<-r+p_focal3$min_dist
NAS[]<-1000000; NASr<-raster::merge(r2,NAS)
P1<-overlay(P1,NASr, fun=min)
proc.time() - ptm # Stop the clock
plot(P1)



#RECORTE 4

#hago el recorte del punto 1 y extraigo los pts de cada celda
coor<-c(-678993,3647080)  #coordenadas del punto 4
ext_p<- extent(vel_ras_res)
ext_p[1]<- coor[1]-radio_buffer;  ext_p[2]<- coor[1]+radio_buffer;  ext_p[3]<- coor[2]-radio_buffer;  ext_p[4]<-coor[2]+radio_buffer
ras_recorte<- crop(vel_ras_res, ext_p)
grilla <- ras_recorte; grilla[] <- 1  #fabrico una grilla vacía
pts <- rasterToPoints(grilla)[,-3]    #guardo en una matriz de 2 columnas las coordenadas de cada pixel 
plot(P1);plot(grilla,add=T)
#hago el recorte del TRmat
vel_ras_rec<- crop(vel_ras_res, ext_p)
DEM_rec<- crop(DEM_saaeac, ext_p)
altDiff <- function(x){x[2] - x[1]}
hd <- transition(DEM_rec, altDiff, 8, symm=FALSE)
slope <- geoCorrection(hd) # pendiente con valores asimétricos para subida y bajada
adj <- adjacent(DEM_rec, cells=1:ncell(DEM_rec), pairs=TRUE, directions=8) #artilugio para descartar los valores negativos
speed_slope <- slope # hago una copia
speed_slope[adj] <- 7 * exp(-3.5 * abs(slope[adj] + 0.05)) #calculo la velocidad de caminata según la pendiente
#ahora las velocidades según las rutas
speed_rutas <- transition(vel_ras_rec, transitionFunction=mean, directions=8) #create TransitionLayer
# finalmente las multiplico
speed_slope_rutas_rec<-speed_slope*speed_rutas
# y calculo la conductancia
TRmat <- geoCorrection(speed_slope_rutas_rec) #matriz de conductancia final
plot(P1);plot(raster(TRmat),add=T)

p_focal1<-subset(p_focales, p==41)
p_focal2<-subset(p_focales, p==42)
plot(raster(TRmat))
points(p_focal1$x,p_focal1$y, col="red")
points(p_focal2$x,p_focal2$y, col="blue")

# punto focal 1 - 22 min
ptm <- proc.time()# Start the clock!
r<-dist_grilla_puerto(TRmat, grilla, puerto=as.matrix(p_focal1[,2:3] ))
r2<-r+p_focal1$min_dist
NAS[]<-1000000; NASr<-raster::merge(r2,NAS)
P1<-overlay(P1,NASr, fun=min)
proc.time() - ptm # Stop the clock
plot(P1)
#defino el punto focal 2
ptm <- proc.time()# Start the clock!
r<-dist_grilla_puerto(TRmat, grilla, puerto=as.matrix(p_focal2[,2:3] ))
r2<-r+p_focal2$min_dist
NAS[]<-1000000; NASr<-raster::merge(r2,NAS)
P1<-overlay(P1,NASr, fun=min)
proc.time() - ptm # Stop the clock
plot(P1)

#writeRaster(P1,paste(dir.SA_saaeac, "sa_MIN_DISTANCIA_A_PUERTO_OSM_CAT1(min)_comp.tif", sep='/'))








#PUERTOS CAT 2
P2<- raster(paste(dir.SA_saaeac, "sa_MIN_DISTANCIA_A_PUERTO_OSM_CAT2(min)_comp.tif", sep='/'))
DIST_PORT_2<-P2
radio_buffer<-700000 #defino el radio del recorte: 700 km
NAS<-P1;  #genero otra copia del raster, para asignar valores altos
NAS[]<-1000000
#creo una función que calcula las distancias entre un set de puntos y un puerto, y devuelve el raster de distancias en minutos
dist_grilla_puerto<- function(TRmat, grilla, puerto){
  res=costDistance(TRmat, fromCoords = pts, toCoords = puerto) #demora como 3 o 4 minutos
  grilla[]<-matrix(res, byrow = T)
  dist_min<-grilla/60
  return(dist_min)}
#Puntos focales extra
p_focales<-read.csv(paste(tablas,"tablas extra - SELS/puntos extra puertos 1.csv",sep="/"))


#RECORTE 1
#hago el recorte del punto 1 y extraigo los pts de cada celda
coor<-c(1022877,2129297)  #coordenadas del punto 1
ext_p<- extent(vel_ras_res)
ext_p[1]<- coor[1]-radio_buffer;  ext_p[2]<- coor[1]+radio_buffer;  ext_p[3]<- coor[2]-radio_buffer;  ext_p[4]<-coor[2]+radio_buffer
ras_recorte<- crop(vel_ras_res, ext_p)
grilla <- ras_recorte; grilla[] <- 1  #fabrico una grilla vacía
pts <- rasterToPoints(grilla)[,-3]    #guardo en una matriz de 2 columnas las coordenadas de cada pixel 
plot(P2);plot(grilla,add=T)

#hago el recorte del TRmat
vel_ras_rec<- crop(vel_ras_res, ext_p)
DEM_rec<- crop(DEM_saaeac, ext_p)
altDiff <- function(x){x[2] - x[1]}
hd <- transition(DEM_rec, altDiff, 8, symm=FALSE)
slope <- geoCorrection(hd) # pendiente con valores asimétricos para subida y bajada
adj <- adjacent(DEM_rec, cells=1:ncell(DEM_rec), pairs=TRUE, directions=8) #artilugio para descartar los valores negativos
speed_slope <- slope # hago una copia
speed_slope[adj] <- 7 * exp(-3.5 * abs(slope[adj] + 0.05)) #calculo la velocidad de caminata según la pendiente
#ahora las velocidades según las rutas
speed_rutas <- transition(vel_ras_rec, transitionFunction=mean, directions=8) #create TransitionLayer
# finalmente las multiplico
speed_slope_rutas_rec<-speed_slope*speed_rutas
# y calculo la conductancia
TRmat <- geoCorrection(speed_slope_rutas_rec) #matriz de conductancia final


p_focal1<-subset(p_focales, p==111)
p_focal2<-subset(p_focales, p==112)
p_focal3<-subset(p_focales, p==13)
p_focal4<-subset(p_focales, p==21)
plot(raster(TRmat))
points(p_focal1$x-50000,p_focal1$y, col="red")
points(p_focal2$x,p_focal2$y, col="blue")
points(p_focal3$x,p_focal3$y, col="darkgreen")
points(p_focal4$x,p_focal4$y, col="violet")

# punto focal 1 - 22 min
ptm <- proc.time()# Start the clock!
p_focal1$x<-p_focal1$x-50000 #corrijo el valor
r<-dist_grilla_puerto(TRmat, grilla, puerto=as.matrix(p_focal1[,2:3]))
r2<-r+p_focal1$min_dist_2
NAS[]<-1000000; NASr<-raster::merge(r2,NAS)
P2<-overlay(P2,NASr, fun=min)
proc.time() - ptm # Stop the clock
plot(P2)
#defino el punto focal 2
ptm <- proc.time()# Start the clock!
r<-dist_grilla_puerto(TRmat, grilla, puerto=as.matrix(p_focal2[,2:3] ))
r2<-r+p_focal2$min_dist_2
NAS[]<-1000000; NASr<-raster::merge(r2,NAS)
P2<-overlay(P2,NASr, fun=min)
proc.time() - ptm # Stop the clock
plot(P2)
#defino el punto focal 3
ptm <- proc.time()# Start the clock!
r<-dist_grilla_puerto(TRmat, grilla, puerto=as.matrix(p_focal3[,2:3] ))
r2<-r+p_focal3$min_dist_2
NAS[]<-1000000; NASr<-raster::merge(r2,NAS)
P2<-overlay(P2,NASr, fun=min)
proc.time() - ptm # Stop the clock
plot(P2)
#defino el punto focal 4
ptm <- proc.time()# Start the clock!
r<-dist_grilla_puerto(TRmat, grilla, puerto=as.matrix(p_focal4[,2:3] ))
r2<-r+p_focal4$min_dist_2
NAS[]<-1000000; NASr<-raster::merge(r2,NAS)
P2<-overlay(P2,NASr, fun=min)
proc.time() - ptm # Stop the clock
plot(P2)




#RECORTE 2

#hago el recorte del punto 1 y extraigo los pts de cada celda
coor<-c(71026,2129297)  #coordenadas del punto 1
ext_p<- extent(vel_ras_res)
ext_p[1]<- coor[1]-radio_buffer;  ext_p[2]<- coor[1]+radio_buffer;  ext_p[3]<- coor[2]-radio_buffer;  ext_p[4]<-coor[2]+radio_buffer
ras_recorte<- crop(vel_ras_res, ext_p)
grilla <- ras_recorte; grilla[] <- 1  #fabrico una grilla vacía
pts <- rasterToPoints(grilla)[,-3]    #guardo en una matriz de 2 columnas las coordenadas de cada pixel 
plot(P2);plot(ras_recorte,add=T)

#hago el recorte del TRmat
vel_ras_rec<- crop(vel_ras_res, ext_p)
DEM_rec<- crop(DEM_saaeac, ext_p)
altDiff <- function(x){x[2] - x[1]}
hd <- transition(DEM_rec, altDiff, 8, symm=FALSE)
slope <- geoCorrection(hd) # pendiente con valores asimétricos para subida y bajada
adj <- adjacent(DEM_rec, cells=1:ncell(DEM_rec), pairs=TRUE, directions=8) #artilugio para descartar los valores negativos
speed_slope <- slope # hago una copia
speed_slope[adj] <- 7 * exp(-3.5 * abs(slope[adj] + 0.05)) #calculo la velocidad de caminata según la pendiente
#ahora las velocidades según las rutas
speed_rutas <- transition(vel_ras_rec, transitionFunction=mean, directions=8) #create TransitionLayer
# finalmente las multiplico
speed_slope_rutas_rec<-speed_slope*speed_rutas
# y calculo la conductancia
TRmat <- geoCorrection(speed_slope_rutas_rec) #matriz de conductancia final

p_focal1<-subset(p_focales, p==21)
p_focal2<-subset(puertos2, n_fila==53)
p_focal3<-subset(puertos2, n_fila==38)
plot(raster(TRmat))
points(p_focal1$x,p_focal1$y, col="red")
plot(p_focal2, col="blue",add=T)
plot(p_focal3, col="darkgreen",add=T)

# punto focal 1 - 
ptm <- proc.time()# Start the clock!
r<-dist_grilla_puerto(TRmat, grilla, puerto=as.matrix(p_focal1[,2:3]))
r2<-r+p_focal1$min_dist_2
NAS[]<-1000000; NASr<-raster::merge(r2,NAS)
P2<-overlay(P2,NASr, fun=min)
proc.time() - ptm # Stop the clock
plot(P2)
#defino el punto focal 2
ptm <- proc.time()# Start the clock!
r<-dist_grilla_puerto(TRmat, grilla, puerto=p_focal2 )
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
P2<-overlay(P2,NASr, fun=min)
proc.time() - ptm # Stop the clock
plot(P2)
#defino el punto focal 3
ptm <- proc.time()# Start the clock!
r<-dist_grilla_puerto(TRmat, grilla, puerto=p_focal3)
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
P2<-overlay(P2,NASr, fun=min)
proc.time() - ptm # Stop the clock
plot(P2)



#RECORTE 4

#hago el recorte del punto 1 y extraigo los pts de cada celda
coor<-c(-678993,3647080)  #coordenadas del punto 4
ext_p<- extent(vel_ras_res)
ext_p[1]<- coor[1]-radio_buffer;  ext_p[2]<- coor[1]+radio_buffer;  ext_p[3]<- coor[2]-radio_buffer;  ext_p[4]<-coor[2]+radio_buffer
ras_recorte<- crop(vel_ras_res, ext_p)
grilla <- ras_recorte; grilla[] <- 1  #fabrico una grilla vacía
pts <- rasterToPoints(grilla)[,-3]    #guardo en una matriz de 2 columnas las coordenadas de cada pixel 
plot(P2);plot(grilla,add=T)
#hago el recorte del TRmat
vel_ras_rec<- crop(vel_ras_res, ext_p)
DEM_rec<- crop(DEM_saaeac, ext_p)
altDiff <- function(x){x[2] - x[1]}
hd <- transition(DEM_rec, altDiff, 8, symm=FALSE)
slope <- geoCorrection(hd) # pendiente con valores asimétricos para subida y bajada
adj <- adjacent(DEM_rec, cells=1:ncell(DEM_rec), pairs=TRUE, directions=8) #artilugio para descartar los valores negativos
speed_slope <- slope # hago una copia
speed_slope[adj] <- 7 * exp(-3.5 * abs(slope[adj] + 0.05)) #calculo la velocidad de caminata según la pendiente
#ahora las velocidades según las rutas
speed_rutas <- transition(vel_ras_rec, transitionFunction=mean, directions=8) #create TransitionLayer
# finalmente las multiplico
speed_slope_rutas_rec<-speed_slope*speed_rutas
# y calculo la conductancia
TRmat <- geoCorrection(speed_slope_rutas_rec) #matriz de conductancia final
plot(P2);plot(raster(TRmat),add=T)

p_focal1<-subset(p_focales, p==131)
p_focal2<-subset(p_focales, p==132)
p_focal3<-subset(puertos2, n_fila==145)
plot(raster(TRmat))
points(p_focal1$x,p_focal1$y, col="red")
points(p_focal2$x,p_focal2$y, col="blue")
plot(p_focal3, col="darkgreen",add=T)

# punto focal 1 - 
ptm <- proc.time()# Start the clock!
r<-dist_grilla_puerto(TRmat, grilla, puerto=as.matrix(p_focal1[,2:3]))
r2<-r+p_focal1$min_dist_2
NAS[]<-1000000; NASr<-raster::merge(r2,NAS)
P2<-overlay(P2,NASr, fun=min)
proc.time() - ptm # Stop the clock
plot(P2)
#defino el punto focal 2
ptm <- proc.time()# Start the clock!
r<-dist_grilla_puerto(TRmat, grilla, puerto=as.matrix(p_focal2[,2:3]) )
r2<-r+p_focal1$min_dist_2
NAS[]<-1000000; NASr<-raster::merge(r2,NAS)
P2<-overlay(P2,NASr, fun=min)
proc.time() - ptm # Stop the clock
plot(P2)
#defino el punto focal 3
ptm <- proc.time()# Start the clock!
r<-dist_grilla_puerto(TRmat, grilla, puerto=p_focal3)
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
P2<-overlay(P2,NASr, fun=min)
proc.time() - ptm # Stop the clock
plot(P2)


#writeRaster(P2,paste(dir.SA_saaeac, "sa_MIN_DISTANCIA_A_PUERTO_OSM_CAT2(min)_comp.tif", sep='/'),overwrite=TRUE)


#algunos recortes extra para completar el P2
p148<- subset(puertos2, n_fila==148) ; p148@coords[2]<-p148@coords[2]-650000
p55<- subset(puertos2, n_fila==55) ; p148@coords[1]<-p148@coords[1]-650000
p69<- subset(puertos2, n_fila==69) ; p148@coords[1]<-p148@coords[1]+650000

#calculo las distancias con la función anterior, la que hace el recorte
ptm <- proc.time()# Start the clock!
r<-recorte_distancias_p(p148)
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
P2<-overlay(P2,NASr, fun=min)
proc.time() - ptm # Stop the clock

ptm <- proc.time()# Start the clock!
r<-recorte_distancias_p(p55)
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
P2<-overlay(P2,NASr, fun=min)
proc.time() - ptm # Stop the clock

ptm <- proc.time()# Start the clock!
r<-recorte_distancias_p(p69)
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
P2<-overlay(P2,NASr, fun=min)
proc.time() - ptm # Stop the clock

#writeRaster(P2,paste(dir.SA_saaeac, "sa_MIN_DISTANCIA_A_PUERTO_OSM_CAT2(min)_comp.tif", sep='/'),overwrite=TRUE)



#creo el distancia a puertos total
P1<-raster(paste(dir.SA_saaeac, "sa_MIN_DISTANCIA_A_PUERTO_OSM_CAT1(min)_comp.tif", sep='/'))
P2<-raster(paste(dir.SA_saaeac, "sa_MIN_DISTANCIA_A_PUERTO_OSM_CAT2(min)_comp.tif", sep='/'))

Ptodos<-overlay(P1,P2, fun=min)
plot(Ptodos)
#writeRaster(Ptodos,paste(dir.SA_saaeac, "sa_MIN_DISTANCIA_A_PUERTO_OSM_todos(min)_comp.tif", sep='/'),overwrite=TRUE)



#####










# SITIOS VACÍOS A LOS PUERTOS MANUAL - CONDUCTANCIA TOTAL... NO CORRE PARA NADA! ENORME 
#------------------------------#
#####
#hago copias por las dudas
P1<- raster(paste(dir.SA_saaeac, "sa_MIN_DISTANCIA_A_PUERTO_OSM_CAT1(min)_3.tif", sep='/'))
P2<- raster(paste(dir.SA_saaeac, "sa_MIN_DISTANCIA_A_PUERTOS_OSM_CAT2(min).tif", sep='/'))

radio_buffer<-700000 #defino el radio del recorte: 700 km
NAS<-P1;  #genero otra copia del raster, para asignar valores altos
NAS[]<-1000000
#creo una función que calcula las distancias entre un set de puntos y un puerto, y devuelve el raster de distancias en minutos
dist_grilla_puerto<- function(TRmat, grilla, puerto){
  res=costDistance(TRmat, fromCoords = pts, toCoords = puerto) #demora como 3 o 4 minutos
  grilla[]<-matrix(res, byrow = T)
  dist_min<-grilla/60
  return(dist_min)}




#hago el recorte del punto 1 y extraigo los pts de cada celda
coor<-c(1022877,2129297)  #coordenadas del punto 1
ext_p<- extent(vel_ras_res)
ext_p[1]<- coor[1]-radio_buffer;  ext_p[2]<- coor[1]+radio_buffer;  ext_p[3]<- coor[2]-radio_buffer;  ext_p[4]<-coor[2]+radio_buffer
ras_recorte<- crop(vel_ras_res, ext_p)
grilla <- ras_recorte; grilla[] <- 1  #fabrico una grilla vacía
pts <- rasterToPoints(grilla)[,-3]    #guardo en una matriz de 2 columnas las coordenadas de cada pixel 
puertos_rec<-subset(puertos1, n_fila %in% c(128,156,120,138,72,8,93,154,137))
plot(P1);plot(ras_recorte,add=T);plot(puertos_rec, add=T)

#hago el recorte del TRmat
ext_puertos<-extent(puertos_rec)
vel_ras_rec<- crop(vel_ras_res, ext_puertos)
DEM_rec<- crop(DEM_saaeac, ext_puertos)
altDiff <- function(x){x[2] - x[1]}
hd <- transition(DEM_rec, altDiff, 8, symm=FALSE)
slope <- geoCorrection(hd) # pendiente con valores asimétricos para subida y bajada
adj <- adjacent(DEM_rec, cells=1:ncell(DEM_rec), pairs=TRUE, directions=8) #artilugio para descartar los valores negativos
speed_slope <- slope # hago una copia
speed_slope[adj] <- 7 * exp(-3.5 * abs(slope[adj] + 0.05)) #calculo la velocidad de caminata según la pendiente
#ahora las velocidades según las rutas
speed_rutas <- transition(vel_ras_rec, transitionFunction=mean, directions=8) #create TransitionLayer
# finalmente las multiplico
speed_slope_rutas_rec<-speed_slope*speed_rutas
# y calculo la conductancia
TRmat <- geoCorrection(speed_slope_rutas_rec) #matriz de conductancia final
plot(raster(TRmat));plot(grilla,add=T);plot(puertos_rec, add=T)


# puerto prueba: 7 segundos por cada punto!!!!! imposible
puerto=puertos_rec[1,]

ptm <- proc.time()# Start the clock!
res=costDistance(TRmat, fromCoords = pts[1,], toCoords = puerto) 
grilla[]<-matrix(res, byrow = T)
dist_min<-grilla/60
r<-dist_min
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
P1<-overlay(P1,NASr, fun=min)
proc.time() - ptm # Stop the clock


# puerto 1
ptm <- proc.time()# Start the clock!
r<-dist_grilla_puerto(TRmat, grilla,puerto=puertos_rec[1,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
P1<-overlay(P1,NASr, fun=min)
proc.time() - ptm # Stop the clock

# puerto 2
ptm <- proc.time()# Start the clock!
r<-dist_grilla_puerto(TRmat, grilla,puerto=puertos_rec[2,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
P1<-overlay(P1,NASr, fun=min)
proc.time() - ptm # Stop the clock

# puerto 3
ptm <- proc.time()# Start the clock!
r<-dist_grilla_puerto(TRmat, grilla,puerto=puertos_rec[3,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
P1<-overlay(P1,NASr, fun=min)
proc.time() - ptm # Stop the clock

# puerto 4
ptm <- proc.time()# Start the clock!
r<-dist_grilla_puerto(TRmat, grilla,puerto=puertos_rec[4,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
P1<-overlay(P1,NASr, fun=min)
proc.time() - ptm # Stop the clock

# puerto 5
ptm <- proc.time()# Start the clock!
r<-dist_grilla_puerto(TRmat, grilla,puerto=puertos_rec[5,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
P1<-overlay(P1,NASr, fun=min)
proc.time() - ptm # Stop the clock

# puerto 6
ptm <- proc.time()# Start the clock!
r<-dist_grilla_puerto(TRmat, grilla,puerto=puertos_rec[6,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
P1<-overlay(P1,NASr, fun=min)
proc.time() - ptm # Stop the clock

# puerto 7
ptm <- proc.time()# Start the clock!
r<-dist_grilla_puerto(TRmat, grilla,puerto=puertos_rec[7,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
P1<-overlay(P1,NASr, fun=min)
proc.time() - ptm # Stop the clock

# puerto 8
ptm <- proc.time()# Start the clock!
r<-dist_grilla_puerto(TRmat, grilla,puerto=puertos_rec[8,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
P1<-overlay(P1,NASr, fun=min)
proc.time() - ptm # Stop the clock

# puerto 9
ptm <- proc.time()# Start the clock!
r<-dist_grilla_puerto(TRmat, grilla,puerto=puertos_rec[9,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
P1<-overlay(P1,NASr, fun=min)
proc.time() - ptm # Stop the clock



#####



# SITIOS VACÍOS A LOS PUERTOS MANUAL - con trmat recortada... igual es muy grande
#------------------------------#
#####
#hago copias por las dudas
P1<- raster(paste(dir.SA_saaeac, "sa_MIN_DISTANCIA_A_PUERTO_OSM_CAT1(min)_3.tif", sep='/'))
P2<- raster(paste(dir.SA_saaeac, "sa_MIN_DISTANCIA_A_PUERTOS_OSM_CAT2(min).tif", sep='/'))

radio_buffer<-700000 #defino el radio del recorte: 700 km
NAS<-P1;  #genero otra copia del raster, para asignar valores altos
NAS[]<-1000000
#creo una función que calcula las distancias entre un set de puntos y un puerto, y devuelve el raster de distancias en minutos
dist_grilla_puerto<- function(TRmat, grilla, puerto){
  res=costDistance(TRmat, fromCoords = pts, toCoords = puerto) #demora como 3 o 4 minutos
  grilla[]<-matrix(res, byrow = T)
  dist_min<-grilla/60
  return(dist_min)}




#hago el recorte del punto 1 y extraigo los pts de cada celda
coor<-c(1022877,2129297)  #coordenadas del punto 1
ext_p<- extent(vel_ras_res)
ext_p[1]<- coor[1]-radio_buffer;  ext_p[2]<- coor[1]+radio_buffer;  ext_p[3]<- coor[2]-radio_buffer;  ext_p[4]<-coor[2]+radio_buffer
ras_recorte<- crop(vel_ras_res, ext_p)
grilla <- ras_recorte; grilla[] <- 1  #fabrico una grilla vacía
pts <- rasterToPoints(grilla)[,-3]    #guardo en una matriz de 2 columnas las coordenadas de cada pixel 
puertos_rec<-subset(puertos1, n_fila %in% c(128,156,120,138,72,8,93,154,137))
plot(P1);plot(ras_recorte,add=T);plot(puertos_rec, add=T)

#hago el recorte del TRmat
ext_puertos<-extent(puertos_rec)
vel_ras_rec<- crop(vel_ras_res, ext_puertos)
DEM_rec<- crop(DEM_saaeac, ext_puertos)
altDiff <- function(x){x[2] - x[1]}
hd <- transition(DEM_rec, altDiff, 8, symm=FALSE)
slope <- geoCorrection(hd) # pendiente con valores asimétricos para subida y bajada
adj <- adjacent(DEM_rec, cells=1:ncell(DEM_rec), pairs=TRUE, directions=8) #artilugio para descartar los valores negativos
speed_slope <- slope # hago una copia
speed_slope[adj] <- 7 * exp(-3.5 * abs(slope[adj] + 0.05)) #calculo la velocidad de caminata según la pendiente
#ahora las velocidades según las rutas
speed_rutas <- transition(vel_ras_rec, transitionFunction=mean, directions=8) #create TransitionLayer
# finalmente las multiplico
speed_slope_rutas_rec<-speed_slope*speed_rutas
# y calculo la conductancia
TRmat <- geoCorrection(speed_slope_rutas_rec) #matriz de conductancia final
plot(raster(TRmat));plot(grilla,add=T);plot(puertos_rec, add=T)


# puerto prueba: 7 segundos por cada punto!!!!! imposible
puerto=puertos_rec[1,]

ptm <- proc.time()# Start the clock!
res=costDistance(TRmat, fromCoords = pts[1,], toCoords = puerto) 
grilla[]<-matrix(res, byrow = T)
dist_min<-grilla/60
r<-dist_min
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
P1<-overlay(P1,NASr, fun=min)
proc.time() - ptm # Stop the clock


# puerto 1
ptm <- proc.time()# Start the clock!
r<-dist_grilla_puerto(TRmat, grilla,puerto=puertos_rec[1,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
P1<-overlay(P1,NASr, fun=min)
proc.time() - ptm # Stop the clock

# puerto 2
ptm <- proc.time()# Start the clock!
r<-dist_grilla_puerto(TRmat, grilla,puerto=puertos_rec[2,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
P1<-overlay(P1,NASr, fun=min)
proc.time() - ptm # Stop the clock

# puerto 3
ptm <- proc.time()# Start the clock!
r<-dist_grilla_puerto(TRmat, grilla,puerto=puertos_rec[3,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
P1<-overlay(P1,NASr, fun=min)
proc.time() - ptm # Stop the clock

# puerto 4
ptm <- proc.time()# Start the clock!
r<-dist_grilla_puerto(TRmat, grilla,puerto=puertos_rec[4,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
P1<-overlay(P1,NASr, fun=min)
proc.time() - ptm # Stop the clock

# puerto 5
ptm <- proc.time()# Start the clock!
r<-dist_grilla_puerto(TRmat, grilla,puerto=puertos_rec[5,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
P1<-overlay(P1,NASr, fun=min)
proc.time() - ptm # Stop the clock

# puerto 6
ptm <- proc.time()# Start the clock!
r<-dist_grilla_puerto(TRmat, grilla,puerto=puertos_rec[6,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
P1<-overlay(P1,NASr, fun=min)
proc.time() - ptm # Stop the clock

# puerto 7
ptm <- proc.time()# Start the clock!
r<-dist_grilla_puerto(TRmat, grilla,puerto=puertos_rec[7,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
P1<-overlay(P1,NASr, fun=min)
proc.time() - ptm # Stop the clock

# puerto 8
ptm <- proc.time()# Start the clock!
r<-dist_grilla_puerto(TRmat, grilla,puerto=puertos_rec[8,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
P1<-overlay(P1,NASr, fun=min)
proc.time() - ptm # Stop the clock

# puerto 9
ptm <- proc.time()# Start the clock!
r<-dist_grilla_puerto(TRmat, grilla,puerto=puertos_rec[9,])
NAS[]<-1000000; NASr<-raster::merge(r,NAS)
P1<-overlay(P1,NASr, fun=min)
proc.time() - ptm # Stop the clock



#####
















# SITIOS VACÍOS A LOS PUERTOS MANUAL - NO CORRE :(
#------------------------------#

P1<- raster(paste(dir.SA_saaeac, "sa_MIN_DISTANCIA_A_PUERTO_OSM_CAT1(min)_3.tif", sep='/'))
P2<- raster(paste(dir.SA_saaeac, "sa_MIN_DISTANCIA_A_PUERTOs_2(min).tif", sep='/'))


#aquí va con los puertos CAT 1

ext_p<-M1_sa # creo la máscara de sitios vacíos
vel_ras_rec<- mask(vel_ras_res, ext_p, maskvalue=FALSE)#enmascaro valores deseados en el raster vel
plot(vel_ras_rec)
DEM_rec<- mask(DEM_saaeac, ext_p, maskvalue=FALSE) #enmascaro valores deseados en el DEM
DEM_rec<- mask(DEM_rec, ext_p, maskvalue=NA) #enmascaro valores deseados en el DEM
plot(DEM_rec)
altDiff <- function(x){x[2] - x[1]}
hd <- transition(DEM_rec, altDiff, 8, symm=FALSE) #creo una matriz de transición por elevación
slope <- geoCorrection(hd) # pendiente con valores asimétricos para subida y bajada
adj <- adjacent(DEM_rec, cells=1:ncell(DEM_rec), pairs=TRUE, directions=8) #artilugio para descartar los valores negativos
speed_slope <- slope # hago una copia
speed_slope[adj] <- 7 * exp(-3.5 * abs(slope[adj] + 0.05)) #calculo la velocidad de caminata según la pendiente
#ahora las velocidades según las rutas
speed_rutas <- transition(vel_ras_rec, transitionFunction=mean, directions=8) #create TransitionLayer
# finalmente las multiplico
speed_slope_rutas_rec<-speed_slope*speed_rutas
# y calculo la conductancia
TRmat <- geoCorrection(speed_slope_rutas_rec) #matriz de conductancia final

puerto<-puertos1[1,]   #puerto de prueba

#creo una función que hace el recorte y calcula las distnaicas. devuelve el raster en minutos
#distancias_p<- function(puerto){
  
  #calculo las distancias
  grilla <- raster(TRmat); grilla[] <- 1  #fabrico una grilla vacía
  pts <- rasterToPoints(grilla)[,-3]    #guardo en una matriz de 2 columnas las coordenadas de cada pixel 
  res=costDistance(TRmat, fromCoords = pts, toCoords = puerto) #demora como 3 o 4 minutos
  grilla[]<-matrix(res, byrow = T)
  dist_min<-grilla/60
  
#  return(dist_min)}


















# RECORTE Y UN PUERTO - loop
#----------------------#

# PRUEBA CON EL PUERTO DE BUENOS AIRES
# defino las variables
TRmat=trc_rec                         #defino la matriz de transición
puerto<-p17                           #elijo un pouerto
grilla <- vel_ras_rec; grilla[] <- 1  #fabrico una grilla vacía

for (i in 1:nrow(grilla)){
  rSub <- grilla[i,1:ncol(grilla),  drop=FALSE] 
  pts <- rasterToPoints(rSub)[,-3]
  res=costDistance(TRmat, fromCoords = pts, toCoords = puerto)
  grilla[i,]<-t(res)}

plot(grilla); plot(p17, add=T)
str(grilla)
ncol(grilla)
nrow(grilla)
#el resultado es un raster donde los valores son los resultados del costDistance de cada celda! :D
#es un raster chico, de 98 x 116 celdas, es decir 11368 pixeles en total
#demoró 14 segundos, unas 800 celdas por segundo


# PRUEBA CON EL PUERTO DE ROSARIO 
TRmat=trc_rec                         #defino la matriz de transición
puerto<-p124                           #elijo un pouerto
grilla <- vel_ras_rec; grilla[] <- 1  #fabrico una grilla vacía

for (i in 1:nrow(grilla)){
  rSub <- grilla[i,1:ncol(grilla),  drop=FALSE] 
  pts <- rasterToPoints(rSub)[,-3]
  res=costDistance(TRmat, fromCoords = pts, toCoords = puerto)
  grilla[i,]<-t(res)
  print(i)}

str(grilla)
plot(grilla); plot(p124, add=T)
#EXCELENTE!!!!



# AHORA RASTER COMPLETO CON PUERTO DE ROSARIO - loop
#---------------------------------------------------#

TRmat=trc                         #defino la matriz de transición
puerto<-p124                           #elijo un pouerto
grilla <- vel_ras; grilla[] <- 1  #fabrico una grilla vacía

ncol(grilla)
nrow(grilla)
#es un raster enorme! de 1790 x 2087 celdas, es decir 3735730 pixeles en total, más de 3 millones!
# a 812 celdas por segundo serían unos 4600 segundos, osea 1 hora y 17 minutos
# unas 59 horas (dos días y 5 horas) para los 46 puertos de CAT=1 

for (i in 1:nrow(grilla)){
  rSub <- grilla[i,1:ncol(grilla),  drop=FALSE] 
  pts <- rasterToPoints(rSub)[,-3]
  res=costDistance(TRmat, fromCoords = pts, toCoords = puerto)
  grilla[i,]<-t(res)
  print(i)} #11:19


str(grilla)
plot(grilla); plot(p124, add=T)

#no va a andar... empieza el loop demorando 6 segundos por iteración y en la 45aba ya demora cerca de un minuto




# AHORA TODO! RASTER COMPLETO CON TODOS LOS PUERTOS - loop
#---------------------------------------------------#

#Oanda pero no lo logra, se clava en la fila 90 de 2000 :/

for(p in 1:5){
  #elijo un pouerto
  np<- puertos1@data[p,1]
  puerto<-subset(puertos1, n_fila ==np)
  print(p)
  
  for (i in 1:nrow(grilla)){
    rSub <- grilla[i,1:ncol(grilla),  drop=FALSE] 
    pts <- rasterToPoints(rSub)[,-3]
    res=costDistance(TRmat, fromCoords = pts, toCoords = puerto)
    grilla[i,]<-t(res)
    print(i)
  } 
  paste("grilla",np, sep="_")<-grilla
}





# RECORTE Y UN PUERTO - sin loop
#------------------------------------#

#puerto de buenos aires
TRmat=trc_rec                         #defino la matriz de transición
puerto<-p17                           #elijo un pouerto
grilla <- vel_ras_rec; grilla[] <- 1  #fabrico una grilla vacía
pts <- rasterToPoints(grilla)[,-3]    #guardo en una matriz de 2 columnas las coordenadas de cada pixel 

#calculo las distancias
res=costDistance(TRmat, fromCoords = pts, toCoords = puerto) #demora 8 segundos!
res
grilla[]<-matrix(res, byrow = T)
plot(grilla); plot(puerto, add=T)

#puerto de rosario
TRmat=trc_rec                         #defino la matriz de transición
puerto<-p124                           #elijo un pouerto
grilla <- vel_ras_rec; grilla[] <- 1  #fabrico una grilla vacía
pts <- rasterToPoints(grilla)[,-3]    #guardo en una matriz de 2 columnas las coordenadas de cada pixel 

#calculo las distancias
res=costDistance(TRmat, fromCoords = pts, toCoords = puerto) #demora 8 segundos!
res
grilla[]<-matrix(res, byrow = T)
plot(grilla); plot(puerto, add=T)




# RASTER COMPLETO Y UN PUERTO - sin loop
#------------------------------------#

#puerto de buenos aires
TRmat=trc                         #defino la matriz de transición
puerto<-p17                           #elijo un pouerto

grilla <- raster(TRmat); grilla[] <- 1  
pts <- rasterToPoints(grilla)[,-3]    
nrow(pts) #2274300

#calculo las distancias
res=costDistance(TRmat, fromCoords = pts, toCoords = puerto) #demora 8 segundos!
res
grilla[]<-matrix(res, byrow = T)
plot(grilla); plot(puerto, add=T)




