# Copyright 2021 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

#roads_WP<-rast(file.path(spatialOutDir,'roads_WP.tif')) #Low - 3, Medium - 100, High 400 weighting
#disturbanceB_WP<-rast(file.path(spatialOutDir,'disturbanceB_WP.tif')) #Binary - disturbed/not disturbed
#Bring in Nightime, slope, elevation, Rivers
Slope30rP<-rast(file.path(spatialOutDir,"Slope30rP.tif"))
DEMSlope<-30
SlopeWt<-1000
m<-c(-999,DEMSlope,1,
     DEMSlope,9999,SlopeWt)
Slope30_w<-terra::classify(Slope30rP,matrix(m, ncol=3,byrow=TRUE),include.lowest=TRUE)
writeRaster(Slope30_w, filename=file.path(spatialOutDir,paste0('Slope30_w.tif')), overwrite=TRUE)
Slope30_w<-rast(file.path(spatialOutDir,"Slope30_w.tif"))
DEMSlope<-40
SlopeWt<-1000
m<-c(-999,DEMSlope,1,
     DEMSlope,9999,SlopeWt)
Slope40_w<-terra::classify(Slope30rP,matrix(m, ncol=3,byrow=TRUE),include.lowest=TRUE)
writeRaster(Slope40_w, filename=file.path(spatialOutDir,paste0('Slope40_w.tif')), overwrite=TRUE)
Slope40_w<-rast(file.path(spatialOutDir,"Slope40_w.tif"))
#2300m
DEM2300rP<-rast(file.path(spatialOutDir,"DEM2300rP.tif"))
DEMThresh<-2300
ElevWt<-1000
m<-c(-999,DEMThresh,1,
     DEMThresh,9999,ElevWt)
DEM2300_w<-terra::classify(DEM2300rP,matrix(m, ncol=3,byrow=TRUE),include.lowest=TRUE)
writeRaster(DEM2300_w, filename=file.path(spatialOutDir,paste0('DEM2300_w.tif')), overwrite=TRUE)
DEM2300_w<-rast(file.path(spatialOutDir,'DEM2300_w.tif'))
#Rivers >28m3
HydroR<-rast(file.path(spatialOutDir,"HydroR.tif"))
HydroWt<-1000
m<-c(-999,28,1,
     28,9999,HydroWt)
Hydro_w<-terra::classify(HydroR,matrix(m, ncol=3,byrow=TRUE),include.lowest=TRUE)
writeRaster(Hydro_w,file.path(spatialOutDir,'Hydro_w.tif'),overwrite=TRUE)
Hydro_w<-rast(file.path(spatialOutDir,'Hydro_w.tif'))
#Roads
RoadsR<-rast(file.path(spatialOutDir,'RoadsR.tif'))
  m<-c(-999,1,10,
       1,2,10,
       2,3,100,
       3,4,1000)
Roads_w<-terra::classify(RoadsR,matrix(m, ncol=3,byrow=TRUE),include.lowest=FALSE)
writeRaster(Roads_w,file.path(spatialOutDir,'Roads_w.tif'),overwrite=TRUE)
Roads_w<-rast(file.path(spatialOutDir,'Roads_w.tif'))

#Assign human footprint resistance_surface
#Combine roads and disturbance areas - assign max weight to pixel
AOI<-BC
Slope30_w<-rast(file.path(spatialOutDir,"Slope30_w.tif"))
DEM2300_w<-rast(file.path(spatialOutDir,'DEM2300_w.tif'))
Hydro_w<-rast(file.path(spatialOutDir,'Hydro_w.tif'))
Roads_w<-rast(file.path(spatialOutDir,'Roads_w.tif'))

disturbance_WPProvWtPASLp40<-rast(file.path(spatialOutDir,'disturbance_WPProvWtPASLp40.tif')) #1 to 1000 weighting depending on type
disturbanceStack<-c(Slope40_w,Hydro_w,Roads_w,disturbance_WPProvWtPASLp40)
resistance_surface_WPASlp40<-max(disturbanceStack,na.rm=TRUE) # combine and take highest weight
resistance_surface_PASlp40<-resistance_surface_WPASlp40  %>%
  merge(Pither_resistance_surfaceB)

disturbance_WP<-rast(file.path(spatialOutDir,'disturbance_WP.tif')) #1 to 1000 weighting depending on type
disturbanceStack<-c(Slope30_w,DEM2300_w,Hydro_w,Roads_w,disturbance_WP)
resistance_surface_WP<-max(disturbanceStack,na.rm=TRUE) # combine and take highest weight
resistance_surfaceP<-resistance_surface_WP  %>%
  merge(Pither_resistance_surfaceB)

disturbance_WR<-rast(file.path(spatialOutDir,'disturbance_WR.tif')) #1 to 1000 weighting depending on type
disturbanceStack<-c(Slope30_w,DEM2300_w,Hydro_w,Roads_w,disturbance_WR)
resistance_surface_WR<-max(disturbanceStack,na.rm=TRUE) # combine and take highest weight
resistance_surfaceR<-resistance_surface_WR  %>%
  merge(Pither_resistance_surfaceB)

disturbance_WN<-rast(file.path(spatialOutDir,'disturbance_WN.tif')) #1 to 1000 weighting depending on type
disturbanceStack<-c(Slope30_w,DEM2300_w,Hydro_w,Roads_w,disturbance_WN)
resistance_surface_WN<-max(disturbanceStack,na.rm=TRUE) # combine and take highest weight
resistance_surfaceN<-resistance_surface_WN  %>%
  merge(Pither_resistance_surfaceB)




##############

#Assign 1000 value to NA to check processing speed
disturbance_WPNa<-rast(file.path(spatialOutDir,'disturbance_WPNa.tif')) #1 to 1000 weighting depending on type
disturbanceStack<-c(Slope30_w,DEM2300_w,Hydro_w,Roads_w,disturbance_WPNa)
resistance_surface_WPNa1<-max(disturbanceStack,na.rm=TRUE) # combine and take highest weight
#resistance_surface_WPNa[resistance_surface_WPNa == 1000] <- NA
m<-c(1,9,1,
     9,99,10,
     99,999,100,
     999,9999,NA)
resistance_surface_WPNa<-terra::classify(resistance_surface_WPNa1,matrix(m, ncol=3,byrow=TRUE),include.lowest=TRUE)
#plot(resistance_surface_WPNa)
resistance_surfaceP_NA<-resistance_surface_WPNa %>%
  crop(rast(BCr), mask=TRUE)





####################

#Assign source_surface for connectivity
#source_surface<-raster(file.path(spatialOutDir,'source_WP.tif'))



#Make binary HF
#Buffer roads by 500m
roadsB_W<-raster(file.path(spatialOutDir,'roadsB_W.tif'))
roadsB_W[roadsB_W == 0] <- NA
#writeRaster(roadsB_W, filename=file.path(spatialOutDir,'roadsB_W'), format="GTiff", overwrite=TRUE)
roadsB_buff <- buffer(roadsB_W, width=500)
writeRaster(roadsB_buff, filename=file.path(spatialOutDir,'roadsB_buff.tif'), format="GTiff", overwrite=TRUE)

roadsB_W_S <- read_stars(file.path(spatialOutDir,'roadsB_W.tif'))
roadsB_W_S_sf<-st_as_sf(roadsB_W_S, as_points=FALSE, na.rm=TRUE)
write_sf(roadsB_W_S_sf, file.path(spatialOutDir,"roadsB_W_S_sf.gpkg"), overwrite=TRUE)

roadsB_W_S_sf<-read_sf(file.path(spatialOutDir,'roadsB_W_S_sf.gpkg'))

roadsB_W_S_sf_U<-st_union(roadsB_W_S_sf, by_feature=FALSE)
write_sf(roadsB_W_S_sf_U, file.path(spatialOutDir,"roadsB_W_S_sf_U.gpkg"), overwrite=TRUE)

roadsB_W_S_sf_U<-read_sf(file.path(spatialOutDir,'roadsB_W_S_sf_U.gpkg'))
roadsB_S_buff<-st_buffer(roadsB_W_S_sf_U, 500)

write_sf(roadsB_S_buff, file.path(spatialOutDir,"roadsB_S_buff.gpkg"), overwrite=TRUE)


HumanFPStack<-stack(roadsB_buff,disturbanceB_WP)
HumanFP_Binary<-max(HumanFPStack,na.rm=TRUE)
writeRaster(HumanFP_Binary, filename=file.path(spatialOutDir,'HumanFP_Binary.tif'), format="GTiff", overwrite=TRUE)

#########
resistance_surface<-resistance_surface_WP %>%
  mask(AOI) %>%
  crop(AOI)

#Assign source_surface
source_surface<-source_WP %>%
  mask(AOI) %>%
  crop(AOI)


