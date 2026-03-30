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

#Slope
SloperP <- terrain(EP_DEM, v = "slope", unit = "degrees")
#SloperP<-terra::resample(Slope,rast(BCr),method="cubic") %>%
#  crop(BCrT, mask=TRUE)
writeRaster(SloperP, filename=file.path(spatialOutDir,paste0('SloperP.tif')), overwrite=TRUE)
#SloperP<-rast(file.path(spatialOutDir,"SloperP.tif"))
#30 degree slope for National layer
DEMSlope<-30
SlopeWt<-1000
m<-c(-999,DEMSlope,1,
     DEMSlope,9999,SlopeWt)
Slope30_w<-terra::classify(SloperP,matrix(m, ncol=3,byrow=TRUE),include.lowest=TRUE)
writeRaster(Slope30_w, filename=file.path(spatialOutDir,paste0('Slope30_w.tif')), overwrite=TRUE)
#40 degree slope for updated Provincial
m<-c(-999,40,1,
     40,9999,1000)
Slope_w<-terra::classify(SloperP,matrix(m, ncol=3,byrow=TRUE),include.lowest=TRUE)
writeRaster(Slope_w, filename=file.path(spatialOutDir,paste0('Slope_w.tif')), overwrite=TRUE)
#Buffer slope to experiment
m<-c(-999,30,1,
     31,40,1,
     40,45,100,
     45,9999,1000)
SlopeB_w<-terra::classify(SloperP,matrix(m, ncol=3,byrow=TRUE),include.lowest=TRUE)
writeRaster(SlopeB_w, filename=file.path(spatialOutDir,paste0('SlopeB_w.tif')), overwrite=TRUE)
#2300m for National
DEMThresh<-2300
ElevWt<-1000
m<-c(-999,DEMThresh,1,
     DEMThresh,9999,ElevWt)
DEM2300_w<-terra::classify(EP_DEM,matrix(m, ncol=3,byrow=TRUE),include.lowest=TRUE)
writeRaster(DEM2300_w, filename=file.path(spatialOutDir,paste0('DEM2300_w.tif')), overwrite=TRUE)

#Rivers >28m3, >400m3 (large and major rivers)
HydroR<-rast(file.path(spatialOutDir,"HydroR.tif"))
HydroWt<-100
Hcutoff<-400
m<-c(-999,Hcutoff,1,
     Hcutoff,9999,HydroWt)
Hydro_w<-terra::classify(HydroR,matrix(m, ncol=3,byrow=TRUE),include.lowest=TRUE)
writeRaster(Hydro_w,file.path(spatialOutDir,'Hydro_w.tif'),overwrite=TRUE)
HydroWt<-1000
Hcutoff<-85
m<-c(-999,Hcutoff,1,
     Hcutoff,9999,HydroWt)
Hydro_w2<-terra::classify(HydroR,matrix(m, ncol=3,byrow=TRUE),include.lowest=TRUE)
writeRaster(Hydro_w2,file.path(spatialOutDir,'Hydro_w2.tif'),overwrite=TRUE)
HydroWt<-1000
Hcutoff<-28
m<-c(-999,Hcutoff,1,
     Hcutoff,9999,HydroWt)
Hydro_wN<-terra::classify(HydroR,matrix(m, ncol=3,byrow=TRUE),include.lowest=TRUE)
writeRaster(Hydro_wN,file.path(spatialOutDir,'Hydro_wN.tif'),overwrite=TRUE)

#Roads
roadsAll<-rast(file.path(spatialOutDir,'roadsAll.tif'))
m<-c(-999,1,1,
     1,2,10,
     2,3,100,
     3,4,1000)
Roads_w<-terra::classify(roadsAll,matrix(m, ncol=3,byrow=TRUE),include.lowest=FALSE)
writeRaster(Roads_w,file.path(spatialOutDir,'Roads_w.tif'),overwrite=TRUE)
#V2
m<-c(-999,1,1,
     1,2,100,
     2,3,1000,
     3,4,1000)
Roads_w2<-terra::classify(roadsAll,matrix(m, ncol=3,byrow=TRUE),include.lowest=FALSE)
writeRaster(Roads_w2,file.path(spatialOutDir,'Roads_w2.tif'),overwrite=TRUE)

#Pipelines
pipeWt<-100
pipelinesAll<-rast(file.path(spatialOutDir,'pipelinesAll.tif'))
Pipelines_w<-pipelinesAll*pipeWt
writeRaster(Pipelines_w,file.path(spatialOutDir,'Pipelines_w.tif'),overwrite=TRUE)
pipeWt<-1000
Pipelines_w2<-pipelinesAll*pipeWt
writeRaster(Pipelines_w2,file.path(spatialOutDir,'Pipelines_w2.tif'),overwrite=TRUE)

#Transmission Lines
transWt<-1
transmissionAll<-rast(file.path(spatialOutDir,'transmissionAll.tif'))
Transmission_w<-transmissionAll*transWt
writeRaster(Transmission_w,file.path(spatialOutDir,'Transmission_w.tif'),overwrite=TRUE)
transWt<-10
Transmission_w2<-transmissionAll*transWt
writeRaster(Transmission_w2,file.path(spatialOutDir,'Transmission_w2.tif'),overwrite=TRUE)

#Rail Lines - EP_railways includes BC
railWt<-10
railwaysR<-rast(file.path(spatialOutDir,'EP_railwaysR.tif'))
Railways_w<-railwaysR*railWt
writeRaster(Railways_w,file.path(spatialOutDir,'Railways_w.tif'),overwrite=TRUE)
railWt<-100
Railways_w2<-railwaysR*railWt
writeRaster(Railways_w2,file.path(spatialOutDir,'Railways_w2.tif'),overwrite=TRUE)
railWt<-1000
Railways_wN<-railwaysR*railWt
writeRaster(Railways_wN,file.path(spatialOutDir,'Railways_wN.tif'),overwrite=TRUE)

#Water buffers
water_buffered<-rast(file.path(spatialOutDir,'water_buffered.tif'))
water_w<-subst(water_buffered,from=c(5,4,3,2,1,0),to=c(1000,1000,100,100,10,10),others=NA)
writeRaster(water_w,filename=file.path(spatialOutDir,'water_w.tif'),overwrite=T)
water_w2<-subst(water_buffered,from=c(5,4,3,2,1,0),to=c(1000,1000,1000,1000,100,10),others=NA)
writeRaster(water_w2,filename=file.path(spatialOutDir,'water_w2.tif'),overwrite=T)
#National Water
watNr<-rast(file.path(spatialOutDir,'watNr.tif'))
water_wN<-watNr*1000
writeRaster(water_wN,filename=file.path(spatialOutDir,'water_wN.tif'),overwrite=T)

#Alpine buffers
alpineG_buffered<-rast(file.path(spatialOutDir,'alpineG_buffered.tif'))
alpineG_w<-subst(alpineG_buffered,from=c(5,4,3,2,1,0),to=c(1000,100,10,1,1,1),others=NA)
writeRaster(alpineG_w,filename=file.path(spatialOutDir,'alpineG_w.tif'),overwrite=T)

#Glacier buffers
#glacier_buffered<-rast(file.path(spatialOutDir,'glacier_buffered.tif'))
#glacier_w<-subst(glacier_buffered,from=c(5,4,3,2,1,0),to=c(1000,1000,1000,1000,1000,1000),others=NA)
#writeRaster(glacier_w,filename=file.path(spatialOutDir,'glacier_w.tif'),overwrite=T)
#glacier_w2<-subst(glacier_buffered,from=c(5,4,3,2,1,0),to=c(1000,1000,1000,1000,1000,1000),others=NA)
#writeRaster(glacier_w2,filename=file.path(spatialOutDir,'glacier_w2.tif'),overwrite=T)

#Glacier
glacierD<-rast(file.path(spatialOutDir,'glacierD.tif'))
glacier_w<-glacierD*1000
writeRaster(glacier_w,filename=file.path(spatialOutDir,'glacier_w.tif'),overwrite=T)

#Assign human footprint resistance_surface
#Combine roads and disturbance areas - assign max weight to pixel
AOI<-BC
Slope_w<-rast(file.path(spatialOutDir,"Slope_w.tif"))
DEM2300_w<-rast(file.path(spatialOutDir,'DEM2300_w.tif'))
Slope30_w<-rast(file.path(spatialOutDir,'Slope30_w.tif'))
Hydro_w<-rast(file.path(spatialOutDir,'Hydro_w.tif'))
Hydro_w2<-rast(file.path(spatialOutDir,'Hydro_w2.tif'))
Hydro_wN<-rast(file.path(spatialOutDir,'Hydro_wN.tif'))
#alpineG_w<-rast(file.path(spatialOutDir,'alpineG_w.tif'))
glacier_w<-rast(file.path(spatialOutDir,'glacier_w.tif'))
glacier_w2<-rast(file.path(spatialOutDir,'glacier_w2.tif'))
water_w<-rast(file.path(spatialOutDir,'water_w.tif'))
water_w2<-rast(file.path(spatialOutDir,'water_w2.tif'))
water_wN<-rast(file.path(spatialOutDir,'water_wN.tif'))
Roads_w<-rast(file.path(spatialOutDir,'Roads_w.tif'))
Roads_w2<-rast(file.path(spatialOutDir,'Roads_w2.tif'))
Railways_w<-rast(file.path(spatialOutDir,'Railways_w.tif'))
Railways_w2<-rast(file.path(spatialOutDir,'Railways_w2.tif'))
Transmission_w<-rast(file.path(spatialOutDir,'Transmission_w.tif'))
Transmission_w2<-rast(file.path(spatialOutDir,'Transmission_w2.tif'))
Pipelines_w<-rast(file.path(spatialOutDir,'Pipelines_w.tif'))
Pipelines_w2<-rast(file.path(spatialOutDir,'Pipelines_w2.tif'))
Pither_resistance_surfaceB<-rast(file.path(SpatialDir,'Pither_resistance_surfaceB.tif'))

disturbance_WP.1<-rast(file.path(spatialOutDir,'disturbance_WP.1.tif')) #1 to 1000 weighting depending on type
disturbanceStack<-c(water_w,Hydro_w,Slope_w,glacier_w,Roads_w,Railways_w,Transmission_w,Pipelines_w,disturbance_WP.1)
resistance_surface_WP<-max(disturbanceStack,na.rm=TRUE) # combine and take highest weight
resistance_surfaceP<-resistance_surface_WP  #%>%
#merge(Pither_resistance_surfaceB)

disturbance_WP.2<-rast(file.path(spatialOutDir,'disturbance_WP.2.tif')) #1 to 1000 weighting depending on type
disturbanceStack<-c(water_w2,Hydro_w2,Slope30_w,glacier_w,Roads_w2,Railways_w2,Transmission_w2,Pipelines_w2,disturbance_WP.2)
resistance_surface_WP2<-max(disturbanceStack,na.rm=TRUE) # combine and take highest weight
resistance_surfaceP2<-resistance_surface_WP2  #%>%
#merge(Pither_resistance_surfaceB)

disturbance_WR<-rast(file.path(spatialOutDir,'disturbance_WR.tif')) #1 to 1000 weighting depending on type
disturbanceStack<-c(water_w,Hydro_w,Slope_w,glacier_w,disturbance_WR)
resistance_surface_WR<-max(disturbanceStack,na.rm=TRUE) # combine and take highest weight
resistance_surfaceR<-resistance_surface_WR  #%>%
#merge(Pither_resistance_surfaceB)

disturbance_WN<-rast(file.path(spatialOutDir,'disturbance_WN.tif')) #1 to 1000 weighting depending on type
disturbanceStack<-c(water_wN,Hydro_wN,Slope30_w,DEM2300_w,glacier_w,Roads_w,Railways_wN,Transmission_w2,Pipelines_w2,disturbance_WN)
resistance_surface_WN<-max(disturbanceStack,na.rm=TRUE) # combine and take highest weight
resistance_surfaceN<-resistance_surface_WN  #%>%
#  merge(Pither_resistance_surfaceB)

