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


BCrv<-rast(BCr) %>%
  extend(2500)
#plot(Pither_resistance_surfaceBuff)
#plot(BCrv,add=TRUE)

Pither_resistance_surface<-rast(file.path(SpatialDir,'Pither_Movement_Cost_Layer.tif')) %>%
  project(Prov_crs,method='near') #compare with max?
writeRaster(Pither_resistance_surface,file.path(SpatialDir,'Pither_resistance_surface.tif'),overwrite=TRUE)
Pither_resistance_surface<-rast(file.path(SpatialDir,'Pither_resistance_surface.tif'))

#Stitch with Provincial Resistance
#Generate a buffer around BC
BC_BB <- st_bbox(BC)
#Make a bounding box x% larger than width
BCBuff_l<-round((BC_BB$xmax-BC_BB$xmin)*0.2,0)
BCBuff <- BC %>%
  st_simplify(dTolerance = 1000) %>%
  st_buffer(dist=BCBuff_l)

#Clip the Pither resistance
#Clip to BC and resample to BC resolution (100m)
Pither_resistance_surfaceBuff <-Pither_resistance_surface %>%
  crop(BCBuff,mask=TRUE) %>%
  resample(BCrv,method='near') #tried modal but changed map dramatically
Pither_resistance_surfaceB<-Pither_resistance_surfaceBuff
#Set BCrv to 9999 for clipping out and setting to NA
BCrv[BCrv==1]<-9999
Pither_resistance_surfaceB[BCrv==9999]<-NA
plot(Pither_resistance_surfaceB)
writeRaster(Pither_resistance_surfaceB,file.path(SpatialDir,'Pither_resistance_surfaceB.tif'),overwrite=TRUE)

