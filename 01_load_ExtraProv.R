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

#Read in Extra Provincial Data
#Roads
EP_highways<-st_read(file.path(SpatialDir,'ExtraProvincialData/highways_final.gpkg')) %>%
  st_buffer(dist=300) %>%
  mutate(RoadUse=4) %>%
  dplyr::select(RoadUse)
EP_HighwayR<-rasterize(vect(EP_highways), ProvRastB, field="RoadUse") %>%
  crop(BCrBuff, mask=TRUE)
writeRaster(EP_HighwayR, file.path(spatialOutDir,'EP_HighwayR.tif'), overwrite=TRUE)

EP_roadsmajor<-st_read(file.path(SpatialDir,'ExtraProvincialData/roadsmajor_final.gpkg')) %>%
  st_buffer(dist=150) %>%
  mutate(RoadUse=3) %>%
  dplyr::select(RoadUse)
EP_roadsmajorR<-rasterize(vect(EP_roadsmajor), ProvRastB, field="RoadUse") %>%
  crop(BCrBuff, mask=TRUE)
writeRaster(EP_roadsmajorR, file.path(spatialOutDir,'EP_roadsmajorR.tif'), overwrite=TRUE)

EP_roadsminor<-st_read(file.path(SpatialDir,'ExtraProvincialData/roadsminor_final.gpkg')) %>%
  st_buffer(dist=150) %>%
  mutate(RoadUse=2) %>%
  dplyr::select(RoadUse)
EP_roadsminorR<-rasterize(vect(EP_roadsminor), ProvRastB, field="RoadUse") %>%
  crop(BCrBuff, mask=TRUE)
writeRaster(EP_roadsminorR, file.path(spatialOutDir,'EP_roadsminorR.tif'), overwrite=TRUE)

#Make a new rast that has the minimum value - ie the highest weighted road
RoadsBR<-max(c(EP_HighwayR,EP_roadsmajorR,EP_roadsminorR),na.rm=TRUE)
writeRaster(RoadsBR, file.path(spatialOutDir,'RoadsBR.tif'), overwrite=TRUE)

#Railways
EP_railways<-st_read(file.path(SpatialDir,'ExtraProvincialData/railways_final.gpkg')) %>%
  st_buffer(dist=50) %>%
  mutate(Rail=1) %>%
  dplyr::select(Rail)
EP_railwaysR<-rasterize(vect(EP_railways), ProvRastB, field="Rail") %>%
  crop(BCrBuff, mask=TRUE)
writeRaster(EP_railwaysR, file.path(spatialOutDir,'EP_railwaysR.tif'), overwrite=TRUE)

#Pipeline
EP_pipelines<-st_read(file.path(SpatialDir,'ExtraProvincialData/pipelines_final.gpkg')) %>%
  st_buffer(dist=50) %>%
  mutate(pipeline=1) %>%
  dplyr::select(pipeline)
EP_pipelinesR<-rasterize(vect(EP_pipelines), ProvRastB, field="pipeline") %>%
  crop(BCrBuff, mask=TRUE)
writeRaster(EP_pipelinesR, file.path(spatialOutDir,'EP_pipelinesR.tif'), overwrite=TRUE)

#Range
Range_final<-st_read(file.path(SpatialDir,'ExtraProvincialData/BLM_Natl_Grazing_Pasture.gpkg')) %>%
  st_transform(st_crs(BCBuff))
write_sf(Range_final,file.path(SpatialDir,'ExtraProvincialData/Range_final.gpkg'), overwrite=T)

#Transmission Lines
EP_transmission<-st_read(file.path(SpatialDir,'ExtraProvincialData/transmission_final.gpkg')) %>%
  st_buffer(dist=50) %>%
  mutate(transmission=1) %>%
  dplyr::select(transmission)
EP_transmissionR<-rasterize(vect(EP_transmission), ProvRastB, field="transmission") %>%
  crop(BCrBuff, mask=TRUE)
writeRaster(EP_transmissionR, file.path(spatialOutDir,'EP_transmissionR.tif'), overwrite=TRUE)

#DEM
EP_DEM<-rast(file.path(SpatialDir,'ExtraProvincialData/dem-100m_r_alos_bc-buff_3005_20-01-2026.tif')) %>%
  terra::resample(ProvRastB, method='bilinear') %>%
  crop(BCrBuff, mask=TRUE)
DEM2300rP<-terra::resample(DEMP,rast(BCr),method="cubic") %>%
  crop(rast(BCr), mask=TRUE)
DEM2300_w<-

SloperP <- terrain(EP_DEM, v = "slope", unit = "degrees")
SloperP<-terra::resample(Slope,rast(BCr),method="cubic") %>%
  crop(BCrT, mask=TRUE)


#Area Layers
Area_EPlist<-c('Range','Crops','cutblocks_0_20_edit','cutblocks_20_plus','rec','Urban','dams')
Area_EPcode<-c(5,9,10,11,12,13,15)
#Loop through areas and read in then combine into 1 layer and rasterize
areas_EP<-lapply(1:length(Area_EPlist),function(i)
  st_read(file.path(SpatialDir,paste0('ExtraProvincialData/',Area_EPlist[i],'_final.gpkg'))) %>%
    mutate(disturb=Area_EPlist[i]) %>%
    mutate(ID=Area_EPcode[i]) %>%
    dplyr::select(ID,disturb)
)
EP_disturbance_sfR1.1 <- do.call(rbind, areas_EP)

#Fix mines - reduce from 10km across circles to average mapped mine size
EP_mineIn<-st_read(file.path(SpatialDir,'ExtraProvincialData/mines_final.gpkg')) %>%
  mutate(area=st_area(.)) %>%
  mutate(perimeter=st_length(st_cast(.,"MULTILINESTRING"))) %>%
  mutate(compactness= as.numeric((4 * pi * area) / (perimeter^2)))
#Pull out the non-point mines
EP_mine1<-EP_mineIn %>%
  dplyr::filter(compactness<=0.9)
AvgMine<-units::drop_units(mean(EP_mine1$area))#/10000
#Calculate average area of mapped mines and assign it as area for point mines.
AvgMineRadius<-sqrt(AvgMine/pi)
#Pull out the point mines and make them the size of the average mapped mines
EP_mineCircles<-EP_mineIn %>%
  dplyr::filter(compactness>0.9)  %>%
  st_centroid(.) %>%
  st_buffer(dist=AvgMineRadius)
EP_mine<-rbind(EP_mine1,EP_mineCircles) %>%
  mutate(disturb='Mining_and_Extraction') %>%
  mutate(ID=19) %>%
  dplyr::select(ID,disturb)
write_sf(EP_mine,file.path(spatialOutDir,'EP_mine.gpkg'),overwrite=T)

EP_disturbance_sfR1 <- rbind(EP_disturbance_sfR1.1, EP_mine)

EP_disturbance_sfR<- fasterize(EP_disturbance_sfR1, raster(file.path(spatialOutDir,'ProvRastB.tif')), field="ID")
writeRaster(EP_disturbance_sfR, filename=file.path(spatialOutDir,'EP_disturbance_sfR'), format="GTiff", overwrite=TRUE)



