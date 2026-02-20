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

#Pull out linear features from Provinvial disturbance and merge with extra Provincial
disturbance_sfR1<-st_read(file.path(spatialOutDir,'disturbance_sfR1.gpkg'))
BCrBuff<-rast(file.path(spatialOutDir,'BCrBuff.tif'))
#Transmission Lines
P_transmission<-disturbance_sfR1 %>%
  dplyr::filter(disturb=='Transmission') %>%
  st_buffer(dist=50) %>%
  mutate(transmission=1) %>%
  dplyr::select(transmission)
P_transmissionR<-rasterize(vect(P_transmission), ProvRastB, field="transmission") %>%
  crop(BCrBuff, mask=TRUE)
writeRaster(P_transmissionR, file.path(spatialOutDir,'P_transmissionR.tif'), overwrite=TRUE)
#Join Extra Provincial data to Provincial data
EP_transmissionR<-rast(file.path(spatialOutDir,'P_transmissionR.tif'))
transmissionAll<-merge(P_transmissionR,EP_transmissionR)
writeRaster(transmissionAll, file.path(spatialOutDir,'transmissionAll.tif'), overwrite=TRUE)

#Pipelines
P_pipelines<-disturbance_sfR1 %>%
  dplyr::filter(disturb=='OGC_Infrastructure') %>%
  st_buffer(dist=50) %>%
  mutate(pipeline=1) %>%
  dplyr::select(pipeline)
P_pipelinesR<-rasterize(vect(P_pipelines), ProvRastB, field="pipeline") %>%
  crop(BCrBuff, mask=TRUE)
writeRaster(P_pipelinesR, file.path(spatialOutDir,'P_pipelinesR.tif'), overwrite=TRUE)
#Join Extra Provincial data to Provincial data
EP_pipelinesR<-rast(file.path(spatialOutDir,'EP_pipelinesR.tif'))
pipelinesAll<-merge(P_pipelinesR,EP_pipelinesR)
writeRaster(pipelinesAll, file.path(spatialOutDir,'pipelinesAll.tif'), overwrite=TRUE)


