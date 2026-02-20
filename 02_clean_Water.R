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


##################
#Lakes<-st_read(file.path(spatialOutDir,'Lakes.gpkg'))
Lakes<-st_read(file.path(SpatialDir,'ExtraProvincialData/freshwater_final.gpkg'))
Rivers<-st_read(file.path(spatialOutDir,'Rivers.gpkg'))
saltwater<-st_read(file.path(SpatialDir,'ExtraProvincialData/saltwater_final.gpkg'))

lakeD.1<-Lakes %>%
  mutate(lake_id=as.numeric(rownames(.))) %>%
  mutate(area_Ha=units::set_units(st_area(.),ha)) %>%
  units::drop_units(.) %>%
  dplyr::filter(area_Ha>10) %>%
  mutate(water=1) %>%
  dplyr::select(lake_id,water,area_Ha)
riversD.1<-Rivers %>%
  mutate(lake_id=as.numeric(rownames(.))) %>%
  mutate(area_Ha=units::set_units(st_area(.),ha)) %>%
  units::drop_units(.) %>%
  dplyr::filter(area_Ha>10) %>%
  mutate(water=1) %>%
  dplyr::select(lake_id,water,area_Ha)
oceanD.1<-saltwater %>%
  mutate(lake_id=as.numeric(rownames(.))) %>%
  mutate(area_Ha=units::set_units(st_area(.),ha)) %>%
  units::drop_units(.) %>%
  dplyr::filter(area_Ha>10) %>%
  mutate(water=1) %>%
  dplyr::select(lake_id,water,area_Ha)
#watD.1<-rbind(lakeD.1,riversD.1,oceanD.1)# most rivers are <100m across
watD.1<-rbind(lakeD.1,oceanD.1)
#Remove any possible interal mapsheet boundaries
watD<-watD.1 %>%
  st_union(.) %>%
  st_as_sf(.)

#mapview(watD.1)+mapview(watD)
write_sf(watD,file.path(spatialOutDir,'watD.gpkg'))

#Buffering freshwater and reclassifying so that resistance increases inwards of freshwater - modified from Cesar Estevo

#Get sf of disturbance and filter for water polygons
#it is easier to buffer polygons than the raster

buffer_fun<-function(x){
  a<-x %>% mutate(Resistance=5)  %>% vect() %>% terra::buffer(width=-500) %>% rasterize(y=ProvRastB,field="Resistance")
  b<-x %>% mutate(Resistance=4)  %>% vect() %>% terra::buffer(width=-400) %>% rasterize(y=ProvRastB,field="Resistance")
  c<-x %>% mutate(Resistance=3)  %>% vect() %>% terra::buffer(width=-300) %>% rasterize(y=ProvRastB,field="Resistance")
  d<-x %>% mutate(Resistance=2)  %>% vect() %>% terra::buffer(width=-200) %>% rasterize(y=ProvRastB,field="Resistance")
  e<-x %>% mutate(Resistance=1)  %>% vect() %>% terra::buffer(width=-100) %>% rasterize(y=ProvRastB,field="Resistance")
  f<-x %>% mutate(Resistance=0)  %>% vect() %>% rasterize(y=ProvRastB,field="Resistance")
  l<-list(a,b,c,d,e,f)
  return(l)
}

water_file<-file.path(spatialOutDir,'water_buffered.tif')
if (!file.exists(water_file)) {
  water_sf_buf<-buffer_fun(watD)
  water_buffered<-rast(water_sf_buf) %>% max(na.rm=T)
  writeRaster(water_buffered,file.path(spatialOutDir,'water_buffered.tif'),overwrite=T)
}else{
  water_buffered<-rast(file.path(spatialOutDir,'water_buffered.tif'))
}

#National Water
watN<-watD.1 %>%
  mutate(area_Ha=units::set_units(st_area(.),ha)) %>%
  units::drop_units(.) %>%
  dplyr::filter(area_Ha>10) %>%
  mutate(water=1)
watNr<-fasterize(watN,BCrBuffd,field="water")
writeRaster(watNr,file.path(spatialOutDir,'watNr.tif'),overwrite=T)



