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

#Glaciers<-st_read(file.path(spatialOutDir,'Glaciers.gpkg'))
Glaciers<-st_read(file.path(SpatialDir,'ExtraProvincialData/glaciers_final.gpkg'))

buffer_fun<-function(x){
  a<-x %>% mutate(Resistance=5)  %>% vect() %>% terra::buffer(width=-500) %>% rasterize(y=ProvRastB,field="Resistance")
  b<-x %>% mutate(Resistance=4)  %>% vect() %>% terra::buffer(width=-400) %>% rasterize(y=ProvRastB,field="Resistance")
  c<-x %>% mutate(Resistance=3)  %>% vect() %>% terra::buffer(width=-300) %>% rasterize(y=ProvRastB,field="Resistance")
  d<-x %>% mutate(Resistance=2)  %>% vect() %>% terra::buffer(width=-200) %>% rasterize(y=ProvRastB,field="Resistance")
  e<-x %>% mutate(Resistance=1)  %>% vect() %>% terra::buffer(width=-100) %>% rasterize(y=ProvRastB,field="Resistance")
  f<-x %>% mutate(Resistance=0)  %>% vect() %>%
    rasterize(y=ProvRastB,field="Resistance")
  l<-list(a,b,c,d,e,f)
  return(l)
}

glacierD.1<-Glaciers %>%
  mutate(glacier_id=as.numeric(rownames(.))) %>%
  mutate(area_Ha=units::set_units(st_area(.),ha)) %>%
  units::drop_units(.) %>%
  dplyr::filter(area_Ha>10) %>%
  mutate(glacier=1) %>%
  dplyr::select(glacier_id,glacier,area_Ha)
#Remove any possible interal mapsheet boundaries
glacierD.2<-glacierD.1 %>%
  st_union(.) %>%
  st_as_sf(.)
#Eliminate islands within glaciers - since they would be inaccessible
glacierDsf<-st_as_sf(glacierD.2)
Threshold <- units::set_units(1000, km^2)
glacierD <- fill_holes(glacierDsf %>% st_union, threshold = Threshold) %>%
  st_cast('POLYGON') %>%
  st_union() %>%
  st_as_sf %>%
  mutate(glacier=1)

#mapview(glacierD.1)+mapview(glacierD)
write_sf(glacierD,file.path(spatialOutDir,'glacierD.gpkg'))
BCrBuffd<-raster(file.path(spatialOutDir,'BCrBuff.tif'), overwrite=T)
glacierD.tif<-fasterize(glacierD,BCrBuffd,,field='glacier')
writeRaster(glacierD.tif,file.path(spatialOutDir,'glacierD.tif'),overwrite=T)

#Buffering glaciers and reclassifying so that resistance increases inwards of ice - modified from Cesar Estevo
glacier_file<-file.path(spatialOutDir,'glacier_buffered.tif')
if (!file.exists(glacier_file)) {
  glacier_sf_buf<-buffer_fun(glacierD)
  glacier_buffered.tif<-rast(glacier_sf_buf) %>% max(na.rm=T)
  writeRaster(glacier_buffered.tif,file.path(spatialOutDir,'glacier_buffered.tif'),overwrite=T)
}else{
  glacier_buffered.tif<-rast(file.path(spatialOutDir,'glacier_buffered.tif'))
}



