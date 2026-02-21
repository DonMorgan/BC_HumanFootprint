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

#assign corridor/climate matrix values to make new raster
#Read in files and prep
BCrBuff <-raster(file.path(spatialOutDir,'BCrBuff.tif'))
wshd<-st_read(file.path(spatialOutDir,"wshd.gpkg")) %>%
   mutate(wshd_id=as.numeric(rownames(.))) %>%
   mutate(area_Ha=units::set_units(st_area(.),ha)) %>%
   units::drop_units(.) %>%
  dplyr::select(wshd_id,area_Ha)
#wshdr<- fasterize(wshd, BCrBuff, field="wshd_id")
wshdv<-vect(wshd)
ConCorr_c<-rast(file.path(spatialOutDir,paste0('ConCorr_c.tif')))
ConCC<-rast(file.path(spatialOutDir,paste0('ConCC.tif')))
CC_c<-rast(file.path(spatialOutDir,paste0('CC_c.tif')))

#Get modal value in watershed
get_mode <- function(x, na.rm = TRUE) {
  if (na.rm) {
    x <- stats::na.omit(x)
  }
  # Calculate mode for a vector of values
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#First modal of climate connectivity
CC_wshd <- terra::extract(CC_c, wshdv, fun = get_mode, na.rm = TRUE)
wshd$CC_wshd <- CC_wshd[,2] # Column 2 is the result, ID maps to row id
CCCls_LUT<-data.frame(CC_wshd=c(1,2,3,4,5),CCClass=c('VeryLow','Low','Mod','High','VeryHigh'))
wshd_cc<-wshd %>%
  left_join(CCCls_LUT)
#write_sf(wshd, file.path(spatialOutDir,'wshd_con_cc.gpkg'), overwrite=TRUE)
#Second modal of straight conservation connectivity
Con_wshd <- terra::extract(ConCorr_c, wshdv, fun = get_mode, na.rm = TRUE)
wshd$Con_wshd <- Con_wshd[,2] # Column 2 is the result, ID maps to row id
ConnCls_LUT<-data.frame(Con_wshd=c(10,20,30,40,50),ConnectClass=c('Limited','Impeded','Diffused','Intensified','Channelized'))
wshd_con_cc<-wshd_cc %>%
  left_join(ConnCls_LUT)
#write_sf(wshd, file.path(spatialOutDir,'wshd_con_cc.gpkg'), overwrite=TRUE)
#Third modal of combined conservation and climate change
ConCC_wshd <- terra::extract(ConCC, wshdv, fun = get_mode, na.rm = TRUE)
wshd_con_cc$ConCC <- ConCC_wshd[,2] # Column 2 is the result, ID maps to row id
write_sf(wshd_con_cc, file.path(spatialOutDir,'wshd_con_cc.gpkg'), overwrite=TRUE)

#Try min?
Con_wshd <- terra::extract(ConCorr_c, wshdv, fun = get_mode, na.rm = TRUE)
wshd$Con_wshd <- Con_wshd[,2] # Column 2 is the result, ID maps to row id
ConnCls_LUT<-data.frame(Con_wshd=c(10,20,30,40,50),ConnectClass=c('Limited','Impeded','Diffused','Intensified','Channelized'))
wshd_con_cc<-wshd %>%
  left_join(ConnCls_LUT)
#write_sf(wshd, file.path(spatialOutDir,'wshd_con_cc.gpkg'), overwrite=TRUE)
#Second modal of combined conservation and climate change
ConCC_wshd <- terra::extract(ConCC, wshdv, fun = get_mode, na.rm = TRUE)
wshd_con_cc$ConCC <- ConCC_wshd[,2] # Column 2 is the result, ID maps to row id
write_sf(wshd_con_cc, file.path(spatialOutDir,'wshd_con_cc.gpkg'), overwrite=TRUE)



