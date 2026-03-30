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


cons_corr_class<-rast(file.path(spatialOutDir,paste0('ConCorr_c.tif'))) #Figure 4
climate_corr_class<-rast(file.path(spatialOutDir,paste0('CC_c.tif'))) #Figure 5
cons_climate_class<-rast(file.path(spatialOutDir,paste0('ConCorr_c.tif'))) #Figure 7
eco_focal_class<-rast(file.path(SpatialDir,paste0('1_eco_focal_class_final.tif'))) #Figure 9
threat_class_raw<-rast(file.path(SpatialDir,paste0('3_threat_combined_wt_max_class.tif'))) #Figure 10
wshd_connect<-st_read(file.path(spatialOutDir,'wshd_connect_mode.gpkg')) #Figure 11
Corridors<-st_read(file.path(spatialOutDir,'Corridors.gpkg')) #Figure 13


writeRaster(cons_corr_class, file.path(spatialOutDir,'1_cons_corr_class.tif'), overwrite=TRUE)
writeRaster(climate_corr_class, file.path(spatialOutDir,'2_climate_corr_class.tif'), overwrite=TRUE)
writeRaster(cons_climate_class, file.path(spatialOutDir,'3_cons_climate_class.tif'), overwrite=TRUE)
writeRaster(eco_focal_class, file.path(spatialOutDir,'4_eco_focal_class.tif'), overwrite=TRUE)
writeRaster(threat_class_raw, file.path(spatialOutDir,'5_threat_class_raw.tif'), overwrite=TRUE)
st_write(wshd_connect, file.path(spatialOutDir,'6_wshd_connect.gpkg'), overwrite=TRUE)
st_write(Corridors,file.path(spatialOutDir,'7_Corridors.gpkg'))

