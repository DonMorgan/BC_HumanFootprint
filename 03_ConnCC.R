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

#Read in corridor file and reclassify into 5 bins
CorrFile<-  'BCP1_2_18Feb2026/CurMapMean_BCP1_2_18Feb2026.tif'
ConCorr<-rast(file.path(CorrDirOut,CorrFile))
unique(values(ConCorr))
m<-c(-999,0.2,1,
     0.2,0.7,2,
     0.7,1.3,3,
     1.3,1.7,4,
     1.7,9999,5)
ConCorr_c<-as.factor(terra::classify(ConCorr,matrix(m, ncol=3,byrow=TRUE),include.lowest=TRUE))
levels(ConCorr_c)<-data.frame(ID=c(1,2,3,4,5),Category=c('Limited','Impeded','Diffused','Intensified','Channelized'))
plot(ConCorr_c)
writeRaster(ConCorr_c, filename=file.path(spatialOutDir,paste0('ConCorr_c.tif')), overwrite=TRUE)

#Read in climate corridor and reclassify into 4 bins
CCFile<-'6_cli_cor_corridor_class.tif'
CC<-rast(file.path(SpatialDir,'Climate',CCFile)) %>%
  resample(ConCorr,method='modal')
#unique(values(CC))
  m<-c(-999,1,1,
       1,2,2,
       2,3,3,
       3,4,4,
       4,9999,5)
CC_c<-as.factor(terra::classify(CC,matrix(m, ncol=3,byrow=TRUE),include.lowest=TRUE))
levels(CC_c)<-data.frame(ID=c(1,2,3,4,5),Category=c('VeryLow','Low','Moderate','High','VeryHigh'))
plot(CC_c)
writeRaster(CC_c, filename=file.path(spatialOutDir,paste0('CC_c.tif')), overwrite=TRUE)

ConCCc<-concats(ConCorr_c,CC_c)
#unique(values(ConnCC))
ConnCC_df<-levels(ConCCc)[[1]]

#Read in Look up table
ConnCC_LUT<-data.frame(read_excel(file.path(DataDir,'ConnCC_LUT_5Mar2026.xlsx')))# ,sheet='ConnCC')
#Reformat LUT to long form
ConCC_Assign_LUT<-pivot_longer(ConnCC_LUT, cols=c('VeryLow','Low','Moderate','High','VeryHigh'),names_to='CC') %>%
  unite(Category_Category, c('Conn','CC')) %>%
  left_join(ConnCC_df) %>%
  dplyr::select(ID,value)

#Assign values from LUT to new raster
ConCC<-classify(ConCCc,ConCC_Assign_LUT)
plot(ConCC)
writeRaster(ConCC, filename=file.path(spatialOutDir,paste0('ConCC.tif')), overwrite=TRUE)

unique(values(ConCC))


